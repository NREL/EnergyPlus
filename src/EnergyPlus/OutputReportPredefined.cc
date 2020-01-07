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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/OutputReportPredefined.hh>

namespace EnergyPlus {

namespace OutputReportPredefined {

    // MODULE INFORMATION:
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   August 2006
    //    MODIFIED       na
    //    RE-ENGINEERED  na
    // PURPOSE OF THIS MODULE:
    //    Support the creation of predefined tabular output.
    // METHODOLOGY EMPLOYED:
    // REFERENCES:
    //    None.
    // OTHER NOTES:.
    // Using/Aliasing
    using namespace DataPrecisionGlobals;

    // Data
    // The following section initializes the predefined column heading variables
    // The variables get their value in AssignPredefined

    // Climate Summary Report
    int pdrClim;
    int pdstDesDay;
    int pdchDDmaxDB;
    int pdchDDrange;
    int pdchDDhumid;
    int pdchDDhumTyp;
    int pdchDDwindSp;
    int pdchDDwindDr;
    int pdstWthr;
    int pdchWthrVal;

    // HVAC Equipment Report
    int pdrEquip;
    int pdstMech;
    int pdchMechType;
    int pdchMechNomCap;
    int pdchMechNomEff;
    int pdchMechIPLVSI;
    int pdchMechIPLVIP;
    // Fan subtable
    int pdstFan;
    int pdchFanType;
    int pdchFanTotEff;
    int pdchFanDeltaP;
    int pdchFanVolFlow;
    int pdchFanMotorIn;
    int pdchFanEnergyIndex;
    int pdchFanEndUse;
    int pdchFanPwr;
    int pdchFanPwrPerFlow;
    int pdchFanDesDay;
    int pdchFanPkTime;
    // Pump subtable
    int pdstPump;
    int pdchPumpType;
    int pdchPumpControl;
    int pdchPumpHead;
    int pdchPumpFlow;
    int pdchPumpPower;
    int pdchPumpPwrPerFlow;
    int pdchPumpEndUse;
    int pdchMotEff;
    // Cooling coil subtable
    int pdstCoolCoil;
    int pdchCoolCoilType;
    int pdchCoolCoilDesCap;
    int pdchCoolCoilTotCap;
    int pdchCoolCoilSensCap;
    int pdchCoolCoilLatCap;
    int pdchCoolCoilSHR;
    int pdchCoolCoilNomEff;
    int pdchCoolCoilUATotal;
    int pdchCoolCoilArea;

    // DX Cooling Coil subtable
    int pdstDXCoolCoil;
    int pdchDXCoolCoilType; // DX cooling coil type

    int pdchDXCoolCoilNetCapSI; // Standard Rated (Net) Cooling Capacity [W]
    int pdchDXCoolCoilCOP;      // EER/COP value in SI unit at AHRI std. 340/360 conditions [W/W]
    int pdchDXCoolCoilSEERIP;   // SEER value in IP unit at AHRI std. 210/240 conditions [Btu/W-hr]
    int pdchDXCoolCoilEERIP;    // EER value in IP unit at AHRI std. 340/360 conditions [Btu/W-h]
    int pdchDXCoolCoilIEERIP;   // IEER value in IP unit at AHRI std. 340/360 conditions

    // DX Cooling Coil subtable per ANSI/ASHRAE Std 127 for Tests A, B, C and D
    int pdstDXCoolCoil2;
    int pdchDXCoolCoilNetCapSIA;  // Standard Rated (Net) Cooling Capacity [W], Test A
    int pdchDXCoolCoilElecPowerA; // Standard Rated Electric Power [W], Test A
    int pdchDXCoolCoilNetCapSIB;  // Standard Rated (Net) Cooling Capacity [W], Test B
    int pdchDXCoolCoilElecPowerB; // Standard Rated Electric Power [W], Test B
    int pdchDXCoolCoilNetCapSIC;  // Standard Rated (Net) Cooling Capacity [W], Test C
    int pdchDXCoolCoilElecPowerC; // Standard Rated Electric Power [W], Test C
    int pdchDXCoolCoilNetCapSID;  // Standard Rated (Net) Cooling Capacity [W], Test D
    int pdchDXCoolCoilElecPowerD; // Standard Rated Electric Power [W], Test D

    // VAV DX Cooling Ratings Details
    int pdstVAVDXCoolCoil; // details for Packaged VAV rating under AHRI 340/360
    int pdchVAVDXCoolCoilType;
    int pdchVAVDXFanName;
    int pdchVAVDXCoolCoilNetCapSI;
    int pdchVAVDXCoolCoilCOP;
    int pdchVAVDXCoolCoilIEERIP;
    int pdchVAVDXCoolCoilEERIP;
    int pdchVAVDXCoolCoilMdotA;
    int pdchVAVDXCoolCoilCOP_B;
    int pdchVAVDXCoolCoilEER_B_IP;
    int pdchVAVDXCoolCoilMdotB;
    int pdchVAVDXCoolCoilCOP_C;
    int pdchVAVDXCoolCoilEER_C_IP;
    int pdchVAVDXCoolCoilMdotC;
    int pdchVAVDXCoolCoilCOP_D;
    int pdchVAVDXCoolCoilEER_D_IP;
    int pdchVAVDXCoolCoilMdotD;

    // DX Heating Coil subtable
    int pdstDXHeatCoil;
    int pdchDXHeatCoilType; // DX Heating coil type
    int pdchDXHeatCoilHighCap;
    int pdchDXHeatCoilLowCap;
    int pdchDXHeatCoilHSPFSI;    // HSPF value in SI unit at AHRI std. 340/360 conditions [W/W]
    int pdchDXHeatCoilHSPFIP;    // HSPF value in IP unit at AHRI std. 340/360 conditions [Btu/W-hr]
    int pdchDXHeatCoilRegionNum; // Region number for which HSPF is calculated

    // Heating Coil subtable
    int pdstHeatCoil;
    int pdchHeatCoilType;
    int pdchHeatCoilDesCap;
    int pdchHeatCoilNomCap;
    int pdchHeatCoilNomEff;
    // SWH subtable
    int pdstSWH;
    int pdchSWHType;
    int pdchSWHVol;
    int pdchSWHHeatIn;
    int pdchSWHThEff;
    int pdchSWHRecEff;
    int pdchSWHEnFac;

    // Envelope Report
    int pdrEnvelope;
    int pdstOpaque;
    int pdchOpCons;
    int pdchOpRefl;
    int pdchOpUfactFilm;
    int pdchOpUfactNoFilm;
    int pdchOpGrArea;
    int pdchOpNetArea;
    int pdchOpAzimuth;
    int pdchOpTilt;
    int pdchOpDir;
    int pdstFen;
    int pdchFenCons;
    int pdchFenAreaOf1;
    int pdchFenGlassAreaOf1;
    int pdchFenFrameAreaOf1;
    int pdchFenDividerAreaOf1;
    int pdchFenArea;
    int pdchFenUfact;
    int pdchFenSHGC;
    int pdchFenVisTr;
    int pdchFenFrameConductance;
    int pdchFenDividerConductance;
    int pdchFenSwitchable;
    int pdchFenParent;
    int pdchFenAzimuth;
    int pdchFenTilt;
    int pdchFenDir;
    int pdstDoor;
    int pdchDrCons;
    int pdchDrUfactFilm;
    int pdchDrUfactNoFilm;
    int pdchDrGrArea;
    int pdchDrParent;
    int pdstIntFen;
    int pdchIntFenCons;
    int pdchIntFenAreaOf1;
    // Include these if interzone windows ever get frame and dividers
    // INTEGER :: pdchIntFenGlassAreaOf1
    // INTEGER :: pdchIntFenFrameAreaOf1
    // INTEGER :: pdchIntFenDividerAreaOf1
    // INTEGER :: pdchIntFenFrameConductance
    // INTEGER :: pdchIntFenDividerConductance
    int pdchIntFenArea;
    int pdchIntFenUfact;
    int pdchIntFenSHGC;
    int pdchIntFenVisTr;
    int pdchIntFenParent;

    // Shading Report
    int pdrShading;
    int pdstSunlitFrac;
    int pdchSlfMar21_9;
    int pdchSlfMar21_12;
    int pdchSlfMar21_15;
    int pdchSlfJun21_9;
    int pdchSlfJun21_12;
    int pdchSlfJun21_15;
    int pdchSlfDec21_9;
    int pdchSlfDec21_12;
    int pdchSlfDec21_15;
    int pdstWindowControl;
    int pdchWscName;
    int pdchWscShading;
    int pdchWscShadCons;
    int pdchWscControl;
    int pdchWscGlare;

    // Lighting Report
    int pdrLighting;
    int pdstInLite;
    int pdchInLtZone;
    int pdchInLtDens;
    int pdchInLtArea;
    int pdchInLtPower;
    int pdchInLtEndUse;
    int pdchInLtSchd;
    int pdchInLtAvgHrSchd;
    int pdchInLtAvgHrOper;
    int pdchInLtFullLoadHrs;
    int pdchInLtRetAir;
    int pdchInLtCond;
    int pdchInLtConsump;
    int pdstExtLite;
    int pdchExLtPower;
    int pdchExLtClock;
    int pdchExLtSchd;
    int pdchExLtAvgHrSchd;
    int pdchExLtAvgHrOper;
    int pdchExLtFullLoadHrs;
    int pdchExLtConsump;
    int pdstDaylight;
    int pdchDyLtZone;
    int pdchDyLtCtrlName;
    int pdchDyLtKind;
    int pdchDyLtCtrlType;
    int pdchDyLtFrac;
    int pdchDyLtWInst;
    int pdchDyLtWCtrl;

    // Sizing Report
    int pdrSizing;
    int pdstZoneClSize;
    int pdchZnClCalcDesLd;
    int pdchZnClUserDesLd;
    int pdchZnClUserDesLdPerArea;
    int pdchZnClCalcDesAirFlow;
    int pdchZnClUserDesAirFlow;
    int pdchZnClDesDay;
    int pdchZnClPkTime;
    int pdchZnClPkTstatTemp;
    int pdchZnClPkIndTemp;
    int pdchZnClPkIndHum;
    int pdchZnClPkOATemp;
    int pdchZnClPkOAHum;
    int pdchZnClPkOAMinFlow;
    int pdchZnClPkDOASHeatGain;
    int pdstZoneHtSize;
    int pdchZnHtCalcDesLd;
    int pdchZnHtUserDesLd;
    int pdchZnHtUserDesLdPerArea;
    int pdchZnHtCalcDesAirFlow;
    int pdchZnHtUserDesAirFlow;
    int pdchZnHtDesDay;
    int pdchZnHtPkTime;
    int pdchZnHtPkTstatTemp;
    int pdchZnHtPkIndTemp;
    int pdchZnHtPkIndHum;
    int pdchZnHtPkOATemp;
    int pdchZnHtPkOAHum;
    int pdchZnHtPkOAMinFlow;
    int pdchZnHtPkDOASHeatGain;
    int pdstSystemSize;
    int pdchSysSizCalcClAir;
    int pdchSysSizUserClAir;
    int pdchSysSizCalcHtAir;
    int pdchSysSizUserHtAir;
    int pdchSysSizAdjustedClAir;
    int pdchSysSizAdjustedHtAir;
    int pdchSysSizAdjustedMainAir;
    int pdchSysSizCalcHeatFlowRatio;
    int pdchSysSizUserHeatFlowRatio;
    int pdstPlantSize;
    int pdchPlantSizCalcVdot;
    int pdchPlantSizMeasVdot;
    int pdchPlantSizPrevVdot;
    //	int pdchPlantSizPass;
    int pdchPlantSizCoincYesNo;
    int pdchPlantSizDesDay;
    int pdchPlantSizPkTimeHour;
    int pdchPlantSizPkTimeDayOfSim;
    int pdchPlantSizPkTimeMin;

    // Coil Selection Table
    int pdrCoilSizingDetailsTable;
    int pdstCoilSummaryCoilSelection;
    int pdchCoilLocation;
    int pdchCoilHVACType;
    int pdchCoilHVACName;
    int pdchCoilZoneName;
    int pdchCoilName;
    int pdchCoilType;
    int pdchSysSizingMethCoinc;
    int pdchSysSizingMethCap;
    int pdchSysSizingMethAir;

    int pdchCoilIsCapAutosized;
    int pdchCoilIsAirFlowAutosized;
    int pdchCoilIsWaterFlowAutosized;
    int pdchCoilIsOATreated;
    int pdchCoilFinalTotalCap;
    int pdchCoilFinalSensCap;
    int pdchCoilFinalAirVolFlowRate;
    int pdchCoilFinalPlantVolFlowRate;

    int pdchCoilDDnameSensIdealPeak;
    int pdchCoilDateTimeSensIdealPeak;
    int pdchCoilDDnameTotIdealPeak;
    int pdchCoilDateTimeTotIdealPeak;
    int pdchCoilDDnameAirFlowIdealPeak;
    int pdchCoilDateTimeAirFlowIdealPeak;
    int pdchCoilPeakLoadTypeToSizeOn;
    int pdchCoilTotalCapIdealPeak;
    int pdchCoilSensCapIdealPeak;
    int pdchCoilAirMassFlowIdealPeak;
    int pdchCoilAirVolumeFlowIdealPeak;
    int pdchCoilEntDryBulbIdealPeak;
    int pdchCoilEntWetBulbIdealPeak;
    int pdchCoilEntHumRatIdealPeak;
    int pdchCoilEntEnthalpyIdealPeak;
    int pdchCoilLvgDryBulbIdealPeak;
    int pdchCoilLvgWetBulbIdealPeak;
    int pdchCoilLvgHumRatIdealPeak;
    int pdchCoilLvgEnthalpyIdealPeak;
    int pdchCoilWaterMassFlowIdealPeak;
    int pdchCoilEntWaterTempIdealPeak;
    int pdchCoilLvgWaterTempIdealPeak;
    int pdchCoilWaterDeltaTempIdealPeak;
    int pdchCoilRatedTotalCap;
    int pdchCoilRatedSensCap;
    int pdchCoilOffRatingCapacityModifierIdealPeak;
    int pdchCoilOffRatingCapacityModifierSimPeak;
    int pdchCoilRatedAirMass;
    int pdchCoilRatedEntDryBulb;
    int pdchCoilRatedEntWetBulb;
    int pdchCoilRatedEntHumRat;
    int pdchCoilRatedEntEnthalpy;
    int pdchCoilRatedLvgDryBulb;
    int pdchCoilRatedLvgWetBulb;
    int pdchCoilRatedLvgHumRat;
    int pdchCoilRatedLvgEnthalpy;
    int pdchCoilRatedWaterMass;
    int pdchCoilRatedEntWaterTemp;
    int pdchCoilRatedLvgWaterTemp;
    int pdchCoilRatedWaterDeltaTemp;

    int pdchFanAssociatedWithCoilName;
    int pdchFanAssociatedWithCoilType;
    int pdchFanAssociatedVdotSize;
    int pdchFanAssociatedMdotSize;

    int pdchFanHeatGainIdealPeak;
    int pdchCoilNetTotalCapacityIdealPeak;

    int pdchCoilPlantLoopName;

    int pdchPlantMassFlowMaximum;
    int pdchPlantRetTempDesign;
    int pdchPlantSupTempDesign;
    int pdchPlantDeltaTempDesign;
    int pdchPlantCapacity;
    int pdchCoilCapPrcntPlantCapacity;
    int pdchCoilFlowPrcntPlantFlow;
    int pdchOADryBulbIdealPeak;
    int pdchOAHumRatIdealPeak;
    int pdchOAWetBulbatIdealPeak;
    int pdchOAVolFlowIdealPeak;
    int pdchOAFlowPrcntIdealPeak;
    int pdchOABarometricPressureIdealPeak;

    int pdchAirSysRADryBulbIdealPeak;
    int pdchAirSysRAHumRatIdealPeak;
    int pdchZoneAirDryBulbIdealPeak;
    int pdchZoneAirHumRatIdealPeak;
    int pdchZoneAirRelHumIdealPeak;
    int pdchCoilUA;
    int pdchZoneSensibleLoadIdealPeak;
    int pdchZoneLatentLoadIdealPeak;
    int pdchCoilIdealSizCapOverSimPeak;
    int pdchCoilIdealSizCapUnderSimPeak;
    int pdchReheatCoilMultiplier;
    int pdchFlowCapRatioLowCapIncreaseRatio;
    int pdchFlowCapRatioHiCapDecreaseRatio;
    int pdchPlantFluidSpecificHeat;
    int pdchPlantFluidDensity;
    int pdchMoistAirSpecificHeat;
    int pdchDryAirSpecificHeat;
    int pdchStandRhoAir;

    // Coil Sizing Summary (subset of Coil Selection Table)
    int pdst2CoilSummaryCoilSelection;
    int pdch2CoilName;
    int pdch2CoilType;
    int pdch2CoilHVACType;
    int pdch2CoilHVACName;

    int pdch2CoilFinalTotalCap;
    int pdch2CoilFinalSensCap;
    int pdch2CoilFinalAirVolFlowRate;
    int pdch2CoilFinalPlantVolFlowRate;
    int pdch2CoilUA;

    int pdch2CoilDDnameSensIdealPeak;
    int pdch2CoilDateTimeSensIdealPeak;
    int pdch2CoilDDnameAirFlowIdealPeak;
    int pdch2CoilDateTimeAirFlowIdealPeak;
    int pdch2CoilTotalCapIdealPeak;
    int pdch2CoilSensCapIdealPeak;
    int pdch2CoilAirVolumeFlowIdealPeak;
    int pdch2CoilEntDryBulbIdealPeak;
    int pdch2CoilEntWetBulbIdealPeak;
    int pdch2CoilEntHumRatIdealPeak;
    int pdch2CoilLvgDryBulbIdealPeak;
    int pdch2CoilLvgWetBulbIdealPeak;
    int pdch2CoilLvgHumRatIdealPeak;
    int pdch2CoilRatedTotalCap;
    int pdch2CoilRatedSensCap;

    int pdch2OADryBulbIdealPeak;
    int pdch2OAHumRatIdealPeak;
    int pdch2OAWetBulbatIdealPeak;
    int pdch2OAFlowPrcntIdealPeak;
    int pdch2ZoneAirDryBulbIdealPeak;
    int pdch2ZoneAirHumRatIdealPeak;
    int pdch2ZoneAirRelHumIdealPeak;
    int pdch2ZoneSensibleLoadIdealPeak;
    int pdch2ZoneLatentLoadIdealPeak;

    // System summary
    int pdrSystem;
    int pdstEconomizer;
    int pdchEcoKind;
    int pdchEcoMinOA;
    int pdchEcoMaxOA;
    int pdchEcoRetTemp;
    int pdchEcoRetEnth;
    int pdchEcoOATempLim;
    int pdchEcoOAEnthLim;
    int pdstDemCntlVent;
    int pdchDCVventMechName;
    int pdchDCVperPerson;
    int pdchDCVperArea;
    int pdchDCVperZone;
    int pdchDCVperACH;
    int pdchDCVMethod;
    int pdchDCVOASchName;

    // added for new DCV
    int pdchDCVZoneADEffCooling;
    int pdchDCVZoneADEffHeating;
    int pdchDCVZoneADEffSchName;

    int pdstSimpleComfort;
    int pdchSCwinterClothes;
    int pdchSCsummerClothes;
    int pdchSCeitherClothes;
    int pdstUnmetLoads;
    int pdchULnotMetHeat;
    int pdchULnotMetCool;
    int pdchULnotMetHeatOcc;
    int pdchULnotMetCoolOcc;

    // Outside Air Report
    int pdrOutsideAir;
    int pdstOAavgOcc;
    int pdchOaoAvgNumOcc1;
    int pdchOaoNomNumOcc1;
    int pdchOaoZoneVol1;
    int pdchOaoAvgMechVent;
    int pdchOaoAvgInfil;
    int pdchOaoAvgAFNInfil;
    int pdchOaoAvgSimpVent;
    int pdchOaoAvgTotVent;
    int pdstOAminOcc;
    int pdchOaoAvgNumOcc2;
    int pdchOaoNomNumOcc2;
    int pdchOaoZoneVol2;
    int pdchOaoMinMechVent;
    int pdchOaoMinInfil;
    int pdchOaoMinAFNInfil;
    int pdchOaoMinSimpVent;
    int pdchOaoMinTotVent;

    // Object Count Report
    int pdrObjCnt;
    int pdstSurfCnt;
    int pdchSurfCntTot;
    int pdchSurfCntExt;
    int pdstHVACcnt;
    int pdchHVACcntVal;
    int pdstFieldCnt;
    int pdchFieldCntVal;

    // Energy Meters Report
    int pdrEnergyMeters;

    int pdstEMelecvalues;
    int pdchEMelecannual;
    int pdchEMelecminvalue;
    int pdchEMelecminvaluetime;
    int pdchEMelecmaxvalue;
    int pdchEMelecmaxvaluetime;

    int pdstEMgasvalues;
    int pdchEMgasannual;
    int pdchEMgasminvalue;
    int pdchEMgasminvaluetime;
    int pdchEMgasmaxvalue;
    int pdchEMgasmaxvaluetime;

    int pdstEMcoolvalues;
    int pdchEMcoolannual;
    int pdchEMcoolminvalue;
    int pdchEMcoolminvaluetime;
    int pdchEMcoolmaxvalue;
    int pdchEMcoolmaxvaluetime;

    int pdstEMwatervalues;
    int pdchEMwaterannual;
    int pdchEMwaterminvalue;
    int pdchEMwaterminvaluetime;
    int pdchEMwatermaxvalue;
    int pdchEMwatermaxvaluetime;

    int pdstEMotherJvalues;
    int pdchEMotherJannual;
    int pdchEMotherJminvalue;
    int pdchEMotherJminvaluetime;
    int pdchEMotherJmaxvalue;
    int pdchEMotherJmaxvaluetime;

    int pdstEMotherKGvalues;
    int pdchEMotherKGannual;
    int pdchEMotherKGminvalue;
    int pdchEMotherKGminvaluetime;
    int pdchEMotherKGmaxvalue;
    int pdchEMotherKGmaxvaluetime;

    int pdstEMotherM3values;
    int pdchEMotherM3annual;
    int pdchEMotherM3minvalue;
    int pdchEMotherM3minvaluetime;
    int pdchEMotherM3maxvalue;
    int pdchEMotherM3maxvaluetime;

    int pdstEMotherLvalues;
    int pdchEMotherLannual;
    int pdchEMotherLminvalue;
    int pdchEMotherLminvaluetime;
    int pdchEMotherLmaxvalue;
    int pdchEMotherLmaxvaluetime;

    // Sensible Heat Gas Component Report
    int pdrSensibleGain;
    // annual
    int pdstSHGSannual;
    int pdchSHGSAnHvacHt;
    int pdchSHGSAnHvacCl;
    int pdchSHGSAnHvacATUHt;
    int pdchSHGSAnHvacATUCl;
    int pdchSHGSAnSurfHt;
    int pdchSHGSAnSurfCl;
    int pdchSHGSAnPeoplAdd;
    int pdchSHGSAnLiteAdd;
    int pdchSHGSAnEquipAdd;
    int pdchSHGSAnWindAdd;
    int pdchSHGSAnIzaAdd;
    int pdchSHGSAnInfilAdd;
    int pdchSHGSAnOtherAdd;
    int pdchSHGSAnEquipRem;
    int pdchSHGSAnWindRem;
    int pdchSHGSAnIzaRem;
    int pdchSHGSAnInfilRem;
    int pdchSHGSAnOtherRem;
    // peak cooling
    int pdstSHGSpkCl;
    int pdchSHGSClTimePeak;
    int pdchSHGSClHvacHt;
    int pdchSHGSClHvacCl;
    int pdchSHGSClHvacATUHt;
    int pdchSHGSClHvacATUCl;
    int pdchSHGSClSurfHt;
    int pdchSHGSClSurfCl;
    int pdchSHGSClPeoplAdd;
    int pdchSHGSClLiteAdd;
    int pdchSHGSClEquipAdd;
    int pdchSHGSClWindAdd;
    int pdchSHGSClIzaAdd;
    int pdchSHGSClInfilAdd;
    int pdchSHGSClOtherAdd;
    int pdchSHGSClEquipRem;
    int pdchSHGSClWindRem;
    int pdchSHGSClIzaRem;
    int pdchSHGSClInfilRem;
    int pdchSHGSClOtherRem;
    // peak heating
    int pdstSHGSpkHt;
    int pdchSHGSHtTimePeak;
    int pdchSHGSHtHvacHt;
    int pdchSHGSHtHvacCl;
    int pdchSHGSHtHvacATUHt;
    int pdchSHGSHtHvacATUCl;
    int pdchSHGSHtSurfHt;
    int pdchSHGSHtSurfCl;
    int pdchSHGSHtPeoplAdd;
    int pdchSHGSHtLiteAdd;
    int pdchSHGSHtEquipAdd;
    int pdchSHGSHtWindAdd;
    int pdchSHGSHtIzaAdd;
    int pdchSHGSHtInfilAdd;
    int pdchSHGSHtOtherAdd;
    int pdchSHGSHtEquipRem;
    int pdchSHGSHtWindRem;
    int pdchSHGSHtIzaRem;
    int pdchSHGSHtInfilRem;
    int pdchSHGSHtOtherRem;
    // Standard62Report
    int pdrStd62;
    int pdstS62sysVentReqCool;
    int pdchS62svrClSumVpz;
    int pdchS62svrClPs;
    int pdchS62svrClSumPz;
    int pdchS62svrClD;
    int pdchS62svrClVou;
    int pdchS62svrClVps;
    int pdchS62svrClXs;
    int pdchS62svrClEv;
    int pdchS62svrClVot;
    int pdchS62svrClPercOA;
    int pdchS62svrClEnvironmentOfPs;
    int pdchS62svrClTimeOfPs;

    int pdstS62sysVentReqHeat;
    int pdchS62svrHtSumVpz;
    int pdchS62svrHtPs;
    int pdchS62svrHtSumPz;
    int pdchS62svrHtD;
    int pdchS62svrHtVou;
    int pdchS62svrHtVps;
    int pdchS62svrHtXs;
    int pdchS62svrHtEv;
    int pdchS62svrHtVot;
    int pdchS62svrHtPercOA;
    int pdchS62svrHtEnvironmentOfPs;
    int pdchS62svrHtTimeOfPs;

    int pdstS62znVentPar;
    int pdchS62zvpAlN;
    int pdchS62zvpRp;
    int pdchS62zvpPz;
    int pdchS62zvpRa;
    int pdchS62zvpAz;
    int pdchS62zvpVbz;
    int pdchS62zvpClEz;
    int pdchS62zvpClVoz;
    int pdchS62zvpHtEz;
    int pdchS62zvpHtVoz;

    int pdstS62sysVentPar;
    int pdchS62svpRp;
    int pdchS62svpPz;
    int pdchS62svpRa;
    int pdchS62svpAz;
    int pdchS62svpVbz;
    int pdchS62svpClVoz;
    int pdchS62svpHtVoz;

    int pdstS62znCoolDes;
    int pdchS62zcdAlN;
    int pdchS62zcdBox;
    int pdchS62zcdVpz;
    int pdchS62zcdVps;
    int pdchS62zcdVsec;
    int pdchS62zcdVdz;
    int pdchS62zcdVpzmin;
    int pdchS62zcdVozclg;
    int pdchS62zcdZpz;
    int pdchS62zcdEp;
    int pdchS62zcdEr;
    int pdchS62zcdFa;
    int pdchS62zcdFb;
    int pdchS62zcdFc;
    int pdchS62zcdEvz;

    int pdstS62sysCoolDes;
    int pdchS62scdVpz;
    int pdchS62scdVps;
    int pdchS62scdVsec;
    int pdchS62scdVdz;
    int pdchS62scdVpzmin;
    int pdchS62scdVozclg;
    int pdchS62scdEvz;

    int pdstS62znHeatDes;
    int pdchS62zhdAlN;
    int pdchS62zhdBox;
    int pdchS62zhdVpz;
    int pdchS62zhdVps;
    int pdchS62zhdVsec;
    int pdchS62zhdVdz;
    int pdchS62zhdVpzmin;
    int pdchS62zhdVozhtg;
    int pdchS62zhdZpz;
    int pdchS62zhdEp;
    int pdchS62zhdEr;
    int pdchS62zhdFa;
    int pdchS62zhdFb;
    int pdchS62zhdFc;
    int pdchS62zhdEvz;

    int pdstS62sysHeatDes;
    int pdchS62shdVpz;
    int pdchS62shdVps;
    int pdchS62shdVsec;
    int pdchS62shdVdz;
    int pdchS62shdVpzmin;
    int pdchS62shdVozhtg;
    int pdchS62shdEvz;

    //  LEED Summary
    int pdrLeed;
    int pdstLeedGenInfo;
    int pdchLeedGenData;

    int pdstLeedSpaceUsageType;
    int pdchLeedSutName;
    int pdchLeedSutSpArea;
    int pdchLeedSutOcArea;
    int pdchLeedSutUnArea;
    int pdchLeedSutHrsWeek;

    int pdstLeedAdvsMsg;
    int pdchLeedAmData;

    int pdstLeedEneTypSum;
    int pdchLeedEtsType;
    int pdchLeedEtsRtNm;
    int pdchLeedEtsVirt;
    int pdchLeedEtsEneUnt;
    int pdchLeedEtsDemUnt;

    int pdstLeedPerf;
    int pdchLeedPerfRot;
    int pdchLeedPerfElEneUse;
    int pdchLeedPerfElDem;
    int pdchLeedPerfGasEneUse;
    int pdchLeedPerfGasDem;
    int pdchLeedPerfAddFuelEneUse;
    int pdchLeedPerfAddFuelDem;
    int pdchLeedPerfDisClEneUse;
    int pdchLeedPerfDisClDem;
    int pdchLeedPerfDisHtEneUse;
    int pdchLeedPerfDisHtDem;

    int pdstLeedEneUseSum;
    int pdchLeedEusUnt;
    int pdchLeedEusProc;
    int pdchLeedEusTotal;

    int pdstLeedEneCostSum;
    int pdchLeedEcUnt;
    int pdchLeedEcsProc;
    int pdchLeedEcsTotal;
    Real64 LEEDelecCostTotal;
    Real64 LEEDgasCostTotal;
    Real64 LEEDothrCostTotal;

    int pdstLeedRenewSum;
    int pdchLeedRenRatCap;
    int pdchLeedRenAnGen;

    int pdstLeedEneUseIntEl;
    int pdchLeedEuiElec;
    int pdstLeedEneUseIntNatG;
    int pdchLeedEuiNatG;
    int pdstLeedEneUseIntOthr;
    int pdchLeedEuiOthr;

    int pdstLeedEneUsePerc;
    int pdchLeedEupPerc;

    int pdstLeedEqFlLdHrs;
    int pdchLeedEflhEflh;
    int pdchLeedEflhNonZerHrs;

    int pdstLeedSchedSetPts;
    int pdChLeedSchStPtFirstObjUsed;
    int pdChLeedSchStPtMonthUsed;
    int pdchLeedSchStPt11amWednesday;
    int pdchLeedSchStPt11amWedCnt;
    int pdchLeedSchStPt11pmWednesday;
    int pdchLeedSchStPt11pmWedCnt;

    // Internal data structures to store information provided by calls

    int const sizeIncrement(100);

    int sizeReportName;
    int numReportName;

    int sizeSubTable;
    int numSubTable;

    int sizeColumnTag;
    int numColumnTag;

    int sizeTableEntry;
    int numTableEntry;

    int sizeCompSizeTableEntry(0); // Autodesk Was used uninitialized in output to .audit files
    int numCompSizeTableEntry(0);  // Autodesk Was used uninitialized in WriteComponentSizing

    int sizeShadowRelate;
    int numShadowRelate;
    int const recKindSurface(1);
    int const recKindSubsurface(2);

    Real64 TotalNotMetHeatingOccupiedForABUPS(0.0);
    Real64 TotalNotMetCoolingOccupiedForABUPS(0.0);
    Real64 TotalNotMetOccupiedForABUPS(0.0);
    Real64 TotalTimeNotSimpleASH55EitherForABUPS(0.0);

    // Object Data
    Array1D<reportNameType> reportName;
    Array1D<SubTableType> subTable;
    Array1D<ColumnTagType> columnTag;
    Array1D<TableEntryType> tableEntry;
    Array1D<CompSizeTableEntryType> CompSizeTableEntry;
    Array1D<ShadowRelateType> ShadowRelate;

    // Functions
    void clear_state()
    {
        pdrClim = 0;
        pdstDesDay = 0;
        pdchDDmaxDB = 0;
        pdchDDrange = 0;
        pdchDDhumid = 0;
        pdchDDhumTyp = 0;
        pdchDDwindSp = 0;
        pdchDDwindDr = 0;
        pdstWthr = 0;
        pdchWthrVal = 0;
        pdrEquip = 0;
        pdstMech = 0;
        pdchMechType = 0;
        pdchMechNomCap = 0;
        pdchMechNomEff = 0;
        pdchMechIPLVSI = 0;
        pdchMechIPLVIP = 0;
        pdstFan = 0;
        pdchFanType = 0;
        pdchFanTotEff = 0;
        pdchFanDeltaP = 0;
        pdchFanVolFlow = 0;
        pdchFanMotorIn = 0;
        pdchFanEnergyIndex = 0;
        pdchFanEndUse = 0;
        pdchFanPwr = 0;
        pdchFanPwrPerFlow = 0;
        pdchFanDesDay = 0;
        pdchFanPkTime = 0;
        pdstPump = 0;
        pdchPumpType = 0;
        pdchPumpControl = 0;
        pdchPumpHead = 0;
        pdchPumpFlow = 0;
        pdchPumpPower = 0;
        pdchPumpPwrPerFlow = 0;
        pdchPumpEndUse = 0;
        pdchMotEff = 0;
        pdstCoolCoil = 0;
        pdchCoolCoilType = 0;
        pdchCoolCoilDesCap = 0;
        pdchCoolCoilTotCap = 0;
        pdchCoolCoilSensCap = 0;
        pdchCoolCoilLatCap = 0;
        pdchCoolCoilSHR = 0;
        pdchCoolCoilNomEff = 0;
        pdchCoolCoilUATotal = 0;
        pdchCoolCoilArea = 0;
        pdstDXCoolCoil = 0;
        pdchDXCoolCoilType = 0;     // DX cooling coil type
        pdchDXCoolCoilNetCapSI = 0; // Standard Rated (Net) Cooling Capacity [W]
        pdchDXCoolCoilCOP = 0;      // EER/COP value in SI unit at AHRI std. 340/360 conditions [W/W]
        pdchDXCoolCoilSEERIP = 0;   // SEER value in IP unit at AHRI std. 210/240 conditions [Btu/W-hr]
        pdchDXCoolCoilEERIP = 0;    // EER value in IP unit at AHRI std. 340/360 conditions [Btu/W-h]
        pdchDXCoolCoilIEERIP = 0;   // IEER value in IP unit at AHRI std. 340/360 conditions
        pdstDXCoolCoil2 = 0;
        pdchDXCoolCoilNetCapSIA = 0;  // Standard Rated (Net) Cooling Capacity [W], Test A
        pdchDXCoolCoilElecPowerA = 0; // Standard Rated Electric Power [W], Test A
        pdchDXCoolCoilNetCapSIB = 0;  // Standard Rated (Net) Cooling Capacity [W], Test B
        pdchDXCoolCoilElecPowerB = 0; // Standard Rated Electric Power [W], Test B
        pdchDXCoolCoilNetCapSIC = 0;  // Standard Rated (Net) Cooling Capacity [W], Test C
        pdchDXCoolCoilElecPowerC = 0; // Standard Rated Electric Power [W], Test C
        pdchDXCoolCoilNetCapSID = 0;  // Standard Rated (Net) Cooling Capacity [W], Test D
        pdchDXCoolCoilElecPowerD = 0; // Standard Rated Electric Power [W], Test D
        pdstVAVDXCoolCoil = 0;        // details for Packaged VAV rating under AHRI 340/360
        pdchVAVDXCoolCoilType = 0;
        pdchVAVDXFanName = 0;
        pdchVAVDXCoolCoilNetCapSI = 0;
        pdchVAVDXCoolCoilCOP = 0;
        pdchVAVDXCoolCoilIEERIP = 0;
        pdchVAVDXCoolCoilEERIP = 0;
        pdchVAVDXCoolCoilMdotA = 0;
        pdchVAVDXCoolCoilCOP_B = 0;
        pdchVAVDXCoolCoilEER_B_IP = 0;
        pdchVAVDXCoolCoilMdotB = 0;
        pdchVAVDXCoolCoilCOP_C = 0;
        pdchVAVDXCoolCoilEER_C_IP = 0;
        pdchVAVDXCoolCoilMdotC = 0;
        pdchVAVDXCoolCoilCOP_D = 0;
        pdchVAVDXCoolCoilEER_D_IP = 0;
        pdchVAVDXCoolCoilMdotD = 0;
        pdstDXHeatCoil = 0;
        pdchDXHeatCoilType = 0; // DX Heating coil type
        pdchDXHeatCoilHighCap = 0;
        pdchDXHeatCoilLowCap = 0;
        pdchDXHeatCoilHSPFSI = 0;    // HSPF value in SI unit at AHRI std. 340/360 conditions [W/W]
        pdchDXHeatCoilHSPFIP = 0;    // HSPF value in IP unit at AHRI std. 340/360 conditions [Btu/W-hr]
        pdchDXHeatCoilRegionNum = 0; // Region number for which HSPF is calculated
        pdstHeatCoil = 0;
        pdchHeatCoilType = 0;
        pdchHeatCoilDesCap = 0;
        pdchHeatCoilNomCap = 0;
        pdchHeatCoilNomEff = 0;
        pdstSWH = 0;
        pdchSWHType = 0;
        pdchSWHVol = 0;
        pdchSWHHeatIn = 0;
        pdchSWHThEff = 0;
        pdchSWHRecEff = 0;
        pdchSWHEnFac = 0;
        pdrEnvelope = 0;
        pdstOpaque = 0;
        pdchOpCons = 0;
        pdchOpRefl = 0;
        pdchOpUfactFilm = 0;
        pdchOpUfactNoFilm = 0;
        pdchOpGrArea = 0;
        pdchOpNetArea = 0;
        pdchOpAzimuth = 0;
        pdchOpTilt = 0;
        pdchOpDir = 0;
        pdstFen = 0;
        pdchFenCons = 0;
        pdchFenAreaOf1 = 0;
        pdchFenGlassAreaOf1 = 0;
        pdchFenFrameAreaOf1 = 0;
        pdchFenDividerAreaOf1 = 0;
        pdchFenArea = 0;
        pdchFenUfact = 0;
        pdchFenSHGC = 0;
        pdchFenVisTr = 0;
        pdchFenFrameConductance = 0;
        pdchFenDividerConductance = 0;
        pdchFenSwitchable = 0;
        pdchFenParent = 0;
        pdchFenAzimuth = 0;
        pdchFenTilt = 0;
        pdchFenDir = 0;
        pdstDoor = 0;
        pdchDrCons = 0;
        pdchDrUfactFilm = 0;
        pdchDrUfactNoFilm = 0;
        pdchDrGrArea = 0;
        pdchDrParent = 0;
        pdstIntFen = 0;
        pdchIntFenCons = 0;
        pdchIntFenAreaOf1 = 0;
        pdchIntFenArea = 0;
        pdchIntFenUfact = 0;
        pdchIntFenSHGC = 0;
        pdchIntFenVisTr = 0;
        pdchIntFenParent = 0;
        pdrShading = 0;
        pdstSunlitFrac = 0;
        pdchSlfMar21_9 = 0;
        pdchSlfMar21_12 = 0;
        pdchSlfMar21_15 = 0;
        pdchSlfJun21_9 = 0;
        pdchSlfJun21_12 = 0;
        pdchSlfJun21_15 = 0;
        pdchSlfDec21_9 = 0;
        pdchSlfDec21_12 = 0;
        pdchSlfDec21_15 = 0;
        pdstWindowControl = 0;
        pdchWscName = 0;
        pdchWscShading = 0;
        pdchWscShadCons = 0;
        pdchWscControl = 0;
        pdchWscGlare = 0;
        pdrLighting = 0;
        pdstInLite = 0;
        pdchInLtZone = 0;
        pdchInLtDens = 0;
        pdchInLtArea = 0;
        pdchInLtPower = 0;
        pdchInLtEndUse = 0;
        pdchInLtSchd = 0;
        pdchInLtAvgHrSchd = 0;
        pdchInLtAvgHrOper = 0;
        pdchInLtFullLoadHrs = 0;
        pdchInLtRetAir = 0;
        pdchInLtCond = 0;
        pdchInLtConsump = 0;
        pdstExtLite = 0;
        pdchExLtPower = 0;
        pdchExLtClock = 0;
        pdchExLtSchd = 0;
        pdchExLtAvgHrSchd = 0;
        pdchExLtAvgHrOper = 0;
        pdchExLtFullLoadHrs = 0;
        pdchExLtConsump = 0;
        pdstDaylight = 0;
        pdchDyLtZone = 0;
        pdchDyLtCtrlName = 0;
        pdchDyLtKind = 0;
        pdchDyLtCtrlType = 0;
        pdchDyLtFrac = 0;
        pdchDyLtWInst = 0;
        pdchDyLtWCtrl = 0;
        pdrSizing = 0;
        pdstZoneClSize = 0;
        pdchZnClCalcDesLd = 0;
        pdchZnClUserDesLd = 0;
        pdchZnClUserDesLdPerArea = 0;
        pdchZnClCalcDesAirFlow = 0;
        pdchZnClUserDesAirFlow = 0;
        pdchZnClDesDay = 0;
        pdchZnClPkTime = 0;
        pdchZnClPkTstatTemp = 0;
        pdchZnClPkIndTemp = 0;
        pdchZnClPkIndHum = 0;
        pdchZnClPkOATemp = 0;
        pdchZnClPkOAHum = 0;
        pdstZoneHtSize = 0;
        pdchZnHtCalcDesLd = 0;
        pdchZnHtUserDesLd = 0;
        pdchZnHtUserDesLdPerArea = 0;
        pdchZnHtCalcDesAirFlow = 0;
        pdchZnHtUserDesAirFlow = 0;
        pdchZnHtDesDay = 0;
        pdchZnHtPkTime = 0;
        pdchZnHtPkTstatTemp = 0;
        pdchZnHtPkIndTemp = 0;
        pdchZnHtPkIndHum = 0;
        pdchZnHtPkOATemp = 0;
        pdchZnHtPkOAHum = 0;
        pdstSystemSize = 0;
        pdchSysSizCalcClAir = 0;
        pdchSysSizUserClAir = 0;
        pdchSysSizCalcHtAir = 0;
        pdchSysSizUserHtAir = 0;
        pdchSysSizAdjustedClAir = 0;
        pdchSysSizAdjustedHtAir = 0;
        pdchSysSizAdjustedMainAir = 0;
        pdchSysSizCalcHeatFlowRatio = 0;
        pdchSysSizUserHeatFlowRatio = 0;
        pdstPlantSize = 0;
        pdchPlantSizCalcVdot = 0;
        pdchPlantSizMeasVdot = 0;
        pdchPlantSizPrevVdot = 0;
        pdchPlantSizCoincYesNo = 0;
        pdchPlantSizDesDay = 0;
        pdchPlantSizPkTimeHour = 0;
        pdchPlantSizPkTimeDayOfSim = 0;
        pdchPlantSizPkTimeMin = 0;

        pdrCoilSizingDetailsTable = 0;
        pdstCoilSummaryCoilSelection = 0;
        pdchCoilLocation = 0;
        pdchCoilHVACType = 0;
        pdchCoilHVACName = 0;
        pdchCoilZoneName = 0;
        pdchCoilName = 0;
        pdchCoilType = 0;
        pdchSysSizingMethCoinc = 0;
        pdchSysSizingMethCap = 0;
        pdchSysSizingMethAir = 0;
        pdchCoilIsCapAutosized = 0;
        pdchCoilIsAirFlowAutosized = 0;
        pdchCoilIsWaterFlowAutosized = 0;
        pdchCoilIsOATreated = 0;
        pdchCoilFinalTotalCap = 0;
        pdchCoilFinalSensCap = 0;
        pdchCoilFinalAirVolFlowRate = 0;
        pdchCoilFinalPlantVolFlowRate = 0;
        pdchCoilDDnameSensIdealPeak = 0;
        pdchCoilDateTimeSensIdealPeak = 0;
        pdchCoilDDnameTotIdealPeak = 0;
        pdchCoilDateTimeTotIdealPeak = 0;
        pdchCoilDDnameAirFlowIdealPeak = 0;
        pdchCoilDateTimeAirFlowIdealPeak = 0;
        pdchCoilPeakLoadTypeToSizeOn = 0;
        pdchCoilTotalCapIdealPeak = 0;
        pdchCoilSensCapIdealPeak = 0;
        pdchCoilAirMassFlowIdealPeak = 0;
        pdchCoilAirVolumeFlowIdealPeak = 0;
        pdchCoilEntDryBulbIdealPeak = 0;
        pdchCoilEntWetBulbIdealPeak = 0;
        pdchCoilEntHumRatIdealPeak = 0;
        pdchCoilEntEnthalpyIdealPeak = 0;
        pdchCoilLvgDryBulbIdealPeak = 0;
        pdchCoilLvgWetBulbIdealPeak = 0;
        pdchCoilLvgHumRatIdealPeak = 0;
        pdchCoilLvgEnthalpyIdealPeak = 0;
        pdchCoilWaterMassFlowIdealPeak = 0;
        pdchCoilEntWaterTempIdealPeak = 0;
        pdchCoilLvgWaterTempIdealPeak = 0;
        pdchCoilWaterDeltaTempIdealPeak = 0;
        pdchCoilRatedTotalCap = 0;
        pdchCoilRatedSensCap = 0;
        pdchCoilOffRatingCapacityModifierIdealPeak = 0;

        pdchCoilRatedAirMass = 0;
        pdchCoilRatedEntDryBulb = 0;
        pdchCoilRatedEntWetBulb = 0;
        pdchCoilRatedEntHumRat = 0;
        pdchCoilRatedEntEnthalpy = 0;
        pdchCoilRatedLvgDryBulb = 0;
        pdchCoilRatedLvgWetBulb = 0;
        pdchCoilRatedLvgHumRat = 0;
        pdchCoilRatedLvgEnthalpy = 0;
        pdchCoilRatedWaterMass = 0;
        pdchCoilRatedEntWaterTemp = 0;
        pdchCoilRatedLvgWaterTemp = 0;
        pdchCoilRatedWaterDeltaTemp = 0;

        pdchFanAssociatedWithCoilName = 0;
        pdchFanAssociatedWithCoilType = 0;
        pdchFanAssociatedVdotSize = 0;
        pdchFanAssociatedMdotSize = 0;

        pdchFanHeatGainIdealPeak = 0;
        pdchCoilNetTotalCapacityIdealPeak = 0;

        pdchCoilPlantLoopName = 0;

        pdchPlantMassFlowMaximum = 0;
        pdchPlantRetTempDesign = 0;
        pdchPlantSupTempDesign = 0;
        pdchPlantDeltaTempDesign = 0;
        pdchPlantCapacity = 0;
        pdchCoilCapPrcntPlantCapacity = 0;
        pdchCoilFlowPrcntPlantFlow = 0;
        pdchOADryBulbIdealPeak = 0;
        pdchOAWetBulbatIdealPeak = 0;
        pdchOAHumRatIdealPeak = 0;
        pdchOAVolFlowIdealPeak = 0;
        pdchOAFlowPrcntIdealPeak = 0;
        pdchAirSysRADryBulbIdealPeak = 0;
        pdchAirSysRAHumRatIdealPeak = 0;
        pdchZoneAirDryBulbIdealPeak = 0;
        pdchZoneAirHumRatIdealPeak = 0;
        pdchZoneAirRelHumIdealPeak = 0;
        pdchCoilUA = 0;
        pdchZoneSensibleLoadIdealPeak = 0;
        pdchZoneLatentLoadIdealPeak = 0;

        pdchReheatCoilMultiplier = 0;
        pdchFlowCapRatioLowCapIncreaseRatio = 0;
        pdchFlowCapRatioHiCapDecreaseRatio = 0;
        pdchPlantFluidSpecificHeat = 0;
        pdchPlantFluidDensity = 0;
        pdchMoistAirSpecificHeat = 0;
        pdchDryAirSpecificHeat = 0;
        pdchStandRhoAir = 0;

        // Coil Sizing Summary (subset of Coil Selection Table)
        pdst2CoilSummaryCoilSelection = 0;
        pdch2CoilName = 0;
        pdch2CoilType = 0;
        pdch2CoilHVACType = 0;
        pdch2CoilHVACName = 0;

        pdch2CoilFinalTotalCap = 0;
        pdch2CoilFinalSensCap = 0;
        pdch2CoilFinalAirVolFlowRate = 0;
        pdch2CoilFinalPlantVolFlowRate = 0;
        pdch2CoilUA = 0;

        pdch2CoilDDnameSensIdealPeak = 0;
        pdch2CoilDateTimeSensIdealPeak = 0;
        pdch2CoilDDnameAirFlowIdealPeak = 0;
        pdch2CoilDateTimeAirFlowIdealPeak = 0;
        pdch2CoilTotalCapIdealPeak = 0;
        pdch2CoilSensCapIdealPeak = 0;
        pdch2CoilAirVolumeFlowIdealPeak = 0;
        pdch2CoilEntDryBulbIdealPeak = 0;
        pdch2CoilEntWetBulbIdealPeak = 0;
        pdch2CoilEntHumRatIdealPeak = 0;
        pdch2CoilLvgDryBulbIdealPeak = 0;
        pdch2CoilLvgWetBulbIdealPeak = 0;
        pdch2CoilLvgHumRatIdealPeak = 0;
        pdch2CoilRatedTotalCap = 0;
        pdch2CoilRatedSensCap = 0;

        pdch2OADryBulbIdealPeak = 0;
        pdch2OAHumRatIdealPeak = 0;
        pdch2OAWetBulbatIdealPeak = 0;
        pdch2OAFlowPrcntIdealPeak = 0;
        pdch2ZoneAirDryBulbIdealPeak = 0;
        pdch2ZoneAirHumRatIdealPeak = 0;
        pdch2ZoneAirRelHumIdealPeak = 0;
        pdch2ZoneSensibleLoadIdealPeak = 0;
        pdch2ZoneLatentLoadIdealPeak = 0;

        pdrSystem = 0;
        pdstEconomizer = 0;
        pdchEcoKind = 0;
        pdchEcoMinOA = 0;
        pdchEcoMaxOA = 0;
        pdchEcoRetTemp = 0;
        pdchEcoRetEnth = 0;
        pdchEcoOATempLim = 0;
        pdchEcoOAEnthLim = 0;
        pdstDemCntlVent = 0;
        pdchDCVventMechName = 0;
        pdchDCVperPerson = 0;
        pdchDCVperArea = 0;
        pdchDCVperZone = 0;
        pdchDCVperACH = 0;
        pdchDCVMethod = 0;
        pdchDCVOASchName = 0;
        pdchDCVZoneADEffCooling = 0;
        pdchDCVZoneADEffHeating = 0;
        pdchDCVZoneADEffSchName = 0;
        pdstSimpleComfort = 0;
        pdchSCwinterClothes = 0;
        pdchSCsummerClothes = 0;
        pdchSCeitherClothes = 0;
        pdstUnmetLoads = 0;
        pdchULnotMetHeat = 0;
        pdchULnotMetCool = 0;
        pdchULnotMetHeatOcc = 0;
        pdchULnotMetCoolOcc = 0;
        pdrOutsideAir = 0;
        pdstOAavgOcc = 0;
        pdchOaoAvgNumOcc1 = 0;
        pdchOaoNomNumOcc1 = 0;
        pdchOaoZoneVol1 = 0;
        pdchOaoAvgMechVent = 0;
        pdchOaoAvgInfil = 0;
        pdchOaoAvgAFNInfil = 0;
        pdchOaoAvgSimpVent = 0;
        pdchOaoAvgTotVent = 0;
        pdstOAminOcc = 0;
        pdchOaoAvgNumOcc2 = 0;
        pdchOaoNomNumOcc2 = 0;
        pdchOaoZoneVol2 = 0;
        pdchOaoMinMechVent = 0;
        pdchOaoMinInfil = 0;
        pdchOaoMinAFNInfil = 0;
        pdchOaoMinSimpVent = 0;
        pdchOaoMinTotVent = 0;
        pdrObjCnt = 0;
        pdstSurfCnt = 0;
        pdchSurfCntTot = 0;
        pdchSurfCntExt = 0;
        pdstHVACcnt = 0;
        pdchHVACcntVal = 0;
        pdstFieldCnt = 0;
        pdchFieldCntVal = 0;
        pdrEnergyMeters = 0;
        pdstEMelecvalues = 0;
        pdchEMelecannual = 0;
        pdchEMelecminvalue = 0;
        pdchEMelecminvaluetime = 0;
        pdchEMelecmaxvalue = 0;
        pdchEMelecmaxvaluetime = 0;
        pdstEMgasvalues = 0;
        pdchEMgasannual = 0;
        pdchEMgasminvalue = 0;
        pdchEMgasminvaluetime = 0;
        pdchEMgasmaxvalue = 0;
        pdchEMgasmaxvaluetime = 0;
        pdstEMcoolvalues = 0;
        pdchEMcoolannual = 0;
        pdchEMcoolminvalue = 0;
        pdchEMcoolminvaluetime = 0;
        pdchEMcoolmaxvalue = 0;
        pdchEMcoolmaxvaluetime = 0;
        pdstEMwatervalues = 0;
        pdchEMwaterannual = 0;
        pdchEMwaterminvalue = 0;
        pdchEMwaterminvaluetime = 0;
        pdchEMwatermaxvalue = 0;
        pdchEMwatermaxvaluetime = 0;
        pdstEMotherJvalues = 0;
        pdchEMotherJannual = 0;
        pdchEMotherJminvalue = 0;
        pdchEMotherJminvaluetime = 0;
        pdchEMotherJmaxvalue = 0;
        pdchEMotherJmaxvaluetime = 0;
        pdstEMotherKGvalues = 0;
        pdchEMotherKGannual = 0;
        pdchEMotherKGminvalue = 0;
        pdchEMotherKGminvaluetime = 0;
        pdchEMotherKGmaxvalue = 0;
        pdchEMotherKGmaxvaluetime = 0;
        pdstEMotherM3values = 0;
        pdchEMotherM3annual = 0;
        pdchEMotherM3minvalue = 0;
        pdchEMotherM3minvaluetime = 0;
        pdchEMotherM3maxvalue = 0;
        pdchEMotherM3maxvaluetime = 0;
        pdstEMotherLvalues = 0;
        pdchEMotherLannual = 0;
        pdchEMotherLminvalue = 0;
        pdchEMotherLminvaluetime = 0;
        pdchEMotherLmaxvalue = 0;
        pdchEMotherLmaxvaluetime = 0;
        pdrSensibleGain = 0;
        pdstSHGSannual = 0;
        pdchSHGSAnHvacHt = 0;
        pdchSHGSAnHvacCl = 0;
        pdchSHGSAnSurfHt = 0;
        pdchSHGSAnSurfCl = 0;
        pdchSHGSAnPeoplAdd = 0;
        pdchSHGSAnLiteAdd = 0;
        pdchSHGSAnEquipAdd = 0;
        pdchSHGSAnWindAdd = 0;
        pdchSHGSAnIzaAdd = 0;
        pdchSHGSAnInfilAdd = 0;
        pdchSHGSAnOtherAdd = 0;
        pdchSHGSAnEquipRem = 0;
        pdchSHGSAnWindRem = 0;
        pdchSHGSAnIzaRem = 0;
        pdchSHGSAnInfilRem = 0;
        pdchSHGSAnOtherRem = 0;
        pdstSHGSpkCl = 0;
        pdchSHGSClTimePeak = 0;
        pdchSHGSClHvacHt = 0;
        pdchSHGSClHvacCl = 0;
        pdchSHGSClSurfHt = 0;
        pdchSHGSClSurfCl = 0;
        pdchSHGSClPeoplAdd = 0;
        pdchSHGSClLiteAdd = 0;
        pdchSHGSClEquipAdd = 0;
        pdchSHGSClWindAdd = 0;
        pdchSHGSClIzaAdd = 0;
        pdchSHGSClInfilAdd = 0;
        pdchSHGSClOtherAdd = 0;
        pdchSHGSClEquipRem = 0;
        pdchSHGSClWindRem = 0;
        pdchSHGSClIzaRem = 0;
        pdchSHGSClInfilRem = 0;
        pdchSHGSClOtherRem = 0;
        pdstSHGSpkHt = 0;
        pdchSHGSHtTimePeak = 0;
        pdchSHGSHtHvacHt = 0;
        pdchSHGSHtHvacCl = 0;
        pdchSHGSHtSurfHt = 0;
        pdchSHGSHtSurfCl = 0;
        pdchSHGSHtPeoplAdd = 0;
        pdchSHGSHtLiteAdd = 0;
        pdchSHGSHtEquipAdd = 0;
        pdchSHGSHtWindAdd = 0;
        pdchSHGSHtIzaAdd = 0;
        pdchSHGSHtInfilAdd = 0;
        pdchSHGSHtOtherAdd = 0;
        pdchSHGSHtEquipRem = 0;
        pdchSHGSHtWindRem = 0;
        pdchSHGSHtIzaRem = 0;
        pdchSHGSHtInfilRem = 0;
        pdchSHGSHtOtherRem = 0;
        pdrStd62 = 0;
        pdstS62sysVentReqCool = 0;
        pdchS62svrClSumVpz = 0;
        pdchS62svrClPs = 0;
        pdchS62svrClSumPz = 0;
        pdchS62svrClD = 0;
        pdchS62svrClVou = 0;
        pdchS62svrClVps = 0;
        pdchS62svrClXs = 0;
        pdchS62svrClEv = 0;
        pdchS62svrClVot = 0;
        pdchS62svrClPercOA = 0;
        pdchS62svrClEnvironmentOfPs = 0;
        pdchS62svrClTimeOfPs = 0;

        pdstS62sysVentReqHeat = 0;
        pdchS62svrHtSumVpz = 0;
        pdchS62svrHtPs = 0;
        pdchS62svrHtSumPz = 0;
        pdchS62svrHtD = 0;
        pdchS62svrHtVou = 0;
        pdchS62svrHtVps = 0;
        pdchS62svrHtXs = 0;
        pdchS62svrHtEv = 0;
        pdchS62svrHtVot = 0;
        pdchS62svrHtPercOA = 0;
        pdchS62svrHtEnvironmentOfPs = 0;
        pdchS62svrHtTimeOfPs = 0;

        pdstS62znVentPar = 0;
        pdchS62zvpAlN = 0;
        pdchS62zvpRp = 0;
        pdchS62zvpPz = 0;
        pdchS62zvpRa = 0;
        pdchS62zvpAz = 0;
        pdchS62zvpVbz = 0;
        pdchS62zvpClEz = 0;
        pdchS62zvpClVoz = 0;
        pdchS62zvpHtEz = 0;
        pdchS62zvpHtVoz = 0;
        pdstS62sysVentPar = 0;
        pdchS62svpRp = 0;
        pdchS62svpPz = 0;
        pdchS62svpRa = 0;
        pdchS62svpAz = 0;
        pdchS62svpVbz = 0;
        pdchS62svpClVoz = 0;
        pdchS62svpHtVoz = 0;
        pdstS62znCoolDes = 0;
        pdchS62zcdAlN = 0;
        pdchS62zcdBox = 0;
        pdchS62zcdVpz = 0;
        pdchS62zcdVps = 0;
        pdchS62zcdVsec = 0;
        pdchS62zcdVdz = 0;
        pdchS62zcdVpzmin = 0;
        pdchS62zcdVozclg = 0;
        pdchS62zcdZpz = 0;
        pdchS62zcdEp = 0;
        pdchS62zcdEr = 0;
        pdchS62zcdFa = 0;
        pdchS62zcdFb = 0;
        pdchS62zcdFc = 0;
        pdchS62zcdEvz = 0;
        pdstS62sysCoolDes = 0;
        pdchS62scdVpz = 0;
        pdchS62scdVps = 0;
        pdchS62scdVsec = 0;
        pdchS62scdVdz = 0;
        pdchS62scdVpzmin = 0;
        pdchS62scdVozclg = 0;
        pdchS62scdEvz = 0;
        pdstS62znHeatDes = 0;
        pdchS62zhdAlN = 0;
        pdchS62zhdBox = 0;
        pdchS62zhdVpz = 0;
        pdchS62zhdVps = 0;
        pdchS62zhdVsec = 0;
        pdchS62zhdVdz = 0;
        pdchS62zhdVpzmin = 0;
        pdchS62zhdVozhtg = 0;
        pdchS62zhdZpz = 0;
        pdchS62zhdEp = 0;
        pdchS62zhdEr = 0;
        pdchS62zhdFa = 0;
        pdchS62zhdFb = 0;
        pdchS62zhdFc = 0;
        pdchS62zhdEvz = 0;
        pdstS62sysHeatDes = 0;
        pdchS62shdVpz = 0;
        pdchS62shdVps = 0;
        pdchS62shdVsec = 0;
        pdchS62shdVdz = 0;
        pdchS62shdVpzmin = 0;
        pdchS62shdVozhtg = 0;
        pdchS62shdEvz = 0;
        pdrLeed = 0;
        pdstLeedGenInfo = 0;
        pdchLeedGenData = 0;
        pdstLeedSpaceUsageType = 0;
        pdchLeedSutName = 0;
        pdchLeedSutSpArea = 0;
        pdchLeedSutOcArea = 0;
        pdchLeedSutUnArea = 0;
        pdchLeedSutHrsWeek = 0;
        pdstLeedAdvsMsg = 0;
        pdchLeedAmData = 0;
        pdstLeedEneTypSum = 0;
        pdchLeedEtsType = 0;
        pdchLeedEtsRtNm = 0;
        pdchLeedEtsVirt = 0;
        pdchLeedEtsEneUnt = 0;
        pdchLeedEtsDemUnt = 0;
        pdstLeedPerf = 0;
        pdchLeedPerfRot = 0;
        pdchLeedPerfElEneUse = 0;
        pdchLeedPerfElDem = 0;
        pdchLeedPerfGasEneUse = 0;
        pdchLeedPerfGasDem = 0;
        pdchLeedPerfAddFuelEneUse = 0;
        pdchLeedPerfAddFuelDem = 0;
        pdchLeedPerfDisClEneUse = 0;
        pdchLeedPerfDisClDem = 0;
        pdchLeedPerfDisHtEneUse = 0;
        pdchLeedPerfDisHtDem = 0;
        pdstLeedEneUseSum = 0;
        pdchLeedEusUnt = 0;
        pdchLeedEusProc = 0;
        pdchLeedEusTotal = 0;
        pdstLeedEneCostSum = 0;
        pdchLeedEcUnt = 0;
        pdchLeedEcsProc = 0;
        pdchLeedEcsTotal = 0;
        LEEDelecCostTotal = 0.0;
        LEEDgasCostTotal = 0.0;
        LEEDothrCostTotal = 0.0;
        pdstLeedRenewSum = 0;
        pdchLeedRenRatCap = 0;
        pdchLeedRenAnGen = 0;
        pdstLeedEneUseIntEl = 0;
        pdchLeedEuiElec = 0;
        pdstLeedEneUseIntNatG = 0;
        pdchLeedEuiNatG = 0;
        pdstLeedEneUseIntOthr = 0;
        pdchLeedEuiOthr = 0;
        pdstLeedEneUsePerc = 0;
        pdchLeedEupPerc = 0;
        sizeReportName = 0;
        numReportName = 0;
        sizeSubTable = 0;
        numSubTable = 0;
        sizeColumnTag = 0;
        numColumnTag = 0;
        sizeTableEntry = 0;
        numTableEntry = 0;
        sizeCompSizeTableEntry = 0; // Autodesk Was used uninitialized in output to .audit files
        numCompSizeTableEntry = 0;  // Autodesk Was used uninitialized in WriteComponentSizing
        sizeShadowRelate = 0;
        numShadowRelate = 0;
        TotalNotMetHeatingOccupiedForABUPS = 0.0;
        TotalNotMetCoolingOccupiedForABUPS = 0.0;
        TotalNotMetOccupiedForABUPS = 0.0;
        TotalTimeNotSimpleASH55EitherForABUPS = 0.0;
        reportName.deallocate();
        subTable.deallocate();
        columnTag.deallocate();
        tableEntry.deallocate();
        CompSizeTableEntry.deallocate();
        ShadowRelate.deallocate();
    }

    void SetPredefinedTables()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   August 2006
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   Creates the structure of the predefined reports
        //   including the name and abreviation of the report
        //   the subtables involved and the column headings.
        //   The variables defined for the columns are then
        //   used throughout the program to assign values
        //   to the subtables.

        // METHODOLOGY EMPLOYED:
        //   Simple assignments to public variables.

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataGlobals::DoSystemSizing;
        using DataGlobals::DoZoneSizing;

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // Climate Summary Report
        pdrClim = newPreDefReport("ClimaticDataSummary", "Clim", "Climatic Data Summary");

        pdstDesDay = newPreDefSubTable(pdrClim, "SizingPeriod:DesignDay");

        pdchDDmaxDB = newPreDefColumn(pdstDesDay, "Maximum Dry Bulb [C]");
        pdchDDrange = newPreDefColumn(pdstDesDay, "Daily Temperature Range [deltaC]");
        pdchDDhumid = newPreDefColumn(pdstDesDay, "Humidity Value");
        pdchDDhumTyp = newPreDefColumn(pdstDesDay, "Humidity Type");
        pdchDDwindSp = newPreDefColumn(pdstDesDay, "Wind Speed [m/s]");
        pdchDDwindDr = newPreDefColumn(pdstDesDay, "Wind Direction");

        pdstWthr = newPreDefSubTable(pdrClim, "Weather Statistics File");
        pdchWthrVal = newPreDefColumn(pdstWthr, "Value");

        // Envelope Report

        pdrEnvelope = newPreDefReport("EnvelopeSummary", "Env", "Envelope Summary");

        pdstOpaque = newPreDefSubTable(pdrEnvelope, "Opaque Exterior");

        pdchOpCons = newPreDefColumn(pdstOpaque, "Construction");
        pdchOpRefl = newPreDefColumn(pdstOpaque, "Reflectance");
        pdchOpUfactFilm = newPreDefColumn(pdstOpaque, "U-Factor with Film [W/m2-K]");
        pdchOpUfactNoFilm = newPreDefColumn(pdstOpaque, "U-Factor no Film [W/m2-K]");
        pdchOpGrArea = newPreDefColumn(pdstOpaque, "Gross Area [m2]");
        pdchOpNetArea = newPreDefColumn(pdstOpaque, "Net Area [m2]");
        pdchOpAzimuth = newPreDefColumn(pdstOpaque, "Azimuth [deg]");
        pdchOpTilt = newPreDefColumn(pdstOpaque, "Tilt [deg]");
        pdchOpDir = newPreDefColumn(pdstOpaque, "Cardinal Direction");

        pdstFen = newPreDefSubTable(pdrEnvelope, "Exterior Fenestration");

        pdchFenCons = newPreDefColumn(pdstFen, "Construction");
        pdchFenGlassAreaOf1 = newPreDefColumn(pdstFen, "Glass Area [m2]");
        pdchFenFrameAreaOf1 = newPreDefColumn(pdstFen, "Frame Area [m2]");
        pdchFenDividerAreaOf1 = newPreDefColumn(pdstFen, "Divider Area [m2]");
        pdchFenAreaOf1 = newPreDefColumn(pdstFen, "Area of One Opening [m2]");
        pdchFenArea = newPreDefColumn(pdstFen, "Area of Multiplied Openings [m2]");
        pdchFenUfact = newPreDefColumn(pdstFen, "Glass U-Factor [W/m2-K]");
        pdchFenSHGC = newPreDefColumn(pdstFen, "Glass SHGC");
        pdchFenVisTr = newPreDefColumn(pdstFen, "Glass Visible Transmittance");
        pdchFenFrameConductance = newPreDefColumn(pdstFen, "Frame Conductance [W/m2-K]");
        pdchFenDividerConductance = newPreDefColumn(pdstFen, "Divider Conductance [W/m2-K]");
        pdchFenSwitchable = newPreDefColumn(pdstFen, "Shade Control");
        pdchFenParent = newPreDefColumn(pdstFen, "Parent Surface");
        pdchFenAzimuth = newPreDefColumn(pdstFen, "Azimuth [deg]");
        pdchFenTilt = newPreDefColumn(pdstFen, "Tilt [deg]");
        pdchFenDir = newPreDefColumn(pdstFen, "Cardinal Direction");

        pdstIntFen = newPreDefSubTable(pdrEnvelope, "Interior Fenestration");

        pdchIntFenCons = newPreDefColumn(pdstIntFen, "Construction");
        pdchIntFenAreaOf1 = newPreDefColumn(pdstIntFen, "Area of One Opening [m2]");
        pdchIntFenArea = newPreDefColumn(pdstIntFen, "Area of Openings [m2]");
        pdchIntFenUfact = newPreDefColumn(pdstIntFen, "Glass U-Factor [W/m2-K]");
        pdchIntFenSHGC = newPreDefColumn(pdstIntFen, "Glass SHGC");
        pdchIntFenVisTr = newPreDefColumn(pdstIntFen, "Glass Visible Transmittance");
        // pdchIntFenGlassAreaOf1 =   newPreDefColumn(pdstIntFen,'Glass Area [m2]')
        // pdchIntFenFrameAreaOf1 =   newPreDefColumn(pdstIntFen,'Frame Area [m2]')
        // pdchIntFenDividerAreaOf1 =   newPreDefColumn(pdstIntFen,'Divider Area [m2]')
        // pdchIntFenFrameConductance =  newPreDefColumn(pdstIntFen,'Frame Conductance [W/m2-K]')
        // pdchIntFenDividerConductance =  newPreDefColumn(pdstIntFen,'Divider Conductance [W/m2-K]')
        pdchIntFenParent = newPreDefColumn(pdstIntFen, "Parent Surface");

        pdstDoor = newPreDefSubTable(pdrEnvelope, "Exterior Door");

        pdchDrCons = newPreDefColumn(pdstDoor, "Construction");
        pdchDrUfactFilm = newPreDefColumn(pdstDoor, "U-Factor with Film [W/m2-K]");
        pdchDrUfactNoFilm = newPreDefColumn(pdstDoor, "U-Factor no Film [W/m2-K]");
        pdchDrGrArea = newPreDefColumn(pdstDoor, "Gross Area [m2]");
        pdchDrParent = newPreDefColumn(pdstDoor, "Parent Surface");

        // Shading Report
        pdrShading = newPreDefReport("ShadingSummary", "Shade", "Shading Summary");

        pdstSunlitFrac = newPreDefSubTable(pdrShading, "Sunlit Fraction");

        pdchSlfMar21_9 = newPreDefColumn(pdstSunlitFrac, "March 21 9am");
        pdchSlfMar21_12 = newPreDefColumn(pdstSunlitFrac, "March 21 noon");
        pdchSlfMar21_15 = newPreDefColumn(pdstSunlitFrac, "March 21 3pm");
        pdchSlfJun21_9 = newPreDefColumn(pdstSunlitFrac, "June 21 9am");
        pdchSlfJun21_12 = newPreDefColumn(pdstSunlitFrac, "June 21 noon");
        pdchSlfJun21_15 = newPreDefColumn(pdstSunlitFrac, "June 21 3pm");
        pdchSlfDec21_9 = newPreDefColumn(pdstSunlitFrac, "December 21 9am");
        pdchSlfDec21_12 = newPreDefColumn(pdstSunlitFrac, "December 21 noon");
        pdchSlfDec21_15 = newPreDefColumn(pdstSunlitFrac, "December 21 3pm");

        pdstWindowControl = newPreDefSubTable(pdrShading, "Window Control");

        pdchWscName = newPreDefColumn(pdstWindowControl, "Name");
        pdchWscShading = newPreDefColumn(pdstWindowControl, "Type");
        pdchWscShadCons = newPreDefColumn(pdstWindowControl, "Shaded Construction");
        pdchWscControl = newPreDefColumn(pdstWindowControl, "Control");
        pdchWscGlare = newPreDefColumn(pdstWindowControl, "Glare Control");

        // Lighting Report
        pdrLighting = newPreDefReport("LightingSummary", "Light", "Lighting Summary");

        pdstInLite = newPreDefSubTable(pdrLighting, "Interior Lighting");

        pdchInLtZone = newPreDefColumn(pdstInLite, "Zone");
        pdchInLtDens = newPreDefColumn(pdstInLite, "Lighting Power Density [W/m2]");
        pdchInLtArea = newPreDefColumn(pdstInLite, "Zone Area [m2]");
        pdchInLtPower = newPreDefColumn(pdstInLite, "Total Power [W]");
        pdchInLtEndUse = newPreDefColumn(pdstInLite, "End Use Subcategory");
        pdchInLtSchd = newPreDefColumn(pdstInLite, "Schedule Name");
        pdchInLtAvgHrSchd = newPreDefColumn(pdstInLite, "Scheduled Hours/Week [hr]");
        pdchInLtAvgHrOper = newPreDefColumn(pdstInLite, "Hours/Week > 1% [hr]");
        pdchInLtFullLoadHrs = newPreDefColumn(pdstInLite, "Full Load Hours/Week [hr]");
        pdchInLtRetAir = newPreDefColumn(pdstInLite, "Return Air Fraction");
        pdchInLtCond = newPreDefColumn(pdstInLite, "Conditioned (Y/N)");
        pdchInLtConsump = newPreDefColumn(pdstInLite, "Consumption [GJ]");

        pdstDaylight = newPreDefSubTable(pdrLighting, "Daylighting");

        pdchDyLtZone = newPreDefColumn(pdstDaylight, "Zone");
        pdchDyLtCtrlName = newPreDefColumn(pdstDaylight, "Control Name");
        pdchDyLtKind = newPreDefColumn(pdstDaylight, "Daylighting Method"); // detailed or DElight
        pdchDyLtCtrlType = newPreDefColumn(pdstDaylight, "Control Type");   // stepped or continuous
        pdchDyLtFrac = newPreDefColumn(pdstDaylight, "Fraction Controlled");
        pdchDyLtWInst = newPreDefColumn(pdstDaylight, "Lighting Installed in Zone [W]");
        pdchDyLtWCtrl = newPreDefColumn(pdstDaylight, "Lighting Controlled [W]");

        pdstExtLite = newPreDefSubTable(pdrLighting, "Exterior Lighting");

        pdchExLtPower = newPreDefColumn(pdstExtLite, "Total Watts");
        pdchExLtClock = newPreDefColumn(pdstExtLite, "Astronomical Clock/Schedule");
        pdchExLtSchd = newPreDefColumn(pdstExtLite, "Schedule Name");
        pdchExLtAvgHrSchd = newPreDefColumn(pdstExtLite, "Scheduled Hours/Week [hr]");
        pdchExLtAvgHrOper = newPreDefColumn(pdstExtLite, "Hours/Week > 1% [hr]");
        pdchExLtFullLoadHrs = newPreDefColumn(pdstExtLite, "Full Load Hours/Week [hr]");
        pdchExLtConsump = newPreDefColumn(pdstExtLite, "Consumption [GJ]");

        // HVAC Equipment Report

        pdrEquip = newPreDefReport("EquipmentSummary", "Equip", "Equipment Summary");

        pdstMech = newPreDefSubTable(pdrEquip, "Central Plant");

        pdchMechType = newPreDefColumn(pdstMech, "Type");
        pdchMechNomCap = newPreDefColumn(pdstMech, "Nominal Capacity [W]");
        pdchMechNomEff = newPreDefColumn(pdstMech, "Nominal Efficiency [W/W]");
        // Note: We don't want any of these to convert.
        // The Btu/W-h isn't going to convert anyways, and the W/W will convert to W/W since it has "SI" in the string as a hint
        pdchMechIPLVSI = newPreDefColumn(pdstMech, "IPLV in SI Units [W/W]");
        pdchMechIPLVIP = newPreDefColumn(pdstMech, "IPLV in IP Units [Btu/W-h]");

        // Ok Constant                        Object Name                            Module                   Example File
        // -- ------------------------------- -------------------------------------- ------------------------ -----------------
        // o  CoilDX_CoolingSingleSpeed       Coil:Cooling:DX:SingleSpeed            DXCoil                   FurnaceWithDXSystem
        // x  CoilDX_CoolingTwoSpeed          Coil:Cooling:DX:TwoSpeed               DXCoil                   5ZoneAutoDXVAV
        // o  CoilDX_CoolingTwoStageWHumControl    Coil:Cooling:DX:                 DXCoil                   SmOffPSZ-MultiModeDX
        //                                    TwoStageWithHumidityControlMode
        // o  CoilDX_MultiSpeedCooling        Coil:Cooling:DX:MultiSpeed             DXCoil                   MultispeedHeatPump
        // o  Coil_CoolingWater               Coil:Cooling:Water                     HVACWaterCoilComponent   5ZoneAirCooled
        // o  Coil_CoolingWaterDetailed       Coil:Cooling:Water:DetailedGeometry    HVACWaterCoilComponent   5zoneWaterSystems
        // o  Coil_CoolingWaterToAirHP        Coil:Cooling:WaterToAirHeatPump:       HVACWaterToAir           5ZoneWaterLoopHeatPump
        //                                      ParameterEstimation
        // o  Coil_CoolingWaterToAirHPSimple  Coil:Cooling:WaterToAirHeatPump:       HVACWaterToAir           HeatPumpWaterToAirEquationFit
        //                                      EquationFit

        // o  CoilDX_HeatingEmpirical         Coil:Heating:DX:SingleSpeed            DXCoil                   HeatPumpAuto
        // o  CoilDX_MultiSpeedHeating        Coil:Heating:DX:MultiSpeed             DXCoil                   MultispeedHeatPump
        // o  Coil_HeatingGasOrOtherFuel                 Coil:Heating:Fuel                       HVACHeatingCoils         5ZoneAutoDXVAV
        // o  Coil_HeatingElectric            Coil:Heating:Electric                  HVACHeatingCoils         PackagedTerminalAirConditioner
        // o  Coil_HeatingDesuperheater       Coil:Heating:Desuperheater             HVACHeatingCoils         SuperMarket_DesuperHeatingCoil
        // o  Coil_HeatingWater               Coil:Heating:Water                     HVACWaterCoilComponent   5ZoneAirCooled
        // o  Coil_HeatingWaterToAirHP        Coil:Heating:WaterToAirHeatPump:       HVACWaterToAir           5ZoneWaterLoopHeatPump
        //                                      ParameterEstimation
        // o  Coil_HeatingWaterToAirHPSimple  Coil:Heating:WaterToAirHeatPump:       HVACWaterToAir           HeatPumpWaterToAirEquationFit
        //                                      EquationFit
        // o  CoilDX_HeatPumpWaterHeater      Coil:WaterHeating:AirToWaterHeatPump   DXCoil                   HeatPumpWaterHeater

        // NOT INCLUDED:
        //    CoilDX_CoolingHXAssisted        CoilSystem:Cooling:DX:                 HVACHXAssistedCooolingCoil
        //                                      HeatExchangerAssisted
        //    CoilWater_CoolingHXAssisted     CoilSystem:Cooling:Water:              HVACHXAssistedCooolingCoil
        //                                      HeatExchangerAssisted

        pdstCoolCoil = newPreDefSubTable(pdrEquip, "Cooling Coils");

        pdchCoolCoilType = newPreDefColumn(pdstCoolCoil, "Type");
        pdchCoolCoilDesCap = newPreDefColumn(pdstCoolCoil, "Design Coil Load [W]");
        pdchCoolCoilTotCap = newPreDefColumn(pdstCoolCoil, "Nominal Total Capacity [W]");
        pdchCoolCoilSensCap = newPreDefColumn(pdstCoolCoil, "Nominal Sensible Capacity [W]");
        pdchCoolCoilLatCap = newPreDefColumn(pdstCoolCoil, "Nominal Latent Capacity [W]");
        pdchCoolCoilSHR = newPreDefColumn(pdstCoolCoil, "Nominal Sensible Heat Ratio");
        pdchCoolCoilNomEff = newPreDefColumn(pdstCoolCoil, "Nominal Efficiency [W/W]");
        pdchCoolCoilUATotal = newPreDefColumn(pdstCoolCoil, "Nominal Coil UA Value [W/C]");
        pdchCoolCoilArea = newPreDefColumn(pdstCoolCoil, "Nominal Coil Surface Area [m2]");

        pdstDXCoolCoil = newPreDefSubTable(pdrEquip, "DX Cooling Coils");
        pdchDXCoolCoilType = newPreDefColumn(pdstDXCoolCoil, "DX Cooling Coil Type");
        pdchDXCoolCoilNetCapSI = newPreDefColumn(pdstDXCoolCoil, "Standard Rated Net Cooling Capacity [W]");

        pdchDXCoolCoilCOP = newPreDefColumn(pdstDXCoolCoil, "Standard Rated Net COP [W/W]");
        pdchDXCoolCoilEERIP = newPreDefColumn(pdstDXCoolCoil, "EER [Btu/W-h]");
        pdchDXCoolCoilSEERIP = newPreDefColumn(pdstDXCoolCoil, "SEER [Btu/W-h]");
        pdchDXCoolCoilIEERIP = newPreDefColumn(pdstDXCoolCoil, "IEER [Btu/W-h]");

        // for DX Cooling Coil ASHRAE 127-12 Report
        pdstDXCoolCoil2 = newPreDefSubTable(pdrEquip, "DX Cooling Coil ASHRAE 127 Standard Ratings Report");
        pdchDXCoolCoilType = newPreDefColumn(pdstDXCoolCoil2, "DX Cooling Coil Type");
        pdchDXCoolCoilNetCapSIA = newPreDefColumn(pdstDXCoolCoil2, "Rated Net Cooling Capacity Test A [W]");
        pdchDXCoolCoilElecPowerA = newPreDefColumn(pdstDXCoolCoil2, "Rated Electric Power Test A [W]");
        pdchDXCoolCoilNetCapSIB = newPreDefColumn(pdstDXCoolCoil2, "Rated Net Cooling Capacity Test B [W]");
        pdchDXCoolCoilElecPowerB = newPreDefColumn(pdstDXCoolCoil2, "Rated Electric Power Test B [W]");
        pdchDXCoolCoilNetCapSIC = newPreDefColumn(pdstDXCoolCoil2, "Rated Net Cooling Capacity Test C [W]");
        pdchDXCoolCoilElecPowerC = newPreDefColumn(pdstDXCoolCoil2, "Rated Electric Power Test C [W]");
        pdchDXCoolCoilNetCapSID = newPreDefColumn(pdstDXCoolCoil2, "Rated Net Cooling Capacity Test D [W]");
        pdchDXCoolCoilElecPowerD = newPreDefColumn(pdstDXCoolCoil2, "Rated Electric Power Test D [W]");

        pdstDXHeatCoil = newPreDefSubTable(pdrEquip, "DX Heating Coils");
        pdchDXHeatCoilType = newPreDefColumn(pdstDXHeatCoil, "DX Heating Coil Type");
        pdchDXHeatCoilHighCap = newPreDefColumn(pdstDXHeatCoil, "High Temperature Heating (net) Rating Capacity [W]");
        pdchDXHeatCoilLowCap = newPreDefColumn(pdstDXHeatCoil, "Low Temperature Heating (net) Rating Capacity [W]");
        pdchDXHeatCoilHSPFIP = newPreDefColumn(pdstDXHeatCoil, "HSPF [Btu/W-h]");
        pdchDXHeatCoilRegionNum = newPreDefColumn(pdstDXHeatCoil, "Region Number");

        pdstHeatCoil = newPreDefSubTable(pdrEquip, "Heating Coils");

        pdchHeatCoilType = newPreDefColumn(pdstHeatCoil, "Type");
        pdchHeatCoilDesCap = newPreDefColumn(pdstHeatCoil, "Design Coil Load [W]");
        pdchHeatCoilNomCap = newPreDefColumn(pdstHeatCoil, "Nominal Total Capacity [W]");
        pdchHeatCoilNomEff = newPreDefColumn(pdstHeatCoil, "Nominal Efficiency [W/W]");

        pdstFan = newPreDefSubTable(pdrEquip, "Fans");

        pdchFanType = newPreDefColumn(pdstFan, "Type");
        pdchFanTotEff = newPreDefColumn(pdstFan, "Total Efficiency [W/W]");
        pdchFanDeltaP = newPreDefColumn(pdstFan, "Delta Pressure [pa]");
        pdchFanVolFlow = newPreDefColumn(pdstFan, "Max Air Flow Rate [m3/s]");
        pdchFanPwr = newPreDefColumn(pdstFan, "Rated Electric Power [W]");
        pdchFanPwrPerFlow = newPreDefColumn(pdstFan, "Rated Power Per Max Air Flow Rate [W-s/m3]");
        pdchFanMotorIn = newPreDefColumn(pdstFan, "Motor Heat In Air Fraction");
        pdchFanEnergyIndex = newPreDefColumn(pdstFan, "Fan Energy Index");
        pdchFanEndUse = newPreDefColumn(pdstFan, "End Use Subcategory");
        pdchFanDesDay = newPreDefColumn(pdstFan, "Design Day Name for Fan Sizing Peak");
        pdchFanPkTime = newPreDefColumn(pdstFan, "Date/Time for Fan Sizing Peak");

        pdstPump = newPreDefSubTable(pdrEquip, "Pumps");
        pdchPumpType = newPreDefColumn(pdstPump, "Type");
        pdchPumpControl = newPreDefColumn(pdstPump, "Control");
        pdchPumpHead = newPreDefColumn(pdstPump, "Head [pa]");
        pdchPumpFlow = newPreDefColumn(pdstPump, "Water Flow [m3/s]");
        pdchPumpPower = newPreDefColumn(pdstPump, "Electric Power [W]");
        pdchPumpPwrPerFlow = newPreDefColumn(pdstPump, "Power Per Water Flow Rate [W-s/m3]");
        pdchMotEff = newPreDefColumn(pdstPump, "Motor Efficiency [W/W]");
        pdchPumpEndUse = newPreDefColumn(pdstPump, "End Use Subcategory");

        pdstSWH = newPreDefSubTable(pdrEquip, "Service Water Heating");
        pdchSWHType = newPreDefColumn(pdstSWH, "Type");
        pdchSWHVol = newPreDefColumn(pdstSWH, "Storage Volume [m3]");
        pdchSWHHeatIn = newPreDefColumn(pdstSWH, "Input [W]");
        pdchSWHThEff = newPreDefColumn(pdstSWH, "Thermal Efficiency [W/W]");
        pdchSWHRecEff = newPreDefColumn(pdstSWH, "Recovery Efficiency [W/W]");
        pdchSWHEnFac = newPreDefColumn(pdstSWH, "Energy Factor");

        // Sizing Report

        pdrSizing = newPreDefReport("HVACSizingSummary", "Size", "HVAC Sizing Summary");

        pdstZoneClSize = newPreDefSubTable(pdrSizing, "Zone Sensible Cooling");

        pdchZnClCalcDesLd = newPreDefColumn(pdstZoneClSize, "Calculated Design Load [W]");
        pdchZnClUserDesLd = newPreDefColumn(pdstZoneClSize, "User Design Load [W]");
        pdchZnClUserDesLdPerArea = newPreDefColumn(pdstZoneClSize, "User Design Load per Area [W/m2]");
        pdchZnClCalcDesAirFlow = newPreDefColumn(pdstZoneClSize, "Calculated Design Air Flow [m3/s]");
        pdchZnClUserDesAirFlow = newPreDefColumn(pdstZoneClSize, "User Design Air Flow [m3/s]");
        pdchZnClDesDay = newPreDefColumn(pdstZoneClSize, "Design Day Name");
        pdchZnClPkTime = newPreDefColumn(pdstZoneClSize, "Date/Time Of Peak {TIMESTAMP}");
        pdchZnClPkTstatTemp = newPreDefColumn(pdstZoneClSize, "Thermostat Setpoint Temperature at Peak Load [C]");
        pdchZnClPkIndTemp = newPreDefColumn(pdstZoneClSize, "Indoor Temperature at Peak Load [C]");
        pdchZnClPkIndHum = newPreDefColumn(pdstZoneClSize, "Indoor Humidity Ratio at Peak Load [kgWater/kgAir]");
        pdchZnClPkOATemp = newPreDefColumn(pdstZoneClSize, "Outdoor Temperature at Peak Load [C]");
        pdchZnClPkOAHum = newPreDefColumn(pdstZoneClSize, "Outdoor Humidity Ratio at Peak Load [kgWater/kgAir]");
        pdchZnClPkOAMinFlow = newPreDefColumn(pdstZoneClSize, "Minimum Outdoor Air Flow Rate [m3/s]");
        pdchZnClPkDOASHeatGain = newPreDefColumn(pdstZoneClSize, "Heat Gain Rate from DOAS [W]");
        addFootNoteSubTable(pdstZoneClSize,
                            "The Design Load is the zone sensible load only. It does not include any system effects or ventilation loads.");
        pdstZoneHtSize = newPreDefSubTable(pdrSizing, "Zone Sensible Heating");

        pdchZnHtCalcDesLd = newPreDefColumn(pdstZoneHtSize, "Calculated Design Load [W]");
        pdchZnHtUserDesLd = newPreDefColumn(pdstZoneHtSize, "User Design Load [W]");
        pdchZnHtUserDesLdPerArea = newPreDefColumn(pdstZoneHtSize, "User Design Load per Area [W/m2]");
        pdchZnHtCalcDesAirFlow = newPreDefColumn(pdstZoneHtSize, "Calculated Design Air Flow [m3/s]");
        pdchZnHtUserDesAirFlow = newPreDefColumn(pdstZoneHtSize, "User Design Air Flow [m3/s]");
        pdchZnHtDesDay = newPreDefColumn(pdstZoneHtSize, "Design Day Name");
        pdchZnHtPkTime = newPreDefColumn(pdstZoneHtSize, "Date/Time Of Peak {TIMESTAMP}");
        pdchZnHtPkTstatTemp = newPreDefColumn(pdstZoneHtSize, "Thermostat Setpoint Temperature at Peak Load [C]");
        pdchZnHtPkIndTemp = newPreDefColumn(pdstZoneHtSize, "Indoor Temperature at Peak Load [C]");
        pdchZnHtPkIndHum = newPreDefColumn(pdstZoneHtSize, "Indoor Humidity Ratio at Peak Load [kgWater/kgAir]");
        pdchZnHtPkOATemp = newPreDefColumn(pdstZoneHtSize, "Outdoor Temperature at Peak Load [C]");
        pdchZnHtPkOAHum = newPreDefColumn(pdstZoneHtSize, "Outdoor Humidity Ratio at Peak Load [kgWater/kgAir]");
        pdchZnHtPkOAMinFlow = newPreDefColumn(pdstZoneHtSize, "Minimum Outdoor Air Flow Rate [m3/s]");
        pdchZnHtPkDOASHeatGain = newPreDefColumn(pdstZoneHtSize, "Heat Gain Rate from DOAS [W]");
        addFootNoteSubTable(pdstZoneHtSize,
                            "The Design Load is the zone sensible load only. It does not include any system effects or ventilation loads.");
        pdstSystemSize = newPreDefSubTable(pdrSizing, "System Design Air Flow Rates");

        pdchSysSizCalcClAir = newPreDefColumn(pdstSystemSize, "Calculated cooling [m3/s]");
        pdchSysSizUserClAir = newPreDefColumn(pdstSystemSize, "User cooling [m3/s]");
        pdchSysSizCalcHtAir = newPreDefColumn(pdstSystemSize, "Calculated heating [m3/s]");
        pdchSysSizUserHtAir = newPreDefColumn(pdstSystemSize, "User heating [m3/s]");
        pdchSysSizAdjustedClAir = newPreDefColumn(pdstSystemSize, "Adjusted cooling [m3/s]");
        pdchSysSizAdjustedHtAir = newPreDefColumn(pdstSystemSize, "Adjusted heating [m3/s]");
        pdchSysSizAdjustedMainAir = newPreDefColumn(pdstSystemSize, "Adjusted main [m3/s]");
        pdchSysSizCalcHeatFlowRatio = newPreDefColumn(pdstSystemSize, "Calculated Heating Air Flow Ratio []");
        pdchSysSizUserHeatFlowRatio = newPreDefColumn(pdstSystemSize, "User Heating Air Flow Ratio []");

        pdstPlantSize = newPreDefSubTable(pdrSizing, "Plant Loop Coincident Design Fluid Flow Rate Adjustments");
        //		pdchPlantSizPass = newPreDefColumn( pdstPlantSize, "Sizing Pass" );
        pdchPlantSizPrevVdot = newPreDefColumn(pdstPlantSize, "Previous Design Volume Flow Rate [m3/s]");
        pdchPlantSizMeasVdot = newPreDefColumn(pdstPlantSize, "Algorithm Volume Flow Rate [m3/s]");
        pdchPlantSizCalcVdot = newPreDefColumn(pdstPlantSize, "Coincident Design Volume Flow Rate [m3/s]");
        pdchPlantSizCoincYesNo = newPreDefColumn(pdstPlantSize, "Coincident Size Adjusted");
        pdchPlantSizDesDay = newPreDefColumn(pdstPlantSize, "Peak Sizing Period Name");
        pdchPlantSizPkTimeDayOfSim = newPreDefColumn(pdstPlantSize, "Peak Day into Period {TIMESTAMP}[day]");
        pdchPlantSizPkTimeHour = newPreDefColumn(pdstPlantSize, "Peak Hour Of Day {TIMESTAMP}[hr]");
        pdchPlantSizPkTimeMin = newPreDefColumn(pdstPlantSize, "Peak Step Start Minute {TIMESTAMP}[min]");

        pdst2CoilSummaryCoilSelection = newPreDefSubTable(pdrSizing, "Coil Sizing Summary");
        // coil meta data information
        //	the first column will be the coil name, the unique user name from input. It has no header or column definition
        pdch2CoilType = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Type");
        pdch2CoilHVACType = newPreDefColumn(pdst2CoilSummaryCoilSelection, "HVAC Type");
        pdch2CoilHVACName = newPreDefColumn(pdst2CoilSummaryCoilSelection, "HVAC Name");

        // coil Final size summary, regardless of how determined (
        pdch2CoilFinalTotalCap = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Final Gross Total Capacity [W]");
        pdch2CoilFinalSensCap = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Final Gross Sensible Capacity [W]");
        pdch2CoilFinalAirVolFlowRate = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Final Reference Air Volume Flow Rate [m3/s]");
        pdch2CoilFinalPlantVolFlowRate = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Final Reference Plant Fluid Volume Flow Rate [m3/s]");
        pdch2CoilUA = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil U-value Times Area Value [W/K]");

        // results from regular zone and system sizing calcs, "At Ideal Loads Peak"
        pdch2CoilDDnameSensIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Design Day Name at Sensible Ideal Loads Peak");
        pdch2CoilDateTimeSensIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Date/Time at Sensible Ideal Loads Peak");
        pdch2CoilDDnameAirFlowIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Design Day Name at Air Flow Ideal Loads Peak");
        pdch2CoilDateTimeAirFlowIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Date/Time at Air Flow Ideal Loads Peak");

        pdch2CoilTotalCapIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Total Capacity at Ideal Loads Peak [W]");
        pdch2CoilSensCapIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Sensible Capacity at Ideal Loads Peak [W]");
        pdch2CoilAirVolumeFlowIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Air Volume Flow Rate at Ideal Loads Peak [m3/s]");
        pdch2CoilEntDryBulbIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Entering Air Drybulb at Ideal Loads Peak [C]");
        pdch2CoilEntWetBulbIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Entering Air Wetbulb at Ideal Loads Peak [C]");
        pdch2CoilEntHumRatIdealPeak =
            newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Entering Air Humidity Ratio at Ideal Loads Peak [KGWATER/KGDRYAIR]");
        pdch2CoilLvgDryBulbIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Leaving Air Drybulb at Ideal Loads Peak [C]");
        pdch2CoilLvgWetBulbIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Leaving Air Wetbulb at Ideal Loads Peak [C]");
        pdch2CoilLvgHumRatIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Leaving Air Humidity Ratio at Ideal Loads Peak [C]");
        pdch2OADryBulbIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Outdoor Air Drybulb at Ideal Loads Peak [C]");
        pdch2OAHumRatIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Outdoor Air Humidity Ratio at Ideal Loads Peak [KGWATER/KGDRYAIR]");
        pdch2OAWetBulbatIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Outdoor Air Wetbulb at Ideal Loads Peak [C]");
        pdch2OAFlowPrcntIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Outdoor Air Flow Percentage at Ideal Loads Peak [%]");
        pdch2ZoneAirDryBulbIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Zone Air Drybulb at Ideal Loads Peak [C]");
        pdch2ZoneAirHumRatIdealPeak =
            newPreDefColumn(pdst2CoilSummaryCoilSelection, "Zone Air Humidity Ratio at Ideal Loads Peak [KGWATER/KGDRYAIR]");
        pdch2ZoneAirRelHumIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Zone Air Relative Humidity at Ideal Loads Peak [%]");
        pdch2ZoneSensibleLoadIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Zone Sensible Heat Gain at Ideal Loads Peak [W]");
        pdch2ZoneLatentLoadIdealPeak = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Zone Latent Heat Gain at Ideal Loads Peak [W]");
        // results for coil at Rated Conditions
        pdch2CoilRatedTotalCap = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Total Capacity at Rating Conditions [W]");
        pdch2CoilRatedSensCap = newPreDefColumn(pdst2CoilSummaryCoilSelection, "Coil Sensible Capacity at Rating Conditions [W]");

        pdrCoilSizingDetailsTable = newPreDefReport("CoilSizingDetails", "Coil", "Coil Sizing Details");
        pdstCoilSummaryCoilSelection = newPreDefSubTable(pdrCoilSizingDetailsTable, "Coils");
        // coil meta data information
        //	the first column will be the coil name, the unique user name from input. It has no header or column definition
        pdchCoilType = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Type");
        pdchCoilLocation = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Location");
        pdchCoilHVACType = newPreDefColumn(pdstCoilSummaryCoilSelection, "HVAC Type");
        pdchCoilHVACName = newPreDefColumn(pdstCoilSummaryCoilSelection, "HVAC Name");
        pdchCoilZoneName = newPreDefColumn(pdstCoilSummaryCoilSelection, "Zone Name(s)");

        pdchSysSizingMethCoinc = newPreDefColumn(pdstCoilSummaryCoilSelection, "System Sizing Method Concurrence");
        pdchSysSizingMethCap = newPreDefColumn(pdstCoilSummaryCoilSelection, "System Sizing Method Capacity");
        pdchSysSizingMethAir = newPreDefColumn(pdstCoilSummaryCoilSelection, "System Sizing Method Air Flow");

        pdchCoilIsCapAutosized = newPreDefColumn(pdstCoilSummaryCoilSelection, "Autosized Coil Capacity?");
        pdchCoilIsAirFlowAutosized = newPreDefColumn(pdstCoilSummaryCoilSelection, "Autosized Coil Airflow?");
        pdchCoilIsWaterFlowAutosized = newPreDefColumn(pdstCoilSummaryCoilSelection, "Autosized Coil Water Flow?");
        pdchCoilIsOATreated = newPreDefColumn(pdstCoilSummaryCoilSelection, "OA Pretreated prior to coil inlet?");

        // coil Final size summary, regardless of how determined (
        // get rid of these, this will be the same as At Rating Conditions.
        pdchCoilFinalTotalCap = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Final Gross Total Capacity [W]");
        pdchCoilFinalSensCap = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Final Gross Sensible Capacity [W]");
        pdchCoilFinalAirVolFlowRate = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Final Reference Air Volume Flow Rate [m3/s]");
        pdchCoilFinalPlantVolFlowRate = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Final Reference Plant Fluid Volume Flow Rate [m3/s]");

        // Misc Design output
        pdchCoilUA = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil U-value Times Area Value [W/K]");
        pdchReheatCoilMultiplier = newPreDefColumn(pdstCoilSummaryCoilSelection, "Terminal Unit Reheat Coil Multiplier");
        pdchFlowCapRatioLowCapIncreaseRatio =
            newPreDefColumn(pdstCoilSummaryCoilSelection, "DX Coil Capacity Increase Ratio from Too Low Flow/Capacity Ratio");
        pdchFlowCapRatioHiCapDecreaseRatio =
            newPreDefColumn(pdstCoilSummaryCoilSelection, "DX Coil Capacity Decrease Ratio from Too High Flow/Capacity Ratio");

        pdchMoistAirSpecificHeat =
            newPreDefColumn(pdstCoilSummaryCoilSelection, "Moist Air Heat Capacity [J/kg-K]");                    // standard? for ideal sizing calcs?
        pdchDryAirSpecificHeat = newPreDefColumn(pdstCoilSummaryCoilSelection, "Dry Air Heat Capacity [J/kg-K]"); // standard? for ideal sizing calcs?
        pdchStandRhoAir = newPreDefColumn(pdstCoilSummaryCoilSelection, "Standard Air Density Adjusted for Elevation [kg/m3]");

        // Fan info for coil
        pdchFanAssociatedWithCoilName = newPreDefColumn(pdstCoilSummaryCoilSelection, "Supply Fan Name for Coil");
        pdchFanAssociatedWithCoilType = newPreDefColumn(pdstCoilSummaryCoilSelection, "Supply Fan Type for Coil");
        pdchFanAssociatedVdotSize = newPreDefColumn(pdstCoilSummaryCoilSelection, "Supply Fan Maximum Air Volume Flow Rate [m3/s]");
        pdchFanAssociatedMdotSize = newPreDefColumn(pdstCoilSummaryCoilSelection, "Supply Fan Maximum Air Mass Flow Rate [kg/s]");

        // Plant info for coil
        pdchCoilPlantLoopName = newPreDefColumn(pdstCoilSummaryCoilSelection, "Plant Name for Coil");
        pdchPlantFluidSpecificHeat = newPreDefColumn(pdstCoilSummaryCoilSelection, "Plant Fluid Specific Heat Capacity [J/kg-K]"); // standard/inits ?
        pdchPlantFluidDensity =
            newPreDefColumn(pdstCoilSummaryCoilSelection, "Plant Fluid Density [kg/m3]"); // standard/inits ? for ideal sizing calcs?
        pdchPlantMassFlowMaximum = newPreDefColumn(pdstCoilSummaryCoilSelection, "Plant Maximum Fluid Mass Flow Rate [kg/s]");
        pdchPlantRetTempDesign = newPreDefColumn(pdstCoilSummaryCoilSelection, "Plant Design Fluid Return Temperature [C]");
        pdchPlantSupTempDesign = newPreDefColumn(pdstCoilSummaryCoilSelection, "Plant Design Fluid Supply Temperature [C]");
        pdchPlantDeltaTempDesign = newPreDefColumn(pdstCoilSummaryCoilSelection, "Plant Design Fluid Temperature Difference [deltaC]");
        pdchPlantCapacity = newPreDefColumn(pdstCoilSummaryCoilSelection, "Plant Design Capacity [W]");
        pdchCoilCapPrcntPlantCapacity = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Capacity Percentage of Plant Design Capacity [%]");
        pdchCoilFlowPrcntPlantFlow = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Fluid Flow Rate Percentage of Plant Design Flow Rate [%]");

        // results from regular zone and system sizing calcs, "At Ideal Loads Peak"
        pdchCoilDDnameSensIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Design Day Name at Sensible Ideal Loads Peak");
        pdchCoilDateTimeSensIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Date/Time at Sensible Ideal Loads Peak");
        pdchCoilDDnameTotIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Design Day Name at Total Ideal Loads Peak");
        pdchCoilDateTimeTotIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Date/Time at Total Ideal Loads Peak");
        pdchCoilDDnameAirFlowIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Design Day Name at Air Flow Ideal Loads Peak");
        pdchCoilDateTimeAirFlowIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Date/Time at Air Flow Ideal Loads Peak");
        pdchCoilPeakLoadTypeToSizeOn = newPreDefColumn(pdstCoilSummaryCoilSelection, "Peak Load Type to Size On");

        pdchCoilTotalCapIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Total Capacity at Ideal Loads Peak [W]");
        pdchCoilSensCapIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Sensible Capacity at Ideal Loads Peak [W]");
        pdchCoilOffRatingCapacityModifierIdealPeak =
            newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Off-Rating Capacity Modifier at Ideal Loads Peak [ ]");
        pdchCoilAirMassFlowIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Air Mass Flow Rate at Ideal Loads Peak [kg/s]");
        pdchCoilAirVolumeFlowIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Air Volume Flow Rate at Ideal Loads Peak [m3/s]");
        pdchCoilEntDryBulbIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Entering Air Drybulb at Ideal Loads Peak [C]");
        pdchCoilEntWetBulbIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Entering Air Wetbulb at Ideal Loads Peak [C]");
        pdchCoilEntHumRatIdealPeak =
            newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Entering Air Humidity Ratio at Ideal Loads Peak [KGWATER/KGDRYAIR]");
        pdchCoilEntEnthalpyIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Entering Air Enthalpy at Ideal Loads Peak [J/KG-K]");
        pdchCoilLvgDryBulbIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Leaving Air Drybulb at Ideal Loads Peak [C]");
        pdchCoilLvgWetBulbIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Leaving Air Wetbulb at Ideal Loads Peak [C]");
        pdchCoilLvgHumRatIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Leaving Air Humidity Ratio at Ideal Loads Peak [C]");
        pdchCoilLvgEnthalpyIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Leaving Air Enthalpy at Ideal Loads Peak [J/KG-K]");
        pdchCoilWaterMassFlowIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Plant Fluid Mass Flow Rate at Ideal Loads Peak [kg/s]");
        pdchCoilEntWaterTempIdealPeak =
            newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Entering Plant Fluid Temperature at Ideal Loads Peak [C]");
        pdchCoilLvgWaterTempIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Leaving Plant Fluid Temperature at Ideal Loads Peak [C]");
        pdchCoilWaterDeltaTempIdealPeak =
            newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Plant Fluid Temperature Difference at Ideal Loads Peak [deltaC]");
        pdchFanHeatGainIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Supply Fan Air Heat Gain at Ideal Loads Peak [W]");
        pdchCoilNetTotalCapacityIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil and Fan Net Total Capacity at Ideal Loads Peak [W]");
        pdchOADryBulbIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Outdoor Air Drybulb at Ideal Loads Peak [C]");
        pdchOAHumRatIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Outdoor Air Humidity Ratio at Ideal Loads Peak [KGWATER/KGDRYAIR]");
        pdchOAWetBulbatIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Outdoor Air Wetbulb at Ideal Loads Peak [C]");
        pdchOAVolFlowIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Outdoor Air Volume Flow Rate at Ideal Loads Peak [m3/s]");
        pdchOAFlowPrcntIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Outdoor Air Flow Percentage at Ideal Loads Peak [%]");
        pdchAirSysRADryBulbIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "System Return Air Drybulb at Ideal Loads Peak [C]");
        pdchAirSysRAHumRatIdealPeak =
            newPreDefColumn(pdstCoilSummaryCoilSelection, "System Return Air Humidity Ratio at Ideal Loads Peak [KGWATER/KGDRYAIR]");
        pdchZoneAirDryBulbIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Zone Air Drybulb at Ideal Loads Peak [C]");
        pdchZoneAirHumRatIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Zone Air Humidity Ratio at Ideal Loads Peak [KGWATER/KGDRYAIR]");
        pdchZoneAirRelHumIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Zone Air Relative Humidity at Ideal Loads Peak [%]");
        pdchZoneSensibleLoadIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Zone Sensible Heat Gain at Ideal Loads Peak [W]");
        pdchZoneLatentLoadIdealPeak = newPreDefColumn(pdstCoilSummaryCoilSelection, "Zone Latent Heat Gain at Ideal Loads Peak [W]");
        // results for coil at Rated Conditions
        pdchCoilRatedTotalCap = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Total Capacity at Rating Conditions [W]");
        pdchCoilRatedSensCap = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Sensible Capacity at Rating Conditions [W]");

        pdchCoilRatedAirMass = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Air Mass Flow Rate at Rating Conditions [kg/s]");
        pdchCoilRatedEntDryBulb = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Entering Air Drybulb at Rating Conditions [C]");
        pdchCoilRatedEntWetBulb = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Entering Air Wetbulb at Rating Conditions [C]");
        pdchCoilRatedEntHumRat =
            newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Entering Air Humidity Ratio at Rating Conditions [KGWATER/KGDRYAIR]");
        pdchCoilRatedEntEnthalpy = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Entering Air Enthalpy at Rating Conditions [J/KG-K]");
        pdchCoilRatedLvgDryBulb = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Leaving Air Drybulb at Rating Conditions [C]");
        pdchCoilRatedLvgWetBulb = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Leaving Air Wetbulb at Rating Conditions [C]");
        pdchCoilRatedLvgHumRat =
            newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Leaving Air Humidity Ratio at Rating Conditions [KGWATER/KGDRYAIR]");
        pdchCoilRatedLvgEnthalpy = newPreDefColumn(pdstCoilSummaryCoilSelection, "Coil Leaving Air Enthalpy at Rating Conditions [J/KG-K]");

        // System Summary Report

        pdrSystem = newPreDefReport("SystemSummary", "Sys", "System Summary");

        pdstEconomizer = newPreDefSubTable(pdrSystem, "Economizer");

        pdchEcoKind = newPreDefColumn(pdstEconomizer, "High Limit Shutoff Control");
        pdchEcoMinOA = newPreDefColumn(pdstEconomizer, "Minimum Outdoor Air [m3/s]");
        pdchEcoMaxOA = newPreDefColumn(pdstEconomizer, "Maximum Outdoor Air [m3/s]");
        pdchEcoRetTemp = newPreDefColumn(pdstEconomizer, "Return Air Temp Limit");
        pdchEcoRetEnth = newPreDefColumn(pdstEconomizer, "Return Air Enthalpy Limit");
        pdchEcoOATempLim = newPreDefColumn(pdstEconomizer, "Outdoor Air Temperature Limit [C]");
        pdchEcoOAEnthLim = newPreDefColumn(pdstEconomizer, "Outdoor Air Enthalpy Limit [C]");

        pdstDemCntlVent = newPreDefSubTable(pdrSystem, "Demand Controlled Ventilation using Controller:MechanicalVentilation");
        pdchDCVventMechName = newPreDefColumn(pdstDemCntlVent, "Controller:MechanicalVentilation Name");
        pdchDCVperPerson = newPreDefColumn(pdstDemCntlVent, "Outdoor Air Per Person [m3/s-person]");
        pdchDCVperArea = newPreDefColumn(pdstDemCntlVent, "Outdoor Air Per Area [m3/s-m2]");
        pdchDCVperZone = newPreDefColumn(pdstDemCntlVent, "Outdoor Air Per Zone [m3/s]");
        pdchDCVperACH = newPreDefColumn(pdstDemCntlVent, "Outdoor Air ACH [ach]");
        pdchDCVMethod = newPreDefColumn(pdstDemCntlVent, "Outdoor Air Method");
        pdchDCVOASchName = newPreDefColumn(pdstDemCntlVent, "Outdoor Air Schedule Name");

        // added for new DCV
        pdchDCVZoneADEffCooling = newPreDefColumn(pdstDemCntlVent, "Air Distribution Effectiveness in Cooling Mode");
        pdchDCVZoneADEffHeating = newPreDefColumn(pdstDemCntlVent, "Air Distribution Effectiveness in Heating Mode");
        pdchDCVZoneADEffSchName = newPreDefColumn(pdstDemCntlVent, "Air Distribution Effectiveness Schedule Name");

        pdstSimpleComfort = newPreDefSubTable(pdrSystem, "Time Not Comfortable Based on Simple ASHRAE 55-2004");
        pdchSCwinterClothes = newPreDefColumn(pdstSimpleComfort, "Winter Clothes [hr]");
        pdchSCsummerClothes = newPreDefColumn(pdstSimpleComfort, "Summer Clothes [hr]");
        pdchSCeitherClothes = newPreDefColumn(pdstSimpleComfort, "Summer or Winter Clothes [hr]");

        pdstUnmetLoads = newPreDefSubTable(pdrSystem, "Time Setpoint Not Met");
        pdchULnotMetHeat = newPreDefColumn(pdstUnmetLoads, "During Heating [hr]");
        pdchULnotMetCool = newPreDefColumn(pdstUnmetLoads, "During Cooling [hr]");
        pdchULnotMetHeatOcc = newPreDefColumn(pdstUnmetLoads, "During Occupied Heating [hr]");
        pdchULnotMetCoolOcc = newPreDefColumn(pdstUnmetLoads, "During Occupied Cooling [hr]");

        // Outside Air Report
        pdrOutsideAir = newPreDefReport("OutdoorAirSummary", "OA", "Outdoor Air Summary");

        pdstOAavgOcc = newPreDefSubTable(pdrOutsideAir, "Average Outdoor Air During Occupied Hours");

        pdchOaoAvgNumOcc1 = newPreDefColumn(pdstOAavgOcc, "Average Number of Occupants");
        pdchOaoNomNumOcc1 = newPreDefColumn(pdstOAavgOcc, "Nominal Number of Occupants");
        pdchOaoZoneVol1 = newPreDefColumn(pdstOAavgOcc, "Zone Volume [m3]");
        pdchOaoAvgMechVent = newPreDefColumn(pdstOAavgOcc, "Mechanical Ventilation [ach]");
        pdchOaoAvgInfil = newPreDefColumn(pdstOAavgOcc, "Infiltration [ach]");
        pdchOaoAvgAFNInfil = newPreDefColumn(pdstOAavgOcc, "AFN Infiltration [ach]");
        pdchOaoAvgSimpVent = newPreDefColumn(pdstOAavgOcc, "Simple Ventilation [ach]");
        // pdchOaoAvgTotVent =   newPreDefColumn(pdstOAavgOcc,'Total Ventilation [ach]')

        addFootNoteSubTable(pdstOAavgOcc, "Values shown for a single zone without multipliers");

        pdstOAminOcc = newPreDefSubTable(pdrOutsideAir, "Minimum Outdoor Air During Occupied Hours");

        pdchOaoAvgNumOcc2 = newPreDefColumn(pdstOAminOcc, "Average Number of Occupants");
        pdchOaoNomNumOcc2 = newPreDefColumn(pdstOAminOcc, "Nominal Number of Occupants");
        pdchOaoZoneVol2 = newPreDefColumn(pdstOAminOcc, "Zone Volume [m3]");
        pdchOaoMinMechVent = newPreDefColumn(pdstOAminOcc, "Mechanical Ventilation [ach]");
        pdchOaoMinInfil = newPreDefColumn(pdstOAminOcc, "Infiltration [ach]");
        pdchOaoMinAFNInfil = newPreDefColumn(pdstOAminOcc, "AFN Infiltration [ach]");
        pdchOaoMinSimpVent = newPreDefColumn(pdstOAminOcc, "Simple Ventilation [ach]");
        // pdchOaoMinTotVent =   newPreDefColumn(pdstOAminOcc,'Total Ventilation [ach]')
        addFootNoteSubTable(pdstOAminOcc, "Values shown for a single zone without multipliers");

        // Object Count Report
        pdrObjCnt = newPreDefReport("ObjectCountSummary", "Count", "Object Count Summary");

        pdstSurfCnt = newPreDefSubTable(pdrObjCnt, "Surfaces by Class");
        pdchSurfCntTot = newPreDefColumn(pdstSurfCnt, "Total");
        pdchSurfCntExt = newPreDefColumn(pdstSurfCnt, "Outdoors");

        pdstHVACcnt = newPreDefSubTable(pdrObjCnt, "HVAC");
        pdchHVACcntVal = newPreDefColumn(pdstHVACcnt, "Count");

        pdstFieldCnt = newPreDefSubTable(pdrObjCnt, "Input Fields");
        pdchFieldCntVal = newPreDefColumn(pdstFieldCnt, "Count");

        // Energy Meters report
        pdrEnergyMeters = newPreDefReport("EnergyMeters", "Meters", "Energy Meters");

        // pdstEMvalues = newPreDefSubTable(pdrEnergyMeters,'Annual and Peak Values')
        // pdchEMannual = newPreDefColumn(pdstEMvalues,'Annual Value [GJ]')
        // pdchEMminvalue = newPreDefColumn(pdstEMvalues,'Minimum Value [J]')
        // pdchEMminvaluetime = newPreDefColumn(pdstEMvalues,'Timestamp of Minimum')
        // pdchEMmaxvalue = newPreDefColumn(pdstEMvalues,'Maximum Value [J]')
        // pdchEMmaxvaluetime = newPreDefColumn(pdstEMvalues,'Timestamp of Maximum')
        // Electricity Sub Table
        pdstEMelecvalues = newPreDefSubTable(pdrEnergyMeters, "Annual and Peak Values - Electricity");
        pdchEMelecannual = newPreDefColumn(pdstEMelecvalues, "Electricity Annual Value [GJ]");
        pdchEMelecminvalue = newPreDefColumn(pdstEMelecvalues, "Electricity Minimum Value [W]");
        pdchEMelecminvaluetime = newPreDefColumn(pdstEMelecvalues, "Timestamp of Minimum {TIMESTAMP}");
        pdchEMelecmaxvalue = newPreDefColumn(pdstEMelecvalues, "Electricity Maximum Value [W]");
        pdchEMelecmaxvaluetime = newPreDefColumn(pdstEMelecvalues, "Timestamp of Maximum {TIMESTAMP}");

        // Gas Sub Table
        pdstEMgasvalues = newPreDefSubTable(pdrEnergyMeters, "Annual and Peak Values - Gas");
        pdchEMgasannual = newPreDefColumn(pdstEMgasvalues, "Gas Annual Value [GJ]");
        pdchEMgasminvalue = newPreDefColumn(pdstEMgasvalues, "Gas Minimum Value [W]");
        pdchEMgasminvaluetime = newPreDefColumn(pdstEMgasvalues, "Timestamp of Minimum {TIMESTAMP}");
        pdchEMgasmaxvalue = newPreDefColumn(pdstEMgasvalues, "Gas Maximum Value [W]");
        pdchEMgasmaxvaluetime = newPreDefColumn(pdstEMgasvalues, "Timestamp of Maximum {TIMESTAMP}");

        // Cool SubTable
        pdstEMcoolvalues = newPreDefSubTable(pdrEnergyMeters, "Annual and Peak Values - Cooling");
        pdchEMcoolannual = newPreDefColumn(pdstEMcoolvalues, "Cooling Annual Value [GJ]");
        pdchEMcoolminvalue = newPreDefColumn(pdstEMcoolvalues, "Cooling Minimum Value [W]");
        pdchEMcoolminvaluetime = newPreDefColumn(pdstEMcoolvalues, "Timestamp of Minimum {TIMESTAMP}");
        pdchEMcoolmaxvalue = newPreDefColumn(pdstEMcoolvalues, "Cooling Maximum Value [W]");
        pdchEMcoolmaxvaluetime = newPreDefColumn(pdstEMcoolvalues, "Timestamp of Maximum {TIMESTAMP}");

        // Water SubTable
        pdstEMwatervalues = newPreDefSubTable(pdrEnergyMeters, "Annual and Peak Values - Water");
        pdchEMwaterannual = newPreDefColumn(pdstEMwatervalues, "Annual Value [m3]");
        pdchEMwaterminvalue = newPreDefColumn(pdstEMwatervalues, "Minimum Value [m3/s]");
        pdchEMwaterminvaluetime = newPreDefColumn(pdstEMwatervalues, "Timestamp of Minimum {TIMESTAMP}");
        pdchEMwatermaxvalue = newPreDefColumn(pdstEMwatervalues, "Maximum Value [m3/s]");
        pdchEMwatermaxvaluetime = newPreDefColumn(pdstEMwatervalues, "Timestamp of Maximum {TIMESTAMP}");

        // Other KG SubTable
        pdstEMotherKGvalues = newPreDefSubTable(pdrEnergyMeters, "Annual and Peak Values - Other by Weight/Mass");
        pdchEMotherKGannual = newPreDefColumn(pdstEMotherKGvalues, "Annual Value [kg]");
        pdchEMotherKGminvalue = newPreDefColumn(pdstEMotherKGvalues, "Minimum Value [kg/s]");
        pdchEMotherKGminvaluetime = newPreDefColumn(pdstEMotherKGvalues, "Timestamp of Minimum {TIMESTAMP}");
        pdchEMotherKGmaxvalue = newPreDefColumn(pdstEMotherKGvalues, "Maximum Value [kg/s]");
        pdchEMotherKGmaxvaluetime = newPreDefColumn(pdstEMotherKGvalues, "Timestamp of Maximum {TIMESTAMP}");

        // Other M3 SubTable
        pdstEMotherM3values = newPreDefSubTable(pdrEnergyMeters, "Annual and Peak Values - Other Volumetric");
        pdchEMotherM3annual = newPreDefColumn(pdstEMotherM3values, "Annual Value [m3]");
        pdchEMotherM3minvalue = newPreDefColumn(pdstEMotherM3values, "Minimum Value [m3/s]");
        pdchEMotherM3minvaluetime = newPreDefColumn(pdstEMotherM3values, "Timestamp of Minimum {TIMESTAMP}");
        pdchEMotherM3maxvalue = newPreDefColumn(pdstEMotherM3values, "Maximum Value [m3/s]");
        pdchEMotherM3maxvaluetime = newPreDefColumn(pdstEMotherM3values, "Timestamp of Maximum {TIMESTAMP}");

        // Other M3 SubTable
        pdstEMotherLvalues = newPreDefSubTable(pdrEnergyMeters, "Annual and Peak Values - Other Liquid/Gas");
        pdchEMotherLannual = newPreDefColumn(pdstEMotherLvalues, "Annual Value [L]");
        pdchEMotherLminvalue = newPreDefColumn(pdstEMotherLvalues, "Minimum Value [L]");
        pdchEMotherLminvaluetime = newPreDefColumn(pdstEMotherLvalues, "Timestamp of Minimum {TIMESTAMP}");
        pdchEMotherLmaxvalue = newPreDefColumn(pdstEMotherLvalues, "Maximum Value [L]");
        pdchEMotherLmaxvaluetime = newPreDefColumn(pdstEMotherLvalues, "Timestamp of Maximum {TIMESTAMP}");

        // Other J SubTable
        pdstEMotherJvalues = newPreDefSubTable(pdrEnergyMeters, "Annual and Peak Values - Other");
        pdchEMotherJannual = newPreDefColumn(pdstEMotherJvalues, "Annual Value [GJ]");
        pdchEMotherJminvalue = newPreDefColumn(pdstEMotherJvalues, "Minimum Value [W]");
        pdchEMotherJminvaluetime = newPreDefColumn(pdstEMotherJvalues, "Timestamp of Minimum {TIMESTAMP}");
        pdchEMotherJmaxvalue = newPreDefColumn(pdstEMotherJvalues, "Maximum Value [W]");
        pdchEMotherJmaxvaluetime = newPreDefColumn(pdstEMotherJvalues, "Timestamp of Maximum {TIMESTAMP}");

        // Sensible Heat Gain Component Report
        pdrSensibleGain = newPreDefReport("SensibleHeatGainSummary", "SHGS", "Sensible Heat Gain Summary");

        pdstSHGSannual = newPreDefSubTable(pdrSensibleGain, "Annual Building Sensible Heat Gain Components");

        pdchSHGSAnHvacHt = newPreDefColumn(pdstSHGSannual, "HVAC Zone Eq & Other Sensible Air Heating [GJ]");
        pdchSHGSAnHvacCl = newPreDefColumn(pdstSHGSannual, "HVAC Zone Eq & Other Sensible Air Cooling [GJ]");
        pdchSHGSAnHvacATUHt = newPreDefColumn(pdstSHGSannual, "HVAC Terminal Unit Sensible Air Heating [GJ]");
        pdchSHGSAnHvacATUCl = newPreDefColumn(pdstSHGSannual, "HVAC Terminal Unit Sensible Air Cooling [GJ]");
        pdchSHGSAnSurfHt = newPreDefColumn(pdstSHGSannual, "HVAC Input Heated Surface Heating [GJ]");
        pdchSHGSAnSurfCl = newPreDefColumn(pdstSHGSannual, "HVAC Input Cooled Surface Cooling [GJ]");
        pdchSHGSAnPeoplAdd = newPreDefColumn(pdstSHGSannual, "People Sensible Heat Addition [GJ]");
        pdchSHGSAnLiteAdd = newPreDefColumn(pdstSHGSannual, "Lights Sensible Heat Addition [GJ]");
        pdchSHGSAnEquipAdd = newPreDefColumn(pdstSHGSannual, "Equipment Sensible Heat Addition [GJ]");
        pdchSHGSAnWindAdd = newPreDefColumn(pdstSHGSannual, "Window Heat Addition [GJ]");
        pdchSHGSAnIzaAdd = newPreDefColumn(pdstSHGSannual, "Interzone Air Transfer Heat Addition [GJ]");
        pdchSHGSAnInfilAdd = newPreDefColumn(pdstSHGSannual, "Infiltration Heat Addition [GJ]");
        pdchSHGSAnOtherAdd = newPreDefColumn(pdstSHGSannual, "Opaque Surface Conduction and Other Heat Addition [GJ]");
        pdchSHGSAnEquipRem = newPreDefColumn(pdstSHGSannual, "Equipment Sensible Heat Removal [GJ]");
        pdchSHGSAnWindRem = newPreDefColumn(pdstSHGSannual, "Window Heat Removal [GJ]");
        pdchSHGSAnIzaRem = newPreDefColumn(pdstSHGSannual, "Interzone Air Transfer Heat Removal [GJ]");
        pdchSHGSAnInfilRem = newPreDefColumn(pdstSHGSannual, "Infiltration Heat Removal [GJ]");
        pdchSHGSAnOtherRem = newPreDefColumn(pdstSHGSannual, "Opaque Surface Conduction and Other Heat Removal [GJ]");

        pdstSHGSpkCl = newPreDefSubTable(pdrSensibleGain, "Peak Cooling Sensible Heat Gain Components");

        pdchSHGSClTimePeak = newPreDefColumn(pdstSHGSpkCl, "Time of Peak {TIMESTAMP}");
        pdchSHGSClHvacHt = newPreDefColumn(pdstSHGSpkCl, "HVAC Zone Eq & Other Sensible Air Heating [W]");
        pdchSHGSClHvacCl = newPreDefColumn(pdstSHGSpkCl, "HVAC Zone Eq & Other Sensible Air Cooling [W]");
        pdchSHGSClHvacATUHt = newPreDefColumn(pdstSHGSpkCl, "HVAC Terminal Unit Sensible Air Heating [W]");
        pdchSHGSClHvacATUCl = newPreDefColumn(pdstSHGSpkCl, "HVAC Terminal Unit Sensible Air Cooling [W]");
        pdchSHGSClSurfHt = newPreDefColumn(pdstSHGSpkCl, "HVAC Input Heated Surface Heating [W]");
        pdchSHGSClSurfCl = newPreDefColumn(pdstSHGSpkCl, "HVAC Input Cooled Surface Cooling [W]");
        pdchSHGSClPeoplAdd = newPreDefColumn(pdstSHGSpkCl, "People Sensible Heat Addition [W]");
        pdchSHGSClLiteAdd = newPreDefColumn(pdstSHGSpkCl, "Lights Sensible Heat Addition [W]");
        pdchSHGSClEquipAdd = newPreDefColumn(pdstSHGSpkCl, "Equipment Sensible Heat Addition [W]");
        pdchSHGSClWindAdd = newPreDefColumn(pdstSHGSpkCl, "Window Heat Addition [W]");
        pdchSHGSClIzaAdd = newPreDefColumn(pdstSHGSpkCl, "Interzone Air Transfer Heat Addition [W]");
        pdchSHGSClInfilAdd = newPreDefColumn(pdstSHGSpkCl, "Infiltration Heat Addition [W]");
        pdchSHGSClOtherAdd = newPreDefColumn(pdstSHGSpkCl, "Opaque Surface Conduction and Other Heat Addition [W]");
        pdchSHGSClEquipRem = newPreDefColumn(pdstSHGSpkCl, "Equipment Sensible Heat Removal [W]");
        pdchSHGSClWindRem = newPreDefColumn(pdstSHGSpkCl, "Window Heat Removal [W]");
        pdchSHGSClIzaRem = newPreDefColumn(pdstSHGSpkCl, "Interzone Air Transfer Heat Removal [W]");
        pdchSHGSClInfilRem = newPreDefColumn(pdstSHGSpkCl, "Infiltration Heat Removal [W]");
        pdchSHGSClOtherRem = newPreDefColumn(pdstSHGSpkCl, "Opaque Surface Conduction and Other Heat Removal [W]");

        pdstSHGSpkHt = newPreDefSubTable(pdrSensibleGain, "Peak Heating Sensible Heat Gain Components");

        pdchSHGSHtTimePeak = newPreDefColumn(pdstSHGSpkHt, "Time of Peak {TIMESTAMP}");
        pdchSHGSHtHvacHt = newPreDefColumn(pdstSHGSpkHt, "HVAC Zone Eq & Other Sensible Air Heating [W]");
        pdchSHGSHtHvacCl = newPreDefColumn(pdstSHGSpkHt, "HVAC Zone Eq & Other Sensible Air Cooling [W]");
        pdchSHGSHtHvacATUHt = newPreDefColumn(pdstSHGSpkHt, "HVAC Terminal Unit Sensible Air Heating [W]");
        pdchSHGSHtHvacATUCl = newPreDefColumn(pdstSHGSpkHt, "HVAC Terminal Unit Sensible Air Cooling [W]");
        pdchSHGSHtSurfHt = newPreDefColumn(pdstSHGSpkHt, "HVAC Input Heated Surface Heating [W]");
        pdchSHGSHtSurfCl = newPreDefColumn(pdstSHGSpkHt, "HVAC Input Cooled Surface Cooling [W]");
        pdchSHGSHtPeoplAdd = newPreDefColumn(pdstSHGSpkHt, "People Sensible Heat Addition [W]");
        pdchSHGSHtLiteAdd = newPreDefColumn(pdstSHGSpkHt, "Lights Sensible Heat Addition [W]");
        pdchSHGSHtEquipAdd = newPreDefColumn(pdstSHGSpkHt, "Equipment Sensible Heat Addition [W]");
        pdchSHGSHtWindAdd = newPreDefColumn(pdstSHGSpkHt, "Window Heat Addition [W]");
        pdchSHGSHtIzaAdd = newPreDefColumn(pdstSHGSpkHt, "Interzone Air Transfer Heat Addition [W]");
        pdchSHGSHtInfilAdd = newPreDefColumn(pdstSHGSpkHt, "Infiltration Heat Addition [W]");
        pdchSHGSHtOtherAdd = newPreDefColumn(pdstSHGSpkHt, "Opaque Surface Conduction and Other Heat Addition [W]");
        pdchSHGSHtEquipRem = newPreDefColumn(pdstSHGSpkHt, "Equipment Sensible Heat Removal [W]");
        pdchSHGSHtWindRem = newPreDefColumn(pdstSHGSpkHt, "Window Heat Removal [W]");
        pdchSHGSHtIzaRem = newPreDefColumn(pdstSHGSpkHt, "Interzone Air Transfer Heat Removal [W]");
        pdchSHGSHtInfilRem = newPreDefColumn(pdstSHGSpkHt, "Infiltration Heat Removal [W]");
        pdchSHGSHtOtherRem = newPreDefColumn(pdstSHGSpkHt, "Opaque Surface Conduction and Other Heat Removal [W]");

        // Standard62Report
        if (DoZoneSizing || DoSystemSizing) {
            pdrStd62 = newPreDefReport("Standard62.1Summary", "Std62", "Standard 62.1 Summary");

            pdstS62sysVentReqCool = newPreDefSubTable(pdrStd62, "System Ventilation Requirements for Cooling");

            pdchS62svrClSumVpz = newPreDefColumn(pdstS62sysVentReqCool, "Sum of Zone Primary Air Flow - Vpz-sum [m3/s]");
            pdchS62svrClPs = newPreDefColumn(pdstS62sysVentReqCool, "System Population - Ps");
            pdchS62svrClSumPz = newPreDefColumn(pdstS62sysVentReqCool, "Sum of Zone Population - Pz-sum");
            pdchS62svrClD = newPreDefColumn(pdstS62sysVentReqCool, "Occupant Diversity - D");
            pdchS62svrClVou = newPreDefColumn(pdstS62sysVentReqCool, "Uncorrected Outdoor Air Intake Airflow - Vou [m3/s]");
            pdchS62svrClVps = newPreDefColumn(pdstS62sysVentReqCool, "System Primary Airflow - Vps [m3/s]");
            pdchS62svrClXs = newPreDefColumn(pdstS62sysVentReqCool, "Average Outdoor Air Fraction - Xs");
            pdchS62svrClEv = newPreDefColumn(pdstS62sysVentReqCool, "System Ventilation Efficiency - Ev");
            pdchS62svrClVot = newPreDefColumn(pdstS62sysVentReqCool, "Outdoor Air Intake Flow - Vot [m3/s]");
            pdchS62svrClPercOA = newPreDefColumn(pdstS62sysVentReqCool, "Percent Outdoor Air - %OA");
            pdchS62svrClEnvironmentOfPs = newPreDefColumn(pdstS62sysVentReqCool, "Environment Name of Peak System Population - Ps");
            pdchS62svrClTimeOfPs = newPreDefColumn(pdstS62sysVentReqCool, "Date and Time of Last Peak System Population - Ps");

            pdstS62sysVentReqHeat = newPreDefSubTable(pdrStd62, "System Ventilation Requirements for Heating");

            pdchS62svrHtSumVpz = newPreDefColumn(pdstS62sysVentReqHeat, "Sum of Zone Primary Air Flow - Vpz-sum [m3/s]");
            pdchS62svrHtPs = newPreDefColumn(pdstS62sysVentReqHeat, "System Population - Ps");
            pdchS62svrHtSumPz = newPreDefColumn(pdstS62sysVentReqHeat, "Sum of Zone Population - Pz-sum");
            pdchS62svrHtD = newPreDefColumn(pdstS62sysVentReqHeat, "Occupant Diversity - D");
            pdchS62svrHtVou = newPreDefColumn(pdstS62sysVentReqHeat, "Uncorrected Outdoor Air Intake Airflow - Vou [m3/s]");
            pdchS62svrHtVps = newPreDefColumn(pdstS62sysVentReqHeat, "System Primary Airflow - Vps [m3/s]");
            pdchS62svrHtXs = newPreDefColumn(pdstS62sysVentReqHeat, "Average Outdoor Air Fraction - Xs");
            pdchS62svrHtEv = newPreDefColumn(pdstS62sysVentReqHeat, "System Ventilation Efficiency - Ev");
            pdchS62svrHtVot = newPreDefColumn(pdstS62sysVentReqHeat, "Outdoor Air Intake Flow Vot [m3/s]");
            pdchS62svrHtPercOA = newPreDefColumn(pdstS62sysVentReqHeat, "Percent Outdoor Air - %OA");
            pdchS62svrHtEnvironmentOfPs = newPreDefColumn(pdstS62sysVentReqHeat, "Environment Name of Peak System Population - Ps");
            pdchS62svrHtTimeOfPs = newPreDefColumn(pdstS62sysVentReqHeat, "Date and Time of Last Peak System Population - Ps");

            pdstS62znVentPar = newPreDefSubTable(pdrStd62, "Zone Ventilation Parameters");

            pdchS62zvpAlN = newPreDefColumn(pdstS62znVentPar, "AirLoop Name");
            pdchS62zvpRp = newPreDefColumn(pdstS62znVentPar, "People Outdoor Air Rate - Rp [m3/s-person]");
            pdchS62zvpPz = newPreDefColumn(pdstS62znVentPar, "Zone Population - Pz");
            pdchS62zvpRa = newPreDefColumn(pdstS62znVentPar, "Area Outdoor Air Rate - Ra [m3/s-m2]");
            pdchS62zvpAz = newPreDefColumn(pdstS62znVentPar, "Zone Floor Area - Az [m2]");
            pdchS62zvpVbz = newPreDefColumn(pdstS62znVentPar, "Breathing Zone Outdoor Airflow - Vbz [m3/s]");
            pdchS62zvpClEz = newPreDefColumn(pdstS62znVentPar, "Cooling Zone Air Distribution Effectiveness - Ez-clg");
            pdchS62zvpClVoz = newPreDefColumn(pdstS62znVentPar, "Cooling Zone Outdoor Airflow - Voz-clg [m3/s]");
            pdchS62zvpHtEz = newPreDefColumn(pdstS62znVentPar, "Heating Zone Air Distribution Effectiveness - Ez-htg");
            pdchS62zvpHtVoz = newPreDefColumn(pdstS62znVentPar, "Heating Zone Outdoor Airflow - Voz-htg [m3/s]");

            pdstS62sysVentPar = newPreDefSubTable(pdrStd62, "System Ventilation Parameters");

            pdchS62svpRp = newPreDefColumn(pdstS62sysVentPar, "People Outdoor Air Rate - Rp [m3/s-person]");
            pdchS62svpPz = newPreDefColumn(pdstS62sysVentPar, "Sum of Zone Population - Pz-sum");
            pdchS62svpRa = newPreDefColumn(pdstS62sysVentPar, "Area Outdoor Air Rate - Ra [m3/s-m2]");
            pdchS62svpAz = newPreDefColumn(pdstS62sysVentPar, "Sum of Zone Floor Area - Az-sum [m2]");
            pdchS62svpVbz = newPreDefColumn(pdstS62sysVentPar, "Breathing Zone Outdoor Airflow - Vbz [m3/s]");
            pdchS62svpClVoz = newPreDefColumn(pdstS62sysVentPar, "Cooling Zone Outdoor Airflow - Voz-clg [m3/s]");
            pdchS62svpHtVoz = newPreDefColumn(pdstS62sysVentPar, "Heating Zone Outdoor Airflow - Voz-htg [m3/s]");

            pdstS62znCoolDes = newPreDefSubTable(pdrStd62, "Zone Ventilation Calculations for Cooling Design");

            pdchS62zcdAlN = newPreDefColumn(pdstS62znCoolDes, "AirLoop Name");
            pdchS62zcdBox = newPreDefColumn(pdstS62znCoolDes, "Box Type");
            pdchS62zcdVpz = newPreDefColumn(pdstS62znCoolDes, "Zone Primary Airflow - Vpz [m3/s]");
            // pdchS62zcdVps =         newPreDefColumn(pdstS62znCoolDes,'System Primary Airflow - Vps [m3/s]')
            // pdchS62zcdVsec =        newPreDefColumn(pdstS62znCoolDes,'Secondary Fan Airflow - Vsec [m3/s]')
            pdchS62zcdVdz = newPreDefColumn(pdstS62znCoolDes, "Zone Discharge Airflow - Vdz [m3/s]");
            pdchS62zcdVpzmin = newPreDefColumn(pdstS62znCoolDes, "Minimum Zone Primary Airflow - Vpz-min [m3/s]");
            pdchS62zcdVozclg = newPreDefColumn(pdstS62znCoolDes, "Zone Outdoor Airflow Cooling - Voz-clg [m3/s]");
            pdchS62zcdZpz = newPreDefColumn(pdstS62znCoolDes, "Primary Outdoor Air Fraction - Zpz");
            pdchS62zcdEp = newPreDefColumn(pdstS62znCoolDes, "Primary Air Fraction - Ep");
            pdchS62zcdEr = newPreDefColumn(pdstS62znCoolDes, "Secondary Recirculation Fraction- Er");
            pdchS62zcdFa = newPreDefColumn(pdstS62znCoolDes, "Supply Air Fraction- Fa");
            pdchS62zcdFb = newPreDefColumn(pdstS62znCoolDes, "Mixed Air Fraction - Fb");
            pdchS62zcdFc = newPreDefColumn(pdstS62znCoolDes, "Outdoor Air Fraction - Fc");
            pdchS62zcdEvz = newPreDefColumn(pdstS62znCoolDes, "Zone Ventilation Efficiency - Evz");

            pdstS62sysCoolDes = newPreDefSubTable(pdrStd62, "System Ventilation Calculations for Cooling Design");

            pdchS62scdVpz = newPreDefColumn(pdstS62sysCoolDes, "Sum of Zone Primary Airflow - Vpz-sum [m3/s]");
            pdchS62scdVps = newPreDefColumn(pdstS62sysCoolDes, "System Primary Airflow - Vps [m3/s]");
            // pdchS62scdVsec =        newPreDefColumn(pdstS62sysCoolDes,'Secondary Fan Airflow - Vsec [m3/s]')
            pdchS62scdVdz = newPreDefColumn(pdstS62sysCoolDes, "Sum of Zone Discharge Airflow - Vdz-sum [m3/s]");
            pdchS62scdVpzmin = newPreDefColumn(pdstS62sysCoolDes, "Sum of Min Zone Primary Airflow - Vpz-min [m3/s]");
            pdchS62scdVozclg = newPreDefColumn(pdstS62sysCoolDes, "Zone Outdoor Airflow Cooling - Voz-clg [m3/s]");
            pdchS62scdEvz = newPreDefColumn(pdstS62sysCoolDes, "Zone Ventilation Efficiency - Evz-min");

            pdstS62znHeatDes = newPreDefSubTable(pdrStd62, "Zone Ventilation Calculations for Heating Design");

            pdchS62zhdAlN = newPreDefColumn(pdstS62znHeatDes, "AirLoop Name");
            pdchS62zhdBox = newPreDefColumn(pdstS62znHeatDes, "Box Type");
            pdchS62zhdVpz = newPreDefColumn(pdstS62znHeatDes, "Zone Primary Airflow - Vpz [m3/s]");
            // pdchS62zhdVps =         newPreDefColumn(pdstS62znHeatDes,'System Primary Airflow - Vps [m3/s]')
            // pdchS62zhdVsec =        newPreDefColumn(pdstS62znHeatDes,'Secondary Fan Airflow - Vsec [m3/s]')
            pdchS62zhdVdz = newPreDefColumn(pdstS62znHeatDes, "Zone Discharge Airflow - Vdz [m3/s]");
            pdchS62zhdVpzmin = newPreDefColumn(pdstS62znHeatDes, "Minimum Zone Primary Airflow - Vpz-min [m3/s]");
            pdchS62zhdVozhtg = newPreDefColumn(pdstS62znHeatDes, "Zone Outdoor Airflow Heating - Voz-htg [m3/s]");
            pdchS62zhdZpz = newPreDefColumn(pdstS62znHeatDes, "Primary Outdoor Air Fraction - Zpz");
            pdchS62zhdEp = newPreDefColumn(pdstS62znHeatDes, "Primary Air Fraction - Ep");
            pdchS62zhdEr = newPreDefColumn(pdstS62znHeatDes, "Secondary Recirculation Fraction- Er");
            pdchS62zhdFa = newPreDefColumn(pdstS62znHeatDes, "Supply Air Fraction- Fa");
            pdchS62zhdFb = newPreDefColumn(pdstS62znHeatDes, "Mixed Air Fraction - Fb");
            pdchS62zhdFc = newPreDefColumn(pdstS62znHeatDes, "Outdoor Air Fraction - Fc");
            pdchS62zhdEvz = newPreDefColumn(pdstS62znHeatDes, "Zone Ventilation Efficiency - Evz");

            pdstS62sysHeatDes = newPreDefSubTable(pdrStd62, "System Ventilation Calculations for Heating Design");

            pdchS62shdVpz = newPreDefColumn(pdstS62sysHeatDes, "Sum of Zone Primary Airflow - Vpz-sum [m3/s]");
            pdchS62shdVps = newPreDefColumn(pdstS62sysHeatDes, "System Primary Airflow - Vps [m3/s]");
            // pdchS62shdVsec =        newPreDefColumn(pdstS62sysHeatDes,'Secondary Fan Airflow - Vsec [m3/s]')
            pdchS62shdVdz = newPreDefColumn(pdstS62sysHeatDes, "Sum of Zone Discharge Airflow - Vdz-sum [m3/s]");
            pdchS62shdVpzmin = newPreDefColumn(pdstS62sysHeatDes, "Sum of Min Zone Primary Airflow - Vpz-min [m3/s]");
            pdchS62shdVozhtg = newPreDefColumn(pdstS62sysHeatDes, "Zone Outdoor Airflow Heating - Voz-htg [m3/s]");
            pdchS62shdEvz = newPreDefColumn(pdstS62sysHeatDes, "Zone Ventilation Efficiency - Evz-min");
        }

        pdrLeed = newPreDefReport("LEEDsummary", "LEED", "LEED Summary");

        pdstLeedGenInfo = newPreDefSubTable(pdrLeed, "Sec1.1A-General Information");
        // single column with rows of:
        //    Principal Heating Source
        //    Weather File
        //    Climate Zone
        //    Heating Degree Days
        //    Cooling Degree Days
        //    HDD and CDD data source
        //    Total gross floor area
        pdchLeedGenData = newPreDefColumn(pdstLeedGenInfo, "Data");

        pdstLeedSpaceUsageType = newPreDefSubTable(pdrLeed, "EAp2-1. Space Usage Type");
        pdchLeedSutSpArea = newPreDefColumn(pdstLeedSpaceUsageType, "Space Area [m2]");
        pdchLeedSutOcArea = newPreDefColumn(pdstLeedSpaceUsageType, "Regularly Occupied Area [m2]");
        pdchLeedSutUnArea = newPreDefColumn(pdstLeedSpaceUsageType, "Unconditioned Area [m2]");
        pdchLeedSutHrsWeek = newPreDefColumn(pdstLeedSpaceUsageType, "Typical Hours/Week in Operation [hr/wk]");

        pdstLeedAdvsMsg = newPreDefSubTable(pdrLeed, "EAp2-2. Advisory Messages");
        // single column with rows of:
        //    Number of hours heating loads not met
        //    Number of hours cooling loads not met
        //    Total
        //    Difference
        //    Number of warning messages
        //    Number of error messages
        //    Number of defaults overridden
        pdchLeedAmData = newPreDefColumn(pdstLeedAdvsMsg, "Data");

        pdstLeedEneTypSum = newPreDefSubTable(pdrLeed, "EAp2-3. Energy Type Summary");
        // multiple columns with rows of
        //    Electricity
        //    Natural Gas
        //    <additional fuels>
        pdchLeedEtsRtNm = newPreDefColumn(pdstLeedEneTypSum, "Utility Rate");
        pdchLeedEtsVirt = newPreDefColumn(pdstLeedEneTypSum, "Virtual Rate [$/unit energy]");
        pdchLeedEtsEneUnt = newPreDefColumn(pdstLeedEneTypSum, "Units of Energy");
        pdchLeedEtsDemUnt = newPreDefColumn(pdstLeedEneTypSum, "Units of Demand");

        pdstLeedPerf = newPreDefSubTable(pdrLeed, "EAp2-4/5. Performance Rating Method Compliance");
        // Multiple colums with rows of:
        //     Interior Lighting
        //     Exterior Lighting
        //     Space Heating
        //     Space Cooling
        //     Pumps
        //     Heat Rejection
        //     Fans-Interior
        //     Fans-Parking Garage
        //     Service Water Heating
        //     Receptacle Equipment
        //     Interior Lighting (process)
        //     Refrigeration Equipment
        //     Cooking
        //     Industrial Process
        //     Elevators and Escalators
        //     Total
        pdchLeedPerfElEneUse = newPreDefColumn(pdstLeedPerf, "Electric Energy Use [GJ]");
        pdchLeedPerfElDem = newPreDefColumn(pdstLeedPerf, "Electric Demand [W]");
        pdchLeedPerfGasEneUse = newPreDefColumn(pdstLeedPerf, "Natural Gas Energy Use [GJ]");
        pdchLeedPerfGasDem = newPreDefColumn(pdstLeedPerf, "Natural Gas Demand [W]");
        pdchLeedPerfAddFuelEneUse = newPreDefColumn(pdstLeedPerf, "Additional Fuel Use [GJ]");
        pdchLeedPerfAddFuelDem = newPreDefColumn(pdstLeedPerf, "Additional Fuel Demand [W]");
        pdchLeedPerfDisClEneUse = newPreDefColumn(pdstLeedPerf, "District Cooling Use [GJ]");
        pdchLeedPerfDisClDem = newPreDefColumn(pdstLeedPerf, "District Cooling Demand [W]");
        pdchLeedPerfDisHtEneUse = newPreDefColumn(pdstLeedPerf, "District Heating Use [GJ]");
        pdchLeedPerfDisHtDem = newPreDefColumn(pdstLeedPerf, "District Heating Demand [W]");

        pdstLeedEneUseSum = newPreDefSubTable(pdrLeed, "EAp2-6. Energy Use Summary");
        // Multiple columns with rows of:
        //    Electricity
        //    Natural Gas
        //    <additional fuels>
        //    Total
        pdchLeedEusProc = newPreDefColumn(pdstLeedEneUseSum, "Process Subtotal [GJ]");
        pdchLeedEusTotal = newPreDefColumn(pdstLeedEneUseSum, "Total Energy Use [GJ]");

        pdstLeedEneCostSum = newPreDefSubTable(pdrLeed, "EAp2-7. Energy Cost Summary");
        // Multiple columns with rows of:
        //    Electricity
        //    Natural Gas
        //    <additional fuels>
        //    Total
        pdchLeedEcsProc = newPreDefColumn(pdstLeedEneCostSum, "Process Subtotal [$]");
        pdchLeedEcsTotal = newPreDefColumn(pdstLeedEneCostSum, "Total Energy Cost [$]");

        pdstLeedRenewSum = newPreDefSubTable(pdrLeed, "L-1. Renewable Energy Source Summary");
        // Multiple columns with rows of each renewable source
        pdchLeedRenRatCap = newPreDefColumn(pdstLeedRenewSum, "Rated Capacity [kW]");
        pdchLeedRenAnGen = newPreDefColumn(pdstLeedRenewSum, "Annual Energy Generated [GJ]");

        pdstLeedEneUseIntEl = newPreDefSubTable(pdrLeed, "EAp2-17a. Energy Use Intensity - Electricity");
        // Single column with rows of:
        //    Interior lighting
        //    Space heating
        //    Space cooling
        //    Fans-interior
        //    Service water heating
        //    Receptacle equipment
        //    Miscellaneous
        //    Subtotal
        pdchLeedEuiElec = newPreDefColumn(pdstLeedEneUseIntEl, "Electricty [MJ/m2]");

        pdstLeedEneUseIntNatG = newPreDefSubTable(pdrLeed, "EAp2-17b. Energy Use Intensity - Natural Gas");
        // Single column with rows of:
        //    Space heating
        //    Service water heating
        //    Miscellaneous
        //    Subtotal
        pdchLeedEuiNatG = newPreDefColumn(pdstLeedEneUseIntNatG, "Natural Gas [MJ/m2]");

        pdstLeedEneUseIntOthr = newPreDefSubTable(pdrLeed, "EAp2-17c. Energy Use Intensity - Additional");
        // Single column with rows of:
        //    Miscellaneous
        //    Subtotal
        pdchLeedEuiOthr = newPreDefColumn(pdstLeedEneUseIntOthr, "Additional [MJ/m2]");

        pdstLeedEneUsePerc = newPreDefSubTable(pdrLeed, "EAp2-18. End Use Percentage");
        // single column with rows of:
        //    Interior Lighting
        //    Space heating
        //    Space cooling
        //    Fans-Interior
        //    Service Water Heating
        //    Receptacle Equipment
        //    Miscellaneous
        pdchLeedEupPerc = newPreDefColumn(pdstLeedEneUsePerc, "Percent [%]");

        pdstLeedEqFlLdHrs = newPreDefSubTable(pdrLeed, "Schedules-Equivalent Full Load Hours (Schedule Type=Fraction)");
        pdchLeedEflhEflh = newPreDefColumn(pdstLeedEqFlLdHrs, "Equivalent Full Load Hours of Operation Per Year [hr]");
        pdchLeedEflhNonZerHrs = newPreDefColumn(pdstLeedEqFlLdHrs, "Hours > 1% [hr]");

        pdstLeedSchedSetPts = newPreDefSubTable(pdrLeed, "Schedules-SetPoints (Schedule Type=Temperature)");
        pdChLeedSchStPtFirstObjUsed = newPreDefColumn(pdstLeedSchedSetPts, "First Object Used");
        pdChLeedSchStPtMonthUsed = newPreDefColumn(pdstLeedSchedSetPts, "Month Assumed");
        pdchLeedSchStPt11amWednesday = newPreDefColumn(pdstLeedSchedSetPts, "11am First Wednesday [C]");
        pdchLeedSchStPt11amWedCnt = newPreDefColumn(pdstLeedSchedSetPts, "Days with Same 11am Value");
        pdchLeedSchStPt11pmWednesday = newPreDefColumn(pdstLeedSchedSetPts, "11pm First Wednesday [C]");
        pdchLeedSchStPt11pmWedCnt = newPreDefColumn(pdstLeedSchedSetPts, "Days with Same 11pm Value");
    }

    void PreDefTableEntry(int const columnIndex, std::string const &objName, Real64 const tableEntryReal, Optional_int_const numSigDigits)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   August 2006
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   Creates an entry for predefined tables when the entry
        //   is a real variable

        // METHODOLOGY EMPLOYED:
        //   Simple assignments to public variables.

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static ObjexxFCL::gio::Fmt fmtI1("(I1)");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int sigDigitCount;
        std::string digitString;
        std::string formatConvert;
        std::string stringEntry;
        int IOS;

        incrementTableEntry();
        // check for number of significant digits
        if (present(numSigDigits)) {
            if ((numSigDigits <= 9) && (numSigDigits >= 0)) {
                sigDigitCount = numSigDigits;
            } else {
                sigDigitCount = 2;
            }
        } else {
            sigDigitCount = 2;
        }
        // convert the integer to a string for the number of digits
        ObjexxFCL::gio::write(digitString, fmtI1) << sigDigitCount;
        // build up the format string
        if (tableEntryReal < 1e8) { // change from 1e10 for more robust entry writing
            formatConvert = "(F12." + digitString + ')';
        } else {
            formatConvert = "(E12." + digitString + ')';
        }
        {
            IOFlags flags;
            ObjexxFCL::gio::write(stringEntry, formatConvert, flags) << tableEntryReal;
            IOS = flags.ios();
        }
        if (IOS != 0) stringEntry = "  Too Big";
        tableEntry(numTableEntry).charEntry = stringEntry;
        tableEntry(numTableEntry).objectName = objName;
        tableEntry(numTableEntry).indexColumn = columnIndex;
        tableEntry(numTableEntry).origRealEntry = tableEntryReal;
        tableEntry(numTableEntry).significantDigits = sigDigitCount;
        tableEntry(numTableEntry).origEntryIsReal = true;
    }

    void PreDefTableEntry(int const columnIndex, std::string const &objName, std::string const &tableEntryChar)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   August 2006
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   Creates an entry for predefined tables when the entry
        //   is a character variable

        // METHODOLOGY EMPLOYED:
        //   Simple assignments to public variables.

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        incrementTableEntry();
        tableEntry(numTableEntry).charEntry = tableEntryChar;
        tableEntry(numTableEntry).objectName = objName;
        tableEntry(numTableEntry).indexColumn = columnIndex;
    }

    void PreDefTableEntry(int const columnIndex, std::string const &objName, int const tableEntryInt)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   August 2006
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   Creates an entry for predefined tables when the entry
        //   is a integer variable

        // METHODOLOGY EMPLOYED:
        //   Simple assignments to public variables.

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static ObjexxFCL::gio::Fmt fmtLD("*");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string stringEntry;

        incrementTableEntry();
        // convert the integer to a string
        ObjexxFCL::gio::write(stringEntry, fmtLD) << tableEntryInt;
        tableEntry(numTableEntry).charEntry = stringEntry;
        tableEntry(numTableEntry).objectName = objName;
        tableEntry(numTableEntry).indexColumn = columnIndex;
    }

    std::string RetrievePreDefTableEntry(int const columnIndex, std::string const &objName)
    {
        for (int iTableEntry = 1; iTableEntry <= numTableEntry; ++iTableEntry) {
            if (tableEntry(iTableEntry).indexColumn == columnIndex && tableEntry(iTableEntry).objectName == objName) {
                return trimmed(ljustified(tableEntry(iTableEntry).charEntry));
                break;
            }
        }
        return "NOT FOUND";
    }

    void incrementTableEntry()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   August 2006
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   Manages the resizing of the TableEntry Array

        // METHODOLOGY EMPLOYED:
        //   Simple assignments to public variables.

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        if (!allocated(tableEntry)) {
            tableEntry.allocate(sizeIncrement);
            sizeTableEntry = sizeIncrement;
            numTableEntry = 1;
        } else {
            ++numTableEntry;
            // if larger than current size grow the array
            if (numTableEntry > sizeTableEntry) {
                tableEntry.redimension(sizeTableEntry *=
                                       2); // Tuned Changed += sizeIncrement to *= 2 for reduced heap allocations (at some space cost)
            }
        }
    }

    void
    AddCompSizeTableEntry(std::string const &FieldType, std::string const &FieldName, std::string const &FieldDescription, Real64 const FieldValue)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   July 2007
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   Creates an entry for component size tables.

        // METHODOLOGY EMPLOYED:
        //   Simple assignments to public variables.

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        if (!allocated(CompSizeTableEntry)) {
            CompSizeTableEntry.allocate(sizeIncrement);
            sizeCompSizeTableEntry = sizeIncrement;
            numCompSizeTableEntry = 1;
        } else {
            ++numCompSizeTableEntry;
            // if larger than current size grow the array
            if (numCompSizeTableEntry > sizeCompSizeTableEntry) {
                CompSizeTableEntry.redimension(sizeCompSizeTableEntry *=
                                               2); // Tuned Changed += sizeIncrement to *= 2 for reduced heap allocations (at some space cost)
            }
        }
        CompSizeTableEntry(numCompSizeTableEntry).typeField = FieldType;
        CompSizeTableEntry(numCompSizeTableEntry).nameField = FieldName;
        CompSizeTableEntry(numCompSizeTableEntry).description = FieldDescription;
        CompSizeTableEntry(numCompSizeTableEntry).valField = FieldValue;
    }

    void AddShadowRelateTableEntry(int const castingField, int const receivingField, int const receivingKind)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   July 2007
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   Creates an entry for any shadow hierarchy tables that consist
        //   of items and one or more subitems for each item.

        // METHODOLOGY EMPLOYED:
        //   Simple assignments to public variables.

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // CHARACTER(len=*),INTENT(IN)  :: castingField
        // CHARACTER(len=*),INTENT(IN)  :: receivingField

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        if (!allocated(ShadowRelate)) {
            ShadowRelate.allocate(sizeIncrement);
            sizeShadowRelate = sizeIncrement;
            numShadowRelate = 1;
        } else {
            ++numShadowRelate;
            // if larger than current size grow the array
            if (numShadowRelate > sizeShadowRelate) {
                ShadowRelate.redimension(sizeShadowRelate *=
                                         2); // Tuned Changed += sizeIncrement to *= 2 for reduced heap allocations (at some space cost)
            }
        }
        ShadowRelate(numShadowRelate).castSurf = castingField;
        ShadowRelate(numShadowRelate).recSurf = receivingField;
        ShadowRelate(numShadowRelate).recKind = receivingKind;
    }

    int newPreDefReport(std::string const &inReportName, std::string const &inReportAbrev, std::string const &inReportNamewithSpaces)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   August 2006
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   Creates a new index for the next predefined report

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Return value
        int newPreDefReport;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        if (!allocated(reportName)) {
            reportName.allocate(sizeIncrement);
            sizeReportName = sizeIncrement;
            numReportName = 1;
        } else {
            ++numReportName;
            // if larger than current size grow the array
            if (numReportName > sizeReportName) {
                reportName.redimension(sizeReportName *=
                                       2); // Tuned Changed += sizeIncrement to *= 2 for reduced heap allocations (at some space cost)
            }
        }
        // initialize new record
        reportName(numReportName).name = inReportName;
        reportName(numReportName).abrev = inReportAbrev;
        reportName(numReportName).namewithspaces = inReportNamewithSpaces;
        reportName(numReportName).show = false;
        newPreDefReport = numReportName;
        return newPreDefReport;
    }

    int newPreDefSubTable(int const reportIndex, std::string const &subTableName)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   August 2006
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   Assigns the index for predefined sub-tables

        // METHODOLOGY EMPLOYED:
        //   Simple assignments to public variables.

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Return value

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        if (!allocated(subTable)) {
            subTable.allocate(sizeIncrement);
            sizeSubTable = sizeIncrement;
            numSubTable = 1;
        } else {
            ++numSubTable;
            // if larger than current size then grow the array
            if (numSubTable > sizeSubTable) {
                subTable.redimension(sizeSubTable *= 2); // Tuned Changed += sizeIncrement to *= 2 for reduced heap allocations (at some space cost)
            }
        }
        // initialize new record)
        subTable(numSubTable).name = subTableName;
        subTable(numSubTable).indexReportName = reportIndex;
        return numSubTable;
    }

    void addFootNoteSubTable(int const subTableIndex, std::string const &footnoteText)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   August 2008
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   Adds a footnote to a subtable

        // METHODOLOGY EMPLOYED:
        //   Simple assignments to public variables.

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        if ((subTableIndex >= 0) && (subTableIndex <= numSubTable)) {
            subTable(subTableIndex).footnote = footnoteText;
        }
    }

    int newPreDefColumn(int const subTableIndex, std::string const &columnHeading)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   August 2006
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   Assigns the index variables for all predefined reports

        // METHODOLOGY EMPLOYED:
        //   Simple assignments to public variables.

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Return value
        int newPreDefColumn;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        if (!allocated(columnTag)) {
            columnTag.allocate(sizeIncrement);
            sizeColumnTag = sizeIncrement;
            numColumnTag = 1;
        } else {
            ++numColumnTag;
            // if larger than current size grow the array
            if (numColumnTag > sizeColumnTag) {
                columnTag.redimension(sizeColumnTag *= 2); // Tuned Changed += sizeIncrement to *= 2 for reduced heap allocations (at some space cost)
            }
        }
        // initialize new record)
        columnTag(numColumnTag).heading = columnHeading;
        columnTag(numColumnTag).indexSubTable = subTableIndex;
        newPreDefColumn = numColumnTag;
        return newPreDefColumn;
    }

} // namespace OutputReportPredefined

} // namespace EnergyPlus
