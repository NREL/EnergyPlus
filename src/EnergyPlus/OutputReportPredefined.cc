// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <OutputReportPredefined.hh>
#include <DataPrecisionGlobals.hh>

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
	int pdchFanEndUse;
	int pdchFanPwr;
	int pdchFanPwrPerFlow;
	// Pump subtable
	int pdstPump;
	int pdchPumpType;
	int pdchPumpControl;
	int pdchPumpHead;
	int pdchPumpFlow;
	int pdchPumpPower;
	int pdchPumpPwrPerFlow;
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
	int pdchDXCoolCoilCOP; // EER/COP value in SI unit at AHRI std. 340/360 conditions [W/W]
	int pdchDXCoolCoilSEERIP; // SEER value in IP unit at AHRI std. 210/240 conditions [Btu/W-hr]
	int pdchDXCoolCoilEERIP; // EER value in IP unit at AHRI std. 340/360 conditions [Btu/W-h]
	int pdchDXCoolCoilIEERIP; // IEER value in IP unit at AHRI std. 340/360 conditions

	// DX Cooling Coil subtable per ANSI/ASHRAE Std 127 for Tests A, B, C and D
	int pdstDXCoolCoil2;
	int pdchDXCoolCoilNetCapSIA; // Standard Rated (Net) Cooling Capacity [W], Test A
	int pdchDXCoolCoilElecPowerA; // Standard Rated Electric Power [W], Test A
	int pdchDXCoolCoilNetCapSIB; // Standard Rated (Net) Cooling Capacity [W], Test B
	int pdchDXCoolCoilElecPowerB; // Standard Rated Electric Power [W], Test B
	int pdchDXCoolCoilNetCapSIC; // Standard Rated (Net) Cooling Capacity [W], Test C
	int pdchDXCoolCoilElecPowerC; // Standard Rated Electric Power [W], Test C
	int pdchDXCoolCoilNetCapSID; // Standard Rated (Net) Cooling Capacity [W], Test D
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
	int pdchDXHeatCoilHSPFSI; // HSPF value in SI unit at AHRI std. 340/360 conditions [W/W]
	int pdchDXHeatCoilHSPFIP; // HSPF value in IP unit at AHRI std. 340/360 conditions [Btu/W-hr]
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
	//INTEGER :: pdchIntFenGlassAreaOf1
	//INTEGER :: pdchIntFenFrameAreaOf1
	//INTEGER :: pdchIntFenDividerAreaOf1
	//INTEGER :: pdchIntFenFrameConductance
	//INTEGER :: pdchIntFenDividerConductance
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
	int pdchDyLtKind;
	int pdchDyLtCtrl;
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
	int pdstSystemSize;
	int pdchSysSizCalcClAir;
	int pdchSysSizUserClAir;
	int pdchSysSizCalcHtAir;
	int pdchSysSizUserHtAir;
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

	//System summary
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

	//added for new DCV
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
	//annual
	int pdstSHGSannual;
	int pdchSHGSAnHvacHt;
	int pdchSHGSAnHvacCl;
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
	//peak cooling
	int pdstSHGSpkCl;
	int pdchSHGSClTimePeak;
	int pdchSHGSClHvacHt;
	int pdchSHGSClHvacCl;
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
	//peak heating
	int pdstSHGSpkHt;
	int pdchSHGSHtTimePeak;
	int pdchSHGSHtHvacHt;
	int pdchSHGSHtHvacCl;
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
	//Standard62Report
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
	int pdchLeedPerfOthEneUse;
	int pdchLeedPerfOthDem;

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

	// Internal data structures to store information provided by calls

	int const sizeIncrement( 100 );

	int sizeReportName;
	int numReportName;

	int sizeSubTable;
	int numSubTable;

	int sizeColumnTag;
	int numColumnTag;

	int sizeTableEntry;
	int numTableEntry;

	int sizeCompSizeTableEntry( 0 ); //Autodesk Was used uninitialized in output to .audit files
	int numCompSizeTableEntry( 0 ); //Autodesk Was used uninitialized in WriteComponentSizing

	int sizeShadowRelate;
	int numShadowRelate;
	int const recKindSurface( 1 );
	int const recKindSubsurface( 2 );

	Real64 TotalNotMetHeatingOccupiedForABUPS( 0.0 );
	Real64 TotalNotMetCoolingOccupiedForABUPS( 0.0 );
	Real64 TotalNotMetOccupiedForABUPS( 0.0 );
	Real64 TotalTimeNotSimpleASH55EitherForABUPS( 0.0 );

	// Object Data
	Array1D< reportNameType > reportName;
	Array1D< SubTableType > subTable;
	Array1D< ColumnTagType > columnTag;
	Array1D< TableEntryType > tableEntry;
	Array1D< CompSizeTableEntryType > CompSizeTableEntry;
	Array1D< ShadowRelateType > ShadowRelate;

	// Functions

	void
	SetPredefinedTables()
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
		using DataGlobals::DoZoneSizing;
		using DataGlobals::DoSystemSizing;

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
		pdrClim = newPreDefReport( "ClimaticDataSummary", "Clim", "Climatic Data Summary" );

		pdstDesDay = newPreDefSubTable( pdrClim, "SizingPeriod:DesignDay" );

		pdchDDmaxDB = newPreDefColumn( pdstDesDay, "Maximum Dry Bulb [C]" );
		pdchDDrange = newPreDefColumn( pdstDesDay, "Daily Temperature Range [deltaC]" );
		pdchDDhumid = newPreDefColumn( pdstDesDay, "Humidity Value" );
		pdchDDhumTyp = newPreDefColumn( pdstDesDay, "Humidity Type" );
		pdchDDwindSp = newPreDefColumn( pdstDesDay, "Wind Speed [m/s]" );
		pdchDDwindDr = newPreDefColumn( pdstDesDay, "Wind Direction" );

		pdstWthr = newPreDefSubTable( pdrClim, "Weather Statistics File" );
		pdchWthrVal = newPreDefColumn( pdstWthr, "Value" );

		// Envelope Report

		pdrEnvelope = newPreDefReport( "EnvelopeSummary", "Env", "Envelope Summary" );

		pdstOpaque = newPreDefSubTable( pdrEnvelope, "Opaque Exterior" );

		pdchOpCons = newPreDefColumn( pdstOpaque, "Construction" );
		pdchOpRefl = newPreDefColumn( pdstOpaque, "Reflectance" );
		pdchOpUfactFilm = newPreDefColumn( pdstOpaque, "U-Factor with Film [W/m2-K]" );
		pdchOpUfactNoFilm = newPreDefColumn( pdstOpaque, "U-Factor no Film [W/m2-K]" );
		pdchOpGrArea = newPreDefColumn( pdstOpaque, "Gross Area [m2]" );
		pdchOpAzimuth = newPreDefColumn( pdstOpaque, "Azimuth [deg]" );
		pdchOpTilt = newPreDefColumn( pdstOpaque, "Tilt [deg]" );
		pdchOpDir = newPreDefColumn( pdstOpaque, "Cardinal Direction" );

		pdstFen = newPreDefSubTable( pdrEnvelope, "Exterior Fenestration" );

		pdchFenCons = newPreDefColumn( pdstFen, "Construction" );
		pdchFenGlassAreaOf1 = newPreDefColumn( pdstFen, "Glass Area [m2]" );
		pdchFenFrameAreaOf1 = newPreDefColumn( pdstFen, "Frame Area [m2]" );
		pdchFenDividerAreaOf1 = newPreDefColumn( pdstFen, "Divider Area [m2]" );
		pdchFenAreaOf1 = newPreDefColumn( pdstFen, "Area of One Opening [m2]" );
		pdchFenArea = newPreDefColumn( pdstFen, "Area of Multiplied Openings [m2]" );
		pdchFenUfact = newPreDefColumn( pdstFen, "Glass U-Factor [W/m2-K]" );
		pdchFenSHGC = newPreDefColumn( pdstFen, "Glass SHGC" );
		pdchFenVisTr = newPreDefColumn( pdstFen, "Glass Visible Transmittance" );
		pdchFenFrameConductance = newPreDefColumn( pdstFen, "Frame Conductance [W/m2-K]" );
		pdchFenDividerConductance = newPreDefColumn( pdstFen, "Divider Conductance [W/m2-K]" );
		pdchFenSwitchable = newPreDefColumn( pdstFen, "Shade Control" );
		pdchFenParent = newPreDefColumn( pdstFen, "Parent Surface" );
		pdchFenAzimuth = newPreDefColumn( pdstFen, "Azimuth [deg]" );
		pdchFenTilt = newPreDefColumn( pdstFen, "Tilt [deg]" );
		pdchFenDir = newPreDefColumn( pdstFen, "Cardinal Direction" );

		pdstIntFen = newPreDefSubTable( pdrEnvelope, "Interior Fenestration" );

		pdchIntFenCons = newPreDefColumn( pdstIntFen, "Construction" );
		pdchIntFenAreaOf1 = newPreDefColumn( pdstIntFen, "Area of One Opening [m2]" );
		pdchIntFenArea = newPreDefColumn( pdstIntFen, "Area of Openings [m2]" );
		pdchIntFenUfact = newPreDefColumn( pdstIntFen, "Glass U-Factor [W/m2-K]" );
		pdchIntFenSHGC = newPreDefColumn( pdstIntFen, "Glass SHGC" );
		pdchIntFenVisTr = newPreDefColumn( pdstIntFen, "Glass Visible Transmittance" );
		//pdchIntFenGlassAreaOf1 =   newPreDefColumn(pdstIntFen,'Glass Area [m2]')
		//pdchIntFenFrameAreaOf1 =   newPreDefColumn(pdstIntFen,'Frame Area [m2]')
		//pdchIntFenDividerAreaOf1 =   newPreDefColumn(pdstIntFen,'Divider Area [m2]')
		//pdchIntFenFrameConductance =  newPreDefColumn(pdstIntFen,'Frame Conductance [W/m2-K]')
		//pdchIntFenDividerConductance =  newPreDefColumn(pdstIntFen,'Divider Conductance [W/m2-K]')
		pdchIntFenParent = newPreDefColumn( pdstIntFen, "Parent Surface" );

		pdstDoor = newPreDefSubTable( pdrEnvelope, "Exterior Door" );

		pdchDrCons = newPreDefColumn( pdstDoor, "Construction" );
		pdchDrUfactFilm = newPreDefColumn( pdstDoor, "U-Factor with Film [W/m2-K]" );
		pdchDrUfactNoFilm = newPreDefColumn( pdstDoor, "U-Factor no Film [W/m2-K]" );
		pdchDrGrArea = newPreDefColumn( pdstDoor, "Gross Area [m2]" );
		pdchDrParent = newPreDefColumn( pdstDoor, "Parent Surface" );

		// Shading Report
		pdrShading = newPreDefReport( "ShadingSummary", "Shade", "Shading Summary" );

		pdstSunlitFrac = newPreDefSubTable( pdrShading, "Sunlit Fraction" );

		pdchSlfMar21_9 = newPreDefColumn( pdstSunlitFrac, "March 21 9am" );
		pdchSlfMar21_12 = newPreDefColumn( pdstSunlitFrac, "March 21 noon" );
		pdchSlfMar21_15 = newPreDefColumn( pdstSunlitFrac, "March 21 3pm" );
		pdchSlfJun21_9 = newPreDefColumn( pdstSunlitFrac, "June 21 9am" );
		pdchSlfJun21_12 = newPreDefColumn( pdstSunlitFrac, "June 21 noon" );
		pdchSlfJun21_15 = newPreDefColumn( pdstSunlitFrac, "June 21 3pm" );
		pdchSlfDec21_9 = newPreDefColumn( pdstSunlitFrac, "December 21 9am" );
		pdchSlfDec21_12 = newPreDefColumn( pdstSunlitFrac, "December 21 noon" );
		pdchSlfDec21_15 = newPreDefColumn( pdstSunlitFrac, "December 21 3pm" );

		pdstWindowControl = newPreDefSubTable( pdrShading, "Window Control" );

		pdchWscName = newPreDefColumn( pdstWindowControl, "Name" );
		pdchWscShading = newPreDefColumn( pdstWindowControl, "Type" );
		pdchWscShadCons = newPreDefColumn( pdstWindowControl, "Shaded Construction" );
		pdchWscControl = newPreDefColumn( pdstWindowControl, "Control" );
		pdchWscGlare = newPreDefColumn( pdstWindowControl, "Glare Control" );

		// Lighting Report
		pdrLighting = newPreDefReport( "LightingSummary", "Light", "Lighting Summary" );

		pdstInLite = newPreDefSubTable( pdrLighting, "Interior Lighting" );

		pdchInLtZone = newPreDefColumn( pdstInLite, "Zone" );
		pdchInLtDens = newPreDefColumn( pdstInLite, "Lighting Power Density [W/m2]" );
		pdchInLtArea = newPreDefColumn( pdstInLite, "Zone Area [m2]" );
		pdchInLtPower = newPreDefColumn( pdstInLite, "Total Power [W]" );
		pdchInLtEndUse = newPreDefColumn( pdstInLite, "End Use Subcategory" );
		pdchInLtSchd = newPreDefColumn( pdstInLite, "Schedule Name" );
		pdchInLtAvgHrSchd = newPreDefColumn( pdstInLite, "Scheduled Hours/Week [hr]" );
		pdchInLtAvgHrOper = newPreDefColumn( pdstInLite, "Hours/Week > 1% [hr]" );
		pdchInLtFullLoadHrs = newPreDefColumn( pdstInLite, "Full Load Hours/Week [hr]" );
		pdchInLtRetAir = newPreDefColumn( pdstInLite, "Return Air Fraction" );
		pdchInLtCond = newPreDefColumn( pdstInLite, "Conditioned (Y/N)" );
		pdchInLtConsump = newPreDefColumn( pdstInLite, "Consumption [GJ]" );

		pdstDaylight = newPreDefSubTable( pdrLighting, "Daylighting" );

		pdchDyLtZone = newPreDefColumn( pdstDaylight, "Zone" );
		pdchDyLtKind = newPreDefColumn( pdstDaylight, "Daylighting Type" ); //detailed or DElight
		pdchDyLtCtrl = newPreDefColumn( pdstDaylight, "Control Type" ); //stepped or continuous
		pdchDyLtFrac = newPreDefColumn( pdstDaylight, "Fraction Controlled" );
		pdchDyLtWInst = newPreDefColumn( pdstDaylight, "Lighting Installed in Zone [W]" );
		pdchDyLtWCtrl = newPreDefColumn( pdstDaylight, "Lighting Controlled [W]" );

		pdstExtLite = newPreDefSubTable( pdrLighting, "Exterior Lighting" );

		pdchExLtPower = newPreDefColumn( pdstExtLite, "Total Watts" );
		pdchExLtClock = newPreDefColumn( pdstExtLite, "Astronomical Clock/Schedule" );
		pdchExLtSchd = newPreDefColumn( pdstExtLite, "Schedule Name" );
		pdchExLtAvgHrSchd = newPreDefColumn( pdstExtLite, "Scheduled Hours/Week [hr]" );
		pdchExLtAvgHrOper = newPreDefColumn( pdstExtLite, "Hours/Week > 1% [hr]" );
		pdchExLtFullLoadHrs = newPreDefColumn( pdstExtLite, "Full Load Hours/Week [hr]" );
		pdchExLtConsump = newPreDefColumn( pdstExtLite, "Consumption [GJ]" );

		// HVAC Equipment Report

		pdrEquip = newPreDefReport( "EquipmentSummary", "Equip", "Equipment Summary" );

		pdstMech = newPreDefSubTable( pdrEquip, "Central Plant" );

		pdchMechType = newPreDefColumn( pdstMech, "Type" );
		pdchMechNomCap = newPreDefColumn( pdstMech, "Nominal Capacity [W]" );
		pdchMechNomEff = newPreDefColumn( pdstMech, "Nominal Efficiency [W/W]" );
		pdchMechIPLVSI = newPreDefColumn( pdstMech, "IPLV in SI Units [W/W]" );
		pdchMechIPLVIP = newPreDefColumn( pdstMech, "IPLV in IP Units [Btu/W-h]" );

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
		// o  Coil_HeatingGas                 Coil:Heating:Gas                       HVACHeatingCoils         5ZoneAutoDXVAV
		// o  Coil_HeatingElectric            Coil:Heating:Electric                  HVACHeatingCoils         PackagedTerminalAirConditioner
		// o  Coil_HeatingDesuperheater       Coil:Heating:Desuperheater             HVACHeatingCoils         SuperMarket_DesuperHeatingCoil
		// o  Coil_HeatingWater               Coil:Heating:Water                     HVACWaterCoilComponent   5ZoneAirCooled
		// o  Coil_HeatingWaterToAirHP        Coil:Heating:WaterToAirHeatPump:       HVACWaterToAir           5ZoneWaterLoopHeatPump
		//                                      ParameterEstimation
		// o  Coil_HeatingWaterToAirHPSimple  Coil:Heating:WaterToAirHeatPump:       HVACWaterToAir           HeatPumpWaterToAirEquationFit
		//                                      EquationFit
		// o  CoilDX_HeatPumpWaterHeater      Coil:WaterHeating:AirToWaterHeatPump   DXCoil                   HeatPumpWaterHeater

		//NOT INCLUDED:
		//    CoilDX_CoolingHXAssisted        CoilSystem:Cooling:DX:                 HVACHXAssistedCooolingCoil
		//                                      HeatExchangerAssisted
		//    CoilWater_CoolingHXAssisted     CoilSystem:Cooling:Water:              HVACHXAssistedCooolingCoil
		//                                      HeatExchangerAssisted

		pdstCoolCoil = newPreDefSubTable( pdrEquip, "Cooling Coils" );

		pdchCoolCoilType = newPreDefColumn( pdstCoolCoil, "Type" );
		pdchCoolCoilDesCap = newPreDefColumn( pdstCoolCoil, "Design Coil Load [W]" );
		pdchCoolCoilTotCap = newPreDefColumn( pdstCoolCoil, "Nominal Total Capacity [W]" );
		pdchCoolCoilSensCap = newPreDefColumn( pdstCoolCoil, "Nominal Sensible Capacity [W]" );
		pdchCoolCoilLatCap = newPreDefColumn( pdstCoolCoil, "Nominal Latent Capacity [W]" );
		pdchCoolCoilSHR = newPreDefColumn( pdstCoolCoil, "Nominal Sensible Heat Ratio" );
		pdchCoolCoilNomEff = newPreDefColumn( pdstCoolCoil, "Nominal Efficiency [W/W]" );
		pdchCoolCoilUATotal = newPreDefColumn( pdstCoolCoil, "Nominal Coil UA Value [W/C]" );
		pdchCoolCoilArea = newPreDefColumn( pdstCoolCoil, "Nominal Coil Surface Area [m2]" );

		pdstDXCoolCoil = newPreDefSubTable( pdrEquip, "DX Cooling Coils" );
		pdchDXCoolCoilType = newPreDefColumn( pdstDXCoolCoil, "DX Cooling Coil Type" );
		pdchDXCoolCoilNetCapSI = newPreDefColumn( pdstDXCoolCoil, "Standard Rated Net Cooling Capacity [W]" );

		pdchDXCoolCoilCOP = newPreDefColumn( pdstDXCoolCoil, "Standard Rated Net COP [W/W]" );
		pdchDXCoolCoilEERIP = newPreDefColumn( pdstDXCoolCoil, "EER [Btu/W-h]" );
		pdchDXCoolCoilSEERIP = newPreDefColumn( pdstDXCoolCoil, "SEER [Btu/W-h]" );
		pdchDXCoolCoilIEERIP = newPreDefColumn( pdstDXCoolCoil, "IEER [Btu/W-h]" );

		// for DX Cooling Coil ASHRAE 127-12 Report
		pdstDXCoolCoil2 = newPreDefSubTable( pdrEquip, "DX Cooling Coil ASHRAE 127 Standard Ratings Report" );
		pdchDXCoolCoilType = newPreDefColumn( pdstDXCoolCoil2, "DX Cooling Coil Type" );
		pdchDXCoolCoilNetCapSIA = newPreDefColumn( pdstDXCoolCoil2, "Rated Net Cooling Capacity Test A [W]" );
		pdchDXCoolCoilElecPowerA = newPreDefColumn( pdstDXCoolCoil2, "Rated Electric Power Test A [W]" );
		pdchDXCoolCoilNetCapSIB = newPreDefColumn( pdstDXCoolCoil2, "Rated Net Cooling Capacity Test B [W]" );
		pdchDXCoolCoilElecPowerB = newPreDefColumn( pdstDXCoolCoil2, "Rated Electric Power Test B [W]" );
		pdchDXCoolCoilNetCapSIC = newPreDefColumn( pdstDXCoolCoil2, "Rated Net Cooling Capacity Test C [W]" );
		pdchDXCoolCoilElecPowerC = newPreDefColumn( pdstDXCoolCoil2, "Rated Electric Power Test C [W]" );
		pdchDXCoolCoilNetCapSID = newPreDefColumn( pdstDXCoolCoil2, "Rated Net Cooling Capacity Test D [W]" );
		pdchDXCoolCoilElecPowerD = newPreDefColumn( pdstDXCoolCoil2, "Rated Electric Power Test D [W]" );

		pdstDXHeatCoil = newPreDefSubTable( pdrEquip, "DX Heating Coils" );
		pdchDXHeatCoilType = newPreDefColumn( pdstDXHeatCoil, "DX Heating Coil Type" );
		pdchDXHeatCoilHighCap = newPreDefColumn( pdstDXHeatCoil, "High Temperature Heating (net) Rating Capacity [W]" );
		pdchDXHeatCoilLowCap = newPreDefColumn( pdstDXHeatCoil, "Low Temperature Heating (net) Rating Capacity [W]" );
		pdchDXHeatCoilHSPFIP = newPreDefColumn( pdstDXHeatCoil, "HSPF [Btu/W-h]" );
		pdchDXHeatCoilRegionNum = newPreDefColumn( pdstDXHeatCoil, "Region Number" );

		pdstHeatCoil = newPreDefSubTable( pdrEquip, "Heating Coils" );

		pdchHeatCoilType = newPreDefColumn( pdstHeatCoil, "Type" );
		pdchHeatCoilDesCap = newPreDefColumn( pdstHeatCoil, "Design Coil Load [W]" );
		pdchHeatCoilNomCap = newPreDefColumn( pdstHeatCoil, "Nominal Total Capacity [W]" );
		pdchHeatCoilNomEff = newPreDefColumn( pdstHeatCoil, "Nominal Efficiency [W/W]" );

		pdstFan = newPreDefSubTable( pdrEquip, "Fans" );

		pdchFanType = newPreDefColumn( pdstFan, "Type" );
		pdchFanTotEff = newPreDefColumn( pdstFan, "Total Efficiency [W/W]" );
		pdchFanDeltaP = newPreDefColumn( pdstFan, "Delta Pressure [pa]" );
		pdchFanVolFlow = newPreDefColumn( pdstFan, "Max Air Flow Rate [m3/s]" );
		pdchFanPwr = newPreDefColumn( pdstFan, "Rated Electric Power [W]" );
		pdchFanPwrPerFlow = newPreDefColumn( pdstFan, "Rated Power Per Max Air Flow Rate [W-s/m3]" );
		pdchFanMotorIn = newPreDefColumn( pdstFan, "Motor Heat In Air Fraction" );
		pdchFanEndUse = newPreDefColumn( pdstFan, "End Use" );

		pdstPump = newPreDefSubTable( pdrEquip, "Pumps" );
		pdchPumpType = newPreDefColumn( pdstPump, "Type" );
		pdchPumpControl = newPreDefColumn( pdstPump, "Control" );
		pdchPumpHead = newPreDefColumn( pdstPump, "Head [pa]" );
		pdchPumpFlow = newPreDefColumn( pdstPump, "Water Flow [m3/s]" );
		pdchPumpPower = newPreDefColumn( pdstPump, "Electric Power [W]" );
		pdchPumpPwrPerFlow = newPreDefColumn( pdstPump, "Power Per Water Flow Rate [W-s/m3]" );
		pdchMotEff = newPreDefColumn( pdstPump, "Motor Efficiency [W/W]" );

		pdstSWH = newPreDefSubTable( pdrEquip, "Service Water Heating" );
		pdchSWHType = newPreDefColumn( pdstSWH, "Type" );
		pdchSWHVol = newPreDefColumn( pdstSWH, "Storage Volume [m3]" );
		pdchSWHHeatIn = newPreDefColumn( pdstSWH, "Input [W]" );
		pdchSWHThEff = newPreDefColumn( pdstSWH, "Thermal Efficiency [W/W]" );
		pdchSWHRecEff = newPreDefColumn( pdstSWH, "Recovery Efficiency [W/W]" );
		pdchSWHEnFac = newPreDefColumn( pdstSWH, "Energy Factor" );

		// Sizing Report

		pdrSizing = newPreDefReport( "HVACSizingSummary", "Size", "HVAC Sizing Summary" );

		pdstZoneClSize = newPreDefSubTable( pdrSizing, "Zone Sensible Cooling" );

		pdchZnClCalcDesLd = newPreDefColumn( pdstZoneClSize, "Calculated Design Load [W]" );
		pdchZnClUserDesLd = newPreDefColumn( pdstZoneClSize, "User Design Load [W]" );
		pdchZnClUserDesLdPerArea = newPreDefColumn( pdstZoneClSize, "User Design Load per Area [W/m2]" );
		pdchZnClCalcDesAirFlow = newPreDefColumn( pdstZoneClSize, "Calculated Design Air Flow [m3/s]" );
		pdchZnClUserDesAirFlow = newPreDefColumn( pdstZoneClSize, "User Design Air Flow [m3/s]" );
		pdchZnClDesDay = newPreDefColumn( pdstZoneClSize, "Design Day Name" );
		pdchZnClPkTime = newPreDefColumn( pdstZoneClSize, "Date/Time Of Peak" );
		pdchZnClPkTstatTemp = newPreDefColumn( pdstZoneClSize, "Thermostat Setpoint Temperature at Peak Load [C]" );
		pdchZnClPkIndTemp = newPreDefColumn( pdstZoneClSize, "Indoor Temperature at Peak Load [C]" );
		pdchZnClPkIndHum = newPreDefColumn( pdstZoneClSize, "Indoor Humidity Ratio at Peak Load [kgWater/kgAir]" );
		pdchZnClPkOATemp = newPreDefColumn( pdstZoneClSize, "Outdoor Temperature at Peak Load [C]" );
		pdchZnClPkOAHum = newPreDefColumn( pdstZoneClSize, "Outdoor Humidity Ratio at Peak Load [kgWater/kgAir]" );
		addFootNoteSubTable( pdstZoneClSize, "The Design Load is the zone sensible load only. It does not include any system effects or ventilation loads." );

		pdstZoneHtSize = newPreDefSubTable( pdrSizing, "Zone Sensible Heating" );

		pdchZnHtCalcDesLd = newPreDefColumn( pdstZoneHtSize, "Calculated Design Load [W]" );
		pdchZnHtUserDesLd = newPreDefColumn( pdstZoneHtSize, "User Design Load [W]" );
		pdchZnHtUserDesLdPerArea = newPreDefColumn( pdstZoneHtSize, "User Design Load per Area [W/m2]" );
		pdchZnHtCalcDesAirFlow = newPreDefColumn( pdstZoneHtSize, "Calculated Design Air Flow [m3/s]" );
		pdchZnHtUserDesAirFlow = newPreDefColumn( pdstZoneHtSize, "User Design Air Flow [m3/s]" );
		pdchZnHtDesDay = newPreDefColumn( pdstZoneHtSize, "Design Day Name" );
		pdchZnHtPkTime = newPreDefColumn( pdstZoneHtSize, "Date/Time Of Peak" );
		pdchZnHtPkTstatTemp = newPreDefColumn( pdstZoneHtSize, "Thermostat Setpoint Temperature at Peak Load [C]" );
		pdchZnHtPkIndTemp = newPreDefColumn( pdstZoneHtSize, "Indoor Temperature at Peak Load [C]" );
		pdchZnHtPkIndHum = newPreDefColumn( pdstZoneHtSize, "Indoor Humidity Ratio at Peak Load [kgWater/kgAir]" );
		pdchZnHtPkOATemp = newPreDefColumn( pdstZoneHtSize, "Outdoor Temperature at Peak Load [C]" );
		pdchZnHtPkOAHum = newPreDefColumn( pdstZoneHtSize, "Outdoor Humidity Ratio at Peak Load [kgWater/kgAir]" );
		addFootNoteSubTable( pdstZoneHtSize, "The Design Load is the zone sensible load only. It does not include any system effects or ventilation loads." );

		pdstSystemSize = newPreDefSubTable( pdrSizing, "System Design Air Flow Rates" );

		pdchSysSizCalcClAir = newPreDefColumn( pdstSystemSize, "Calculated cooling [m3/s]" );
		pdchSysSizUserClAir = newPreDefColumn( pdstSystemSize, "User cooling [m3/s]" );
		pdchSysSizCalcHtAir = newPreDefColumn( pdstSystemSize, "Calculated heating [m3/s]" );
		pdchSysSizUserHtAir = newPreDefColumn( pdstSystemSize, "User heating [m3/s]" );

		pdstPlantSize = newPreDefSubTable( pdrSizing, "Plant Loop Coincident Design Fluid Flow Rate Adjustments" );
//		pdchPlantSizPass = newPreDefColumn( pdstPlantSize, "Sizing Pass" );
		pdchPlantSizPrevVdot = newPreDefColumn( pdstPlantSize, "Previous Design Volume Flow Rate [m3/s]" );
		pdchPlantSizMeasVdot = newPreDefColumn( pdstPlantSize, "Algorithm Volume Flow Rate [m3/s]" );
		pdchPlantSizCalcVdot = newPreDefColumn( pdstPlantSize, "Coincident Design Volume Flow Rate [m3/s]" );
		pdchPlantSizCoincYesNo = newPreDefColumn( pdstPlantSize, "Coincident Size Adjusted" );
		pdchPlantSizDesDay = newPreDefColumn( pdstPlantSize, "Peak Sizing Period Name" );
		pdchPlantSizPkTimeDayOfSim = newPreDefColumn( pdstPlantSize, "Peak Day into Period" );
		pdchPlantSizPkTimeHour = newPreDefColumn( pdstPlantSize, "Peak Hour Of Day" );
		pdchPlantSizPkTimeMin = newPreDefColumn( pdstPlantSize, "Peak Step Start Minute" );

		// System Summary Report

		pdrSystem = newPreDefReport( "SystemSummary", "Sys", "System Summary" );

		pdstEconomizer = newPreDefSubTable( pdrSystem, "Economizer" );

		pdchEcoKind = newPreDefColumn( pdstEconomizer, "High Limit Shutoff Control" );
		pdchEcoMinOA = newPreDefColumn( pdstEconomizer, "Minimum Outdoor Air [m3/s]" );
		pdchEcoMaxOA = newPreDefColumn( pdstEconomizer, "Maximum Outdoor Air [m3/s]" );
		pdchEcoRetTemp = newPreDefColumn( pdstEconomizer, "Return Air Temp Limit" );
		pdchEcoRetEnth = newPreDefColumn( pdstEconomizer, "Return Air Enthalpy Limit" );
		pdchEcoOATempLim = newPreDefColumn( pdstEconomizer, "Outdoor Air Temperature Limit [C]" );
		pdchEcoOAEnthLim = newPreDefColumn( pdstEconomizer, "Outdoor Air Enthalpy Limit [C]" );

		pdstDemCntlVent = newPreDefSubTable( pdrSystem, "Demand Controlled Ventilation using Controller:MechanicalVentilation" );
		pdchDCVventMechName = newPreDefColumn( pdstDemCntlVent, "Controller:MechanicalVentilation Name" );
		pdchDCVperPerson = newPreDefColumn( pdstDemCntlVent, "Outdoor Air Per Person [m3/s-person]" );
		pdchDCVperArea = newPreDefColumn( pdstDemCntlVent, "Outdoor Air Per Area [m3/s-m2]" );

		// added for new DCV
		pdchDCVZoneADEffCooling = newPreDefColumn( pdstDemCntlVent, "Air Distribution Effectiveness in Cooling Mode" );
		pdchDCVZoneADEffHeating = newPreDefColumn( pdstDemCntlVent, "Air Distribution Effectiveness in Heating Mode" );
		pdchDCVZoneADEffSchName = newPreDefColumn( pdstDemCntlVent, "Air Distribution Effectiveness Schedule" );

		pdstSimpleComfort = newPreDefSubTable( pdrSystem, "Time Not Comfortable Based on Simple ASHRAE 55-2004" );
		pdchSCwinterClothes = newPreDefColumn( pdstSimpleComfort, "Winter Clothes [hr]" );
		pdchSCsummerClothes = newPreDefColumn( pdstSimpleComfort, "Summer Clothes [hr]" );
		pdchSCeitherClothes = newPreDefColumn( pdstSimpleComfort, "Summer or Winter Clothes [hr]" );

		pdstUnmetLoads = newPreDefSubTable( pdrSystem, "Time Setpoint Not Met" );
		pdchULnotMetHeat = newPreDefColumn( pdstUnmetLoads, "During Heating [hr]" );
		pdchULnotMetCool = newPreDefColumn( pdstUnmetLoads, "During Cooling [hr]" );
		pdchULnotMetHeatOcc = newPreDefColumn( pdstUnmetLoads, "During Occupied Heating [hr]" );
		pdchULnotMetCoolOcc = newPreDefColumn( pdstUnmetLoads, "During Occupied Cooling [hr]" );

		// Outside Air Report
		pdrOutsideAir = newPreDefReport( "OutdoorAirSummary", "OA", "Outdoor Air Summary" );

		pdstOAavgOcc = newPreDefSubTable( pdrOutsideAir, "Average Outdoor Air During Occupied Hours" );

		pdchOaoAvgNumOcc1 = newPreDefColumn( pdstOAavgOcc, "Average Number of Occupants" );
		pdchOaoNomNumOcc1 = newPreDefColumn( pdstOAavgOcc, "Nominal Number of Occupants" );
		pdchOaoZoneVol1 = newPreDefColumn( pdstOAavgOcc, "Zone Volume [m3]" );
		pdchOaoAvgMechVent = newPreDefColumn( pdstOAavgOcc, "Mechanical Ventilation [ach]" );
		pdchOaoAvgInfil = newPreDefColumn( pdstOAavgOcc, "Infiltration [ach]" );
		pdchOaoAvgAFNInfil = newPreDefColumn( pdstOAavgOcc, "AFN Infiltration [ach]" );
		pdchOaoAvgSimpVent = newPreDefColumn( pdstOAavgOcc, "Simple Ventilation [ach]" );
		//pdchOaoAvgTotVent =   newPreDefColumn(pdstOAavgOcc,'Total Ventilation [ach]')

		addFootNoteSubTable( pdstOAavgOcc, "Values shown for a single zone without multipliers" );

		pdstOAminOcc = newPreDefSubTable( pdrOutsideAir, "Minimum Outdoor Air During Occupied Hours" );

		pdchOaoAvgNumOcc2 = newPreDefColumn( pdstOAminOcc, "Average Number of Occupants" );
		pdchOaoNomNumOcc2 = newPreDefColumn( pdstOAminOcc, "Nominal Number of Occupants" );
		pdchOaoZoneVol2 = newPreDefColumn( pdstOAminOcc, "Zone Volume [m3]" );
		pdchOaoMinMechVent = newPreDefColumn( pdstOAminOcc, "Mechanical Ventilation [ach]" );
		pdchOaoMinInfil = newPreDefColumn( pdstOAminOcc, "Infiltration [ach]" );
		pdchOaoMinAFNInfil = newPreDefColumn( pdstOAminOcc, "AFN Infiltration [ach]" );
		pdchOaoMinSimpVent = newPreDefColumn( pdstOAminOcc, "Simple Ventilation [ach]" );
		//pdchOaoMinTotVent =   newPreDefColumn(pdstOAminOcc,'Total Ventilation [ach]')
		addFootNoteSubTable( pdstOAminOcc, "Values shown for a single zone without multipliers" );

		// Object Count Report
		pdrObjCnt = newPreDefReport( "ObjectCountSummary", "Count", "Object Count Summary" );

		pdstSurfCnt = newPreDefSubTable( pdrObjCnt, "Surfaces by Class" );
		pdchSurfCntTot = newPreDefColumn( pdstSurfCnt, "Total" );
		pdchSurfCntExt = newPreDefColumn( pdstSurfCnt, "Outdoors" );

		pdstHVACcnt = newPreDefSubTable( pdrObjCnt, "HVAC" );
		pdchHVACcntVal = newPreDefColumn( pdstHVACcnt, "Count" );

		pdstFieldCnt = newPreDefSubTable( pdrObjCnt, "Input Fields" );
		pdchFieldCntVal = newPreDefColumn( pdstFieldCnt, "Count" );

		// Energy Meters report
		pdrEnergyMeters = newPreDefReport( "EnergyMeters", "Meters", "Energy Meters" );

		//pdstEMvalues = newPreDefSubTable(pdrEnergyMeters,'Annual and Peak Values')
		//pdchEMannual = newPreDefColumn(pdstEMvalues,'Annual Value [GJ]')
		//pdchEMminvalue = newPreDefColumn(pdstEMvalues,'Minimum Value [J]')
		//pdchEMminvaluetime = newPreDefColumn(pdstEMvalues,'Timestamp of Minimum')
		//pdchEMmaxvalue = newPreDefColumn(pdstEMvalues,'Maximum Value [J]')
		//pdchEMmaxvaluetime = newPreDefColumn(pdstEMvalues,'Timestamp of Maximum')
		// Electricity Sub Table
		pdstEMelecvalues = newPreDefSubTable( pdrEnergyMeters, "Annual and Peak Values - Electricity" );
		pdchEMelecannual = newPreDefColumn( pdstEMelecvalues, "Electricity Annual Value [GJ]" );
		pdchEMelecminvalue = newPreDefColumn( pdstEMelecvalues, "Electricity Minimum Value [W]" );
		pdchEMelecminvaluetime = newPreDefColumn( pdstEMelecvalues, "Timestamp of Minimum" );
		pdchEMelecmaxvalue = newPreDefColumn( pdstEMelecvalues, "Electricity Maximum Value [W]" );
		pdchEMelecmaxvaluetime = newPreDefColumn( pdstEMelecvalues, "Timestamp of Maximum" );

		// Gas Sub Table
		pdstEMgasvalues = newPreDefSubTable( pdrEnergyMeters, "Annual and Peak Values - Gas" );
		pdchEMgasannual = newPreDefColumn( pdstEMgasvalues, "Gas Annual Value [GJ]" );
		pdchEMgasminvalue = newPreDefColumn( pdstEMgasvalues, "Gas Minimum Value [W]" );
		pdchEMgasminvaluetime = newPreDefColumn( pdstEMgasvalues, "Timestamp of Minimum" );
		pdchEMgasmaxvalue = newPreDefColumn( pdstEMgasvalues, "Gas Maximum Value [W]" );
		pdchEMgasmaxvaluetime = newPreDefColumn( pdstEMgasvalues, "Timestamp of Maximum" );

		// Cool SubTable
		pdstEMcoolvalues = newPreDefSubTable( pdrEnergyMeters, "Annual and Peak Values - Cooling" );
		pdchEMcoolannual = newPreDefColumn( pdstEMcoolvalues, "Cooling Annual Value [GJ]" );
		pdchEMcoolminvalue = newPreDefColumn( pdstEMcoolvalues, "Cooling Minimum Value [W]" );
		pdchEMcoolminvaluetime = newPreDefColumn( pdstEMcoolvalues, "Timestamp of Minimum" );
		pdchEMcoolmaxvalue = newPreDefColumn( pdstEMcoolvalues, "Cooling Maximum Value [W]" );
		pdchEMcoolmaxvaluetime = newPreDefColumn( pdstEMcoolvalues, "Timestamp of Maximum" );

		// Water SubTable
		pdstEMwatervalues = newPreDefSubTable( pdrEnergyMeters, "Annual and Peak Values - Water" );
		pdchEMwaterannual = newPreDefColumn( pdstEMwatervalues, "Annual Value [m3]" );
		pdchEMwaterminvalue = newPreDefColumn( pdstEMwatervalues, "Minimum Value [m3/s]" );
		pdchEMwaterminvaluetime = newPreDefColumn( pdstEMwatervalues, "Timestamp of Minimum" );
		pdchEMwatermaxvalue = newPreDefColumn( pdstEMwatervalues, "Maximum Value [m3/s]" );
		pdchEMwatermaxvaluetime = newPreDefColumn( pdstEMwatervalues, "Timestamp of Maximum" );

		// Other KG SubTable
		pdstEMotherKGvalues = newPreDefSubTable( pdrEnergyMeters, "Annual and Peak Values - Other by Weight/Mass" );
		pdchEMotherKGannual = newPreDefColumn( pdstEMotherKGvalues, "Annual Value [kg]" );
		pdchEMotherKGminvalue = newPreDefColumn( pdstEMotherKGvalues, "Minimum Value [kg/s]" );
		pdchEMotherKGminvaluetime = newPreDefColumn( pdstEMotherKGvalues, "Timestamp of Minimum" );
		pdchEMotherKGmaxvalue = newPreDefColumn( pdstEMotherKGvalues, "Maximum Value [kg/s]" );
		pdchEMotherKGmaxvaluetime = newPreDefColumn( pdstEMotherKGvalues, "Timestamp of Maximum" );

		// Other M3 SubTable
		pdstEMotherM3values = newPreDefSubTable( pdrEnergyMeters, "Annual and Peak Values - Other Volumetric" );
		pdchEMotherM3annual = newPreDefColumn( pdstEMotherM3values, "Annual Value [m3]" );
		pdchEMotherM3minvalue = newPreDefColumn( pdstEMotherM3values, "Minimum Value [m3/s]" );
		pdchEMotherM3minvaluetime = newPreDefColumn( pdstEMotherM3values, "Timestamp of Minimum" );
		pdchEMotherM3maxvalue = newPreDefColumn( pdstEMotherM3values, "Maximum Value [m3/s]" );
		pdchEMotherM3maxvaluetime = newPreDefColumn( pdstEMotherM3values, "Timestamp of Maximum" );

		// Other M3 SubTable
		pdstEMotherLvalues = newPreDefSubTable( pdrEnergyMeters, "Annual and Peak Values - Other Liquid/Gas" );
		pdchEMotherLannual = newPreDefColumn( pdstEMotherLvalues, "Annual Value [L]" );
		pdchEMotherLminvalue = newPreDefColumn( pdstEMotherLvalues, "Minimum Value [L]" );
		pdchEMotherLminvaluetime = newPreDefColumn( pdstEMotherLvalues, "Timestamp of Minimum" );
		pdchEMotherLmaxvalue = newPreDefColumn( pdstEMotherLvalues, "Maximum Value [L]" );
		pdchEMotherLmaxvaluetime = newPreDefColumn( pdstEMotherLvalues, "Timestamp of Maximum" );

		// Other J SubTable
		pdstEMotherJvalues = newPreDefSubTable( pdrEnergyMeters, "Annual and Peak Values - Other" );
		pdchEMotherJannual = newPreDefColumn( pdstEMotherJvalues, "Annual Value [GJ]" );
		pdchEMotherJminvalue = newPreDefColumn( pdstEMotherJvalues, "Minimum Value [W]" );
		pdchEMotherJminvaluetime = newPreDefColumn( pdstEMotherJvalues, "Timestamp of Minimum" );
		pdchEMotherJmaxvalue = newPreDefColumn( pdstEMotherJvalues, "Maximum Value [W]" );
		pdchEMotherJmaxvaluetime = newPreDefColumn( pdstEMotherJvalues, "Timestamp of Maximum" );

		// Sensible Heat Gas Component Report
		pdrSensibleGain = newPreDefReport( "SensibleHeatGainSummary", "SHGS", "Sensible Heat Gain Summary" );

		pdstSHGSannual = newPreDefSubTable( pdrSensibleGain, "Annual Building Sensible Heat Gain Components" );

		pdchSHGSAnHvacHt = newPreDefColumn( pdstSHGSannual, "HVAC Input Sensible Air Heating [GJ]" );
		pdchSHGSAnHvacCl = newPreDefColumn( pdstSHGSannual, "HVAC Input Sensible Air Cooling [GJ]" );
		pdchSHGSAnSurfHt = newPreDefColumn( pdstSHGSannual, "HVAC Input Heated Surface Heating [GJ]" );
		pdchSHGSAnSurfCl = newPreDefColumn( pdstSHGSannual, "HVAC Input Cooled Surface Cooling [GJ]" );
		pdchSHGSAnPeoplAdd = newPreDefColumn( pdstSHGSannual, "People Sensible Heat Addition [GJ]" );
		pdchSHGSAnLiteAdd = newPreDefColumn( pdstSHGSannual, "Lights Sensible Heat Addition [GJ]" );
		pdchSHGSAnEquipAdd = newPreDefColumn( pdstSHGSannual, "Equipment Sensible Heat Addition [GJ]" );
		pdchSHGSAnWindAdd = newPreDefColumn( pdstSHGSannual, "Window Heat Addition [GJ]" );
		pdchSHGSAnIzaAdd = newPreDefColumn( pdstSHGSannual, "Interzone Air Transfer Heat Addition [GJ]" );
		pdchSHGSAnInfilAdd = newPreDefColumn( pdstSHGSannual, "Infiltration Heat Addition [GJ]" );
		pdchSHGSAnOtherAdd = newPreDefColumn( pdstSHGSannual, "Opaque Surface Conduction and Other Heat Addition [GJ]" );
		pdchSHGSAnEquipRem = newPreDefColumn( pdstSHGSannual, "Equipment Sensible Heat Removal [GJ]" );
		pdchSHGSAnWindRem = newPreDefColumn( pdstSHGSannual, "Window Heat Removal [GJ]" );
		pdchSHGSAnIzaRem = newPreDefColumn( pdstSHGSannual, "Interzone Air Transfer Heat Removal [GJ]" );
		pdchSHGSAnInfilRem = newPreDefColumn( pdstSHGSannual, "Infiltration Heat Removal [GJ]" );
		pdchSHGSAnOtherRem = newPreDefColumn( pdstSHGSannual, "Opaque Surface Conduction and Other Heat Removal [GJ]" );

		pdstSHGSpkCl = newPreDefSubTable( pdrSensibleGain, "Peak Cooling Sensible Heat Gain Components" );

		pdchSHGSClTimePeak = newPreDefColumn( pdstSHGSpkCl, "Time of Peak" );
		pdchSHGSClHvacHt = newPreDefColumn( pdstSHGSpkCl, "HVAC Input Sensible Air Heating [W]" );
		pdchSHGSClHvacCl = newPreDefColumn( pdstSHGSpkCl, "HVAC Input Sensible Air Cooling [W]" );
		pdchSHGSClSurfHt = newPreDefColumn( pdstSHGSpkCl, "HVAC Input Heated Surface Heating [W]" );
		pdchSHGSClSurfCl = newPreDefColumn( pdstSHGSpkCl, "HVAC Input Cooled Surface Cooling [W]" );
		pdchSHGSClPeoplAdd = newPreDefColumn( pdstSHGSpkCl, "People Sensible Heat Addition [W]" );
		pdchSHGSClLiteAdd = newPreDefColumn( pdstSHGSpkCl, "Lights Sensible Heat Addition [W]" );
		pdchSHGSClEquipAdd = newPreDefColumn( pdstSHGSpkCl, "Equipment Sensible Heat Addition [W]" );
		pdchSHGSClWindAdd = newPreDefColumn( pdstSHGSpkCl, "Window Heat Addition [W]" );
		pdchSHGSClIzaAdd = newPreDefColumn( pdstSHGSpkCl, "Interzone Air Transfer Heat Addition [W]" );
		pdchSHGSClInfilAdd = newPreDefColumn( pdstSHGSpkCl, "Infiltration Heat Addition [W]" );
		pdchSHGSClOtherAdd = newPreDefColumn( pdstSHGSpkCl, "Opaque Surface Conduction and Other Heat Addition [W]" );
		pdchSHGSClEquipRem = newPreDefColumn( pdstSHGSpkCl, "Equipment Sensible Heat Removal [W]" );
		pdchSHGSClWindRem = newPreDefColumn( pdstSHGSpkCl, "Window Heat Removal [W]" );
		pdchSHGSClIzaRem = newPreDefColumn( pdstSHGSpkCl, "Interzone Air Transfer Heat Removal [W]" );
		pdchSHGSClInfilRem = newPreDefColumn( pdstSHGSpkCl, "Infiltration Heat Removal [W]" );
		pdchSHGSClOtherRem = newPreDefColumn( pdstSHGSpkCl, "Opaque Surface Conduction and Other Heat Removal [W]" );

		pdstSHGSpkHt = newPreDefSubTable( pdrSensibleGain, "Peak Heating Sensible Heat Gain Components" );

		pdchSHGSHtTimePeak = newPreDefColumn( pdstSHGSpkHt, "Time of Peak" );
		pdchSHGSHtHvacHt = newPreDefColumn( pdstSHGSpkHt, "HVAC Input Sensible Air Heating [W]" );
		pdchSHGSHtHvacCl = newPreDefColumn( pdstSHGSpkHt, "HVAC Input Sensible Air Cooling [W]" );
		pdchSHGSHtSurfHt = newPreDefColumn( pdstSHGSpkHt, "HVAC Input Heated Surface Heating [W]" );
		pdchSHGSHtSurfCl = newPreDefColumn( pdstSHGSpkHt, "HVAC Input Cooled Surface Cooling [W]" );
		pdchSHGSHtPeoplAdd = newPreDefColumn( pdstSHGSpkHt, "People Sensible Heat Addition [W]" );
		pdchSHGSHtLiteAdd = newPreDefColumn( pdstSHGSpkHt, "Lights Sensible Heat Addition [W]" );
		pdchSHGSHtEquipAdd = newPreDefColumn( pdstSHGSpkHt, "Equipment Sensible Heat Addition [W]" );
		pdchSHGSHtWindAdd = newPreDefColumn( pdstSHGSpkHt, "Window Heat Addition [W]" );
		pdchSHGSHtIzaAdd = newPreDefColumn( pdstSHGSpkHt, "Interzone Air Transfer Heat Addition [W]" );
		pdchSHGSHtInfilAdd = newPreDefColumn( pdstSHGSpkHt, "Infiltration Heat Addition [W]" );
		pdchSHGSHtOtherAdd = newPreDefColumn( pdstSHGSpkHt, "Opaque Surface Conduction and Other Heat Addition [W]" );
		pdchSHGSHtEquipRem = newPreDefColumn( pdstSHGSpkHt, "Equipment Sensible Heat Removal [W]" );
		pdchSHGSHtWindRem = newPreDefColumn( pdstSHGSpkHt, "Window Heat Removal [W]" );
		pdchSHGSHtIzaRem = newPreDefColumn( pdstSHGSpkHt, "Interzone Air Transfer Heat Removal [W]" );
		pdchSHGSHtInfilRem = newPreDefColumn( pdstSHGSpkHt, "Infiltration Heat Removal [W]" );
		pdchSHGSHtOtherRem = newPreDefColumn( pdstSHGSpkHt, "Opaque Surface Conduction and Other Heat Removal [W]" );

		//Standard62Report
		if ( DoZoneSizing || DoSystemSizing ) {
			pdrStd62 = newPreDefReport( "Standard62.1Summary", "Std62", "Standard 62.1 Summary" );

			pdstS62sysVentReqCool = newPreDefSubTable( pdrStd62, "System Ventilation Requirements for Cooling" );

			pdchS62svrClSumVpz = newPreDefColumn( pdstS62sysVentReqCool, "Sum of Zone Primary Air Flow - Vpz-sum [m3/s]" );
			pdchS62svrClPs = newPreDefColumn( pdstS62sysVentReqCool, "System Population - Ps" );
			pdchS62svrClSumPz = newPreDefColumn( pdstS62sysVentReqCool, "Sum of Zone Population - Pz-sum" );
			pdchS62svrClD = newPreDefColumn( pdstS62sysVentReqCool, "Occupant Diversity - D" );
			pdchS62svrClVou = newPreDefColumn( pdstS62sysVentReqCool, "Uncorrected Outdoor Air Intake Airflow - Vou [m3/s]" );
			pdchS62svrClVps = newPreDefColumn( pdstS62sysVentReqCool, "System Primary Airflow - Vps [m3/s]" );
			pdchS62svrClXs = newPreDefColumn( pdstS62sysVentReqCool, "Average Outdoor Air Fraction - Xs" );
			pdchS62svrClEv = newPreDefColumn( pdstS62sysVentReqCool, "System Ventilation Efficiency - Ev" );
			pdchS62svrClVot = newPreDefColumn( pdstS62sysVentReqCool, "Outdoor Air Intake Flow - Vot [m3/s]" );
			pdchS62svrClPercOA = newPreDefColumn( pdstS62sysVentReqCool, "Percent Outdoor Air - %OA" );

			pdstS62sysVentReqHeat = newPreDefSubTable( pdrStd62, "System Ventilation Requirements for Heating" );

			pdchS62svrHtSumVpz = newPreDefColumn( pdstS62sysVentReqHeat, "Sum of Zone Primary Air Flow - Vpz-sum [m3/s]" );
			pdchS62svrHtPs = newPreDefColumn( pdstS62sysVentReqHeat, "System Population - Ps" );
			pdchS62svrHtSumPz = newPreDefColumn( pdstS62sysVentReqHeat, "Sum of Zone Population - Pz-sum" );
			pdchS62svrHtD = newPreDefColumn( pdstS62sysVentReqHeat, "Occupant Diversity - D" );
			pdchS62svrHtVou = newPreDefColumn( pdstS62sysVentReqHeat, "Uncorrected Outdoor Air Intake Airflow - Vou [m3/s]" );
			pdchS62svrHtVps = newPreDefColumn( pdstS62sysVentReqHeat, "System Primary Airflow - Vps [m3/s]" );
			pdchS62svrHtXs = newPreDefColumn( pdstS62sysVentReqHeat, "Average Outdoor Air Fraction - Xs" );
			pdchS62svrHtEv = newPreDefColumn( pdstS62sysVentReqHeat, "System Ventilation Efficiency - Ev" );
			pdchS62svrHtVot = newPreDefColumn( pdstS62sysVentReqHeat, "Outdoor Air Intake Flow Vot [m3/s]" );
			pdchS62svrHtPercOA = newPreDefColumn( pdstS62sysVentReqHeat, "Percent Outdoor Air - %OA" );

			pdstS62znVentPar = newPreDefSubTable( pdrStd62, "Zone Ventilation Parameters" );

			pdchS62zvpAlN = newPreDefColumn( pdstS62znVentPar, "AirLoop Name" );
			pdchS62zvpRp = newPreDefColumn( pdstS62znVentPar, "People Outdoor Air Rate - Rp [m3/s-person]" );
			pdchS62zvpPz = newPreDefColumn( pdstS62znVentPar, "Zone Population - Pz" );
			pdchS62zvpRa = newPreDefColumn( pdstS62znVentPar, "Area Outdoor Air Rate - Ra [m3/s-m2]" );
			pdchS62zvpAz = newPreDefColumn( pdstS62znVentPar, "Zone Floor Area - Az [m2]" );
			pdchS62zvpVbz = newPreDefColumn( pdstS62znVentPar, "Breathing Zone Outdoor Airflow - Vbz [m3/s]" );
			pdchS62zvpClEz = newPreDefColumn( pdstS62znVentPar, "Cooling Zone Air Distribution Effectiveness - Ez-clg" );
			pdchS62zvpClVoz = newPreDefColumn( pdstS62znVentPar, "Cooling Zone Outdoor Airflow - Voz-clg [m3/s]" );
			pdchS62zvpHtEz = newPreDefColumn( pdstS62znVentPar, "Heating Zone Air Distribution Effectiveness - Ez-htg" );
			pdchS62zvpHtVoz = newPreDefColumn( pdstS62znVentPar, "Heating Zone Outdoor Airflow - Voz-htg [m3/s]" );

			pdstS62sysVentPar = newPreDefSubTable( pdrStd62, "System Ventilation Parameters" );

			pdchS62svpRp = newPreDefColumn( pdstS62sysVentPar, "People Outdoor Air Rate - Rp [m3/s-person]" );
			pdchS62svpPz = newPreDefColumn( pdstS62sysVentPar, "Sum of Zone Population - Pz-sum" );
			pdchS62svpRa = newPreDefColumn( pdstS62sysVentPar, "Area Outdoor Air Rate - Ra [m3/s-m2]" );
			pdchS62svpAz = newPreDefColumn( pdstS62sysVentPar, "Sum of Zone Floor Area - Az-sum [m2]" );
			pdchS62svpVbz = newPreDefColumn( pdstS62sysVentPar, "Breathing Zone Outdoor Airflow - Vbz [m3/s]" );
			pdchS62svpClVoz = newPreDefColumn( pdstS62sysVentPar, "Cooling Zone Outdoor Airflow - Voz-clg [m3/s]" );
			pdchS62svpHtVoz = newPreDefColumn( pdstS62sysVentPar, "Heating Zone Outdoor Airflow - Voz-htg [m3/s]" );

			pdstS62znCoolDes = newPreDefSubTable( pdrStd62, "Zone Ventilation Calculations for Cooling Design" );

			pdchS62zcdAlN = newPreDefColumn( pdstS62znCoolDes, "AirLoop Name" );
			pdchS62zcdBox = newPreDefColumn( pdstS62znCoolDes, "Box Type" );
			pdchS62zcdVpz = newPreDefColumn( pdstS62znCoolDes, "Zone Primary Airflow - Vpz [m3/s]" );
			//pdchS62zcdVps =         newPreDefColumn(pdstS62znCoolDes,'System Primary Airflow - Vps [m3/s]')
			//pdchS62zcdVsec =        newPreDefColumn(pdstS62znCoolDes,'Secondary Fan Airflow - Vsec [m3/s]')
			pdchS62zcdVdz = newPreDefColumn( pdstS62znCoolDes, "Zone Discharge Airflow - Vdz [m3/s]" );
			pdchS62zcdVpzmin = newPreDefColumn( pdstS62znCoolDes, "Minimum Zone Primary Airflow - Vpz-min [m3/s]" );
			pdchS62zcdVozclg = newPreDefColumn( pdstS62znCoolDes, "Zone Outdoor Airflow Cooling - Voz-clg [m3/s]" );
			pdchS62zcdZpz = newPreDefColumn( pdstS62znCoolDes, "Primary Outdoor Air Fraction - Zpz" );
			pdchS62zcdEp = newPreDefColumn( pdstS62znCoolDes, "Primary Air Fraction - Ep" );
			pdchS62zcdEr = newPreDefColumn( pdstS62znCoolDes, "Secondary Recirculation Fraction- Er" );
			pdchS62zcdFa = newPreDefColumn( pdstS62znCoolDes, "Supply Air Fraction- Fa" );
			pdchS62zcdFb = newPreDefColumn( pdstS62znCoolDes, "Mixed Air Fraction - Fb" );
			pdchS62zcdFc = newPreDefColumn( pdstS62znCoolDes, "Outdoor Air Fraction - Fc" );
			pdchS62zcdEvz = newPreDefColumn( pdstS62znCoolDes, "Zone Ventilation Efficiency - Evz" );

			pdstS62sysCoolDes = newPreDefSubTable( pdrStd62, "System Ventilation Calculations for Cooling Design" );

			pdchS62scdVpz = newPreDefColumn( pdstS62sysCoolDes, "Sum of Zone Primary Airflow - Vpz-sum [m3/s]" );
			pdchS62scdVps = newPreDefColumn( pdstS62sysCoolDes, "System Primary Airflow - Vps [m3/s]" );
			//pdchS62scdVsec =        newPreDefColumn(pdstS62sysCoolDes,'Secondary Fan Airflow - Vsec [m3/s]')
			pdchS62scdVdz = newPreDefColumn( pdstS62sysCoolDes, "Sum of Zone Discharge Airflow - Vdz-sum [m3/s]" );
			pdchS62scdVpzmin = newPreDefColumn( pdstS62sysCoolDes, "Sum of Min Zone Primary Airflow - Vpz-min [m3/s]" );
			pdchS62scdVozclg = newPreDefColumn( pdstS62sysCoolDes, "Zone Outdoor Airflow Cooling - Voz-clg [m3/s]" );
			pdchS62scdEvz = newPreDefColumn( pdstS62sysCoolDes, "Zone Ventilation Efficiency - Evz-min" );

			pdstS62znHeatDes = newPreDefSubTable( pdrStd62, "Zone Ventilation Calculations for Heating Design" );

			pdchS62zhdAlN = newPreDefColumn( pdstS62znHeatDes, "AirLoop Name" );
			pdchS62zhdBox = newPreDefColumn( pdstS62znHeatDes, "Box Type" );
			pdchS62zhdVpz = newPreDefColumn( pdstS62znHeatDes, "Zone Primary Airflow - Vpz [m3/s]" );
			//pdchS62zhdVps =         newPreDefColumn(pdstS62znHeatDes,'System Primary Airflow - Vps [m3/s]')
			//pdchS62zhdVsec =        newPreDefColumn(pdstS62znHeatDes,'Secondary Fan Airflow - Vsec [m3/s]')
			pdchS62zhdVdz = newPreDefColumn( pdstS62znHeatDes, "Zone Discharge Airflow - Vdz [m3/s]" );
			pdchS62zhdVpzmin = newPreDefColumn( pdstS62znHeatDes, "Minimum Zone Primary Airflow - Vpz-min [m3/s]" );
			pdchS62zhdVozhtg = newPreDefColumn( pdstS62znHeatDes, "Zone Outdoor Airflow Heating - Voz-htg [m3/s]" );
			pdchS62zhdZpz = newPreDefColumn( pdstS62znHeatDes, "Primary Outdoor Air Fraction - Zpz" );
			pdchS62zhdEp = newPreDefColumn( pdstS62znHeatDes, "Primary Air Fraction - Ep" );
			pdchS62zhdEr = newPreDefColumn( pdstS62znHeatDes, "Secondary Recirculation Fraction- Er" );
			pdchS62zhdFa = newPreDefColumn( pdstS62znHeatDes, "Supply Air Fraction- Fa" );
			pdchS62zhdFb = newPreDefColumn( pdstS62znHeatDes, "Mixed Air Fraction - Fb" );
			pdchS62zhdFc = newPreDefColumn( pdstS62znHeatDes, "Outdoor Air Fraction - Fc" );
			pdchS62zhdEvz = newPreDefColumn( pdstS62znHeatDes, "Zone Ventilation Efficiency - Evz" );

			pdstS62sysHeatDes = newPreDefSubTable( pdrStd62, "System Ventilation Calculations for Heating Design" );

			pdchS62shdVpz = newPreDefColumn( pdstS62sysHeatDes, "Sum of Zone Primary Airflow - Vpz-sum [m3/s]" );
			pdchS62shdVps = newPreDefColumn( pdstS62sysHeatDes, "System Primary Airflow - Vps [m3/s]" );
			//pdchS62shdVsec =        newPreDefColumn(pdstS62sysHeatDes,'Secondary Fan Airflow - Vsec [m3/s]')
			pdchS62shdVdz = newPreDefColumn( pdstS62sysHeatDes, "Sum of Zone Discharge Airflow - Vdz-sum [m3/s]" );
			pdchS62shdVpzmin = newPreDefColumn( pdstS62sysHeatDes, "Sum of Min Zone Primary Airflow - Vpz-min [m3/s]" );
			pdchS62shdVozhtg = newPreDefColumn( pdstS62sysHeatDes, "Zone Outdoor Airflow Heating - Voz-htg [m3/s]" );
			pdchS62shdEvz = newPreDefColumn( pdstS62sysHeatDes, "Zone Ventilation Efficiency - Evz-min" );
		}

		pdrLeed = newPreDefReport( "LEEDsummary", "LEED", "LEED Summary" );

		pdstLeedGenInfo = newPreDefSubTable( pdrLeed, "Sec1.1A-General Information" );
		// single column with rows of:
		//    Principal Heating Source
		//    Weather File
		//    Climate Zone
		//    Heating Degree Days
		//    Cooling Degree Days
		//    HDD and CDD data source
		//    Total gross floor area
		pdchLeedGenData = newPreDefColumn( pdstLeedGenInfo, "Data" );

		pdstLeedSpaceUsageType = newPreDefSubTable( pdrLeed, "EAp2-1. Space Usage Type" );
		pdchLeedSutSpArea = newPreDefColumn( pdstLeedSpaceUsageType, "Space Area [m2]" );
		pdchLeedSutOcArea = newPreDefColumn( pdstLeedSpaceUsageType, "Regularly Occupied Area [m2]" );
		pdchLeedSutUnArea = newPreDefColumn( pdstLeedSpaceUsageType, "Unconditioned Area [m2]" );
		pdchLeedSutHrsWeek = newPreDefColumn( pdstLeedSpaceUsageType, "Typical Hours/Week in Operation [hr/wk]" );

		pdstLeedAdvsMsg = newPreDefSubTable( pdrLeed, "EAp2-2. Advisory Messages" );
		// single column with rows of:
		//    Number of hours heating loads not met
		//    Number of hours cooling loads not met
		//    Total
		//    Difference
		//    Number of warning messages
		//    Number of error messages
		//    Number of defaults overridden
		pdchLeedAmData = newPreDefColumn( pdstLeedAdvsMsg, "Data" );

		pdstLeedEneTypSum = newPreDefSubTable( pdrLeed, "EAp2-3. Energy Type Summary" );
		// multiple columns with rows of
		//    Electricity
		//    Natural Gas
		//    <additional fuels>
		pdchLeedEtsRtNm = newPreDefColumn( pdstLeedEneTypSum, "Utility Rate" );
		pdchLeedEtsVirt = newPreDefColumn( pdstLeedEneTypSum, "Virtual Rate [$/unit energy]" );
		pdchLeedEtsEneUnt = newPreDefColumn( pdstLeedEneTypSum, "Units of Energy" );
		pdchLeedEtsDemUnt = newPreDefColumn( pdstLeedEneTypSum, "Units of Demand" );

		pdstLeedPerf = newPreDefSubTable( pdrLeed, "EAp2-4/5. Performance Rating Method Compliance" );
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
		pdchLeedPerfElEneUse = newPreDefColumn( pdstLeedPerf, "Electric Energy Use [GJ]" );
		pdchLeedPerfElDem = newPreDefColumn( pdstLeedPerf, "Electric Demand [W]" );
		pdchLeedPerfGasEneUse = newPreDefColumn( pdstLeedPerf, "Natural Gas Energy Use [GJ]" );
		pdchLeedPerfGasDem = newPreDefColumn( pdstLeedPerf, "Natural Gas Demand [W]" );
		pdchLeedPerfOthEneUse = newPreDefColumn( pdstLeedPerf, "Additional Energy Use [GJ]" );
		pdchLeedPerfOthDem = newPreDefColumn( pdstLeedPerf, "Additional Demand [W]" );

		pdstLeedEneUseSum = newPreDefSubTable( pdrLeed, "EAp2-6. Energy Use Summary" );
		// Multiple columns with rows of:
		//    Electricity
		//    Natural Gas
		//    <additional fuels>
		//    Total
		pdchLeedEusProc = newPreDefColumn( pdstLeedEneUseSum, "Process Subtotal [GJ]" );
		pdchLeedEusTotal = newPreDefColumn( pdstLeedEneUseSum, "Total Energy Use [GJ]" );

		pdstLeedEneCostSum = newPreDefSubTable( pdrLeed, "EAp2-7. Energy Cost Summary" );
		// Multiple columns with rows of:
		//    Electricity
		//    Natural Gas
		//    <additional fuels>
		//    Total
		pdchLeedEcsProc = newPreDefColumn( pdstLeedEneCostSum, "Process Subtotal [$]" );
		pdchLeedEcsTotal = newPreDefColumn( pdstLeedEneCostSum, "Total Energy Cost [$]" );

		pdstLeedRenewSum = newPreDefSubTable( pdrLeed, "L-1. Renewable Energy Source Summary" );
		// Multiple columns with rows of each renewable source
		pdchLeedRenRatCap = newPreDefColumn( pdstLeedRenewSum, "Rated Capacity [kW]" );
		pdchLeedRenAnGen = newPreDefColumn( pdstLeedRenewSum, "Annual Energy Generated [GJ]" );

		pdstLeedEneUseIntEl = newPreDefSubTable( pdrLeed, "EAp2-17a. Energy Use Intensity - Electricity" );
		// Single column with rows of:
		//    Interior lighting
		//    Space heating
		//    Space cooling
		//    Fans-interior
		//    Service water heating
		//    Receptacle equipment
		//    Miscellaneous
		//    Subtotal
		pdchLeedEuiElec = newPreDefColumn( pdstLeedEneUseIntEl, "Electricty [MJ/m2]" );

		pdstLeedEneUseIntNatG = newPreDefSubTable( pdrLeed, "EAp2-17b. Energy Use Intensity - Natural Gas" );
		// Single column with rows of:
		//    Space heating
		//    Service water heating
		//    Miscellaneous
		//    Subtotal
		pdchLeedEuiNatG = newPreDefColumn( pdstLeedEneUseIntNatG, "Natural Gas [MJ/m2]" );

		pdstLeedEneUseIntOthr = newPreDefSubTable( pdrLeed, "EAp2-17c. Energy Use Intensity - Additional" );
		// Single column with rows of:
		//    Miscellaneous
		//    Subtotal
		pdchLeedEuiOthr = newPreDefColumn( pdstLeedEneUseIntOthr, "Additional [MJ/m2]" );

		pdstLeedEneUsePerc = newPreDefSubTable( pdrLeed, "EAp2-18. End Use Percentage" );
		// single column with rows of:
		//    Interior Lighting
		//    Space heating
		//    Space cooling
		//    Fans-Interior
		//    Service Water Heating
		//    Receptacle Equipment
		//    Miscellaneous
		pdchLeedEupPerc = newPreDefColumn( pdstLeedEneUsePerc, "Percent [%]" );

	}

	void
	PreDefTableEntry(
		int const columnIndex,
		std::string const & objName,
		Real64 const tableEntryReal,
		Optional_int_const numSigDigits
	)
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
		static gio::Fmt fmtI1( "(I1)" );

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
		//check for number of significant digits
		if ( present( numSigDigits ) ) {
			if ( ( numSigDigits <= 9 ) && ( numSigDigits >= 0 ) ) {
				sigDigitCount = numSigDigits;
			} else {
				sigDigitCount = 2;
			}
		} else {
			sigDigitCount = 2;
		}
		// convert the integer to a string for the number of digits
		gio::write( digitString, fmtI1 ) << sigDigitCount;
		// build up the format string
		if ( tableEntryReal < 1e10 ) {
			formatConvert = "(F12." + digitString + ')';
		} else {
			formatConvert = "(E12." + digitString + ')';
		}
		{ IOFlags flags; gio::write( stringEntry, formatConvert, flags ) << tableEntryReal; IOS = flags.ios(); }
		if ( IOS != 0 ) stringEntry = "  Too Big";
		tableEntry( numTableEntry ).charEntry = stringEntry;
		tableEntry( numTableEntry ).objectName = objName;
		tableEntry( numTableEntry ).indexColumn = columnIndex;
		tableEntry( numTableEntry ).origRealEntry = tableEntryReal;
		tableEntry( numTableEntry ).significantDigits = sigDigitCount;
		tableEntry( numTableEntry ).origEntryIsReal = true;
	}

	void
	PreDefTableEntry(
		int const columnIndex,
		std::string const & objName,
		std::string const & tableEntryChar
	)
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
		tableEntry( numTableEntry ).charEntry = tableEntryChar;
		tableEntry( numTableEntry ).objectName = objName;
		tableEntry( numTableEntry ).indexColumn = columnIndex;
	}

	void
	PreDefTableEntry(
		int const columnIndex,
		std::string const & objName,
		int const tableEntryInt
	)
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
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string stringEntry;

		incrementTableEntry();
		// convert the integer to a string
		gio::write( stringEntry, fmtLD ) << tableEntryInt;
		tableEntry( numTableEntry ).charEntry = stringEntry;
		tableEntry( numTableEntry ).objectName = objName;
		tableEntry( numTableEntry ).indexColumn = columnIndex;
	}

	void
	incrementTableEntry()
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
		if ( ! allocated( tableEntry ) ) {
			tableEntry.allocate( sizeIncrement );
			sizeTableEntry = sizeIncrement;
			numTableEntry = 1;
		} else {
			++numTableEntry;
			// if larger than current size grow the array
			if ( numTableEntry > sizeTableEntry ) {
				tableEntry.redimension( sizeTableEntry *= 2 ); //Tuned Changed += sizeIncrement to *= 2 for reduced heap allocations (at some space cost)
			}
		}
	}

	void
	AddCompSizeTableEntry(
		std::string const & FieldType,
		std::string const & FieldName,
		std::string const & FieldDescription,
		Real64 const FieldValue
	)
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

		if ( ! allocated( CompSizeTableEntry ) ) {
			CompSizeTableEntry.allocate( sizeIncrement );
			sizeCompSizeTableEntry = sizeIncrement;
			numCompSizeTableEntry = 1;
		} else {
			++numCompSizeTableEntry;
			// if larger than current size grow the array
			if ( numCompSizeTableEntry > sizeCompSizeTableEntry ) {
				CompSizeTableEntry.redimension( sizeCompSizeTableEntry *= 2 ); //Tuned Changed += sizeIncrement to *= 2 for reduced heap allocations (at some space cost)
			}
		}
		CompSizeTableEntry( numCompSizeTableEntry ).typeField = FieldType;
		CompSizeTableEntry( numCompSizeTableEntry ).nameField = FieldName;
		CompSizeTableEntry( numCompSizeTableEntry ).description = FieldDescription;
		CompSizeTableEntry( numCompSizeTableEntry ).valField = FieldValue;
	}

	void
	AddShadowRelateTableEntry(
		int const castingField,
		int const receivingField,
		int const receivingKind
	)
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
		//CHARACTER(len=*),INTENT(IN)  :: castingField
		//CHARACTER(len=*),INTENT(IN)  :: receivingField

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		if ( ! allocated( ShadowRelate ) ) {
			ShadowRelate.allocate( sizeIncrement );
			sizeShadowRelate = sizeIncrement;
			numShadowRelate = 1;
		} else {
			++numShadowRelate;
			// if larger than current size grow the array
			if ( numShadowRelate > sizeShadowRelate ) {
				ShadowRelate.redimension( sizeShadowRelate *= 2 ); //Tuned Changed += sizeIncrement to *= 2 for reduced heap allocations (at some space cost)
			}
		}
		ShadowRelate( numShadowRelate ).castSurf = castingField;
		ShadowRelate( numShadowRelate ).recSurf = receivingField;
		ShadowRelate( numShadowRelate ).recKind = receivingKind;
	}

	int
	newPreDefReport(
		std::string const & inReportName,
		std::string const & inReportAbrev,
		std::string const & inReportNamewithSpaces
	)
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
		if ( ! allocated( reportName ) ) {
			reportName.allocate( sizeIncrement );
			sizeReportName = sizeIncrement;
			numReportName = 1;
		} else {
			++numReportName;
			// if larger than current size grow the array
			if ( numReportName > sizeReportName ) {
				reportName.redimension( sizeReportName *= 2 ); //Tuned Changed += sizeIncrement to *= 2 for reduced heap allocations (at some space cost)
			}
		}
		// initialize new record
		reportName( numReportName ).name = inReportName;
		reportName( numReportName ).abrev = inReportAbrev;
		reportName( numReportName ).namewithspaces = inReportNamewithSpaces;
		reportName( numReportName ).show = false;
		newPreDefReport = numReportName;
		return newPreDefReport;
	}

	int
	newPreDefSubTable(
		int const reportIndex,
		std::string const & subTableName
	)
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
		if ( ! allocated( subTable ) ) {
			subTable.allocate( sizeIncrement );
			sizeSubTable = sizeIncrement;
			numSubTable = 1;
		} else {
			++numSubTable;
			// if larger than current size then grow the array
			if ( numSubTable > sizeSubTable ) {
				subTable.redimension( sizeSubTable *= 2 ); //Tuned Changed += sizeIncrement to *= 2 for reduced heap allocations (at some space cost)
			}
		}
		// initialize new record)
		subTable( numSubTable ).name = subTableName;
		subTable( numSubTable ).indexReportName = reportIndex;
		return numSubTable;
	}

	void
	addFootNoteSubTable(
		int const subTableIndex,
		std::string const & footnoteText
	)
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
		if ( ( subTableIndex >= 0 ) && ( subTableIndex <= numSubTable ) ) {
			subTable( subTableIndex ).footnote = footnoteText;
		}
	}

	int
	newPreDefColumn(
		int const subTableIndex,
		std::string const & columnHeading
	)
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
		if ( ! allocated( columnTag ) ) {
			columnTag.allocate( sizeIncrement );
			sizeColumnTag = sizeIncrement;
			numColumnTag = 1;
		} else {
			++numColumnTag;
			// if larger than current size grow the array
			if ( numColumnTag > sizeColumnTag ) {
				columnTag.redimension( sizeColumnTag *= 2 ); //Tuned Changed += sizeIncrement to *= 2 for reduced heap allocations (at some space cost)
			}
		}
		// initialize new record)
		columnTag( numColumnTag ).heading = columnHeading;
		columnTag( numColumnTag ).indexSubTable = subTableIndex;
		newPreDefColumn = numColumnTag;
		return newPreDefColumn;
	}

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // OutputReportPredefined

} // EnergyPlus
