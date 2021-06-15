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

#ifndef OutputReportPredefined_hh_INCLUDED
#define OutputReportPredefined_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace OutputReportPredefined {

    // Using/Aliasing

    // Data
    // The following section initializes the predefined column heading variables
    // The variables get their value in AssignPredefined

    // Internal data structures to store information provided by calls
    int constexpr sizeIncrement(100);
    int constexpr recKindSurface(1);
    int constexpr recKindSubsurface(2);

    // Types

    struct reportNameType
    {
        // Members
        std::string name;
        std::string namewithspaces; // a "prettier version" than the key value
        std::string abrev;
        bool show;

        // Default Constructor
        reportNameType() : show(false)
        {
        }
    };

    struct SubTableType
    {
        // Members
        std::string name;
        int indexReportName;
        std::string footnote;

        // Default Constructor
        SubTableType() : indexReportName(0)
        {
        }
    };

    struct ColumnTagType
    {
        // Members
        std::string heading;
        int indexSubTable;

        // Default Constructor
        ColumnTagType() : indexSubTable(0)
        {
        }
    };

    struct TableEntryType
    {
        // Members
        std::string charEntry;
        std::string objectName;
        int indexColumn;
        int subTableIndex;
        int uniqueObjName;
        Real64 origRealEntry;
        int significantDigits;
        bool origEntryIsReal;

        // Default Constructor
        TableEntryType() : indexColumn(0), subTableIndex(0), uniqueObjName(0), origRealEntry(0.0), significantDigits(0), origEntryIsReal(false)
        {
        }
    };

    struct CompSizeTableEntryType
    {
        // Members
        std::string typeField;
        std::string nameField;
        std::string description;
        Real64 valField;
        bool active;
        bool written;

        // Default Constructor
        CompSizeTableEntryType() : valField(0.0), active(false), written(false)
        {
        }
    };

    struct ShadowRelateType
    {
        // Members
        int castSurf;
        int recSurf;
        int recKind;

        // Default Constructor
        ShadowRelateType() : castSurf(0), recSurf(0), recKind(0)
        {
        }
    };

    void SetPredefinedTables(EnergyPlusData &state);

    // Creates an entry for predefined tables when the entry
    // is a real variable. numSigDigits defaults to 2, and if supplied must be in [0-9]
    // Internally it uses a Fortran-Style write statement, meaning numbers are rounded rather than trimmed
    void PreDefTableEntry(
        EnergyPlusData &state, int const columnIndex, std::string_view objName, Real64 const tableEntryReal, Optional_int_const numSigDigits = _);

    void PreDefTableEntry(EnergyPlusData &state, int const columnIndex, std::string_view objName, std::string_view tableEntryChar);

    void PreDefTableEntry(EnergyPlusData &state, int const columnIndex, std::string_view objName, int const tableEntryInt);

    std::string RetrievePreDefTableEntry(EnergyPlusData &state, int const columnIndex, std::string_view objName);

    void incrementTableEntry(EnergyPlusData &state);

    void AddCompSizeTableEntry(EnergyPlusData &state,
                               std::string_view FieldType,
                               std::string_view FieldName,
                               std::string_view FieldDescription,
                               Real64 const FieldValue);

    void AddShadowRelateTableEntry(EnergyPlusData &state, int const castingField, int const receivingField, int const receivingKind);

    int newPreDefReport(EnergyPlusData &state,
                        std::string_view inReportName,
                        std::string_view inReportAbrev,
                        std::string_view inReportNamewithSpaces);

    int newPreDefSubTable(EnergyPlusData &state, int const reportIndex, std::string_view subTableName);

    void addFootNoteSubTable(EnergyPlusData &state, int const subTableIndex, std::string_view footnoteText);

    int newPreDefColumn(EnergyPlusData &state, int const subTableIndex, std::string_view columnHeading);

} // namespace OutputReportPredefined

struct OutputReportPredefinedData : BaseGlobalStruct
{

    // Climate Summary Report
    int pdrClim = 0;
    int pdstDesDay = 0;
    int pdchDDmaxDB = 0;
    int pdchDDrange = 0;
    int pdchDDhumid = 0;
    int pdchDDhumTyp = 0;
    int pdchDDwindSp = 0;
    int pdchDDwindDr = 0;
    int pdstWthr = 0;
    int pdchWthrVal = 0;

    // HVAC Equipment Report
    int pdrEquip = 0;
    int pdstMech = 0;
    int pdchMechType = 0;
    int pdchMechNomCap = 0;
    int pdchMechNomEff = 0;
    int pdchMechIPLVSI = 0;
    int pdchMechIPLVIP = 0;

    // Fan subtable
    int pdstFan = 0;
    int pdchFanType = 0;
    int pdchFanTotEff = 0;
    int pdchFanDeltaP = 0;
    int pdchFanVolFlow = 0;
    int pdchFanMotorIn = 0;
    int pdchFanEnergyIndex = 0;
    int pdchFanEndUse = 0;
    int pdchFanPwr = 0;
    int pdchFanPwrPerFlow = 0;
    int pdchFanDesDay = 0;
    int pdchFanPkTime = 0;

    // Pump subtable
    int pdstPump = 0;
    int pdchPumpType = 0;
    int pdchPumpControl = 0;
    int pdchPumpHead = 0;
    int pdchPumpFlow = 0;
    int pdchPumpPower = 0;
    int pdchPumpPwrPerFlow = 0;
    int pdchPumpEndUse = 0;
    int pdchMotEff = 0;

    // Cooling coil subtable
    int pdstCoolCoil = 0;
    int pdchCoolCoilType = 0;
    int pdchCoolCoilDesCap = 0;
    int pdchCoolCoilTotCap = 0;
    int pdchCoolCoilSensCap = 0;
    int pdchCoolCoilLatCap = 0;
    int pdchCoolCoilSHR = 0;
    int pdchCoolCoilNomEff = 0;
    int pdchCoolCoilUATotal = 0;
    int pdchCoolCoilArea = 0;

    // DX Cooling Coil subtable
    int pdstDXCoolCoil = 0;
    int pdchDXCoolCoilType = 0;           // DX cooling coil type
    int pdchDXCoolCoilNetCapSI = 0;       // Standard Rated (Net) Cooling Capacity [W]
    int pdchDXCoolCoilCOP = 0;            // EER/COP value in SI unit at AHRI std. 340/360 conditions [W/W]
    int pdchDXCoolCoilSEERUserIP = 0;     // SEER value in IP unit at AHRI std. 210/240 conditions and user PLF curve [Btu/W-hr]
    int pdchDXCoolCoilSEERStandardIP = 0; // SEER value in IP unit at AHRI std. 210/240 conditions and default PLF curve and C_D value [Btu/W-hr]
    int pdchDXCoolCoilEERIP = 0;          // EER value in IP unit at AHRI std. 340/360 conditions [Btu/W-h]
    int pdchDXCoolCoilIEERIP = 0;         // IEER value in IP unit at AHRI std. 340/360 conditions

    // DX Cooling Coil subtable per ANSI/ASHRAE Std 127 for Tests A, B, C and D
    int pdstDXCoolCoil2 = 0;
    int pdchDXCoolCoilNetCapSIA = 0;  // Standard Rated (Net) Cooling Capacity [W], Test A
    int pdchDXCoolCoilElecPowerA = 0; // Standard Rated Electric Power [W], Test A
    int pdchDXCoolCoilNetCapSIB = 0;  // Standard Rated (Net) Cooling Capacity [W], Test B
    int pdchDXCoolCoilElecPowerB = 0; // Standard Rated Electric Power [W], Test B
    int pdchDXCoolCoilNetCapSIC = 0;  // Standard Rated (Net) Cooling Capacity [W], Test C
    int pdchDXCoolCoilElecPowerC = 0; // Standard Rated Electric Power [W], Test C
    int pdchDXCoolCoilNetCapSID = 0;  // Standard Rated (Net) Cooling Capacity [W], Test D
    int pdchDXCoolCoilElecPowerD = 0; // Standard Rated Electric Power [W], Test D

    // VAV DX Cooling Ratings Details
    int pdstVAVDXCoolCoil = 0; // details for Packaged VAV rating under AHRI 340/360
    int pdchVAVDXCoolCoilType = 0;
    int pdchVAVDXFanName = 0;
    int pdchVAVDXCoolCoilNetCapSI = 0;
    int pdchVAVDXCoolCoilCOP = 0;
    int pdchVAVDXCoolCoilIEERIP = 0;
    int pdchVAVDXCoolCoilEERIP = 0;
    int pdchVAVDXCoolCoilMdotA = 0;
    int pdchVAVDXCoolCoilCOP_B = 0;
    int pdchVAVDXCoolCoilEER_B_IP = 0;
    int pdchVAVDXCoolCoilMdotB = 0;
    int pdchVAVDXCoolCoilCOP_C = 0;
    int pdchVAVDXCoolCoilEER_C_IP = 0;
    int pdchVAVDXCoolCoilMdotC = 0;
    int pdchVAVDXCoolCoilCOP_D = 0;
    int pdchVAVDXCoolCoilEER_D_IP = 0;
    int pdchVAVDXCoolCoilMdotD = 0;

    // DX Heating Coil subtable
    int pdstDXHeatCoil;
    int pdchDXHeatCoilType; // DX Heating coil type
    int pdchDXHeatCoilHighCap;
    int pdchDXHeatCoilLowCap;
    int pdchDXHeatCoilHSPFSI;    // HSPF value in SI unit at AHRI std. 340/360 conditions [W/W]
    int pdchDXHeatCoilHSPFIP;    // HSPF value in IP unit at AHRI std. 340/360 conditions [Btu/W-hr]
    int pdchDXHeatCoilRegionNum; // Region number for which HSPF is calculated

    // Heating Coil subtable
    int pdstHeatCoil = 0;
    int pdchHeatCoilType = 0;
    int pdchHeatCoilDesCap = 0;
    int pdchHeatCoilNomCap = 0;
    int pdchHeatCoilNomEff = 0;

    // SWH subtable
    int pdstSWH = 0;
    int pdchSWHType = 0;
    int pdchSWHVol = 0;
    int pdchSWHHeatIn = 0;
    int pdchSWHThEff = 0;
    int pdchSWHRecEff = 0;
    int pdchSWHEnFac = 0;

    // Envelope Report
    int pdrEnvelope = 0;
    int pdstOpaque = 0;
    int pdchOpCons = 0;
    int pdchOpRefl = 0;
    int pdchOpUfactFilm = 0;
    int pdchOpUfactNoFilm = 0;
    int pdchOpGrArea = 0;
    int pdchOpNetArea = 0;
    int pdchOpAzimuth = 0;
    int pdchOpTilt = 0;
    int pdchOpDir = 0;
    int pdstIntOpaque = 0;
    int pdchIntOpCons = 0;
    int pdchIntOpRefl = 0;
    int pdchIntOpUfactFilm = 0;
    int pdchIntOpUfactNoFilm = 0;
    int pdchIntOpGrArea = 0;
    int pdchIntOpNetArea = 0;
    int pdchIntOpAzimuth = 0;
    int pdchIntOpTilt = 0;
    int pdchIntOpDir = 0;
    int pdstFen = 0;
    int pdchFenCons = 0;
    int pdchFenAreaOf1 = 0;
    int pdchFenGlassAreaOf1 = 0;
    int pdchFenFrameAreaOf1 = 0;
    int pdchFenDividerAreaOf1 = 0;
    int pdchFenArea = 0;
    int pdchFenUfact = 0;
    int pdchFenSHGC = 0;
    int pdchFenVisTr = 0;
    int pdchFenFrameConductance = 0;
    int pdchFenDividerConductance = 0;
    int pdchFenSwitchable = 0;
    int pdchFenParent = 0;
    int pdchFenAzimuth = 0;
    int pdchFenTilt = 0;
    int pdchFenDir = 0;
    int pdstDoor = 0;
    int pdchDrCons = 0;
    int pdchDrUfactFilm = 0;
    int pdchDrUfactNoFilm = 0;
    int pdchDrGrArea = 0;
    int pdchDrParent = 0;
    int pdstIntDoor = 0;
    int pdchIntDrCons = 0;
    int pdchIntDrUfactFilm = 0;
    int pdchIntDrUfactNoFilm = 0;
    int pdchIntDrGrArea = 0;
    int pdchIntDrParent = 0;
    int pdstIntFen = 0;
    int pdchIntFenCons = 0;
    int pdchIntFenAreaOf1 = 0;
    int pdchIntFenArea = 0;
    int pdchIntFenUfact = 0;
    int pdchIntFenSHGC = 0;
    int pdchIntFenVisTr = 0;
    int pdchIntFenParent = 0;

    // Shading Report
    int pdrShading = 0;
    int pdstSunlitFrac = 0;
    int pdchSlfMar21_9 = 0;
    int pdchSlfMar21_12 = 0;
    int pdchSlfMar21_15 = 0;
    int pdchSlfJun21_9 = 0;
    int pdchSlfJun21_12 = 0;
    int pdchSlfJun21_15 = 0;
    int pdchSlfDec21_9 = 0;
    int pdchSlfDec21_12 = 0;
    int pdchSlfDec21_15 = 0;
    int pdstWindowControl = 0;
    int pdchWscName = 0;
    int pdchWscShading = 0;
    int pdchWscShadCons = 0;
    int pdchWscControl = 0;
    int pdchWscGlare = 0;

    // Lighting Report
    int pdrLighting = 0;
    int pdstInLite = 0;
    int pdchInLtZone = 0;
    int pdchInLtDens = 0;
    int pdchInLtArea = 0;
    int pdchInLtPower = 0;
    int pdchInLtEndUse = 0;
    int pdchInLtSchd = 0;
    int pdchInLtAvgHrSchd = 0;
    int pdchInLtAvgHrOper = 0;
    int pdchInLtFullLoadHrs = 0;
    int pdchInLtRetAir = 0;
    int pdchInLtCond = 0;
    int pdchInLtConsump = 0;
    int pdstExtLite = 0;
    int pdchExLtPower = 0;
    int pdchExLtClock = 0;
    int pdchExLtSchd = 0;
    int pdchExLtAvgHrSchd = 0;
    int pdchExLtAvgHrOper = 0;
    int pdchExLtFullLoadHrs = 0;
    int pdchExLtConsump = 0;
    int pdstDaylight = 0;
    int pdchDyLtZone = 0;
    int pdchDyLtCtrlName = 0;
    int pdchDyLtKind = 0;
    int pdchDyLtCtrlType = 0;
    int pdchDyLtFrac = 0;
    int pdchDyLtWInst = 0;
    int pdchDyLtWCtrl = 0;

    // Sizing Report
    int pdrSizing = 0;
    int pdstZoneClSize = 0;
    int pdchZnClCalcDesLd = 0;
    int pdchZnClUserDesLd = 0;
    int pdchZnClUserDesLdPerArea = 0;
    int pdchZnClCalcDesAirFlow = 0;
    int pdchZnClUserDesAirFlow = 0;
    int pdchZnClDesDay = 0;
    int pdchZnClPkTime = 0;
    int pdchZnClPkTstatTemp = 0;
    int pdchZnClPkIndTemp = 0;
    int pdchZnClPkIndHum = 0;
    int pdchZnClPkOATemp = 0;
    int pdchZnClPkOAHum = 0;
    int pdchZnClPkOAMinFlow = 0;
    int pdchZnClPkDOASHeatGain = 0;
    int pdstZoneHtSize = 0;
    int pdchZnHtCalcDesLd = 0;
    int pdchZnHtUserDesLd = 0;
    int pdchZnHtUserDesLdPerArea = 0;
    int pdchZnHtCalcDesAirFlow = 0;
    int pdchZnHtUserDesAirFlow = 0;
    int pdchZnHtDesDay = 0;
    int pdchZnHtPkTime = 0;
    int pdchZnHtPkTstatTemp = 0;
    int pdchZnHtPkIndTemp = 0;
    int pdchZnHtPkIndHum = 0;
    int pdchZnHtPkOATemp = 0;
    int pdchZnHtPkOAHum = 0;
    int pdchZnHtPkOAMinFlow = 0;
    int pdchZnHtPkDOASHeatGain = 0;
    int pdstSystemSize = 0;
    int pdchSysSizCalcClAir = 0;
    int pdchSysSizUserClAir = 0;
    int pdchSysSizCalcHtAir = 0;
    int pdchSysSizUserHtAir = 0;
    int pdchSysSizAdjustedClAir = 0;
    int pdchSysSizAdjustedHtAir = 0;
    int pdchSysSizAdjustedMainAir = 0;
    int pdchSysSizCalcHeatFlowRatio = 0;
    int pdchSysSizUserHeatFlowRatio = 0;
    int pdstPlantSize = 0;
    int pdchPlantSizCalcVdot = 0;
    int pdchPlantSizMeasVdot = 0;
    int pdchPlantSizPrevVdot = 0;
    int pdchPlantSizCoincYesNo = 0;
    int pdchPlantSizDesDay = 0;
    int pdchPlantSizPkTimeHour = 0;
    int pdchPlantSizPkTimeDayOfSim = 0;
    int pdchPlantSizPkTimeMin = 0;

    // Coil Selection Table
    int pdrCoilSizingDetailsTable = 0;
    int pdstCoilSummaryCoilSelection = 0;
    int pdchCoilLocation = 0;
    int pdchCoilHVACType = 0;
    int pdchCoilHVACName = 0;
    int pdchCoilZoneName = 0;
    int pdchCoilName = 0;
    int pdchCoilType = 0;
    int pdchSysSizingMethCoinc = 0;
    int pdchSysSizingMethCap = 0;
    int pdchSysSizingMethAir = 0;

    int pdchCoilIsCapAutosized = 0;
    int pdchCoilIsAirFlowAutosized = 0;
    int pdchCoilIsWaterFlowAutosized = 0;
    int pdchCoilIsOATreated = 0;
    int pdchCoilFinalTotalCap = 0;
    int pdchCoilFinalSensCap = 0;
    int pdchCoilFinalAirVolFlowRate = 0;
    int pdchCoilFinalPlantVolFlowRate = 0;

    int pdchCoilDDnameSensIdealPeak = 0;
    int pdchCoilDateTimeSensIdealPeak = 0;
    int pdchCoilDDnameTotIdealPeak = 0;
    int pdchCoilDateTimeTotIdealPeak = 0;
    int pdchCoilDDnameAirFlowIdealPeak = 0;
    int pdchCoilDateTimeAirFlowIdealPeak = 0;
    int pdchCoilPeakLoadTypeToSizeOn = 0;
    int pdchCoilTotalCapIdealPeak = 0;
    int pdchCoilSensCapIdealPeak = 0;
    int pdchCoilAirMassFlowIdealPeak = 0;
    int pdchCoilAirVolumeFlowIdealPeak = 0;
    int pdchCoilEntDryBulbIdealPeak = 0;
    int pdchCoilEntWetBulbIdealPeak = 0;
    int pdchCoilEntHumRatIdealPeak = 0;
    int pdchCoilEntEnthalpyIdealPeak = 0;
    int pdchCoilLvgDryBulbIdealPeak = 0;
    int pdchCoilLvgWetBulbIdealPeak = 0;
    int pdchCoilLvgHumRatIdealPeak = 0;
    int pdchCoilLvgEnthalpyIdealPeak = 0;
    int pdchCoilWaterMassFlowIdealPeak = 0;
    int pdchCoilEntWaterTempIdealPeak = 0;
    int pdchCoilLvgWaterTempIdealPeak = 0;
    int pdchCoilWaterDeltaTempIdealPeak = 0;
    int pdchCoilRatedTotalCap = 0;
    int pdchCoilRatedSensCap = 0;
    int pdchCoilOffRatingCapacityModifierIdealPeak = 0;
    int pdchCoilOffRatingCapacityModifierSimPeak = 0;
    int pdchCoilRatedAirMass = 0;
    int pdchCoilRatedEntDryBulb = 0;
    int pdchCoilRatedEntWetBulb = 0;
    int pdchCoilRatedEntHumRat = 0;
    int pdchCoilRatedEntEnthalpy = 0;
    int pdchCoilRatedLvgDryBulb = 0;
    int pdchCoilRatedLvgWetBulb = 0;
    int pdchCoilRatedLvgHumRat = 0;
    int pdchCoilRatedLvgEnthalpy = 0;
    int pdchCoilRatedWaterMass = 0;
    int pdchCoilRatedEntWaterTemp = 0;
    int pdchCoilRatedLvgWaterTemp = 0;
    int pdchCoilRatedWaterDeltaTemp = 0;

    int pdchFanAssociatedWithCoilName = 0;
    int pdchFanAssociatedWithCoilType = 0;
    int pdchFanAssociatedVdotSize = 0;
    int pdchFanAssociatedMdotSize = 0;

    int pdchFanHeatGainIdealPeak = 0;
    int pdchCoilNetTotalCapacityIdealPeak = 0;
    int pdchCoilPlantLoopName = 0;

    int pdchPlantMassFlowMaximum = 0;
    int pdchPlantRetTempDesign = 0;
    int pdchPlantSupTempDesign = 0;
    int pdchPlantDeltaTempDesign = 0;
    int pdchPlantCapacity = 0;
    int pdchCoilCapPrcntPlantCapacity = 0;
    int pdchCoilFlowPrcntPlantFlow = 0;
    int pdchOADryBulbIdealPeak = 0;
    int pdchOAHumRatIdealPeak = 0;
    int pdchOAWetBulbatIdealPeak = 0;
    int pdchOAVolFlowIdealPeak = 0;
    int pdchOAFlowPrcntIdealPeak = 0;
    int pdchOABarometricPressureIdealPeak = 0;

    int pdchAirSysRADryBulbIdealPeak = 0;
    int pdchAirSysRAHumRatIdealPeak = 0;
    int pdchZoneAirDryBulbIdealPeak = 0;
    int pdchZoneAirHumRatIdealPeak = 0;
    int pdchZoneAirRelHumIdealPeak = 0;
    int pdchCoilUA = 0;
    int pdchZoneSensibleLoadIdealPeak = 0;
    int pdchZoneLatentLoadIdealPeak = 0;
    int pdchCoilIdealSizCapOverSimPeak = 0;
    int pdchCoilIdealSizCapUnderSimPeak = 0;
    int pdchReheatCoilMultiplier = 0;
    int pdchFlowCapRatioLowCapIncreaseRatio = 0;
    int pdchFlowCapRatioHiCapDecreaseRatio = 0;
    int pdchPlantFluidSpecificHeat = 0;
    int pdchPlantFluidDensity = 0;
    int pdchMoistAirSpecificHeat = 0;
    int pdchDryAirSpecificHeat = 0;
    int pdchStandRhoAir = 0;

    // Coil Sizing Summary (subset of Coil Selection Table)
    int pdst2CoilSummaryCoilSelection = 0;
    int pdch2CoilName = 0;
    int pdch2CoilType = 0;
    int pdch2CoilHVACType = 0;
    int pdch2CoilHVACName = 0;

    int pdch2CoilFinalTotalCap = 0;
    int pdch2CoilFinalSensCap = 0;
    int pdch2CoilFinalAirVolFlowRate = 0;
    int pdch2CoilFinalPlantVolFlowRate = 0;
    int pdch2CoilUA = 0;

    int pdch2CoilDDnameSensIdealPeak = 0;
    int pdch2CoilDateTimeSensIdealPeak = 0;
    int pdch2CoilDDnameAirFlowIdealPeak = 0;
    int pdch2CoilDateTimeAirFlowIdealPeak = 0;
    int pdch2CoilTotalCapIdealPeak = 0;
    int pdch2CoilSensCapIdealPeak = 0;
    int pdch2CoilAirVolumeFlowIdealPeak = 0;
    int pdch2CoilEntDryBulbIdealPeak = 0;
    int pdch2CoilEntWetBulbIdealPeak = 0;
    int pdch2CoilEntHumRatIdealPeak = 0;
    int pdch2CoilLvgDryBulbIdealPeak = 0;
    int pdch2CoilLvgWetBulbIdealPeak = 0;
    int pdch2CoilLvgHumRatIdealPeak = 0;
    int pdch2CoilRatedTotalCap = 0;
    int pdch2CoilRatedSensCap = 0;

    int pdch2OADryBulbIdealPeak = 0;
    int pdch2OAHumRatIdealPeak = 0;
    int pdch2OAWetBulbatIdealPeak = 0;
    int pdch2OAFlowPrcntIdealPeak = 0;
    int pdch2ZoneAirDryBulbIdealPeak = 0;
    int pdch2ZoneAirHumRatIdealPeak = 0;
    int pdch2ZoneAirRelHumIdealPeak = 0;
    int pdch2ZoneSensibleLoadIdealPeak = 0;
    int pdch2ZoneLatentLoadIdealPeak = 0;

    // System summary
    int pdrSystem = 0;
    int pdstEconomizer = 0;
    int pdchEcoKind = 0;
    int pdchEcoMinOA = 0;
    int pdchEcoMaxOA = 0;
    int pdchEcoRetTemp = 0;
    int pdchEcoRetEnth = 0;
    int pdchEcoOATempLim = 0;
    int pdchEcoOAEnthLim = 0;
    int pdstDemCntlVent = 0;
    int pdchDCVventMechName = 0;
    int pdchDCVperPerson = 0;
    int pdchDCVperArea = 0;
    int pdchDCVperZone = 0;
    int pdchDCVperACH = 0;
    int pdchDCVMethod = 0;
    int pdchDCVOASchName = 0;

    // added for new DCV
    int pdchDCVZoneADEffCooling = 0;
    int pdchDCVZoneADEffHeating = 0;
    int pdchDCVZoneADEffSchName = 0;

    int pdstSimpleComfort = 0;
    int pdchSCwinterClothes = 0;
    int pdchSCsummerClothes = 0;
    int pdchSCeitherClothes = 0;
    int pdstUnmetLoads = 0;
    int pdchULnotMetHeat = 0;
    int pdchULnotMetCool = 0;
    int pdchULnotMetHeatOcc = 0;
    int pdchULnotMetCoolOcc = 0;

    // Outdoor Air Report
    int pdrOutsideAir = 0;
    int pdstOAavgOcc = 0;
    int pdchOaoAvgNumOcc1 = 0;
    int pdchOaoNomNumOcc1 = 0;
    int pdchOaoZoneVol1 = 0;
    int pdchOaoAvgMechVent = 0;
    int pdchOaoAvgInfil = 0;
    int pdchOaoAvgAFNInfil = 0;
    int pdchOaoAvgSimpVent = 0;
    int pdchOaoAvgTotVent = 0;
    int pdstOAminOcc = 0;
    int pdchOaoAvgNumOcc2 = 0;
    int pdchOaoNomNumOcc2 = 0;
    int pdchOaoZoneVol2 = 0;
    int pdchOaoMinMechVent = 0;
    int pdchOaoMinInfil = 0;
    int pdchOaoMinAFNInfil = 0;
    int pdchOaoMinSimpVent = 0;
    int pdchOaoMinTotVent = 0;

    // Outdoor Air Details Report
    int pdrOutsideAirDetails = 0;

    int pdstOAmechVentParByZone = 0;
    int pdchOaMvAirLpNm = 0;
    int pdchOaMvAvgNumOcc = 0;
    int pdchOaMvNomNumOcc = 0;
    int pdchOaMvZoneVol = 0;
    int pdchOaMvZoneArea = 0;
    int pdchOaMvDesZnOa = 0;
    int pdchOaMvMinDynTrgVent = 0;

    int pdstOAtotAirByZone = 0;
    int pdchOaTaBzMechVent = 0;
    int pdchOaTaBzNatVent = 0;
    int pdchOaTaBzTotVent = 0;
    int pdchOaTaBzInfil = 0;
    int pdchOaTaBzTotVentInfil = 0;
    int pdchOaTaBzDynTrgVent = 0;
    int pdchOaTaBzTmBelow = 0;
    int pdchOaTaBzTmAt = 0;
    int pdchOaTaBzTmAbove = 0;
    int pdchOaTaBzTmAboveUnocc = 0;

    int pdstOAavgOccByZone = 0;
    int pdchOaOccBzMechVent = 0;
    int pdchOaOccBzNatVent = 0;
    int pdchOaOccBzTotVent = 0;
    int pdchOaOccBzInfil = 0;
    int pdchOaOccBzTotVentInfil = 0;
    int pdchOaOccBzDynTrgVent = 0;
    int pdchOaOccBzTmBelow = 0;
    int pdchOaOccBzTmAt = 0;
    int pdchOaOccBzTmAbove = 0;

    int pdstOAtotAirByLoop = 0;
    int pdchOaTaAlMechVent = 0;
    int pdchOaTaAlNatVent = 0;
    int pdchOaTaAlTotVent = 0;
    int pdchOaTaAlSumDynTrgVent = 0;
    int pdchOaTaAlTmBelow = 0;
    int pdchOaTaAlTmAt = 0;
    int pdchOaTaAlTmAbove = 0;
    int pdchOaTaAlTmAboveUnocc = 0;

    int pdstOAavgOccByLoop = 0;
    int pdchOaOccAlMechVent = 0;
    int pdchOaOccAlNatVent = 0;
    int pdchOaOccAlTotVent = 0;
    int pdchOaOccAlSumDynTrgVent = 0;
    int pdchOaOccAlTmBelow = 0;
    int pdchOaOccAlTmAt = 0;
    int pdchOaOccAlTmAbove = 0;

    int pdstOAtimeFactorsDurOcc = 0;
    int pdchOaTmFctNoLimit = 0;
    int pdchOaTmFctHiHumid = 0;
    int pdchOaTmFctNiteVent = 0;
    int pdchOaTmFctEcono = 0;
    int pdchOaTmFctDCV = 0;
    int pdchOaTmFctExhaust = 0;
    int pdchOaTmFctMixedLimit = 0;
    int pdchOaTmFctLimit = 0;
    int pdchOaTmFctDemand = 0;
    int pdchOaTmFctEMS = 0;

    int pdstOAavgFactorsDurOcc = 0;
    int pdchOaAvFctNoLimit = 0;
    int pdchOaAvFctHiHumid = 0;
    int pdchOaAvFctNiteVent = 0;
    int pdchOaAvFctEcono = 0;
    int pdchOaAvFctDCV = 0;
    int pdchOaAvFctExhaust = 0;
    int pdchOaAvFctMixedLimit = 0;
    int pdchOaAvFctLimit = 0;
    int pdchOaAvFctDemand = 0;
    int pdchOaAvFctEMS = 0;

    // Object Count Report
    int pdrObjCnt = 0;
    int pdstSurfCnt = 0;
    int pdchSurfCntTot = 0;
    int pdchSurfCntExt = 0;
    int pdstHVACcnt = 0;
    int pdchHVACcntVal = 0;
    int pdstFieldCnt = 0;
    int pdchFieldCntVal = 0;

    // Energy Meters Report
    int pdrEnergyMeters = 0;

    int pdstEMelecvalues = 0;
    int pdchEMelecannual = 0;
    int pdchEMelecminvalue = 0;
    int pdchEMelecminvaluetime = 0;
    int pdchEMelecmaxvalue = 0;
    int pdchEMelecmaxvaluetime = 0;

    int pdstEMgasvalues = 0;
    int pdchEMgasannual = 0;
    int pdchEMgasminvalue = 0;
    int pdchEMgasminvaluetime = 0;
    int pdchEMgasmaxvalue = 0;
    int pdchEMgasmaxvaluetime = 0;

    int pdstEMcoolvalues = 0;
    int pdchEMcoolannual = 0;
    int pdchEMcoolminvalue = 0;
    int pdchEMcoolminvaluetime = 0;
    int pdchEMcoolmaxvalue = 0;
    int pdchEMcoolmaxvaluetime = 0;

    int pdstEMwatervalues = 0;
    int pdchEMwaterannual = 0;
    int pdchEMwaterminvalue = 0;
    int pdchEMwaterminvaluetime = 0;
    int pdchEMwatermaxvalue = 0;
    int pdchEMwatermaxvaluetime = 0;

    int pdstEMotherJvalues = 0;
    int pdchEMotherJannual = 0;
    int pdchEMotherJminvalue = 0;
    int pdchEMotherJminvaluetime = 0;
    int pdchEMotherJmaxvalue = 0;
    int pdchEMotherJmaxvaluetime = 0;

    int pdstEMotherKGvalues = 0;
    int pdchEMotherKGannual = 0;
    int pdchEMotherKGminvalue = 0;
    int pdchEMotherKGminvaluetime = 0;
    int pdchEMotherKGmaxvalue = 0;
    int pdchEMotherKGmaxvaluetime = 0;

    int pdstEMotherM3values = 0;
    int pdchEMotherM3annual = 0;
    int pdchEMotherM3minvalue = 0;
    int pdchEMotherM3minvaluetime = 0;
    int pdchEMotherM3maxvalue = 0;
    int pdchEMotherM3maxvaluetime = 0;

    int pdstEMotherLvalues = 0;
    int pdchEMotherLannual = 0;
    int pdchEMotherLminvalue = 0;
    int pdchEMotherLminvaluetime = 0;
    int pdchEMotherLmaxvalue = 0;
    int pdchEMotherLmaxvaluetime = 0;

    // Sensible Heat Gas Component Report
    int pdrSensibleGain = 0;
    // annual
    int pdstSHGSannual = 0;
    int pdchSHGSAnZoneEqHt = 0;
    int pdchSHGSAnZoneEqCl = 0;
    int pdchSHGSAnHvacATUHt = 0;
    int pdchSHGSAnHvacATUCl = 0;
    int pdchSHGSAnSurfHt = 0;
    int pdchSHGSAnSurfCl = 0;
    int pdchSHGSAnPeoplAdd = 0;
    int pdchSHGSAnLiteAdd = 0;
    int pdchSHGSAnEquipAdd = 0;
    int pdchSHGSAnWindAdd = 0;
    int pdchSHGSAnIzaAdd = 0;
    int pdchSHGSAnInfilAdd = 0;
    int pdchSHGSAnOtherAdd = 0;
    int pdchSHGSAnEquipRem = 0;
    int pdchSHGSAnWindRem = 0;
    int pdchSHGSAnIzaRem = 0;
    int pdchSHGSAnInfilRem = 0;
    int pdchSHGSAnOtherRem = 0;
    // peak cooling
    int pdstSHGSpkCl = 0;
    int pdchSHGSClTimePeak = 0;
    int pdchSHGSClHvacHt = 0;
    int pdchSHGSClHvacCl = 0;
    int pdchSHGSClHvacATUHt = 0;
    int pdchSHGSClHvacATUCl = 0;
    int pdchSHGSClSurfHt = 0;
    int pdchSHGSClSurfCl = 0;
    int pdchSHGSClPeoplAdd = 0;
    int pdchSHGSClLiteAdd = 0;
    int pdchSHGSClEquipAdd = 0;
    int pdchSHGSClWindAdd = 0;
    int pdchSHGSClIzaAdd = 0;
    int pdchSHGSClInfilAdd = 0;
    int pdchSHGSClOtherAdd = 0;
    int pdchSHGSClEquipRem = 0;
    int pdchSHGSClWindRem = 0;
    int pdchSHGSClIzaRem = 0;
    int pdchSHGSClInfilRem = 0;
    int pdchSHGSClOtherRem = 0;
    // peak heating
    int pdstSHGSpkHt = 0;
    int pdchSHGSHtTimePeak = 0;
    int pdchSHGSHtHvacHt = 0;
    int pdchSHGSHtHvacCl = 0;
    int pdchSHGSHtHvacATUHt = 0;
    int pdchSHGSHtHvacATUCl = 0;
    int pdchSHGSHtSurfHt = 0;
    int pdchSHGSHtSurfCl = 0;
    int pdchSHGSHtPeoplAdd = 0;
    int pdchSHGSHtLiteAdd = 0;
    int pdchSHGSHtEquipAdd = 0;
    int pdchSHGSHtWindAdd = 0;
    int pdchSHGSHtIzaAdd = 0;
    int pdchSHGSHtInfilAdd = 0;
    int pdchSHGSHtOtherAdd = 0;
    int pdchSHGSHtEquipRem = 0;
    int pdchSHGSHtWindRem = 0;
    int pdchSHGSHtIzaRem = 0;
    int pdchSHGSHtInfilRem = 0;
    int pdchSHGSHtOtherRem = 0;
    // Standard62Report
    int pdrStd62 = 0;
    int pdstS62sysVentReqCool = 0;
    int pdchS62svrClSumVpz = 0;
    int pdchS62svrClPs = 0;
    int pdchS62svrClSumPz = 0;
    int pdchS62svrClD = 0;
    int pdchS62svrClVou = 0;
    int pdchS62svrClVps = 0;
    int pdchS62svrClXs = 0;
    int pdchS62svrClEv = 0;
    int pdchS62svrClVot = 0;
    int pdchS62svrClPercOA = 0;
    int pdchS62svrClEnvironmentOfPs = 0;
    int pdchS62svrClTimeOfPs = 0;

    int pdstS62sysVentReqHeat = 0;
    int pdchS62svrHtSumVpz = 0;
    int pdchS62svrHtPs = 0;
    int pdchS62svrHtSumPz = 0;
    int pdchS62svrHtD = 0;
    int pdchS62svrHtVou = 0;
    int pdchS62svrHtVps = 0;
    int pdchS62svrHtXs = 0;
    int pdchS62svrHtEv = 0;
    int pdchS62svrHtVot = 0;
    int pdchS62svrHtPercOA = 0;
    int pdchS62svrHtEnvironmentOfPs = 0;
    int pdchS62svrHtTimeOfPs = 0;

    int pdstS62znVentPar = 0;
    int pdchS62zvpAlN = 0;
    int pdchS62zvpRp = 0;
    int pdchS62zvpPz = 0;
    int pdchS62zvpRa = 0;
    int pdchS62zvpAz = 0;
    int pdchS62zvpVbz = 0;
    int pdchS62zvpClEz = 0;
    int pdchS62zvpClVoz = 0;
    int pdchS62zvpHtEz = 0;
    int pdchS62zvpHtVoz = 0;

    int pdstS62sysVentPar = 0;
    int pdchS62svpRp = 0;
    int pdchS62svpPz = 0;
    int pdchS62svpRa = 0;
    int pdchS62svpAz = 0;
    int pdchS62svpVbz = 0;
    int pdchS62svpClVoz = 0;
    int pdchS62svpHtVoz = 0;

    int pdstS62znCoolDes = 0;
    int pdchS62zcdAlN = 0;
    int pdchS62zcdBox = 0;
    int pdchS62zcdVpz = 0;
    int pdchS62zcdVps = 0;
    int pdchS62zcdVsec = 0;
    int pdchS62zcdVdz = 0;
    int pdchS62zcdVpzmin = 0;
    int pdchS62zcdVozclg = 0;
    int pdchS62zcdZpz = 0;
    int pdchS62zcdEp = 0;
    int pdchS62zcdEr = 0;
    int pdchS62zcdFa = 0;
    int pdchS62zcdFb = 0;
    int pdchS62zcdFc = 0;
    int pdchS62zcdEvz = 0;

    int pdstS62sysCoolDes = 0;
    int pdchS62scdVpz = 0;
    int pdchS62scdVps = 0;
    int pdchS62scdVsec = 0;
    int pdchS62scdVdz = 0;
    int pdchS62scdVpzmin = 0;
    int pdchS62scdVozclg = 0;
    int pdchS62scdEvz = 0;

    int pdstS62znHeatDes = 0;
    int pdchS62zhdAlN = 0;
    int pdchS62zhdBox = 0;
    int pdchS62zhdVpz = 0;
    int pdchS62zhdVps = 0;
    int pdchS62zhdVsec = 0;
    int pdchS62zhdVdz = 0;
    int pdchS62zhdVpzmin = 0;
    int pdchS62zhdVozhtg = 0;
    int pdchS62zhdZpz = 0;
    int pdchS62zhdEp = 0;
    int pdchS62zhdEr = 0;
    int pdchS62zhdFa = 0;
    int pdchS62zhdFb = 0;
    int pdchS62zhdFc = 0;
    int pdchS62zhdEvz = 0;

    int pdstS62sysHeatDes = 0;
    int pdchS62shdVpz = 0;
    int pdchS62shdVps = 0;
    int pdchS62shdVsec = 0;
    int pdchS62shdVdz = 0;
    int pdchS62shdVpzmin = 0;
    int pdchS62shdVozhtg = 0;
    int pdchS62shdEvz = 0;

    //  LEED Summary
    int pdrLeed = 0;
    int pdstLeedGenInfo = 0;
    int pdchLeedGenData = 0;

    int pdstLeedSpaceUsageType = 0;
    int pdchLeedSutName = 0;
    int pdchLeedSutSpArea = 0;
    int pdchLeedSutOcArea = 0;
    int pdchLeedSutUnArea = 0;
    int pdchLeedSutHrsWeek = 0;

    int pdstLeedAdvsMsg = 0;
    int pdchLeedAmData = 0;

    int pdstLeedEneTypSum = 0;
    int pdchLeedEtsType = 0;
    int pdchLeedEtsRtNm = 0;
    int pdchLeedEtsVirt = 0;
    int pdchLeedEtsEneUnt = 0;
    int pdchLeedEtsDemUnt = 0;

    int pdstLeedPerf = 0;
    int pdchLeedPerfRot = 0;
    int pdchLeedPerfElEneUse = 0;
    int pdchLeedPerfElDem = 0;
    int pdchLeedPerfGasEneUse = 0;
    int pdchLeedPerfGasDem = 0;
    int pdchLeedPerfGasolineEneUse = 0;
    int pdchLeedPerfGasolineDem = 0;
    int pdchLeedPerfDieselEneUse = 0;
    int pdchLeedPerfDieselDem = 0;
    int pdchLeedPerfCoalEneUse = 0;
    int pdchLeedPerfCoalDem = 0;
    int pdchLeedPerfFuelOil1EneUse = 0;
    int pdchLeedPerfFuelOil1Dem = 0;
    int pdchLeedPerfFuelOil2EneUse = 0;
    int pdchLeedPerfFuelOil2Dem = 0;
    int pdchLeedPerfPropaneEneUse = 0;
    int pdchLeedPerfPropaneDem = 0;
    int pdchLeedPerfOtherFuel1EneUse = 0;
    int pdchLeedPerfOtherFuel1Dem = 0;
    int pdchLeedPerfOtherFuel2EneUse = 0;
    int pdchLeedPerfOtherFuel2Dem = 0;
    int pdchLeedPerfDisClEneUse = 0;
    int pdchLeedPerfDisClDem = 0;
    int pdchLeedPerfDisHtEneUse = 0;
    int pdchLeedPerfDisHtDem = 0;

    int pdstLeedEneUseSum = 0;
    int pdchLeedEusUnt = 0;
    int pdchLeedEusProc = 0;
    int pdchLeedEusTotal = 0;

    int pdstLeedEneCostSum = 0;
    int pdchLeedEcUnt = 0;
    int pdchLeedEcsProc = 0;
    int pdchLeedEcsTotal = 0;
    Real64 LEEDelecCostTotal = 0;
    Real64 LEEDgasCostTotal = 0;
    Real64 LEEDothrCostTotal = 0;

    int pdstLeedRenewSum = 0;
    int pdchLeedRenRatCap = 0;
    int pdchLeedRenAnGen = 0;

    int pdstLeedEneUseIntEl = 0;
    int pdchLeedEuiElec = 0;
    int pdstLeedEneUseIntNatG = 0;
    int pdchLeedEuiNatG = 0;
    int pdstLeedEneUseIntOthr = 0;
    int pdchLeedEuiOthr = 0;

    int pdstLeedEneUsePerc = 0;
    int pdchLeedEupPerc = 0;

    int pdstLeedEqFlLdHrs = 0;
    int pdchLeedEflhEflh = 0;
    int pdchLeedEflhNonZerHrs = 0;

    int pdstLeedSchedSetPts = 0;
    int pdChLeedSchStPtFirstObjUsed = 0;
    int pdChLeedSchStPtMonthUsed = 0;
    int pdchLeedSchStPt11amWednesday = 0;
    int pdchLeedSchStPt11amWedCnt = 0;
    int pdchLeedSchStPt11pmWednesday = 0;
    int pdchLeedSchStPt11pmWedCnt = 0;

    int pdrThermalResilience = 0;
    int pdstHIHours = 0;
    int pdchHIHourSafe = 0;
    int pdchHIHourCaution = 0;
    int pdchHIHourExtremeCaution = 0;
    int pdchHIHourDanger = 0;
    int pdchHIHourExtremeDanger = 0;
    int pdstHIOccuHours = 0;
    int pdchHIOccuHourSafe = 0;
    int pdchHIOccuHourCaution = 0;
    int pdchHIOccuHourExtremeCaution = 0;
    int pdchHIOccuHourDanger = 0;
    int pdchHIOccuHourExtremeDanger = 0;
    int pdstHumidexHours = 0;
    int pdchHumidexHourLittle = 0;
    int pdchHumidexHourSome = 0;
    int pdchHumidexHourGreat = 0;
    int pdchHumidexHourDanger = 0;
    int pdchHumidexHourStroke = 0;
    int pdstHumidexOccuHours = 0;
    int pdchHumidexOccuHourLittle = 0;
    int pdchHumidexOccuHourSome = 0;
    int pdchHumidexOccuHourGreat = 0;
    int pdchHumidexOccuHourDanger = 0;
    int pdchHumidexOccuHourStroke = 0;

    int pdstHeatingSETHours = 0;
    int pdchHeatingSETHours = 0;
    int pdchHeatingSETOccuHours = 0;
    int pdchHeatingSETUnmetDuration = 0;
    int pdchHeatingSETUnmetTime = 0;
    int pdstCoolingSETHours = 0;
    int pdchCoolingSETHours = 0;
    int pdchCoolingSETOccuHours = 0;
    int pdchCoolingSETUnmetDuration = 0;
    int pdchCoolingSETUnmetTime = 0;

    int pdrCO2Resilience = 0;
    int pdstCO2Hours = 0;
    int pdchCO2HourSafe = 0;
    int pdchCO2HourCaution = 0;
    int pdchCO2HourHazard = 0;
    int pdstCO2OccuHours = 0;
    int pdchCO2OccuHourSafe = 0;
    int pdchCO2OccuHourCaution = 0;
    int pdchCO2OccuHourHazard = 0;

    int pdrVisualResilience = 0;
    int pdstIllumHours = 0;
    int pdchIllumHourDark = 0;
    int pdchIllumHourDim = 0;
    int pdchIllumHourAdequate = 0;
    int pdchIllumHourBright = 0;
    int pdstIllumOccuHours = 0;
    int pdchIllumOccuHourDark = 0;
    int pdchIllumOccuHourDim = 0;
    int pdchIllumOccuHourAdequate = 0;
    int pdchIllumOccuHourBright = 0;

    int sizeReportName = 0;
    int numReportName = 0;

    int sizeSubTable = 0;
    int numSubTable = 0;

    int sizeColumnTag = 0;
    int numColumnTag = 0;

    int sizeTableEntry = 0;
    int numTableEntry = 0;

    int sizeCompSizeTableEntry = 0;
    int numCompSizeTableEntry = 0;

    int sizeShadowRelate = 0;
    int numShadowRelate = 0;

    // Totals for ABUPS report
    Real64 TotalNotMetHeatingOccupiedForABUPS = 0.0;
    Real64 TotalNotMetCoolingOccupiedForABUPS = 0.0;
    Real64 TotalNotMetOccupiedForABUPS = 0.0;
    Real64 TotalTimeNotSimpleASH55EitherForABUPS = 0.0;

    // Totals for OA details report
    Real64 TotalVozMax = 0.0;                     // total Voz (max of heating and cooling)
    Real64 TotalAnyZoneBelowVozDynForOA = 0.0;    // total time any zone below target Voz-Dyn
    Real64 TotalAllZonesAtVozDynForOA = 0.0;      // total time all zones at target Voz-Dyn
    Real64 TotalAnyZoneAboveVozDynForOA = 0.0;    // total time any zone above target Voz-Dyn
    Real64 TotalAnyZoneBelowVozDynOccForOA = 0.0; // total time any zone below target Voz-Dyn
    Real64 TotalAllZonesAtVozDynOccForOA = 0.0;   // total time all zones at target Voz-Dyn
    Real64 TotalAnyZoneAboveVozDynOccForOA = 0.0; // total time any zone above target Voz-Dyn
    Real64 TotalAnyZoneVentUnoccForOA = 0.0;      // total time any zone has ventilation when unoccupied

    Real64 TotalAnyAirLoopBelowVozDynForOA = 0.0;    // total time any air loop below target Voz-Dyn
    Real64 TotalAllAirLoopsAtVozDynForOA = 0.0;      // total time all air loops at target Voz-Dyn
    Real64 TotalAnyAirLoopAboveVozDynForOA = 0.0;    // total time any air loop above target Voz-Dyn
    Real64 TotalAnyAirLoopBelowVozDynOccForOA = 0.0; // total time any air loop below target Voz-Dyn
    Real64 TotalAllAirLoopsAtVozDynOccForOA = 0.0;   // total time all air loops at target Voz-Dyn
    Real64 TotalAnyAirLoopAboveVozDynOccForOA = 0.0; // total time any air loop above target Voz-Dyn
    Real64 TotalAnyAirLoopVentUnoccForOA = 0.0;      // total time any air loop has ventilation when unoccupied

    Array1D<OutputReportPredefined::reportNameType> reportName;
    Array1D<OutputReportPredefined::SubTableType> subTable;
    Array1D<OutputReportPredefined::ColumnTagType> columnTag;
    Array1D<OutputReportPredefined::TableEntryType> tableEntry;
    Array1D<OutputReportPredefined::CompSizeTableEntryType> CompSizeTableEntry;
    Array1D<OutputReportPredefined::ShadowRelateType> ShadowRelate;

    void clear_state() override
    {
        this->pdrClim = 0;
        this->pdstDesDay = 0;
        this->pdchDDmaxDB = 0;
        this->pdchDDrange = 0;
        this->pdchDDhumid = 0;
        this->pdchDDhumTyp = 0;
        this->pdchDDwindSp = 0;
        this->pdchDDwindDr = 0;
        this->pdstWthr = 0;
        this->pdchWthrVal = 0;
        this->pdrEquip = 0;
        this->pdstMech = 0;
        this->pdchMechType = 0;
        this->pdchMechNomCap = 0;
        this->pdchMechNomEff = 0;
        this->pdchMechIPLVSI = 0;
        this->pdchMechIPLVIP = 0;
        this->pdstFan = 0;
        this->pdchFanType = 0;
        this->pdchFanTotEff = 0;
        this->pdchFanDeltaP = 0;
        this->pdchFanVolFlow = 0;
        this->pdchFanMotorIn = 0;
        this->pdchFanEnergyIndex = 0;
        this->pdchFanEndUse = 0;
        this->pdchFanPwr = 0;
        this->pdchFanPwrPerFlow = 0;
        this->pdchFanDesDay = 0;
        this->pdchFanPkTime = 0;
        this->pdstPump = 0;
        this->pdchPumpType = 0;
        this->pdchPumpControl = 0;
        this->pdchPumpHead = 0;
        this->pdchPumpFlow = 0;
        this->pdchPumpPower = 0;
        this->pdchPumpPwrPerFlow = 0;
        this->pdchPumpEndUse = 0;
        this->pdchMotEff = 0;
        this->pdstCoolCoil = 0;
        this->pdchCoolCoilType = 0;
        this->pdchCoolCoilDesCap = 0;
        this->pdchCoolCoilTotCap = 0;
        this->pdchCoolCoilSensCap = 0;
        this->pdchCoolCoilLatCap = 0;
        this->pdchCoolCoilSHR = 0;
        this->pdchCoolCoilNomEff = 0;
        this->pdchCoolCoilUATotal = 0;
        this->pdchCoolCoilArea = 0;
        this->pdstDXCoolCoil = 0;
        this->pdchDXCoolCoilType = 0;
        this->pdchDXCoolCoilNetCapSI = 0;
        this->pdchDXCoolCoilCOP = 0;
        this->pdchDXCoolCoilSEERUserIP = 0;
        this->pdchDXCoolCoilSEERStandardIP = 0;
        this->pdchDXCoolCoilEERIP = 0;
        this->pdchDXCoolCoilIEERIP = 0;
        this->pdstDXCoolCoil2 = 0;
        this->pdchDXCoolCoilNetCapSIA = 0;
        this->pdchDXCoolCoilElecPowerA = 0;
        this->pdchDXCoolCoilNetCapSIB = 0;
        this->pdchDXCoolCoilElecPowerB = 0;
        this->pdchDXCoolCoilNetCapSIC = 0;
        this->pdchDXCoolCoilElecPowerC = 0;
        this->pdchDXCoolCoilNetCapSID = 0;
        this->pdchDXCoolCoilElecPowerD = 0;
        this->pdstVAVDXCoolCoil = 0;
        this->pdchVAVDXCoolCoilType = 0;
        this->pdchVAVDXFanName = 0;
        this->pdchVAVDXCoolCoilNetCapSI = 0;
        this->pdchVAVDXCoolCoilCOP = 0;
        this->pdchVAVDXCoolCoilIEERIP = 0;
        this->pdchVAVDXCoolCoilEERIP = 0;
        this->pdchVAVDXCoolCoilMdotA = 0;
        this->pdchVAVDXCoolCoilCOP_B = 0;
        this->pdchVAVDXCoolCoilEER_B_IP = 0;
        this->pdchVAVDXCoolCoilMdotB = 0;
        this->pdchVAVDXCoolCoilCOP_C = 0;
        this->pdchVAVDXCoolCoilEER_C_IP = 0;
        this->pdchVAVDXCoolCoilMdotC = 0;
        this->pdchVAVDXCoolCoilCOP_D = 0;
        this->pdchVAVDXCoolCoilEER_D_IP = 0;
        this->pdchVAVDXCoolCoilMdotD = 0;
        this->pdstHeatCoil = 0;
        this->pdchHeatCoilType = 0;
        this->pdchHeatCoilDesCap = 0;
        this->pdchHeatCoilNomCap = 0;
        this->pdchHeatCoilNomEff = 0;
        this->pdstSWH = 0;
        this->pdchSWHType = 0;
        this->pdchSWHVol = 0;
        this->pdchSWHHeatIn = 0;
        this->pdchSWHThEff = 0;
        this->pdchSWHRecEff = 0;
        this->pdchSWHEnFac = 0;
        this->pdrEnvelope = 0;
        this->pdstOpaque = 0;
        this->pdchOpCons = 0;
        this->pdchOpRefl = 0;
        this->pdchOpUfactFilm = 0;
        this->pdchOpUfactNoFilm = 0;
        this->pdchOpGrArea = 0;
        this->pdchOpNetArea = 0;
        this->pdchOpAzimuth = 0;
        this->pdchOpTilt = 0;
        this->pdchOpDir = 0;
        this->pdstIntOpaque = 0;
        this->pdchIntOpCons = 0;
        this->pdchIntOpRefl = 0;
        this->pdchIntOpUfactFilm = 0;
        this->pdchIntOpUfactNoFilm = 0;
        this->pdchIntOpGrArea = 0;
        this->pdchIntOpNetArea = 0;
        this->pdchIntOpAzimuth = 0;
        this->pdchIntOpTilt = 0;
        this->pdchIntOpDir = 0;
        this->pdstFen = 0;
        this->pdchFenCons = 0;
        this->pdchFenAreaOf1 = 0;
        this->pdchFenGlassAreaOf1 = 0;
        this->pdchFenFrameAreaOf1 = 0;
        this->pdchFenDividerAreaOf1 = 0;
        this->pdchFenArea = 0;
        this->pdchFenUfact = 0;
        this->pdchFenSHGC = 0;
        this->pdchFenVisTr = 0;
        this->pdchFenFrameConductance = 0;
        this->pdchFenDividerConductance = 0;
        this->pdchFenSwitchable = 0;
        this->pdchFenParent = 0;
        this->pdchFenAzimuth = 0;
        this->pdchFenTilt = 0;
        this->pdchFenDir = 0;
        this->pdstDoor = 0;
        this->pdchDrCons = 0;
        this->pdchDrUfactFilm = 0;
        this->pdchDrUfactNoFilm = 0;
        this->pdchDrGrArea = 0;
        this->pdchDrParent = 0;
        this->pdstIntDoor = 0;
        this->pdchIntDrCons = 0;
        this->pdchIntDrUfactFilm = 0;
        this->pdchIntDrUfactNoFilm = 0;
        this->pdchIntDrGrArea = 0;
        this->pdchIntDrParent = 0;
        this->pdstIntFen = 0;
        this->pdchIntFenCons = 0;
        this->pdchIntFenAreaOf1 = 0;
        this->pdchIntFenArea = 0;
        this->pdchIntFenUfact = 0;
        this->pdchIntFenSHGC = 0;
        this->pdchIntFenVisTr = 0;
        this->pdchIntFenParent = 0;
        this->pdrShading = 0;
        this->pdstSunlitFrac = 0;
        this->pdchSlfMar21_9 = 0;
        this->pdchSlfMar21_12 = 0;
        this->pdchSlfMar21_15 = 0;
        this->pdchSlfJun21_9 = 0;
        this->pdchSlfJun21_12 = 0;
        this->pdchSlfJun21_15 = 0;
        this->pdchSlfDec21_9 = 0;
        this->pdchSlfDec21_12 = 0;
        this->pdchSlfDec21_15 = 0;
        this->pdstWindowControl = 0;
        this->pdchWscName = 0;
        this->pdchWscShading = 0;
        this->pdchWscShadCons = 0;
        this->pdchWscControl = 0;
        this->pdchWscGlare = 0;
        this->pdrLighting = 0;
        this->pdstInLite = 0;
        this->pdchInLtZone = 0;
        this->pdchInLtDens = 0;
        this->pdchInLtArea = 0;
        this->pdchInLtPower = 0;
        this->pdchInLtEndUse = 0;
        this->pdchInLtSchd = 0;
        this->pdchInLtAvgHrSchd = 0;
        this->pdchInLtAvgHrOper = 0;
        this->pdchInLtFullLoadHrs = 0;
        this->pdchInLtRetAir = 0;
        this->pdchInLtCond = 0;
        this->pdchInLtConsump = 0;
        this->pdstExtLite = 0;
        this->pdchExLtPower = 0;
        this->pdchExLtClock = 0;
        this->pdchExLtSchd = 0;
        this->pdchExLtAvgHrSchd = 0;
        this->pdchExLtAvgHrOper = 0;
        this->pdchExLtFullLoadHrs = 0;
        this->pdchExLtConsump = 0;
        this->pdstDaylight = 0;
        this->pdchDyLtZone = 0;
        this->pdchDyLtCtrlName = 0;
        this->pdchDyLtKind = 0;
        this->pdchDyLtCtrlType = 0;
        this->pdchDyLtFrac = 0;
        this->pdchDyLtWInst = 0;
        this->pdchDyLtWCtrl = 0;
        this->pdrSizing = 0;
        this->pdstZoneClSize = 0;
        this->pdchZnClCalcDesLd = 0;
        this->pdchZnClUserDesLd = 0;
        this->pdchZnClUserDesLdPerArea = 0;
        this->pdchZnClCalcDesAirFlow = 0;
        this->pdchZnClUserDesAirFlow = 0;
        this->pdchZnClDesDay = 0;
        this->pdchZnClPkTime = 0;
        this->pdchZnClPkTstatTemp = 0;
        this->pdchZnClPkIndTemp = 0;
        this->pdchZnClPkIndHum = 0;
        this->pdchZnClPkOATemp = 0;
        this->pdchZnClPkOAHum = 0;
        this->pdchZnClPkOAMinFlow = 0;
        this->pdchZnClPkDOASHeatGain = 0;
        this->pdstZoneHtSize = 0;
        this->pdchZnHtCalcDesLd = 0;
        this->pdchZnHtUserDesLd = 0;
        this->pdchZnHtUserDesLdPerArea = 0;
        this->pdchZnHtCalcDesAirFlow = 0;
        this->pdchZnHtUserDesAirFlow = 0;
        this->pdchZnHtDesDay = 0;
        this->pdchZnHtPkTime = 0;
        this->pdchZnHtPkTstatTemp = 0;
        this->pdchZnHtPkIndTemp = 0;
        this->pdchZnHtPkIndHum = 0;
        this->pdchZnHtPkOATemp = 0;
        this->pdchZnHtPkOAHum = 0;
        this->pdchZnHtPkOAMinFlow = 0;
        this->pdchZnHtPkDOASHeatGain = 0;
        this->pdstSystemSize = 0;
        this->pdchSysSizCalcClAir = 0;
        this->pdchSysSizUserClAir = 0;
        this->pdchSysSizCalcHtAir = 0;
        this->pdchSysSizUserHtAir = 0;
        this->pdchSysSizAdjustedClAir = 0;
        this->pdchSysSizAdjustedHtAir = 0;
        this->pdchSysSizAdjustedMainAir = 0;
        this->pdchSysSizCalcHeatFlowRatio = 0;
        this->pdchSysSizUserHeatFlowRatio = 0;
        this->pdstPlantSize = 0;
        this->pdchPlantSizCalcVdot = 0;
        this->pdchPlantSizMeasVdot = 0;
        this->pdchPlantSizPrevVdot = 0;
        this->pdchPlantSizCoincYesNo = 0;
        this->pdchPlantSizDesDay = 0;
        this->pdchPlantSizPkTimeHour = 0;
        this->pdchPlantSizPkTimeDayOfSim = 0;
        this->pdchPlantSizPkTimeMin = 0;
        this->pdrCoilSizingDetailsTable = 0;
        this->pdstCoilSummaryCoilSelection = 0;
        this->pdchCoilLocation = 0;
        this->pdchCoilHVACType = 0;
        this->pdchCoilHVACName = 0;
        this->pdchCoilZoneName = 0;
        this->pdchCoilName = 0;
        this->pdchCoilType = 0;
        this->pdchSysSizingMethCoinc = 0;
        this->pdchSysSizingMethCap = 0;
        this->pdchSysSizingMethAir = 0;
        this->pdchCoilIsCapAutosized = 0;
        this->pdchCoilIsAirFlowAutosized = 0;
        this->pdchCoilIsWaterFlowAutosized = 0;
        this->pdchCoilIsOATreated = 0;
        this->pdchCoilFinalTotalCap = 0;
        this->pdchCoilFinalSensCap = 0;
        this->pdchCoilFinalAirVolFlowRate = 0;
        this->pdchCoilFinalPlantVolFlowRate = 0;
        this->pdchCoilDDnameSensIdealPeak = 0;
        this->pdchCoilDateTimeSensIdealPeak = 0;
        this->pdchCoilDDnameTotIdealPeak = 0;
        this->pdchCoilDateTimeTotIdealPeak = 0;
        this->pdchCoilDDnameAirFlowIdealPeak = 0;
        this->pdchCoilDateTimeAirFlowIdealPeak = 0;
        this->pdchCoilPeakLoadTypeToSizeOn = 0;
        this->pdchCoilTotalCapIdealPeak = 0;
        this->pdchCoilSensCapIdealPeak = 0;
        this->pdchCoilAirMassFlowIdealPeak = 0;
        this->pdchCoilAirVolumeFlowIdealPeak = 0;
        this->pdchCoilEntDryBulbIdealPeak = 0;
        this->pdchCoilEntWetBulbIdealPeak = 0;
        this->pdchCoilEntHumRatIdealPeak = 0;
        this->pdchCoilEntEnthalpyIdealPeak = 0;
        this->pdchCoilLvgDryBulbIdealPeak = 0;
        this->pdchCoilLvgWetBulbIdealPeak = 0;
        this->pdchCoilLvgHumRatIdealPeak = 0;
        this->pdchCoilLvgEnthalpyIdealPeak = 0;
        this->pdchCoilWaterMassFlowIdealPeak = 0;
        this->pdchCoilEntWaterTempIdealPeak = 0;
        this->pdchCoilLvgWaterTempIdealPeak = 0;
        this->pdchCoilWaterDeltaTempIdealPeak = 0;
        this->pdchCoilRatedTotalCap = 0;
        this->pdchCoilRatedSensCap = 0;
        this->pdchCoilOffRatingCapacityModifierIdealPeak = 0;
        this->pdchCoilOffRatingCapacityModifierSimPeak = 0;
        this->pdchCoilRatedAirMass = 0;
        this->pdchCoilRatedEntDryBulb = 0;
        this->pdchCoilRatedEntWetBulb = 0;
        this->pdchCoilRatedEntHumRat = 0;
        this->pdchCoilRatedEntEnthalpy = 0;
        this->pdchCoilRatedLvgDryBulb = 0;
        this->pdchCoilRatedLvgWetBulb = 0;
        this->pdchCoilRatedLvgHumRat = 0;
        this->pdchCoilRatedLvgEnthalpy = 0;
        this->pdchCoilRatedWaterMass = 0;
        this->pdchCoilRatedEntWaterTemp = 0;
        this->pdchCoilRatedLvgWaterTemp = 0;
        this->pdchCoilRatedWaterDeltaTemp = 0;
        this->pdchFanAssociatedWithCoilName = 0;
        this->pdchFanAssociatedWithCoilType = 0;
        this->pdchFanAssociatedVdotSize = 0;
        this->pdchFanAssociatedMdotSize = 0;
        this->pdchFanHeatGainIdealPeak = 0;
        this->pdchCoilNetTotalCapacityIdealPeak = 0;
        this->pdchCoilPlantLoopName = 0;
        this->pdchPlantMassFlowMaximum = 0;
        this->pdchPlantRetTempDesign = 0;
        this->pdchPlantSupTempDesign = 0;
        this->pdchPlantDeltaTempDesign = 0;
        this->pdchPlantCapacity = 0;
        this->pdchCoilCapPrcntPlantCapacity = 0;
        this->pdchCoilFlowPrcntPlantFlow = 0;
        this->pdchOADryBulbIdealPeak = 0;
        this->pdchOAHumRatIdealPeak = 0;
        this->pdchOAWetBulbatIdealPeak = 0;
        this->pdchOAVolFlowIdealPeak = 0;
        this->pdchOAFlowPrcntIdealPeak = 0;
        this->pdchOABarometricPressureIdealPeak = 0;
        this->pdchAirSysRADryBulbIdealPeak = 0;
        this->pdchAirSysRAHumRatIdealPeak = 0;
        this->pdchZoneAirDryBulbIdealPeak = 0;
        this->pdchZoneAirHumRatIdealPeak = 0;
        this->pdchZoneAirRelHumIdealPeak = 0;
        this->pdchCoilUA = 0;
        this->pdchZoneSensibleLoadIdealPeak = 0;
        this->pdchZoneLatentLoadIdealPeak = 0;
        this->pdchCoilIdealSizCapOverSimPeak = 0;
        this->pdchCoilIdealSizCapUnderSimPeak = 0;
        this->pdchReheatCoilMultiplier = 0;
        this->pdchFlowCapRatioLowCapIncreaseRatio = 0;
        this->pdchFlowCapRatioHiCapDecreaseRatio = 0;
        this->pdchPlantFluidSpecificHeat = 0;
        this->pdchPlantFluidDensity = 0;
        this->pdchMoistAirSpecificHeat = 0;
        this->pdchDryAirSpecificHeat = 0;
        this->pdchStandRhoAir = 0;
        this->pdst2CoilSummaryCoilSelection = 0;
        this->pdch2CoilName = 0;
        this->pdch2CoilType = 0;
        this->pdch2CoilHVACType = 0;
        this->pdch2CoilHVACName = 0;
        this->pdch2CoilFinalTotalCap = 0;
        this->pdch2CoilFinalSensCap = 0;
        this->pdch2CoilFinalAirVolFlowRate = 0;
        this->pdch2CoilFinalPlantVolFlowRate = 0;
        this->pdch2CoilUA = 0;
        this->pdch2CoilDDnameSensIdealPeak = 0;
        this->pdch2CoilDateTimeSensIdealPeak = 0;
        this->pdch2CoilDDnameAirFlowIdealPeak = 0;
        this->pdch2CoilDateTimeAirFlowIdealPeak = 0;
        this->pdch2CoilTotalCapIdealPeak = 0;
        this->pdch2CoilSensCapIdealPeak = 0;
        this->pdch2CoilAirVolumeFlowIdealPeak = 0;
        this->pdch2CoilEntDryBulbIdealPeak = 0;
        this->pdch2CoilEntWetBulbIdealPeak = 0;
        this->pdch2CoilEntHumRatIdealPeak = 0;
        this->pdch2CoilLvgDryBulbIdealPeak = 0;
        this->pdch2CoilLvgWetBulbIdealPeak = 0;
        this->pdch2CoilLvgHumRatIdealPeak = 0;
        this->pdch2CoilRatedTotalCap = 0;
        this->pdch2CoilRatedSensCap = 0;
        this->pdch2OADryBulbIdealPeak = 0;
        this->pdch2OAHumRatIdealPeak = 0;
        this->pdch2OAWetBulbatIdealPeak = 0;
        this->pdch2OAFlowPrcntIdealPeak = 0;
        this->pdch2ZoneAirDryBulbIdealPeak = 0;
        this->pdch2ZoneAirHumRatIdealPeak = 0;
        this->pdch2ZoneAirRelHumIdealPeak = 0;
        this->pdch2ZoneSensibleLoadIdealPeak = 0;
        this->pdch2ZoneLatentLoadIdealPeak = 0;
        this->pdrSystem = 0;
        this->pdstEconomizer = 0;
        this->pdchEcoKind = 0;
        this->pdchEcoMinOA = 0;
        this->pdchEcoMaxOA = 0;
        this->pdchEcoRetTemp = 0;
        this->pdchEcoRetEnth = 0;
        this->pdchEcoOATempLim = 0;
        this->pdchEcoOAEnthLim = 0;
        this->pdstDemCntlVent = 0;
        this->pdchDCVventMechName = 0;
        this->pdchDCVperPerson = 0;
        this->pdchDCVperArea = 0;
        this->pdchDCVperZone = 0;
        this->pdchDCVperACH = 0;
        this->pdchDCVMethod = 0;
        this->pdchDCVOASchName = 0;
        this->pdchDCVZoneADEffCooling = 0;
        this->pdchDCVZoneADEffHeating = 0;
        this->pdchDCVZoneADEffSchName = 0;
        this->pdstSimpleComfort = 0;
        this->pdchSCwinterClothes = 0;
        this->pdchSCsummerClothes = 0;
        this->pdchSCeitherClothes = 0;
        this->pdstUnmetLoads = 0;
        this->pdchULnotMetHeat = 0;
        this->pdchULnotMetCool = 0;
        this->pdchULnotMetHeatOcc = 0;
        this->pdchULnotMetCoolOcc = 0;

        this->pdrOutsideAir = 0;
        this->pdstOAavgOcc = 0;
        this->pdchOaoAvgNumOcc1 = 0;
        this->pdchOaoNomNumOcc1 = 0;
        this->pdchOaoZoneVol1 = 0;
        this->pdchOaoAvgMechVent = 0;
        this->pdchOaoAvgInfil = 0;
        this->pdchOaoAvgAFNInfil = 0;
        this->pdchOaoAvgSimpVent = 0;
        this->pdchOaoAvgTotVent = 0;
        this->pdstOAminOcc = 0;
        this->pdchOaoAvgNumOcc2 = 0;
        this->pdchOaoNomNumOcc2 = 0;
        this->pdchOaoZoneVol2 = 0;
        this->pdchOaoMinMechVent = 0;
        this->pdchOaoMinInfil = 0;
        this->pdchOaoMinAFNInfil = 0;
        this->pdchOaoMinSimpVent = 0;
        this->pdchOaoMinTotVent = 0;

        this->pdrOutsideAirDetails = 0;
        this->pdstOAmechVentParByZone = 0;
        this->pdchOaMvAirLpNm = 0;
        this->pdchOaMvAvgNumOcc = 0;
        this->pdchOaMvNomNumOcc = 0;
        this->pdchOaMvZoneVol = 0;
        this->pdchOaMvZoneArea = 0;
        this->pdchOaMvDesZnOa = 0;
        this->pdchOaMvMinDynTrgVent = 0;

        this->pdstOAtotAirByZone = 0;
        this->pdchOaTaBzMechVent = 0;
        this->pdchOaTaBzNatVent = 0;
        this->pdchOaTaBzTotVent = 0;
        this->pdchOaTaBzInfil = 0;
        this->pdchOaTaBzTotVentInfil = 0;
        this->pdchOaTaBzDynTrgVent = 0;
        this->pdchOaTaBzTmBelow = 0;
        this->pdchOaTaBzTmAt = 0;
        this->pdchOaTaBzTmAbove = 0;
        this->pdchOaTaBzTmAboveUnocc = 0;

        this->pdstOAavgOccByZone = 0;
        this->pdchOaOccBzMechVent = 0;
        this->pdchOaOccBzNatVent = 0;
        this->pdchOaOccBzTotVent = 0;
        this->pdchOaOccBzInfil = 0;
        this->pdchOaOccBzTotVentInfil = 0;
        this->pdchOaOccBzDynTrgVent = 0;
        this->pdchOaOccBzTmBelow = 0;
        this->pdchOaOccBzTmAt = 0;
        this->pdchOaOccBzTmAbove = 0;

        this->pdstOAtotAirByLoop = 0;
        this->pdchOaTaAlMechVent = 0;
        this->pdchOaTaAlNatVent = 0;
        this->pdchOaTaAlTotVent = 0;
        this->pdchOaTaAlSumDynTrgVent = 0;
        this->pdchOaTaAlTmBelow = 0;
        this->pdchOaTaAlTmAt = 0;
        this->pdchOaTaAlTmAbove = 0;
        this->pdchOaTaAlTmAboveUnocc = 0;

        this->pdstOAavgOccByLoop = 0;
        this->pdchOaOccAlMechVent = 0;
        this->pdchOaOccAlNatVent = 0;
        this->pdchOaOccAlTotVent = 0;
        this->pdchOaOccAlSumDynTrgVent = 0;
        this->pdchOaOccAlTmBelow = 0;
        this->pdchOaOccAlTmAt = 0;
        this->pdchOaOccAlTmAbove = 0;

        this->pdstOAtimeFactorsDurOcc = 0;
        this->pdchOaTmFctNoLimit = 0;
        this->pdchOaTmFctHiHumid = 0;
        this->pdchOaTmFctNiteVent = 0;
        this->pdchOaTmFctEcono = 0;
        this->pdchOaTmFctDCV = 0;
        this->pdchOaTmFctExhaust = 0;
        this->pdchOaTmFctLimit = 0;
        this->pdchOaTmFctDemand = 0;
        this->pdchOaTmFctEMS = 0;

        this->pdstOAavgFactorsDurOcc = 0;
        this->pdchOaAvFctNoLimit = 0;
        this->pdchOaAvFctHiHumid = 0;
        this->pdchOaAvFctNiteVent = 0;
        this->pdchOaAvFctEcono = 0;
        this->pdchOaAvFctDCV = 0;
        this->pdchOaAvFctExhaust = 0;
        this->pdchOaAvFctLimit = 0;
        this->pdchOaAvFctDemand = 0;
        this->pdchOaAvFctEMS = 0;

        this->pdrObjCnt = 0;
        this->pdstSurfCnt = 0;
        this->pdchSurfCntTot = 0;
        this->pdchSurfCntExt = 0;
        this->pdstHVACcnt = 0;
        this->pdchHVACcntVal = 0;
        this->pdstFieldCnt = 0;
        this->pdchFieldCntVal = 0;
        this->pdrEnergyMeters = 0;
        this->pdstEMelecvalues = 0;
        this->pdchEMelecannual = 0;
        this->pdchEMelecminvalue = 0;
        this->pdchEMelecminvaluetime = 0;
        this->pdchEMelecmaxvalue = 0;
        this->pdchEMelecmaxvaluetime = 0;
        this->pdstEMgasvalues = 0;
        this->pdchEMgasannual = 0;
        this->pdchEMgasminvalue = 0;
        this->pdchEMgasminvaluetime = 0;
        this->pdchEMgasmaxvalue = 0;
        this->pdchEMgasmaxvaluetime = 0;
        this->pdstEMcoolvalues = 0;
        this->pdchEMcoolannual = 0;
        this->pdchEMcoolminvalue = 0;
        this->pdchEMcoolminvaluetime = 0;
        this->pdchEMcoolmaxvalue = 0;
        this->pdchEMcoolmaxvaluetime = 0;
        this->pdstEMwatervalues = 0;
        this->pdchEMwaterannual = 0;
        this->pdchEMwaterminvalue = 0;
        this->pdchEMwaterminvaluetime = 0;
        this->pdchEMwatermaxvalue = 0;
        this->pdchEMwatermaxvaluetime = 0;
        this->pdstEMotherJvalues = 0;
        this->pdchEMotherJannual = 0;
        this->pdchEMotherJminvalue = 0;
        this->pdchEMotherJminvaluetime = 0;
        this->pdchEMotherJmaxvalue = 0;
        this->pdchEMotherJmaxvaluetime = 0;
        this->pdstEMotherKGvalues = 0;
        this->pdchEMotherKGannual = 0;
        this->pdchEMotherKGminvalue = 0;
        this->pdchEMotherKGminvaluetime = 0;
        this->pdchEMotherKGmaxvalue = 0;
        this->pdchEMotherKGmaxvaluetime = 0;
        this->pdstEMotherM3values = 0;
        this->pdchEMotherM3annual = 0;
        this->pdchEMotherM3minvalue = 0;
        this->pdchEMotherM3minvaluetime = 0;
        this->pdchEMotherM3maxvalue = 0;
        this->pdchEMotherM3maxvaluetime = 0;
        this->pdstEMotherLvalues = 0;
        this->pdchEMotherLannual = 0;
        this->pdchEMotherLminvalue = 0;
        this->pdchEMotherLminvaluetime = 0;
        this->pdchEMotherLmaxvalue = 0;
        this->pdchEMotherLmaxvaluetime = 0;
        this->pdrSensibleGain = 0;
        this->pdstSHGSannual = 0;
        this->pdchSHGSAnZoneEqHt = 0;
        this->pdchSHGSAnZoneEqCl = 0;
        this->pdchSHGSAnHvacATUHt = 0;
        this->pdchSHGSAnHvacATUCl = 0;
        this->pdchSHGSAnSurfHt = 0;
        this->pdchSHGSAnSurfCl = 0;
        this->pdchSHGSAnPeoplAdd = 0;
        this->pdchSHGSAnLiteAdd = 0;
        this->pdchSHGSAnEquipAdd = 0;
        this->pdchSHGSAnWindAdd = 0;
        this->pdchSHGSAnIzaAdd = 0;
        this->pdchSHGSAnInfilAdd = 0;
        this->pdchSHGSAnOtherAdd = 0;
        this->pdchSHGSAnEquipRem = 0;
        this->pdchSHGSAnWindRem = 0;
        this->pdchSHGSAnIzaRem = 0;
        this->pdchSHGSAnInfilRem = 0;
        this->pdchSHGSAnOtherRem = 0;
        this->pdstSHGSpkCl = 0;
        this->pdchSHGSClTimePeak = 0;
        this->pdchSHGSClHvacHt = 0;
        this->pdchSHGSClHvacCl = 0;
        this->pdchSHGSClHvacATUHt = 0;
        this->pdchSHGSClHvacATUCl = 0;
        this->pdchSHGSClSurfHt = 0;
        this->pdchSHGSClSurfCl = 0;
        this->pdchSHGSClPeoplAdd = 0;
        this->pdchSHGSClLiteAdd = 0;
        this->pdchSHGSClEquipAdd = 0;
        this->pdchSHGSClWindAdd = 0;
        this->pdchSHGSClIzaAdd = 0;
        this->pdchSHGSClInfilAdd = 0;
        this->pdchSHGSClOtherAdd = 0;
        this->pdchSHGSClEquipRem = 0;
        this->pdchSHGSClWindRem = 0;
        this->pdchSHGSClIzaRem = 0;
        this->pdchSHGSClInfilRem = 0;
        this->pdchSHGSClOtherRem = 0;
        this->pdstSHGSpkHt = 0;
        this->pdchSHGSHtTimePeak = 0;
        this->pdchSHGSHtHvacHt = 0;
        this->pdchSHGSHtHvacCl = 0;
        this->pdchSHGSHtHvacATUHt = 0;
        this->pdchSHGSHtHvacATUCl = 0;
        this->pdchSHGSHtSurfHt = 0;
        this->pdchSHGSHtSurfCl = 0;
        this->pdchSHGSHtPeoplAdd = 0;
        this->pdchSHGSHtLiteAdd = 0;
        this->pdchSHGSHtEquipAdd = 0;
        this->pdchSHGSHtWindAdd = 0;
        this->pdchSHGSHtIzaAdd = 0;
        this->pdchSHGSHtInfilAdd = 0;
        this->pdchSHGSHtOtherAdd = 0;
        this->pdchSHGSHtEquipRem = 0;
        this->pdchSHGSHtWindRem = 0;
        this->pdchSHGSHtIzaRem = 0;
        this->pdchSHGSHtInfilRem = 0;
        this->pdchSHGSHtOtherRem = 0;
        this->pdrStd62 = 0;
        this->pdstS62sysVentReqCool = 0;
        this->pdchS62svrClSumVpz = 0;
        this->pdchS62svrClPs = 0;
        this->pdchS62svrClSumPz = 0;
        this->pdchS62svrClD = 0;
        this->pdchS62svrClVou = 0;
        this->pdchS62svrClVps = 0;
        this->pdchS62svrClXs = 0;
        this->pdchS62svrClEv = 0;
        this->pdchS62svrClVot = 0;
        this->pdchS62svrClPercOA = 0;
        this->pdchS62svrClEnvironmentOfPs = 0;
        this->pdchS62svrClTimeOfPs = 0;
        this->pdstS62sysVentReqHeat = 0;
        this->pdchS62svrHtSumVpz = 0;
        this->pdchS62svrHtPs = 0;
        this->pdchS62svrHtSumPz = 0;
        this->pdchS62svrHtD = 0;
        this->pdchS62svrHtVou = 0;
        this->pdchS62svrHtVps = 0;
        this->pdchS62svrHtXs = 0;
        this->pdchS62svrHtEv = 0;
        this->pdchS62svrHtVot = 0;
        this->pdchS62svrHtPercOA = 0;
        this->pdchS62svrHtEnvironmentOfPs = 0;
        this->pdchS62svrHtTimeOfPs = 0;
        this->pdstS62znVentPar = 0;
        this->pdchS62zvpAlN = 0;
        this->pdchS62zvpRp = 0;
        this->pdchS62zvpPz = 0;
        this->pdchS62zvpRa = 0;
        this->pdchS62zvpAz = 0;
        this->pdchS62zvpVbz = 0;
        this->pdchS62zvpClEz = 0;
        this->pdchS62zvpClVoz = 0;
        this->pdchS62zvpHtEz = 0;
        this->pdchS62zvpHtVoz = 0;
        this->pdstS62sysVentPar = 0;
        this->pdchS62svpRp = 0;
        this->pdchS62svpPz = 0;
        this->pdchS62svpRa = 0;
        this->pdchS62svpAz = 0;
        this->pdchS62svpVbz = 0;
        this->pdchS62svpClVoz = 0;
        this->pdchS62svpHtVoz = 0;
        this->pdstS62znCoolDes = 0;
        this->pdchS62zcdAlN = 0;
        this->pdchS62zcdBox = 0;
        this->pdchS62zcdVpz = 0;
        this->pdchS62zcdVps = 0;
        this->pdchS62zcdVsec = 0;
        this->pdchS62zcdVdz = 0;
        this->pdchS62zcdVpzmin = 0;
        this->pdchS62zcdVozclg = 0;
        this->pdchS62zcdZpz = 0;
        this->pdchS62zcdEp = 0;
        this->pdchS62zcdEr = 0;
        this->pdchS62zcdFa = 0;
        this->pdchS62zcdFb = 0;
        this->pdchS62zcdFc = 0;
        this->pdchS62zcdEvz = 0;
        this->pdstS62sysCoolDes = 0;
        this->pdchS62scdVpz = 0;
        this->pdchS62scdVps = 0;
        this->pdchS62scdVsec = 0;
        this->pdchS62scdVdz = 0;
        this->pdchS62scdVpzmin = 0;
        this->pdchS62scdVozclg = 0;
        this->pdchS62scdEvz = 0;
        this->pdstS62znHeatDes = 0;
        this->pdchS62zhdAlN = 0;
        this->pdchS62zhdBox = 0;
        this->pdchS62zhdVpz = 0;
        this->pdchS62zhdVps = 0;
        this->pdchS62zhdVsec = 0;
        this->pdchS62zhdVdz = 0;
        this->pdchS62zhdVpzmin = 0;
        this->pdchS62zhdVozhtg = 0;
        this->pdchS62zhdZpz = 0;
        this->pdchS62zhdEp = 0;
        this->pdchS62zhdEr = 0;
        this->pdchS62zhdFa = 0;
        this->pdchS62zhdFb = 0;
        this->pdchS62zhdFc = 0;
        this->pdchS62zhdEvz = 0;
        this->pdstS62sysHeatDes = 0;
        this->pdchS62shdVpz = 0;
        this->pdchS62shdVps = 0;
        this->pdchS62shdVsec = 0;
        this->pdchS62shdVdz = 0;
        this->pdchS62shdVpzmin = 0;
        this->pdchS62shdVozhtg = 0;
        this->pdchS62shdEvz = 0;
        this->pdrLeed = 0;
        this->pdstLeedGenInfo = 0;
        this->pdchLeedGenData = 0;
        this->pdstLeedSpaceUsageType = 0;
        this->pdchLeedSutName = 0;
        this->pdchLeedSutSpArea = 0;
        this->pdchLeedSutOcArea = 0;
        this->pdchLeedSutUnArea = 0;
        this->pdchLeedSutHrsWeek = 0;
        this->pdstLeedAdvsMsg = 0;
        this->pdchLeedAmData = 0;
        this->pdstLeedEneTypSum = 0;
        this->pdchLeedEtsType = 0;
        this->pdchLeedEtsRtNm = 0;
        this->pdchLeedEtsVirt = 0;
        this->pdchLeedEtsEneUnt = 0;
        this->pdchLeedEtsDemUnt = 0;
        this->pdstLeedPerf = 0;
        this->pdchLeedPerfRot = 0;
        this->pdchLeedPerfElEneUse = 0;
        this->pdchLeedPerfElDem = 0;
        this->pdchLeedPerfGasEneUse = 0;
        this->pdchLeedPerfGasDem = 0;
        this->pdchLeedPerfGasolineEneUse = 0;
        this->pdchLeedPerfGasolineDem = 0;
        this->pdchLeedPerfDieselEneUse = 0;
        this->pdchLeedPerfDieselDem = 0;
        this->pdchLeedPerfCoalEneUse = 0;
        this->pdchLeedPerfCoalDem = 0;
        this->pdchLeedPerfFuelOil1EneUse = 0;
        this->pdchLeedPerfFuelOil1Dem = 0;
        this->pdchLeedPerfFuelOil2EneUse = 0;
        this->pdchLeedPerfFuelOil2Dem = 0;
        this->pdchLeedPerfPropaneEneUse = 0;
        this->pdchLeedPerfPropaneDem = 0;
        this->pdchLeedPerfOtherFuel1EneUse = 0;
        this->pdchLeedPerfOtherFuel1Dem = 0;
        this->pdchLeedPerfOtherFuel2EneUse = 0;
        this->pdchLeedPerfOtherFuel2Dem = 0;
        this->pdchLeedPerfDisClEneUse = 0;
        this->pdchLeedPerfDisClDem = 0;
        this->pdchLeedPerfDisHtEneUse = 0;
        this->pdchLeedPerfDisHtDem = 0;
        this->pdstLeedEneUseSum = 0;
        this->pdchLeedEusUnt = 0;
        this->pdchLeedEusProc = 0;
        this->pdchLeedEusTotal = 0;
        this->pdstLeedEneCostSum = 0;
        this->pdchLeedEcUnt = 0;
        this->pdchLeedEcsProc = 0;
        this->pdchLeedEcsTotal = 0;
        this->LEEDelecCostTotal = 0;
        this->LEEDgasCostTotal = 0;
        this->LEEDothrCostTotal = 0;
        this->pdstLeedRenewSum = 0;
        this->pdchLeedRenRatCap = 0;
        this->pdchLeedRenAnGen = 0;
        this->pdstLeedEneUseIntEl = 0;
        this->pdchLeedEuiElec = 0;
        this->pdstLeedEneUseIntNatG = 0;
        this->pdchLeedEuiNatG = 0;
        this->pdstLeedEneUseIntOthr = 0;
        this->pdchLeedEuiOthr = 0;
        this->pdstLeedEneUsePerc = 0;
        this->pdchLeedEupPerc = 0;
        this->pdstLeedEqFlLdHrs = 0;
        this->pdchLeedEflhEflh = 0;
        this->pdchLeedEflhNonZerHrs = 0;
        this->pdstLeedSchedSetPts = 0;
        this->pdChLeedSchStPtFirstObjUsed = 0;
        this->pdChLeedSchStPtMonthUsed = 0;
        this->pdchLeedSchStPt11amWednesday = 0;
        this->pdchLeedSchStPt11amWedCnt = 0;
        this->pdchLeedSchStPt11pmWednesday = 0;
        this->pdchLeedSchStPt11pmWedCnt = 0;
        this->pdrThermalResilience = 0;
        this->pdstHIHours = 0;
        this->pdchHIHourSafe = 0;
        this->pdchHIHourCaution = 0;
        this->pdchHIHourExtremeCaution = 0;
        this->pdchHIHourDanger = 0;
        this->pdchHIHourExtremeDanger = 0;
        this->pdstHIOccuHours = 0;
        this->pdchHIOccuHourSafe = 0;
        this->pdchHIOccuHourCaution = 0;
        this->pdchHIOccuHourExtremeCaution = 0;
        this->pdchHIOccuHourDanger = 0;
        this->pdchHIOccuHourExtremeDanger = 0;
        this->pdstHumidexHours = 0;
        this->pdchHumidexHourLittle = 0;
        this->pdchHumidexHourSome = 0;
        this->pdchHumidexHourGreat = 0;
        this->pdchHumidexHourDanger = 0;
        this->pdchHumidexHourStroke = 0;
        this->pdstHumidexOccuHours = 0;
        this->pdchHumidexOccuHourLittle = 0;
        this->pdchHumidexOccuHourSome = 0;
        this->pdchHumidexOccuHourGreat = 0;
        this->pdchHumidexOccuHourDanger = 0;
        this->pdchHumidexOccuHourStroke = 0;
        this->pdstHeatingSETHours = 0;
        this->pdchHeatingSETHours = 0;
        this->pdchHeatingSETOccuHours = 0;
        this->pdchHeatingSETUnmetDuration = 0;
        this->pdchHeatingSETUnmetTime = 0;
        this->pdstCoolingSETHours = 0;
        this->pdchCoolingSETHours = 0;
        this->pdchCoolingSETOccuHours = 0;
        this->pdchCoolingSETUnmetDuration = 0;
        this->pdchCoolingSETUnmetTime = 0;
        this->pdrCO2Resilience = 0;
        this->pdstCO2Hours = 0;
        this->pdchCO2HourSafe = 0;
        this->pdchCO2HourCaution = 0;
        this->pdchCO2HourHazard = 0;
        this->pdstCO2OccuHours = 0;
        this->pdchCO2OccuHourSafe = 0;
        this->pdchCO2OccuHourCaution = 0;
        this->pdchCO2OccuHourHazard = 0;
        this->pdrVisualResilience = 0;
        this->pdstIllumHours = 0;
        this->pdchIllumHourDark = 0;
        this->pdchIllumHourDim = 0;
        this->pdchIllumHourAdequate = 0;
        this->pdchIllumHourBright = 0;
        this->pdstIllumOccuHours = 0;
        this->pdchIllumOccuHourDark = 0;
        this->pdchIllumOccuHourDim = 0;
        this->pdchIllumOccuHourAdequate = 0;
        this->pdchIllumOccuHourBright = 0;
        this->sizeReportName = 0;
        this->numReportName = 0;
        this->sizeSubTable = 0;
        this->numSubTable = 0;
        this->sizeColumnTag = 0;
        this->numColumnTag = 0;
        this->sizeTableEntry = 0;
        this->numTableEntry = 0;
        this->sizeCompSizeTableEntry = 0;
        this->numCompSizeTableEntry = 0;
        this->sizeShadowRelate = 0;
        this->numShadowRelate = 0;
        this->TotalNotMetHeatingOccupiedForABUPS = 0.0;
        this->TotalNotMetCoolingOccupiedForABUPS = 0.0;
        this->TotalNotMetOccupiedForABUPS = 0.0;
        this->TotalTimeNotSimpleASH55EitherForABUPS = 0.0;
        this->TotalVozMax = 0.0;
        this->TotalAnyZoneBelowVozDynForOA = 0.0;
        this->TotalAllZonesAtVozDynForOA = 0.0;
        this->TotalAnyZoneAboveVozDynForOA = 0.0;
        this->TotalAnyZoneBelowVozDynOccForOA = 0.0;
        this->TotalAllZonesAtVozDynOccForOA = 0.0;
        this->TotalAnyZoneAboveVozDynOccForOA = 0.0;
        this->TotalAnyZoneVentUnoccForOA = 0.0;
        this->TotalAnyAirLoopBelowVozDynForOA = 0.0;
        this->TotalAllAirLoopsAtVozDynForOA = 0.0;
        this->TotalAnyAirLoopAboveVozDynForOA = 0.0;
        this->TotalAnyAirLoopBelowVozDynOccForOA = 0.0;
        this->TotalAllAirLoopsAtVozDynOccForOA = 0.0;
        this->TotalAnyAirLoopAboveVozDynOccForOA = 0.0;
        this->TotalAnyAirLoopVentUnoccForOA = 0.0;
        this->reportName.deallocate();
        this->subTable.deallocate();
        this->columnTag.deallocate();
        this->tableEntry.deallocate();
        this->CompSizeTableEntry.deallocate();
        this->ShadowRelate.deallocate();
    }
};

} // namespace EnergyPlus

#endif
