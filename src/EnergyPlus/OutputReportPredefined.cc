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

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
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
    // Data
    // The following section initializes the predefined column heading variables
    // The variables get their value in AssignPredefined

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

    void SetPredefinedTables(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   August 2006
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   Creates the structure of the predefined reports
        //   including the name and abbreviation of the report
        //   the subtables involved and the column headings.
        //   The variables defined for the columns are then
        //   used throughout the program to assign values
        //   to the subtables.

        // METHODOLOGY EMPLOYED:
        //   Simple assignments to public variables.

        // Climate Summary Report

        auto &s(state.dataOutRptPredefined);

        s->pdrClim = newPreDefReport("ClimaticDataSummary", "Clim", "Climatic Data Summary");

        s->pdstDesDay = newPreDefSubTable(s->pdrClim, "SizingPeriod:DesignDay");

        s->pdchDDmaxDB = newPreDefColumn(s->pdstDesDay, "Maximum Dry Bulb [C]");
        s->pdchDDrange = newPreDefColumn(s->pdstDesDay, "Daily Temperature Range [deltaC]");
        s->pdchDDhumid = newPreDefColumn(s->pdstDesDay, "Humidity Value");
        s->pdchDDhumTyp = newPreDefColumn(s->pdstDesDay, "Humidity Type");
        s->pdchDDwindSp = newPreDefColumn(s->pdstDesDay, "Wind Speed [m/s]");
        s->pdchDDwindDr = newPreDefColumn(s->pdstDesDay, "Wind Direction");

        s->pdstWthr = newPreDefSubTable(s->pdrClim, "Weather Statistics File");
        s->pdchWthrVal = newPreDefColumn(s->pdstWthr, "Value");

        // Envelope Report

        s->pdrEnvelope = newPreDefReport("EnvelopeSummary", "Env", "Envelope Summary");

        s->pdstOpaque = newPreDefSubTable(s->pdrEnvelope, "Opaque Exterior");

        s->pdchOpCons = newPreDefColumn(s->pdstOpaque, "Construction");
        s->pdchOpRefl = newPreDefColumn(s->pdstOpaque, "Reflectance");
        s->pdchOpUfactFilm = newPreDefColumn(s->pdstOpaque, "U-Factor with Film [W/m2-K]");
        s->pdchOpUfactNoFilm = newPreDefColumn(s->pdstOpaque, "U-Factor no Film [W/m2-K]");
        s->pdchOpGrArea = newPreDefColumn(s->pdstOpaque, "Gross Area [m2]");
        s->pdchOpNetArea = newPreDefColumn(s->pdstOpaque, "Net Area [m2]");
        s->pdchOpAzimuth = newPreDefColumn(s->pdstOpaque, "Azimuth [deg]");
        s->pdchOpTilt = newPreDefColumn(s->pdstOpaque, "Tilt [deg]");
        s->pdchOpDir = newPreDefColumn(s->pdstOpaque, "Cardinal Direction");

        s->pdstIntOpaque = newPreDefSubTable(s->pdrEnvelope, "Opaque Interior");

        s->pdchIntOpCons = newPreDefColumn(s->pdstIntOpaque, "Construction");
        s->pdchIntOpRefl = newPreDefColumn(s->pdstIntOpaque, "Reflectance");
        s->pdchIntOpUfactFilm = newPreDefColumn(s->pdstIntOpaque, "U-Factor with Film [W/m2-K]");
        s->pdchIntOpUfactNoFilm = newPreDefColumn(s->pdstIntOpaque, "U-Factor no Film [W/m2-K]");
        s->pdchIntOpGrArea = newPreDefColumn(s->pdstIntOpaque, "Gross Area [m2]");
        s->pdchIntOpNetArea = newPreDefColumn(s->pdstIntOpaque, "Net Area [m2]");
        s->pdchIntOpAzimuth = newPreDefColumn(s->pdstIntOpaque, "Azimuth [deg]");
        s->pdchIntOpTilt = newPreDefColumn(s->pdstIntOpaque, "Tilt [deg]");
        s->pdchIntOpDir = newPreDefColumn(s->pdstIntOpaque, "Cardinal Direction");

        s->pdstFen = newPreDefSubTable(s->pdrEnvelope, "Exterior Fenestration");

        s->pdchFenCons = newPreDefColumn(s->pdstFen, "Construction");
        s->pdchFenGlassAreaOf1 = newPreDefColumn(s->pdstFen, "Glass Area [m2]");
        s->pdchFenFrameAreaOf1 = newPreDefColumn(s->pdstFen, "Frame Area [m2]");
        s->pdchFenDividerAreaOf1 = newPreDefColumn(s->pdstFen, "Divider Area [m2]");
        s->pdchFenAreaOf1 = newPreDefColumn(s->pdstFen, "Area of One Opening [m2]");
        s->pdchFenArea = newPreDefColumn(s->pdstFen, "Area of Multiplied Openings [m2]");
        s->pdchFenUfact = newPreDefColumn(s->pdstFen, "Glass U-Factor [W/m2-K]");
        s->pdchFenSHGC = newPreDefColumn(s->pdstFen, "Glass SHGC");
        s->pdchFenVisTr = newPreDefColumn(s->pdstFen, "Glass Visible Transmittance");
        s->pdchFenFrameConductance = newPreDefColumn(s->pdstFen, "Frame Conductance [W/m2-K]");
        s->pdchFenDividerConductance = newPreDefColumn(s->pdstFen, "Divider Conductance [W/m2-K]");
        s->pdchFenSwitchable = newPreDefColumn(s->pdstFen, "Shade Control");
        s->pdchFenParent = newPreDefColumn(s->pdstFen, "Parent Surface");
        s->pdchFenAzimuth = newPreDefColumn(s->pdstFen, "Azimuth [deg]");
        s->pdchFenTilt = newPreDefColumn(s->pdstFen, "Tilt [deg]");
        s->pdchFenDir = newPreDefColumn(s->pdstFen, "Cardinal Direction");

        s->pdstIntFen = newPreDefSubTable(s->pdrEnvelope, "Interior Fenestration");

        s->pdchIntFenCons = newPreDefColumn(s->pdstIntFen, "Construction");
        s->pdchIntFenAreaOf1 = newPreDefColumn(s->pdstIntFen, "Area of One Opening [m2]");
        s->pdchIntFenArea = newPreDefColumn(s->pdstIntFen, "Area of Openings [m2]");
        s->pdchIntFenUfact = newPreDefColumn(s->pdstIntFen, "Glass U-Factor [W/m2-K]");
        s->pdchIntFenSHGC = newPreDefColumn(s->pdstIntFen, "Glass SHGC");
        s->pdchIntFenVisTr = newPreDefColumn(s->pdstIntFen, "Glass Visible Transmittance");
        // s->pdchIntFenGlassAreaOf1 =   newPreDefColumn(s->pdstIntFen,'Glass Area [m2]')
        // s->pdchIntFenFrameAreaOf1 =   newPreDefColumn(s->pdstIntFen,'Frame Area [m2]')
        // s->pdchIntFenDividerAreaOf1 =   newPreDefColumn(s->pdstIntFen,'Divider Area [m2]')
        // s->pdchIntFenFrameConductance =  newPreDefColumn(s->pdstIntFen,'Frame Conductance [W/m2-K]')
        // s->pdchIntFenDividerConductance =  newPreDefColumn(s->pdstIntFen,'Divider Conductance [W/m2-K]')
        s->pdchIntFenParent = newPreDefColumn(s->pdstIntFen, "Parent Surface");

        s->pdstDoor = newPreDefSubTable(s->pdrEnvelope, "Exterior Door");

        s->pdchDrCons = newPreDefColumn(s->pdstDoor, "Construction");
        s->pdchDrUfactFilm = newPreDefColumn(s->pdstDoor, "U-Factor with Film [W/m2-K]");
        s->pdchDrUfactNoFilm = newPreDefColumn(s->pdstDoor, "U-Factor no Film [W/m2-K]");
        s->pdchDrGrArea = newPreDefColumn(s->pdstDoor, "Gross Area [m2]");
        s->pdchDrParent = newPreDefColumn(s->pdstDoor, "Parent Surface");

        s->pdstIntDoor = newPreDefSubTable(s->pdrEnvelope, "Interior Door");

        s->pdchIntDrCons = newPreDefColumn(s->pdstIntDoor, "Construction");
        s->pdchIntDrUfactFilm = newPreDefColumn(s->pdstIntDoor, "U-Factor with Film [W/m2-K]");
        s->pdchIntDrUfactNoFilm = newPreDefColumn(s->pdstIntDoor, "U-Factor no Film [W/m2-K]");
        s->pdchIntDrGrArea = newPreDefColumn(s->pdstIntDoor, "Gross Area [m2]");
        s->pdchIntDrParent = newPreDefColumn(s->pdstIntDoor, "Parent Surface");

        // Shading Report
        s->pdrShading = newPreDefReport("ShadingSummary", "Shade", "Shading Summary");

        s->pdstSunlitFrac = newPreDefSubTable(s->pdrShading, "Sunlit Fraction");

        s->pdchSlfMar21_9 = newPreDefColumn(s->pdstSunlitFrac, "March 21 9am");
        s->pdchSlfMar21_12 = newPreDefColumn(s->pdstSunlitFrac, "March 21 noon");
        s->pdchSlfMar21_15 = newPreDefColumn(s->pdstSunlitFrac, "March 21 3pm");
        s->pdchSlfJun21_9 = newPreDefColumn(s->pdstSunlitFrac, "June 21 9am");
        s->pdchSlfJun21_12 = newPreDefColumn(s->pdstSunlitFrac, "June 21 noon");
        s->pdchSlfJun21_15 = newPreDefColumn(s->pdstSunlitFrac, "June 21 3pm");
        s->pdchSlfDec21_9 = newPreDefColumn(s->pdstSunlitFrac, "December 21 9am");
        s->pdchSlfDec21_12 = newPreDefColumn(s->pdstSunlitFrac, "December 21 noon");
        s->pdchSlfDec21_15 = newPreDefColumn(s->pdstSunlitFrac, "December 21 3pm");

        s->pdstWindowControl = newPreDefSubTable(s->pdrShading, "Window Control");

        s->pdchWscName = newPreDefColumn(s->pdstWindowControl, "Name");
        s->pdchWscShading = newPreDefColumn(s->pdstWindowControl, "Type");
        s->pdchWscShadCons = newPreDefColumn(s->pdstWindowControl, "Shaded Construction");
        s->pdchWscControl = newPreDefColumn(s->pdstWindowControl, "Control");
        s->pdchWscGlare = newPreDefColumn(s->pdstWindowControl, "Glare Control");

        // Lighting Report
        s->pdrLighting = newPreDefReport("LightingSummary", "Light", "Lighting Summary");

        s->pdstInLite = newPreDefSubTable(s->pdrLighting, "Interior Lighting");

        s->pdchInLtZone = newPreDefColumn(s->pdstInLite, "Zone");
        s->pdchInLtDens = newPreDefColumn(s->pdstInLite, "Lighting Power Density [W/m2]");
        s->pdchInLtArea = newPreDefColumn(s->pdstInLite, "Zone Area [m2]");
        s->pdchInLtPower = newPreDefColumn(s->pdstInLite, "Total Power [W]");
        s->pdchInLtEndUse = newPreDefColumn(s->pdstInLite, "End Use Subcategory");
        s->pdchInLtSchd = newPreDefColumn(s->pdstInLite, "Schedule Name");
        s->pdchInLtAvgHrSchd = newPreDefColumn(s->pdstInLite, "Scheduled Hours/Week [hr]");
        s->pdchInLtAvgHrOper = newPreDefColumn(s->pdstInLite, "Hours/Week > 1% [hr]");
        s->pdchInLtFullLoadHrs = newPreDefColumn(s->pdstInLite, "Full Load Hours/Week [hr]");
        s->pdchInLtRetAir = newPreDefColumn(s->pdstInLite, "Return Air Fraction");
        s->pdchInLtCond = newPreDefColumn(s->pdstInLite, "Conditioned (Y/N)");
        s->pdchInLtConsump = newPreDefColumn(s->pdstInLite, "Consumption [GJ]");

        s->pdstDaylight = newPreDefSubTable(s->pdrLighting, "Daylighting");

        s->pdchDyLtZone = newPreDefColumn(s->pdstDaylight, "Zone");
        s->pdchDyLtCtrlName = newPreDefColumn(s->pdstDaylight, "Control Name");
        s->pdchDyLtKind = newPreDefColumn(s->pdstDaylight, "Daylighting Method"); // detailed or DElight
        s->pdchDyLtCtrlType = newPreDefColumn(s->pdstDaylight, "Control Type");   // stepped or continuous
        s->pdchDyLtFrac = newPreDefColumn(s->pdstDaylight, "Fraction Controlled");
        s->pdchDyLtWInst = newPreDefColumn(s->pdstDaylight, "Lighting Installed in Zone [W]");
        s->pdchDyLtWCtrl = newPreDefColumn(s->pdstDaylight, "Lighting Controlled [W]");

        s->pdstExtLite = newPreDefSubTable(s->pdrLighting, "Exterior Lighting");

        s->pdchExLtPower = newPreDefColumn(s->pdstExtLite, "Total Watts");
        s->pdchExLtClock = newPreDefColumn(s->pdstExtLite, "Astronomical Clock/Schedule");
        s->pdchExLtSchd = newPreDefColumn(s->pdstExtLite, "Schedule Name");
        s->pdchExLtAvgHrSchd = newPreDefColumn(s->pdstExtLite, "Scheduled Hours/Week [hr]");
        s->pdchExLtAvgHrOper = newPreDefColumn(s->pdstExtLite, "Hours/Week > 1% [hr]");
        s->pdchExLtFullLoadHrs = newPreDefColumn(s->pdstExtLite, "Full Load Hours/Week [hr]");
        s->pdchExLtConsump = newPreDefColumn(s->pdstExtLite, "Consumption [GJ]");

        // HVAC Equipment Report

        s->pdrEquip = newPreDefReport("EquipmentSummary", "Equip", "Equipment Summary");

        s->pdstMech = newPreDefSubTable(s->pdrEquip, "Central Plant");

        s->pdchMechType = newPreDefColumn(s->pdstMech, "Type");
        s->pdchMechNomCap = newPreDefColumn(s->pdstMech, "Nominal Capacity [W]");
        s->pdchMechNomEff = newPreDefColumn(s->pdstMech, "Nominal Efficiency [W/W]");
        // Note: We don't want any of these to convert.
        // The Btu/W-h isn't going to convert anyways, and the W/W will convert to W/W since it has "SI" in the string as a hint
        s->pdchMechIPLVSI = newPreDefColumn(s->pdstMech, "IPLV in SI Units [W/W]");
        s->pdchMechIPLVIP = newPreDefColumn(s->pdstMech, "IPLV in IP Units [Btu/W-h]");

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

        s->pdstCoolCoil = newPreDefSubTable(s->pdrEquip, "Cooling Coils");

        s->pdchCoolCoilType = newPreDefColumn(s->pdstCoolCoil, "Type");
        s->pdchCoolCoilDesCap = newPreDefColumn(s->pdstCoolCoil, "Design Coil Load [W]");
        s->pdchCoolCoilTotCap = newPreDefColumn(s->pdstCoolCoil, "Nominal Total Capacity [W]");
        s->pdchCoolCoilSensCap = newPreDefColumn(s->pdstCoolCoil, "Nominal Sensible Capacity [W]");
        s->pdchCoolCoilLatCap = newPreDefColumn(s->pdstCoolCoil, "Nominal Latent Capacity [W]");
        s->pdchCoolCoilSHR = newPreDefColumn(s->pdstCoolCoil, "Nominal Sensible Heat Ratio");
        s->pdchCoolCoilNomEff = newPreDefColumn(s->pdstCoolCoil, "Nominal Efficiency [W/W]");
        s->pdchCoolCoilUATotal = newPreDefColumn(s->pdstCoolCoil, "Nominal Coil UA Value [W/C]");
        s->pdchCoolCoilArea = newPreDefColumn(s->pdstCoolCoil, "Nominal Coil Surface Area [m2]");

        s->pdstDXCoolCoil = newPreDefSubTable(s->pdrEquip, "DX Cooling Coils");
        s->pdchDXCoolCoilType = newPreDefColumn(s->pdstDXCoolCoil, "DX Cooling Coil Type");
        s->pdchDXCoolCoilNetCapSI = newPreDefColumn(s->pdstDXCoolCoil, "Standard Rated Net Cooling Capacity [W]");

        s->pdchDXCoolCoilCOP = newPreDefColumn(s->pdstDXCoolCoil, "Standard Rated Net COP [W/W]");
        s->pdchDXCoolCoilEERIP = newPreDefColumn(s->pdstDXCoolCoil, "EER [Btu/W-h]");
        s->pdchDXCoolCoilSEERUserIP = newPreDefColumn(s->pdstDXCoolCoil, "SEER User [Btu/W-h]");
        s->pdchDXCoolCoilSEERStandardIP = newPreDefColumn(s->pdstDXCoolCoil, "SEER Standard [Btu/W-h]");
        s->pdchDXCoolCoilIEERIP = newPreDefColumn(s->pdstDXCoolCoil, "IEER [Btu/W-h]");

        // for DX Cooling Coil ASHRAE 127-12 Report
        s->pdstDXCoolCoil2 = newPreDefSubTable(s->pdrEquip, "DX Cooling Coil ASHRAE 127 Standard Ratings Report");
        s->pdchDXCoolCoilType = newPreDefColumn(s->pdstDXCoolCoil2, "DX Cooling Coil Type");
        s->pdchDXCoolCoilNetCapSIA = newPreDefColumn(s->pdstDXCoolCoil2, "Rated Net Cooling Capacity Test A [W]");
        s->pdchDXCoolCoilElecPowerA = newPreDefColumn(s->pdstDXCoolCoil2, "Rated Electric Power Test A [W]");
        s->pdchDXCoolCoilNetCapSIB = newPreDefColumn(s->pdstDXCoolCoil2, "Rated Net Cooling Capacity Test B [W]");
        s->pdchDXCoolCoilElecPowerB = newPreDefColumn(s->pdstDXCoolCoil2, "Rated Electric Power Test B [W]");
        s->pdchDXCoolCoilNetCapSIC = newPreDefColumn(s->pdstDXCoolCoil2, "Rated Net Cooling Capacity Test C [W]");
        s->pdchDXCoolCoilElecPowerC = newPreDefColumn(s->pdstDXCoolCoil2, "Rated Electric Power Test C [W]");
        s->pdchDXCoolCoilNetCapSID = newPreDefColumn(s->pdstDXCoolCoil2, "Rated Net Cooling Capacity Test D [W]");
        s->pdchDXCoolCoilElecPowerD = newPreDefColumn(s->pdstDXCoolCoil2, "Rated Electric Power Test D [W]");

        s->pdstDXHeatCoil = newPreDefSubTable(s->pdrEquip, "DX Heating Coils");
        s->pdchDXHeatCoilType = newPreDefColumn(s->pdstDXHeatCoil, "DX Heating Coil Type");
        s->pdchDXHeatCoilHighCap = newPreDefColumn(s->pdstDXHeatCoil, "High Temperature Heating (net) Rating Capacity [W]");
        s->pdchDXHeatCoilLowCap = newPreDefColumn(s->pdstDXHeatCoil, "Low Temperature Heating (net) Rating Capacity [W]");
        s->pdchDXHeatCoilHSPFIP = newPreDefColumn(s->pdstDXHeatCoil, "HSPF [Btu/W-h]");
        s->pdchDXHeatCoilRegionNum = newPreDefColumn(s->pdstDXHeatCoil, "Region Number");

        s->pdstHeatCoil = newPreDefSubTable(s->pdrEquip, "Heating Coils");

        s->pdchHeatCoilType = newPreDefColumn(s->pdstHeatCoil, "Type");
        s->pdchHeatCoilDesCap = newPreDefColumn(s->pdstHeatCoil, "Design Coil Load [W]");
        s->pdchHeatCoilNomCap = newPreDefColumn(s->pdstHeatCoil, "Nominal Total Capacity [W]");
        s->pdchHeatCoilNomEff = newPreDefColumn(s->pdstHeatCoil, "Nominal Efficiency [W/W]");

        s->pdstFan = newPreDefSubTable(s->pdrEquip, "Fans");

        s->pdchFanType = newPreDefColumn(s->pdstFan, "Type");
        s->pdchFanTotEff = newPreDefColumn(s->pdstFan, "Total Efficiency [W/W]");
        s->pdchFanDeltaP = newPreDefColumn(s->pdstFan, "Delta Pressure [pa]");
        s->pdchFanVolFlow = newPreDefColumn(s->pdstFan, "Max Air Flow Rate [m3/s]");
        s->pdchFanPwr = newPreDefColumn(s->pdstFan, "Rated Electricity Rate [W]");
        s->pdchFanPwrPerFlow = newPreDefColumn(s->pdstFan, "Rated Power Per Max Air Flow Rate [W-s/m3]");
        s->pdchFanMotorIn = newPreDefColumn(s->pdstFan, "Motor Heat In Air Fraction");
        s->pdchFanEnergyIndex = newPreDefColumn(s->pdstFan, "Fan Energy Index");
        s->pdchFanEndUse = newPreDefColumn(s->pdstFan, "End Use Subcategory");
        s->pdchFanDesDay = newPreDefColumn(s->pdstFan, "Design Day Name for Fan Sizing Peak");
        s->pdchFanPkTime = newPreDefColumn(s->pdstFan, "Date/Time for Fan Sizing Peak");

        s->pdstPump = newPreDefSubTable(s->pdrEquip, "Pumps");
        s->pdchPumpType = newPreDefColumn(s->pdstPump, "Type");
        s->pdchPumpControl = newPreDefColumn(s->pdstPump, "Control");
        s->pdchPumpHead = newPreDefColumn(s->pdstPump, "Head [pa]");
        s->pdchPumpFlow = newPreDefColumn(s->pdstPump, "Water Flow [m3/s]");
        s->pdchPumpPower = newPreDefColumn(s->pdstPump, "Electricity Rate [W]");
        s->pdchPumpPwrPerFlow = newPreDefColumn(s->pdstPump, "Power Per Water Flow Rate [W-s/m3]");
        s->pdchMotEff = newPreDefColumn(s->pdstPump, "Motor Efficiency [W/W]");
        s->pdchPumpEndUse = newPreDefColumn(s->pdstPump, "End Use Subcategory");

        s->pdstSWH = newPreDefSubTable(s->pdrEquip, "Service Water Heating");
        s->pdchSWHType = newPreDefColumn(s->pdstSWH, "Type");
        s->pdchSWHVol = newPreDefColumn(s->pdstSWH, "Storage Volume [m3]");
        s->pdchSWHHeatIn = newPreDefColumn(s->pdstSWH, "Input [W]");
        s->pdchSWHThEff = newPreDefColumn(s->pdstSWH, "Thermal Efficiency [W/W]");
        s->pdchSWHRecEff = newPreDefColumn(s->pdstSWH, "Recovery Efficiency [W/W]");
        s->pdchSWHEnFac = newPreDefColumn(s->pdstSWH, "Energy Factor");

        // Sizing Report

        s->pdrSizing = newPreDefReport("HVACSizingSummary", "Size", "HVAC Sizing Summary");

        s->pdstZoneClSize = newPreDefSubTable(s->pdrSizing, "Zone Sensible Cooling");

        s->pdchZnClCalcDesLd = newPreDefColumn(s->pdstZoneClSize, "Calculated Design Load [W]");
        s->pdchZnClUserDesLd = newPreDefColumn(s->pdstZoneClSize, "User Design Load [W]");
        s->pdchZnClUserDesLdPerArea = newPreDefColumn(s->pdstZoneClSize, "User Design Load per Area [W/m2]");
        s->pdchZnClCalcDesAirFlow = newPreDefColumn(s->pdstZoneClSize, "Calculated Design Air Flow [m3/s]");
        s->pdchZnClUserDesAirFlow = newPreDefColumn(s->pdstZoneClSize, "User Design Air Flow [m3/s]");
        s->pdchZnClDesDay = newPreDefColumn(s->pdstZoneClSize, "Design Day Name");
        s->pdchZnClPkTime = newPreDefColumn(s->pdstZoneClSize, "Date/Time Of Peak {TIMESTAMP}");
        s->pdchZnClPkTstatTemp = newPreDefColumn(s->pdstZoneClSize, "Thermostat Setpoint Temperature at Peak Load [C]");
        s->pdchZnClPkIndTemp = newPreDefColumn(s->pdstZoneClSize, "Indoor Temperature at Peak Load [C]");
        s->pdchZnClPkIndHum = newPreDefColumn(s->pdstZoneClSize, "Indoor Humidity Ratio at Peak Load [kgWater/kgDryAir]");
        s->pdchZnClPkOATemp = newPreDefColumn(s->pdstZoneClSize, "Outdoor Temperature at Peak Load [C]");
        s->pdchZnClPkOAHum = newPreDefColumn(s->pdstZoneClSize, "Outdoor Humidity Ratio at Peak Load [kgWater/kgDryAir]");
        s->pdchZnClPkOAMinFlow = newPreDefColumn(s->pdstZoneClSize, "Minimum Outdoor Air Flow Rate [m3/s]");
        s->pdchZnClPkDOASHeatGain = newPreDefColumn(s->pdstZoneClSize, "Heat Gain Rate from DOAS [W]");
        addFootNoteSubTable(s->pdstZoneClSize,
                            "The Design Load is the zone sensible load only. It does not include any system effects or ventilation loads.");
        s->pdstZoneHtSize = newPreDefSubTable(s->pdrSizing, "Zone Sensible Heating");

        s->pdchZnHtCalcDesLd = newPreDefColumn(s->pdstZoneHtSize, "Calculated Design Load [W]");
        s->pdchZnHtUserDesLd = newPreDefColumn(s->pdstZoneHtSize, "User Design Load [W]");
        s->pdchZnHtUserDesLdPerArea = newPreDefColumn(s->pdstZoneHtSize, "User Design Load per Area [W/m2]");
        s->pdchZnHtCalcDesAirFlow = newPreDefColumn(s->pdstZoneHtSize, "Calculated Design Air Flow [m3/s]");
        s->pdchZnHtUserDesAirFlow = newPreDefColumn(s->pdstZoneHtSize, "User Design Air Flow [m3/s]");
        s->pdchZnHtDesDay = newPreDefColumn(s->pdstZoneHtSize, "Design Day Name");
        s->pdchZnHtPkTime = newPreDefColumn(s->pdstZoneHtSize, "Date/Time Of Peak {TIMESTAMP}");
        s->pdchZnHtPkTstatTemp = newPreDefColumn(s->pdstZoneHtSize, "Thermostat Setpoint Temperature at Peak Load [C]");
        s->pdchZnHtPkIndTemp = newPreDefColumn(s->pdstZoneHtSize, "Indoor Temperature at Peak Load [C]");
        s->pdchZnHtPkIndHum = newPreDefColumn(s->pdstZoneHtSize, "Indoor Humidity Ratio at Peak Load [kgWater/kgDryAir]");
        s->pdchZnHtPkOATemp = newPreDefColumn(s->pdstZoneHtSize, "Outdoor Temperature at Peak Load [C]");
        s->pdchZnHtPkOAHum = newPreDefColumn(s->pdstZoneHtSize, "Outdoor Humidity Ratio at Peak Load [kgWater/kgDryAir]");
        s->pdchZnHtPkOAMinFlow = newPreDefColumn(s->pdstZoneHtSize, "Minimum Outdoor Air Flow Rate [m3/s]");
        s->pdchZnHtPkDOASHeatGain = newPreDefColumn(s->pdstZoneHtSize, "Heat Gain Rate from DOAS [W]");
        addFootNoteSubTable(s->pdstZoneHtSize,
                            "The Design Load is the zone sensible load only. It does not include any system effects or ventilation loads.");
        s->pdstSystemSize = newPreDefSubTable(s->pdrSizing, "System Design Air Flow Rates");

        s->pdchSysSizCalcClAir = newPreDefColumn(s->pdstSystemSize, "Calculated cooling [m3/s]");
        s->pdchSysSizUserClAir = newPreDefColumn(s->pdstSystemSize, "User cooling [m3/s]");
        s->pdchSysSizCalcHtAir = newPreDefColumn(s->pdstSystemSize, "Calculated heating [m3/s]");
        s->pdchSysSizUserHtAir = newPreDefColumn(s->pdstSystemSize, "User heating [m3/s]");
        s->pdchSysSizAdjustedClAir = newPreDefColumn(s->pdstSystemSize, "Adjusted cooling [m3/s]");
        s->pdchSysSizAdjustedHtAir = newPreDefColumn(s->pdstSystemSize, "Adjusted heating [m3/s]");
        s->pdchSysSizAdjustedMainAir = newPreDefColumn(s->pdstSystemSize, "Adjusted main [m3/s]");
        s->pdchSysSizCalcHeatFlowRatio = newPreDefColumn(s->pdstSystemSize, "Calculated Heating Air Flow Ratio []");
        s->pdchSysSizUserHeatFlowRatio = newPreDefColumn(s->pdstSystemSize, "User Heating Air Flow Ratio []");

        s->pdstPlantSize = newPreDefSubTable(s->pdrSizing, "Plant Loop Coincident Design Fluid Flow Rate Adjustments");
        //		s->pdchPlantSizPass = newPreDefColumn( s->pdstPlantSize, "Sizing Pass" );
        s->pdchPlantSizPrevVdot = newPreDefColumn(s->pdstPlantSize, "Previous Design Volume Flow Rate [m3/s]");
        s->pdchPlantSizMeasVdot = newPreDefColumn(s->pdstPlantSize, "Algorithm Volume Flow Rate [m3/s]");
        s->pdchPlantSizCalcVdot = newPreDefColumn(s->pdstPlantSize, "Coincident Design Volume Flow Rate [m3/s]");
        s->pdchPlantSizCoincYesNo = newPreDefColumn(s->pdstPlantSize, "Coincident Size Adjusted");
        s->pdchPlantSizDesDay = newPreDefColumn(s->pdstPlantSize, "Peak Sizing Period Name");
        s->pdchPlantSizPkTimeDayOfSim = newPreDefColumn(s->pdstPlantSize, "Peak Day into Period {TIMESTAMP}[day]");
        s->pdchPlantSizPkTimeHour = newPreDefColumn(s->pdstPlantSize, "Peak Hour Of Day {TIMESTAMP}[hr]");
        s->pdchPlantSizPkTimeMin = newPreDefColumn(s->pdstPlantSize, "Peak Step Start Minute {TIMESTAMP}[min]");

        s->pdst2CoilSummaryCoilSelection = newPreDefSubTable(s->pdrSizing, "Coil Sizing Summary");
        // coil meta data information
        //	the first column will be the coil name, the unique user name from input. It has no header or column definition
        s->pdch2CoilType = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Type");
        s->pdch2CoilHVACType = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "HVAC Type");
        s->pdch2CoilHVACName = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "HVAC Name");

        // coil Final size summary, regardless of how determined (
        s->pdch2CoilFinalTotalCap = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Final Gross Total Capacity [W]");
        s->pdch2CoilFinalSensCap = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Final Gross Sensible Capacity [W]");
        s->pdch2CoilFinalAirVolFlowRate = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Final Reference Air Volume Flow Rate [m3/s]");
        s->pdch2CoilFinalPlantVolFlowRate = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Final Reference Plant Fluid Volume Flow Rate [m3/s]");
        s->pdch2CoilUA = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil U-value Times Area Value [W/K]");

        // results from regular zone and system sizing calcs, "At Ideal Loads Peak"
        s->pdch2CoilDDnameSensIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Design Day Name at Sensible Ideal Loads Peak");
        s->pdch2CoilDateTimeSensIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Date/Time at Sensible Ideal Loads Peak");
        s->pdch2CoilDDnameAirFlowIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Design Day Name at Air Flow Ideal Loads Peak");
        s->pdch2CoilDateTimeAirFlowIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Date/Time at Air Flow Ideal Loads Peak");

        s->pdch2CoilTotalCapIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Total Capacity at Ideal Loads Peak [W]");
        s->pdch2CoilSensCapIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Sensible Capacity at Ideal Loads Peak [W]");
        s->pdch2CoilAirVolumeFlowIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Air Volume Flow Rate at Ideal Loads Peak [m3/s]");
        s->pdch2CoilEntDryBulbIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Entering Air Drybulb at Ideal Loads Peak [C]");
        s->pdch2CoilEntWetBulbIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Entering Air Wetbulb at Ideal Loads Peak [C]");
        s->pdch2CoilEntHumRatIdealPeak =
            newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Entering Air Humidity Ratio at Ideal Loads Peak [kgWater/kgDryAir]");
        s->pdch2CoilLvgDryBulbIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Leaving Air Drybulb at Ideal Loads Peak [C]");
        s->pdch2CoilLvgWetBulbIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Leaving Air Wetbulb at Ideal Loads Peak [C]");
        s->pdch2CoilLvgHumRatIdealPeak =
            newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Leaving Air Humidity Ratio at Ideal Loads Peak [kgWater/kgDryAir]");
        s->pdch2OADryBulbIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Outdoor Air Drybulb at Ideal Loads Peak [C]");
        s->pdch2OAHumRatIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Outdoor Air Humidity Ratio at Ideal Loads Peak [kgWater/kgDryAir]");
        s->pdch2OAWetBulbatIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Outdoor Air Wetbulb at Ideal Loads Peak [C]");
        s->pdch2OAFlowPrcntIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Outdoor Air Flow Percentage at Ideal Loads Peak [%]");
        s->pdch2ZoneAirDryBulbIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Zone Air Drybulb at Ideal Loads Peak [C]");
        s->pdch2ZoneAirHumRatIdealPeak =
            newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Zone Air Humidity Ratio at Ideal Loads Peak [kgWater/kgDryAir]");
        s->pdch2ZoneAirRelHumIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Zone Air Relative Humidity at Ideal Loads Peak [%]");
        s->pdch2ZoneSensibleLoadIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Zone Sensible Heat Gain at Ideal Loads Peak [W]");
        s->pdch2ZoneLatentLoadIdealPeak = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Zone Latent Heat Gain at Ideal Loads Peak [W]");
        // results for coil at Rated Conditions
        s->pdch2CoilRatedTotalCap = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Total Capacity at Rating Conditions [W]");
        s->pdch2CoilRatedSensCap = newPreDefColumn(s->pdst2CoilSummaryCoilSelection, "Coil Sensible Capacity at Rating Conditions [W]");

        s->pdrCoilSizingDetailsTable = newPreDefReport("CoilSizingDetails", "Coil", "Coil Sizing Details");
        s->pdstCoilSummaryCoilSelection = newPreDefSubTable(s->pdrCoilSizingDetailsTable, "Coils");
        // coil meta data information
        //	the first column will be the coil name, the unique user name from input. It has no header or column definition
        s->pdchCoilType = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Type");
        s->pdchCoilLocation = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Location");
        s->pdchCoilHVACType = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "HVAC Type");
        s->pdchCoilHVACName = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "HVAC Name");
        s->pdchCoilZoneName = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Zone Name(s)");

        s->pdchSysSizingMethCoinc = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "System Sizing Method Concurrence");
        s->pdchSysSizingMethCap = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "System Sizing Method Capacity");
        s->pdchSysSizingMethAir = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "System Sizing Method Air Flow");

        s->pdchCoilIsCapAutosized = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Autosized Coil Capacity?");
        s->pdchCoilIsAirFlowAutosized = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Autosized Coil Airflow?");
        s->pdchCoilIsWaterFlowAutosized = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Autosized Coil Water Flow?");
        s->pdchCoilIsOATreated = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "OA Pretreated prior to coil inlet?");

        // coil Final size summary, regardless of how determined (
        // get rid of these, this will be the same as At Rating Conditions.
        s->pdchCoilFinalTotalCap = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Final Gross Total Capacity [W]");
        s->pdchCoilFinalSensCap = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Final Gross Sensible Capacity [W]");
        s->pdchCoilFinalAirVolFlowRate = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Final Reference Air Volume Flow Rate [m3/s]");
        s->pdchCoilFinalPlantVolFlowRate = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Final Reference Plant Fluid Volume Flow Rate [m3/s]");

        // Misc Design output
        s->pdchCoilUA = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil U-value Times Area Value [W/K]");
        s->pdchReheatCoilMultiplier = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Terminal Unit Reheat Coil Multiplier");
        s->pdchFlowCapRatioLowCapIncreaseRatio =
            newPreDefColumn(s->pdstCoilSummaryCoilSelection, "DX Coil Capacity Increase Ratio from Too Low Flow/Capacity Ratio");
        s->pdchFlowCapRatioHiCapDecreaseRatio =
            newPreDefColumn(s->pdstCoilSummaryCoilSelection, "DX Coil Capacity Decrease Ratio from Too High Flow/Capacity Ratio");

        s->pdchMoistAirSpecificHeat =
            newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Moist Air Heat Capacity [J/kg-K]");                    // standard? for ideal sizing calcs?
        s->pdchDryAirSpecificHeat = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Dry Air Heat Capacity [J/kg-K]"); // standard? for ideal sizing calcs?
        s->pdchStandRhoAir = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Standard Air Density Adjusted for Elevation [kg/m3]");

        // Fan info for coil
        s->pdchFanAssociatedWithCoilName = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Supply Fan Name for Coil");
        s->pdchFanAssociatedWithCoilType = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Supply Fan Type for Coil");
        s->pdchFanAssociatedVdotSize = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Supply Fan Maximum Air Volume Flow Rate [m3/s]");
        s->pdchFanAssociatedMdotSize = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Supply Fan Maximum Air Mass Flow Rate [kg/s]");

        // Plant info for coil
        s->pdchCoilPlantLoopName = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Plant Name for Coil");
        s->pdchPlantFluidSpecificHeat = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Plant Fluid Specific Heat Capacity [J/kg-K]"); // standard/inits ?
        s->pdchPlantFluidDensity =
            newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Plant Fluid Density [kg/m3]"); // standard/inits ? for ideal sizing calcs?
        s->pdchPlantMassFlowMaximum = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Plant Maximum Fluid Mass Flow Rate [kg/s]");
        s->pdchPlantRetTempDesign = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Plant Design Fluid Return Temperature [C]");
        s->pdchPlantSupTempDesign = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Plant Design Fluid Supply Temperature [C]");
        s->pdchPlantDeltaTempDesign = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Plant Design Fluid Temperature Difference [deltaC]");
        s->pdchPlantCapacity = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Plant Design Capacity [W]");
        s->pdchCoilCapPrcntPlantCapacity = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Capacity Percentage of Plant Design Capacity [%]");
        s->pdchCoilFlowPrcntPlantFlow = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Fluid Flow Rate Percentage of Plant Design Flow Rate [%]");

        // results from regular zone and system sizing calcs, "At Ideal Loads Peak"
        s->pdchCoilDDnameSensIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Design Day Name at Sensible Ideal Loads Peak");
        s->pdchCoilDateTimeSensIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Date/Time at Sensible Ideal Loads Peak");
        s->pdchCoilDDnameTotIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Design Day Name at Total Ideal Loads Peak");
        s->pdchCoilDateTimeTotIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Date/Time at Total Ideal Loads Peak");
        s->pdchCoilDDnameAirFlowIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Design Day Name at Air Flow Ideal Loads Peak");
        s->pdchCoilDateTimeAirFlowIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Date/Time at Air Flow Ideal Loads Peak");
        s->pdchCoilPeakLoadTypeToSizeOn = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Peak Load Type to Size On");

        s->pdchCoilTotalCapIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Total Capacity at Ideal Loads Peak [W]");
        s->pdchCoilSensCapIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Sensible Capacity at Ideal Loads Peak [W]");
        s->pdchCoilOffRatingCapacityModifierIdealPeak =
            newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Off-Rating Capacity Modifier at Ideal Loads Peak [ ]");
        s->pdchCoilAirMassFlowIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Air Mass Flow Rate at Ideal Loads Peak [kg/s]");
        s->pdchCoilAirVolumeFlowIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Air Volume Flow Rate at Ideal Loads Peak [m3/s]");
        s->pdchCoilEntDryBulbIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Entering Air Drybulb at Ideal Loads Peak [C]");
        s->pdchCoilEntWetBulbIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Entering Air Wetbulb at Ideal Loads Peak [C]");
        s->pdchCoilEntHumRatIdealPeak =
            newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Entering Air Humidity Ratio at Ideal Loads Peak [kgWater/kgDryAir]");
        s->pdchCoilEntEnthalpyIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Entering Air Enthalpy at Ideal Loads Peak [J/KG-K]");
        s->pdchCoilLvgDryBulbIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Leaving Air Drybulb at Ideal Loads Peak [C]");
        s->pdchCoilLvgWetBulbIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Leaving Air Wetbulb at Ideal Loads Peak [C]");
        s->pdchCoilLvgHumRatIdealPeak =
            newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Leaving Air Humidity Ratio at Ideal Loads Peak [kgWater/kgDryAir]");
        s->pdchCoilLvgEnthalpyIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Leaving Air Enthalpy at Ideal Loads Peak [J/KG-K]");
        s->pdchCoilWaterMassFlowIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Plant Fluid Mass Flow Rate at Ideal Loads Peak [kg/s]");
        s->pdchCoilEntWaterTempIdealPeak =
            newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Entering Plant Fluid Temperature at Ideal Loads Peak [C]");
        s->pdchCoilLvgWaterTempIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Leaving Plant Fluid Temperature at Ideal Loads Peak [C]");
        s->pdchCoilWaterDeltaTempIdealPeak =
            newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Plant Fluid Temperature Difference at Ideal Loads Peak [deltaC]");
        s->pdchFanHeatGainIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Supply Fan Air Heat Gain at Ideal Loads Peak [W]");
        s->pdchCoilNetTotalCapacityIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil and Fan Net Total Capacity at Ideal Loads Peak [W]");
        s->pdchOADryBulbIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Outdoor Air Drybulb at Ideal Loads Peak [C]");
        s->pdchOAHumRatIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Outdoor Air Humidity Ratio at Ideal Loads Peak [kgWater/kgDryAir]");
        s->pdchOAWetBulbatIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Outdoor Air Wetbulb at Ideal Loads Peak [C]");
        s->pdchOAVolFlowIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Outdoor Air Volume Flow Rate at Ideal Loads Peak [m3/s]");
        s->pdchOAFlowPrcntIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Outdoor Air Flow Percentage at Ideal Loads Peak [%]");
        s->pdchAirSysRADryBulbIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "System Return Air Drybulb at Ideal Loads Peak [C]");
        s->pdchAirSysRAHumRatIdealPeak =
            newPreDefColumn(s->pdstCoilSummaryCoilSelection, "System Return Air Humidity Ratio at Ideal Loads Peak [kgWater/kgDryAir]");
        s->pdchZoneAirDryBulbIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Zone Air Drybulb at Ideal Loads Peak [C]");
        s->pdchZoneAirHumRatIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Zone Air Humidity Ratio at Ideal Loads Peak [kgWater/kgDryAir]");
        s->pdchZoneAirRelHumIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Zone Air Relative Humidity at Ideal Loads Peak [%]");
        s->pdchZoneSensibleLoadIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Zone Sensible Heat Gain at Ideal Loads Peak [W]");
        s->pdchZoneLatentLoadIdealPeak = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Zone Latent Heat Gain at Ideal Loads Peak [W]");
        // results for coil at Rated Conditions
        s->pdchCoilRatedTotalCap = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Total Capacity at Rating Conditions [W]");
        s->pdchCoilRatedSensCap = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Sensible Capacity at Rating Conditions [W]");

        s->pdchCoilRatedAirMass = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Air Mass Flow Rate at Rating Conditions [kg/s]");
        s->pdchCoilRatedEntDryBulb = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Entering Air Drybulb at Rating Conditions [C]");
        s->pdchCoilRatedEntWetBulb = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Entering Air Wetbulb at Rating Conditions [C]");
        s->pdchCoilRatedEntHumRat =
            newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Entering Air Humidity Ratio at Rating Conditions [kgWater/kgDryAir]");
        s->pdchCoilRatedEntEnthalpy = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Entering Air Enthalpy at Rating Conditions [J/KG-K]");
        s->pdchCoilRatedLvgDryBulb = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Leaving Air Drybulb at Rating Conditions [C]");
        s->pdchCoilRatedLvgWetBulb = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Leaving Air Wetbulb at Rating Conditions [C]");
        s->pdchCoilRatedLvgHumRat =
            newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Leaving Air Humidity Ratio at Rating Conditions [kgWater/kgDryAir]");
        s->pdchCoilRatedLvgEnthalpy = newPreDefColumn(s->pdstCoilSummaryCoilSelection, "Coil Leaving Air Enthalpy at Rating Conditions [J/KG-K]");

        // System Summary Report

        s->pdrSystem = newPreDefReport("SystemSummary", "Sys", "System Summary");

        s->pdstEconomizer = newPreDefSubTable(s->pdrSystem, "Economizer");

        s->pdchEcoKind = newPreDefColumn(s->pdstEconomizer, "High Limit Shutoff Control");
        s->pdchEcoMinOA = newPreDefColumn(s->pdstEconomizer, "Minimum Outdoor Air [m3/s]");
        s->pdchEcoMaxOA = newPreDefColumn(s->pdstEconomizer, "Maximum Outdoor Air [m3/s]");
        s->pdchEcoRetTemp = newPreDefColumn(s->pdstEconomizer, "Return Air Temp Limit");
        s->pdchEcoRetEnth = newPreDefColumn(s->pdstEconomizer, "Return Air Enthalpy Limit");
        s->pdchEcoOATempLim = newPreDefColumn(s->pdstEconomizer, "Outdoor Air Temperature Limit [C]");
        s->pdchEcoOAEnthLim = newPreDefColumn(s->pdstEconomizer, "Outdoor Air Enthalpy Limit [C]");

        s->pdstDemCntlVent = newPreDefSubTable(s->pdrSystem, "Demand Controlled Ventilation using Controller:MechanicalVentilation");
        s->pdchDCVventMechName = newPreDefColumn(s->pdstDemCntlVent, "Controller:MechanicalVentilation Name");
        s->pdchDCVperPerson = newPreDefColumn(s->pdstDemCntlVent, "Outdoor Air Per Person [m3/s-person]");
        s->pdchDCVperArea = newPreDefColumn(s->pdstDemCntlVent, "Outdoor Air Per Area [m3/s-m2]");
        s->pdchDCVperZone = newPreDefColumn(s->pdstDemCntlVent, "Outdoor Air Per Zone [m3/s]");
        s->pdchDCVperACH = newPreDefColumn(s->pdstDemCntlVent, "Outdoor Air ACH [ach]");
        s->pdchDCVMethod = newPreDefColumn(s->pdstDemCntlVent, "Outdoor Air Method");
        s->pdchDCVOASchName = newPreDefColumn(s->pdstDemCntlVent, "Outdoor Air Schedule Name");

        // added for new DCV
        s->pdchDCVZoneADEffCooling = newPreDefColumn(s->pdstDemCntlVent, "Air Distribution Effectiveness in Cooling Mode");
        s->pdchDCVZoneADEffHeating = newPreDefColumn(s->pdstDemCntlVent, "Air Distribution Effectiveness in Heating Mode");
        s->pdchDCVZoneADEffSchName = newPreDefColumn(s->pdstDemCntlVent, "Air Distribution Effectiveness Schedule Name");

        s->pdstSimpleComfort = newPreDefSubTable(s->pdrSystem, "Time Not Comfortable Based on Simple ASHRAE 55-2004");
        s->pdchSCwinterClothes = newPreDefColumn(s->pdstSimpleComfort, "Winter Clothes [hr]");
        s->pdchSCsummerClothes = newPreDefColumn(s->pdstSimpleComfort, "Summer Clothes [hr]");
        s->pdchSCeitherClothes = newPreDefColumn(s->pdstSimpleComfort, "Summer or Winter Clothes [hr]");

        s->pdstUnmetLoads = newPreDefSubTable(s->pdrSystem, "Time Setpoint Not Met");
        s->pdchULnotMetHeat = newPreDefColumn(s->pdstUnmetLoads, "During Heating [hr]");
        s->pdchULnotMetCool = newPreDefColumn(s->pdstUnmetLoads, "During Cooling [hr]");
        s->pdchULnotMetHeatOcc = newPreDefColumn(s->pdstUnmetLoads, "During Occupied Heating [hr]");
        s->pdchULnotMetCoolOcc = newPreDefColumn(s->pdstUnmetLoads, "During Occupied Cooling [hr]");

        // Outside Air Report
        s->pdrOutsideAir = newPreDefReport("OutdoorAirSummary", "OA", "Outdoor Air Summary");

        s->pdstOAavgOcc = newPreDefSubTable(s->pdrOutsideAir, "Average Outdoor Air During Occupied Hours");

        s->pdchOaoAvgNumOcc1 = newPreDefColumn(s->pdstOAavgOcc, "Average Number of Occupants");
        s->pdchOaoNomNumOcc1 = newPreDefColumn(s->pdstOAavgOcc, "Nominal Number of Occupants");
        s->pdchOaoZoneVol1 = newPreDefColumn(s->pdstOAavgOcc, "Zone Volume [m3]");
        s->pdchOaoAvgMechVent = newPreDefColumn(s->pdstOAavgOcc, "Mechanical Ventilation [ach]");
        s->pdchOaoAvgInfil = newPreDefColumn(s->pdstOAavgOcc, "Infiltration [ach]");
        s->pdchOaoAvgAFNInfil = newPreDefColumn(s->pdstOAavgOcc, "AFN Infiltration [ach]");
        s->pdchOaoAvgSimpVent = newPreDefColumn(s->pdstOAavgOcc, "Simple Ventilation [ach]");
        // s->pdchOaoAvgTotVent =   newPreDefColumn(s->pdstOAavgOcc,'Total Ventilation [ach]')

        addFootNoteSubTable(s->pdstOAavgOcc, "Values shown for a single zone without multipliers");

        s->pdstOAminOcc = newPreDefSubTable(s->pdrOutsideAir, "Minimum Outdoor Air During Occupied Hours");

        s->pdchOaoAvgNumOcc2 = newPreDefColumn(s->pdstOAminOcc, "Average Number of Occupants");
        s->pdchOaoNomNumOcc2 = newPreDefColumn(s->pdstOAminOcc, "Nominal Number of Occupants");
        s->pdchOaoZoneVol2 = newPreDefColumn(s->pdstOAminOcc, "Zone Volume [m3]");
        s->pdchOaoMinMechVent = newPreDefColumn(s->pdstOAminOcc, "Mechanical Ventilation [ach]");
        s->pdchOaoMinInfil = newPreDefColumn(s->pdstOAminOcc, "Infiltration [ach]");
        s->pdchOaoMinAFNInfil = newPreDefColumn(s->pdstOAminOcc, "AFN Infiltration [ach]");
        s->pdchOaoMinSimpVent = newPreDefColumn(s->pdstOAminOcc, "Simple Ventilation [ach]");
        // s->pdchOaoMinTotVent =   newPreDefColumn(s->pdstOAminOcc,'Total Ventilation [ach]')
        addFootNoteSubTable(s->pdstOAminOcc, "Values shown for a single zone without multipliers");

        // Object Count Report
        s->pdrObjCnt = newPreDefReport("ObjectCountSummary", "Count", "Object Count Summary");

        s->pdstSurfCnt = newPreDefSubTable(s->pdrObjCnt, "Surfaces by Class");
        s->pdchSurfCntTot = newPreDefColumn(s->pdstSurfCnt, "Total");
        s->pdchSurfCntExt = newPreDefColumn(s->pdstSurfCnt, "Outdoors");

        s->pdstHVACcnt = newPreDefSubTable(s->pdrObjCnt, "HVAC");
        s->pdchHVACcntVal = newPreDefColumn(s->pdstHVACcnt, "Count");

        s->pdstFieldCnt = newPreDefSubTable(s->pdrObjCnt, "Input Fields");
        s->pdchFieldCntVal = newPreDefColumn(s->pdstFieldCnt, "Count");

        // Energy Meters report
        s->pdrEnergyMeters = newPreDefReport("EnergyMeters", "Meters", "Energy Meters");

        // s->pdstEMvalues = newPreDefSubTable(s->pdrEnergyMeters,'Annual and Peak Values')
        // s->pdchEMannual = newPreDefColumn(s->pdstEMvalues,'Annual Value [GJ]')
        // s->pdchEMminvalue = newPreDefColumn(s->pdstEMvalues,'Minimum Value [J]')
        // s->pdchEMminvaluetime = newPreDefColumn(s->pdstEMvalues,'Timestamp of Minimum')
        // s->pdchEMmaxvalue = newPreDefColumn(s->pdstEMvalues,'Maximum Value [J]')
        // s->pdchEMmaxvaluetime = newPreDefColumn(s->pdstEMvalues,'Timestamp of Maximum')
        // Electricity Sub Table
        s->pdstEMelecvalues = newPreDefSubTable(s->pdrEnergyMeters, "Annual and Peak Values - Electricity");
        s->pdchEMelecannual = newPreDefColumn(s->pdstEMelecvalues, "Electricity Annual Value [GJ]");
        s->pdchEMelecminvalue = newPreDefColumn(s->pdstEMelecvalues, "Electricity Minimum Value [W]");
        s->pdchEMelecminvaluetime = newPreDefColumn(s->pdstEMelecvalues, "Timestamp of Minimum {TIMESTAMP}");
        s->pdchEMelecmaxvalue = newPreDefColumn(s->pdstEMelecvalues, "Electricity Maximum Value [W]");
        s->pdchEMelecmaxvaluetime = newPreDefColumn(s->pdstEMelecvalues, "Timestamp of Maximum {TIMESTAMP}");

        // Gas Sub Table
        s->pdstEMgasvalues = newPreDefSubTable(s->pdrEnergyMeters, "Annual and Peak Values - Natural Gas");
        s->pdchEMgasannual = newPreDefColumn(s->pdstEMgasvalues, "Natural Gas Annual Value [GJ]");
        s->pdchEMgasminvalue = newPreDefColumn(s->pdstEMgasvalues, "Natural Gas Minimum Value [W]");
        s->pdchEMgasminvaluetime = newPreDefColumn(s->pdstEMgasvalues, "Timestamp of Minimum {TIMESTAMP}");
        s->pdchEMgasmaxvalue = newPreDefColumn(s->pdstEMgasvalues, "Natural Gas Maximum Value [W]");
        s->pdchEMgasmaxvaluetime = newPreDefColumn(s->pdstEMgasvalues, "Timestamp of Maximum {TIMESTAMP}");

        // Cool SubTable
        s->pdstEMcoolvalues = newPreDefSubTable(s->pdrEnergyMeters, "Annual and Peak Values - Cooling");
        s->pdchEMcoolannual = newPreDefColumn(s->pdstEMcoolvalues, "Cooling Annual Value [GJ]");
        s->pdchEMcoolminvalue = newPreDefColumn(s->pdstEMcoolvalues, "Cooling Minimum Value [W]");
        s->pdchEMcoolminvaluetime = newPreDefColumn(s->pdstEMcoolvalues, "Timestamp of Minimum {TIMESTAMP}");
        s->pdchEMcoolmaxvalue = newPreDefColumn(s->pdstEMcoolvalues, "Cooling Maximum Value [W]");
        s->pdchEMcoolmaxvaluetime = newPreDefColumn(s->pdstEMcoolvalues, "Timestamp of Maximum {TIMESTAMP}");

        // Water SubTable
        s->pdstEMwatervalues = newPreDefSubTable(s->pdrEnergyMeters, "Annual and Peak Values - Water");
        s->pdchEMwaterannual = newPreDefColumn(s->pdstEMwatervalues, "Annual Value [m3]");
        s->pdchEMwaterminvalue = newPreDefColumn(s->pdstEMwatervalues, "Minimum Value [m3/s]");
        s->pdchEMwaterminvaluetime = newPreDefColumn(s->pdstEMwatervalues, "Timestamp of Minimum {TIMESTAMP}");
        s->pdchEMwatermaxvalue = newPreDefColumn(s->pdstEMwatervalues, "Maximum Value [m3/s]");
        s->pdchEMwatermaxvaluetime = newPreDefColumn(s->pdstEMwatervalues, "Timestamp of Maximum {TIMESTAMP}");

        // Other KG SubTable
        s->pdstEMotherKGvalues = newPreDefSubTable(s->pdrEnergyMeters, "Annual and Peak Values - Other by Weight/Mass");
        s->pdchEMotherKGannual = newPreDefColumn(s->pdstEMotherKGvalues, "Annual Value [kg]");
        s->pdchEMotherKGminvalue = newPreDefColumn(s->pdstEMotherKGvalues, "Minimum Value [kg/s]");
        s->pdchEMotherKGminvaluetime = newPreDefColumn(s->pdstEMotherKGvalues, "Timestamp of Minimum {TIMESTAMP}");
        s->pdchEMotherKGmaxvalue = newPreDefColumn(s->pdstEMotherKGvalues, "Maximum Value [kg/s]");
        s->pdchEMotherKGmaxvaluetime = newPreDefColumn(s->pdstEMotherKGvalues, "Timestamp of Maximum {TIMESTAMP}");

        // Other M3 SubTable
        s->pdstEMotherM3values = newPreDefSubTable(s->pdrEnergyMeters, "Annual and Peak Values - Other Volumetric");
        s->pdchEMotherM3annual = newPreDefColumn(s->pdstEMotherM3values, "Annual Value [m3]");
        s->pdchEMotherM3minvalue = newPreDefColumn(s->pdstEMotherM3values, "Minimum Value [m3/s]");
        s->pdchEMotherM3minvaluetime = newPreDefColumn(s->pdstEMotherM3values, "Timestamp of Minimum {TIMESTAMP}");
        s->pdchEMotherM3maxvalue = newPreDefColumn(s->pdstEMotherM3values, "Maximum Value [m3/s]");
        s->pdchEMotherM3maxvaluetime = newPreDefColumn(s->pdstEMotherM3values, "Timestamp of Maximum {TIMESTAMP}");

        // Other M3 SubTable
        s->pdstEMotherLvalues = newPreDefSubTable(s->pdrEnergyMeters, "Annual and Peak Values - Other Liquid/Gas");
        s->pdchEMotherLannual = newPreDefColumn(s->pdstEMotherLvalues, "Annual Value [L]");
        s->pdchEMotherLminvalue = newPreDefColumn(s->pdstEMotherLvalues, "Minimum Value [L]");
        s->pdchEMotherLminvaluetime = newPreDefColumn(s->pdstEMotherLvalues, "Timestamp of Minimum {TIMESTAMP}");
        s->pdchEMotherLmaxvalue = newPreDefColumn(s->pdstEMotherLvalues, "Maximum Value [L]");
        s->pdchEMotherLmaxvaluetime = newPreDefColumn(s->pdstEMotherLvalues, "Timestamp of Maximum {TIMESTAMP}");

        // Other J SubTable
        s->pdstEMotherJvalues = newPreDefSubTable(s->pdrEnergyMeters, "Annual and Peak Values - Other");
        s->pdchEMotherJannual = newPreDefColumn(s->pdstEMotherJvalues, "Annual Value [GJ]");
        s->pdchEMotherJminvalue = newPreDefColumn(s->pdstEMotherJvalues, "Minimum Value [W]");
        s->pdchEMotherJminvaluetime = newPreDefColumn(s->pdstEMotherJvalues, "Timestamp of Minimum {TIMESTAMP}");
        s->pdchEMotherJmaxvalue = newPreDefColumn(s->pdstEMotherJvalues, "Maximum Value [W]");
        s->pdchEMotherJmaxvaluetime = newPreDefColumn(s->pdstEMotherJvalues, "Timestamp of Maximum {TIMESTAMP}");

        // Sensible Heat Gain Component Report
        s->pdrSensibleGain = newPreDefReport("SensibleHeatGainSummary", "SHGS", "Sensible Heat Gain Summary");

        s->pdstSHGSannual = newPreDefSubTable(s->pdrSensibleGain, "Annual Building Sensible Heat Gain Components");

        s->pdchSHGSAnZoneEqHt = newPreDefColumn(s->pdstSHGSannual, "HVAC Zone Eq & Other Sensible Air Heating [GJ]");
        s->pdchSHGSAnZoneEqCl = newPreDefColumn(s->pdstSHGSannual, "HVAC Zone Eq & Other Sensible Air Cooling [GJ]");
        s->pdchSHGSAnHvacATUHt = newPreDefColumn(s->pdstSHGSannual, "HVAC Terminal Unit Sensible Air Heating [GJ]");
        s->pdchSHGSAnHvacATUCl = newPreDefColumn(s->pdstSHGSannual, "HVAC Terminal Unit Sensible Air Cooling [GJ]");
        s->pdchSHGSAnSurfHt = newPreDefColumn(s->pdstSHGSannual, "HVAC Input Heated Surface Heating [GJ]");
        s->pdchSHGSAnSurfCl = newPreDefColumn(s->pdstSHGSannual, "HVAC Input Cooled Surface Cooling [GJ]");
        s->pdchSHGSAnPeoplAdd = newPreDefColumn(s->pdstSHGSannual, "People Sensible Heat Addition [GJ]");
        s->pdchSHGSAnLiteAdd = newPreDefColumn(s->pdstSHGSannual, "Lights Sensible Heat Addition [GJ]");
        s->pdchSHGSAnEquipAdd = newPreDefColumn(s->pdstSHGSannual, "Equipment Sensible Heat Addition [GJ]");
        s->pdchSHGSAnWindAdd = newPreDefColumn(s->pdstSHGSannual, "Window Heat Addition [GJ]");
        s->pdchSHGSAnIzaAdd = newPreDefColumn(s->pdstSHGSannual, "Interzone Air Transfer Heat Addition [GJ]");
        s->pdchSHGSAnInfilAdd = newPreDefColumn(s->pdstSHGSannual, "Infiltration Heat Addition [GJ]");
        s->pdchSHGSAnOtherAdd = newPreDefColumn(s->pdstSHGSannual, "Opaque Surface Conduction and Other Heat Addition [GJ]");
        s->pdchSHGSAnEquipRem = newPreDefColumn(s->pdstSHGSannual, "Equipment Sensible Heat Removal [GJ]");
        s->pdchSHGSAnWindRem = newPreDefColumn(s->pdstSHGSannual, "Window Heat Removal [GJ]");
        s->pdchSHGSAnIzaRem = newPreDefColumn(s->pdstSHGSannual, "Interzone Air Transfer Heat Removal [GJ]");
        s->pdchSHGSAnInfilRem = newPreDefColumn(s->pdstSHGSannual, "Infiltration Heat Removal [GJ]");
        s->pdchSHGSAnOtherRem = newPreDefColumn(s->pdstSHGSannual, "Opaque Surface Conduction and Other Heat Removal [GJ]");

        s->pdstSHGSpkCl = newPreDefSubTable(s->pdrSensibleGain, "Peak Cooling Sensible Heat Gain Components");

        s->pdchSHGSClTimePeak = newPreDefColumn(s->pdstSHGSpkCl, "Time of Peak {TIMESTAMP}");
        s->pdchSHGSClHvacHt = newPreDefColumn(s->pdstSHGSpkCl, "HVAC Zone Eq & Other Sensible Air Heating [W]");
        s->pdchSHGSClHvacCl = newPreDefColumn(s->pdstSHGSpkCl, "HVAC Zone Eq & Other Sensible Air Cooling [W]");
        s->pdchSHGSClHvacATUHt = newPreDefColumn(s->pdstSHGSpkCl, "HVAC Terminal Unit Sensible Air Heating [W]");
        s->pdchSHGSClHvacATUCl = newPreDefColumn(s->pdstSHGSpkCl, "HVAC Terminal Unit Sensible Air Cooling [W]");
        s->pdchSHGSClSurfHt = newPreDefColumn(s->pdstSHGSpkCl, "HVAC Input Heated Surface Heating [W]");
        s->pdchSHGSClSurfCl = newPreDefColumn(s->pdstSHGSpkCl, "HVAC Input Cooled Surface Cooling [W]");
        s->pdchSHGSClPeoplAdd = newPreDefColumn(s->pdstSHGSpkCl, "People Sensible Heat Addition [W]");
        s->pdchSHGSClLiteAdd = newPreDefColumn(s->pdstSHGSpkCl, "Lights Sensible Heat Addition [W]");
        s->pdchSHGSClEquipAdd = newPreDefColumn(s->pdstSHGSpkCl, "Equipment Sensible Heat Addition [W]");
        s->pdchSHGSClWindAdd = newPreDefColumn(s->pdstSHGSpkCl, "Window Heat Addition [W]");
        s->pdchSHGSClIzaAdd = newPreDefColumn(s->pdstSHGSpkCl, "Interzone Air Transfer Heat Addition [W]");
        s->pdchSHGSClInfilAdd = newPreDefColumn(s->pdstSHGSpkCl, "Infiltration Heat Addition [W]");
        s->pdchSHGSClOtherAdd = newPreDefColumn(s->pdstSHGSpkCl, "Opaque Surface Conduction and Other Heat Addition [W]");
        s->pdchSHGSClEquipRem = newPreDefColumn(s->pdstSHGSpkCl, "Equipment Sensible Heat Removal [W]");
        s->pdchSHGSClWindRem = newPreDefColumn(s->pdstSHGSpkCl, "Window Heat Removal [W]");
        s->pdchSHGSClIzaRem = newPreDefColumn(s->pdstSHGSpkCl, "Interzone Air Transfer Heat Removal [W]");
        s->pdchSHGSClInfilRem = newPreDefColumn(s->pdstSHGSpkCl, "Infiltration Heat Removal [W]");
        s->pdchSHGSClOtherRem = newPreDefColumn(s->pdstSHGSpkCl, "Opaque Surface Conduction and Other Heat Removal [W]");

        s->pdstSHGSpkHt = newPreDefSubTable(s->pdrSensibleGain, "Peak Heating Sensible Heat Gain Components");

        s->pdchSHGSHtTimePeak = newPreDefColumn(s->pdstSHGSpkHt, "Time of Peak {TIMESTAMP}");
        s->pdchSHGSHtHvacHt = newPreDefColumn(s->pdstSHGSpkHt, "HVAC Zone Eq & Other Sensible Air Heating [W]");
        s->pdchSHGSHtHvacCl = newPreDefColumn(s->pdstSHGSpkHt, "HVAC Zone Eq & Other Sensible Air Cooling [W]");
        s->pdchSHGSHtHvacATUHt = newPreDefColumn(s->pdstSHGSpkHt, "HVAC Terminal Unit Sensible Air Heating [W]");
        s->pdchSHGSHtHvacATUCl = newPreDefColumn(s->pdstSHGSpkHt, "HVAC Terminal Unit Sensible Air Cooling [W]");
        s->pdchSHGSHtSurfHt = newPreDefColumn(s->pdstSHGSpkHt, "HVAC Input Heated Surface Heating [W]");
        s->pdchSHGSHtSurfCl = newPreDefColumn(s->pdstSHGSpkHt, "HVAC Input Cooled Surface Cooling [W]");
        s->pdchSHGSHtPeoplAdd = newPreDefColumn(s->pdstSHGSpkHt, "People Sensible Heat Addition [W]");
        s->pdchSHGSHtLiteAdd = newPreDefColumn(s->pdstSHGSpkHt, "Lights Sensible Heat Addition [W]");
        s->pdchSHGSHtEquipAdd = newPreDefColumn(s->pdstSHGSpkHt, "Equipment Sensible Heat Addition [W]");
        s->pdchSHGSHtWindAdd = newPreDefColumn(s->pdstSHGSpkHt, "Window Heat Addition [W]");
        s->pdchSHGSHtIzaAdd = newPreDefColumn(s->pdstSHGSpkHt, "Interzone Air Transfer Heat Addition [W]");
        s->pdchSHGSHtInfilAdd = newPreDefColumn(s->pdstSHGSpkHt, "Infiltration Heat Addition [W]");
        s->pdchSHGSHtOtherAdd = newPreDefColumn(s->pdstSHGSpkHt, "Opaque Surface Conduction and Other Heat Addition [W]");
        s->pdchSHGSHtEquipRem = newPreDefColumn(s->pdstSHGSpkHt, "Equipment Sensible Heat Removal [W]");
        s->pdchSHGSHtWindRem = newPreDefColumn(s->pdstSHGSpkHt, "Window Heat Removal [W]");
        s->pdchSHGSHtIzaRem = newPreDefColumn(s->pdstSHGSpkHt, "Interzone Air Transfer Heat Removal [W]");
        s->pdchSHGSHtInfilRem = newPreDefColumn(s->pdstSHGSpkHt, "Infiltration Heat Removal [W]");
        s->pdchSHGSHtOtherRem = newPreDefColumn(s->pdstSHGSpkHt, "Opaque Surface Conduction and Other Heat Removal [W]");

        // Standard62Report
        if (state.dataGlobal->DoZoneSizing || state.dataGlobal->DoSystemSizing) {
            s->pdrStd62 = newPreDefReport("Standard62.1Summary", "Std62", "Standard 62.1 Summary");

            s->pdstS62sysVentReqCool = newPreDefSubTable(s->pdrStd62, "System Ventilation Requirements for Cooling");

            s->pdchS62svrClSumVpz = newPreDefColumn(s->pdstS62sysVentReqCool, "Sum of Zone Primary Air Flow - Vpz-sum [m3/s]");
            s->pdchS62svrClPs = newPreDefColumn(s->pdstS62sysVentReqCool, "System Population - Ps");
            s->pdchS62svrClSumPz = newPreDefColumn(s->pdstS62sysVentReqCool, "Sum of Zone Population - Pz-sum");
            s->pdchS62svrClD = newPreDefColumn(s->pdstS62sysVentReqCool, "Occupant Diversity - D");
            s->pdchS62svrClVou = newPreDefColumn(s->pdstS62sysVentReqCool, "Uncorrected Outdoor Air Intake Airflow - Vou [m3/s]");
            s->pdchS62svrClVps = newPreDefColumn(s->pdstS62sysVentReqCool, "System Primary Airflow - Vps [m3/s]");
            s->pdchS62svrClXs = newPreDefColumn(s->pdstS62sysVentReqCool, "Average Outdoor Air Fraction - Xs");
            s->pdchS62svrClEv = newPreDefColumn(s->pdstS62sysVentReqCool, "System Ventilation Efficiency - Ev");
            s->pdchS62svrClVot = newPreDefColumn(s->pdstS62sysVentReqCool, "Outdoor Air Intake Flow - Vot [m3/s]");
            s->pdchS62svrClPercOA = newPreDefColumn(s->pdstS62sysVentReqCool, "Percent Outdoor Air - %OA");
            s->pdchS62svrClEnvironmentOfPs = newPreDefColumn(s->pdstS62sysVentReqCool, "Environment Name of Peak System Population - Ps");
            s->pdchS62svrClTimeOfPs = newPreDefColumn(s->pdstS62sysVentReqCool, "Date and Time of Last Peak System Population - Ps");

            s->pdstS62sysVentReqHeat = newPreDefSubTable(s->pdrStd62, "System Ventilation Requirements for Heating");

            s->pdchS62svrHtSumVpz = newPreDefColumn(s->pdstS62sysVentReqHeat, "Sum of Zone Primary Air Flow - Vpz-sum [m3/s]");
            s->pdchS62svrHtPs = newPreDefColumn(s->pdstS62sysVentReqHeat, "System Population - Ps");
            s->pdchS62svrHtSumPz = newPreDefColumn(s->pdstS62sysVentReqHeat, "Sum of Zone Population - Pz-sum");
            s->pdchS62svrHtD = newPreDefColumn(s->pdstS62sysVentReqHeat, "Occupant Diversity - D");
            s->pdchS62svrHtVou = newPreDefColumn(s->pdstS62sysVentReqHeat, "Uncorrected Outdoor Air Intake Airflow - Vou [m3/s]");
            s->pdchS62svrHtVps = newPreDefColumn(s->pdstS62sysVentReqHeat, "System Primary Airflow - Vps [m3/s]");
            s->pdchS62svrHtXs = newPreDefColumn(s->pdstS62sysVentReqHeat, "Average Outdoor Air Fraction - Xs");
            s->pdchS62svrHtEv = newPreDefColumn(s->pdstS62sysVentReqHeat, "System Ventilation Efficiency - Ev");
            s->pdchS62svrHtVot = newPreDefColumn(s->pdstS62sysVentReqHeat, "Outdoor Air Intake Flow Vot [m3/s]");
            s->pdchS62svrHtPercOA = newPreDefColumn(s->pdstS62sysVentReqHeat, "Percent Outdoor Air - %OA");
            s->pdchS62svrHtEnvironmentOfPs = newPreDefColumn(s->pdstS62sysVentReqHeat, "Environment Name of Peak System Population - Ps");
            s->pdchS62svrHtTimeOfPs = newPreDefColumn(s->pdstS62sysVentReqHeat, "Date and Time of Last Peak System Population - Ps");

            s->pdstS62znVentPar = newPreDefSubTable(s->pdrStd62, "Zone Ventilation Parameters");

            s->pdchS62zvpAlN = newPreDefColumn(s->pdstS62znVentPar, "AirLoop Name");
            s->pdchS62zvpRp = newPreDefColumn(s->pdstS62znVentPar, "People Outdoor Air Rate - Rp [m3/s-person]");
            s->pdchS62zvpPz = newPreDefColumn(s->pdstS62znVentPar, "Zone Population - Pz");
            s->pdchS62zvpRa = newPreDefColumn(s->pdstS62znVentPar, "Area Outdoor Air Rate - Ra [m3/s-m2]");
            s->pdchS62zvpAz = newPreDefColumn(s->pdstS62znVentPar, "Zone Floor Area - Az [m2]");
            s->pdchS62zvpVbz = newPreDefColumn(s->pdstS62znVentPar, "Breathing Zone Outdoor Airflow - Vbz [m3/s]");
            s->pdchS62zvpClEz = newPreDefColumn(s->pdstS62znVentPar, "Cooling Zone Air Distribution Effectiveness - Ez-clg");
            s->pdchS62zvpClVoz = newPreDefColumn(s->pdstS62znVentPar, "Cooling Zone Outdoor Airflow - Voz-clg [m3/s]");
            s->pdchS62zvpHtEz = newPreDefColumn(s->pdstS62znVentPar, "Heating Zone Air Distribution Effectiveness - Ez-htg");
            s->pdchS62zvpHtVoz = newPreDefColumn(s->pdstS62znVentPar, "Heating Zone Outdoor Airflow - Voz-htg [m3/s]");

            s->pdstS62sysVentPar = newPreDefSubTable(s->pdrStd62, "System Ventilation Parameters");

            s->pdchS62svpRp = newPreDefColumn(s->pdstS62sysVentPar, "People Outdoor Air Rate - Rp [m3/s-person]");
            s->pdchS62svpPz = newPreDefColumn(s->pdstS62sysVentPar, "Sum of Zone Population - Pz-sum");
            s->pdchS62svpRa = newPreDefColumn(s->pdstS62sysVentPar, "Area Outdoor Air Rate - Ra [m3/s-m2]");
            s->pdchS62svpAz = newPreDefColumn(s->pdstS62sysVentPar, "Sum of Zone Floor Area - Az-sum [m2]");
            s->pdchS62svpVbz = newPreDefColumn(s->pdstS62sysVentPar, "Breathing Zone Outdoor Airflow - Vbz [m3/s]");
            s->pdchS62svpClVoz = newPreDefColumn(s->pdstS62sysVentPar, "Cooling Zone Outdoor Airflow - Voz-clg [m3/s]");
            s->pdchS62svpHtVoz = newPreDefColumn(s->pdstS62sysVentPar, "Heating Zone Outdoor Airflow - Voz-htg [m3/s]");

            s->pdstS62znCoolDes = newPreDefSubTable(s->pdrStd62, "Zone Ventilation Calculations for Cooling Design");

            s->pdchS62zcdAlN = newPreDefColumn(s->pdstS62znCoolDes, "AirLoop Name");
            s->pdchS62zcdBox = newPreDefColumn(s->pdstS62znCoolDes, "Box Type");
            s->pdchS62zcdVpz = newPreDefColumn(s->pdstS62znCoolDes, "Zone Primary Airflow - Vpz [m3/s]");
            // s->pdchS62zcdVps =         newPreDefColumn(s->pdstS62znCoolDes,'System Primary Airflow - Vps [m3/s]')
            // s->pdchS62zcdVsec =        newPreDefColumn(s->pdstS62znCoolDes,'Secondary Fan Airflow - Vsec [m3/s]')
            s->pdchS62zcdVdz = newPreDefColumn(s->pdstS62znCoolDes, "Zone Discharge Airflow - Vdz [m3/s]");
            s->pdchS62zcdVpzmin = newPreDefColumn(s->pdstS62znCoolDes, "Minimum Zone Primary Airflow - Vpz-min [m3/s]");
            s->pdchS62zcdVozclg = newPreDefColumn(s->pdstS62znCoolDes, "Zone Outdoor Airflow Cooling - Voz-clg [m3/s]");
            s->pdchS62zcdZpz = newPreDefColumn(s->pdstS62znCoolDes, "Primary Outdoor Air Fraction - Zpz");
            s->pdchS62zcdEp = newPreDefColumn(s->pdstS62znCoolDes, "Primary Air Fraction - Ep");
            s->pdchS62zcdEr = newPreDefColumn(s->pdstS62znCoolDes, "Secondary Recirculation Fraction- Er");
            s->pdchS62zcdFa = newPreDefColumn(s->pdstS62znCoolDes, "Supply Air Fraction- Fa");
            s->pdchS62zcdFb = newPreDefColumn(s->pdstS62znCoolDes, "Mixed Air Fraction - Fb");
            s->pdchS62zcdFc = newPreDefColumn(s->pdstS62znCoolDes, "Outdoor Air Fraction - Fc");
            s->pdchS62zcdEvz = newPreDefColumn(s->pdstS62znCoolDes, "Zone Ventilation Efficiency - Evz");

            s->pdstS62sysCoolDes = newPreDefSubTable(s->pdrStd62, "System Ventilation Calculations for Cooling Design");

            s->pdchS62scdVpz = newPreDefColumn(s->pdstS62sysCoolDes, "Sum of Zone Primary Airflow - Vpz-sum [m3/s]");
            s->pdchS62scdVps = newPreDefColumn(s->pdstS62sysCoolDes, "System Primary Airflow - Vps [m3/s]");
            // s->pdchS62scdVsec =        newPreDefColumn(s->pdstS62sysCoolDes,'Secondary Fan Airflow - Vsec [m3/s]')
            s->pdchS62scdVdz = newPreDefColumn(s->pdstS62sysCoolDes, "Sum of Zone Discharge Airflow - Vdz-sum [m3/s]");
            s->pdchS62scdVpzmin = newPreDefColumn(s->pdstS62sysCoolDes, "Sum of Min Zone Primary Airflow - Vpz-min [m3/s]");
            s->pdchS62scdVozclg = newPreDefColumn(s->pdstS62sysCoolDes, "Zone Outdoor Airflow Cooling - Voz-clg [m3/s]");
            s->pdchS62scdEvz = newPreDefColumn(s->pdstS62sysCoolDes, "Zone Ventilation Efficiency - Evz-min");

            s->pdstS62znHeatDes = newPreDefSubTable(s->pdrStd62, "Zone Ventilation Calculations for Heating Design");

            s->pdchS62zhdAlN = newPreDefColumn(s->pdstS62znHeatDes, "AirLoop Name");
            s->pdchS62zhdBox = newPreDefColumn(s->pdstS62znHeatDes, "Box Type");
            s->pdchS62zhdVpz = newPreDefColumn(s->pdstS62znHeatDes, "Zone Primary Airflow - Vpz [m3/s]");
            // s->pdchS62zhdVps =         newPreDefColumn(s->pdstS62znHeatDes,'System Primary Airflow - Vps [m3/s]')
            // s->pdchS62zhdVsec =        newPreDefColumn(s->pdstS62znHeatDes,'Secondary Fan Airflow - Vsec [m3/s]')
            s->pdchS62zhdVdz = newPreDefColumn(s->pdstS62znHeatDes, "Zone Discharge Airflow - Vdz [m3/s]");
            s->pdchS62zhdVpzmin = newPreDefColumn(s->pdstS62znHeatDes, "Minimum Zone Primary Airflow - Vpz-min [m3/s]");
            s->pdchS62zhdVozhtg = newPreDefColumn(s->pdstS62znHeatDes, "Zone Outdoor Airflow Heating - Voz-htg [m3/s]");
            s->pdchS62zhdZpz = newPreDefColumn(s->pdstS62znHeatDes, "Primary Outdoor Air Fraction - Zpz");
            s->pdchS62zhdEp = newPreDefColumn(s->pdstS62znHeatDes, "Primary Air Fraction - Ep");
            s->pdchS62zhdEr = newPreDefColumn(s->pdstS62znHeatDes, "Secondary Recirculation Fraction- Er");
            s->pdchS62zhdFa = newPreDefColumn(s->pdstS62znHeatDes, "Supply Air Fraction- Fa");
            s->pdchS62zhdFb = newPreDefColumn(s->pdstS62znHeatDes, "Mixed Air Fraction - Fb");
            s->pdchS62zhdFc = newPreDefColumn(s->pdstS62znHeatDes, "Outdoor Air Fraction - Fc");
            s->pdchS62zhdEvz = newPreDefColumn(s->pdstS62znHeatDes, "Zone Ventilation Efficiency - Evz");

            s->pdstS62sysHeatDes = newPreDefSubTable(s->pdrStd62, "System Ventilation Calculations for Heating Design");

            s->pdchS62shdVpz = newPreDefColumn(s->pdstS62sysHeatDes, "Sum of Zone Primary Airflow - Vpz-sum [m3/s]");
            s->pdchS62shdVps = newPreDefColumn(s->pdstS62sysHeatDes, "System Primary Airflow - Vps [m3/s]");
            // s->pdchS62shdVsec =        newPreDefColumn(s->pdstS62sysHeatDes,'Secondary Fan Airflow - Vsec [m3/s]')
            s->pdchS62shdVdz = newPreDefColumn(s->pdstS62sysHeatDes, "Sum of Zone Discharge Airflow - Vdz-sum [m3/s]");
            s->pdchS62shdVpzmin = newPreDefColumn(s->pdstS62sysHeatDes, "Sum of Min Zone Primary Airflow - Vpz-min [m3/s]");
            s->pdchS62shdVozhtg = newPreDefColumn(s->pdstS62sysHeatDes, "Zone Outdoor Airflow Heating - Voz-htg [m3/s]");
            s->pdchS62shdEvz = newPreDefColumn(s->pdstS62sysHeatDes, "Zone Ventilation Efficiency - Evz-min");
        }

        s->pdrLeed = newPreDefReport("LEEDsummary", "LEED", "LEED Summary");

        s->pdstLeedGenInfo = newPreDefSubTable(s->pdrLeed, "Sec1.1A-General Information");
        // single column with rows of:
        //    Principal Heating Source
        //    Weather File
        //    Climate Zone
        //    Heating Degree Days
        //    Cooling Degree Days
        //    HDD and CDD data source
        //    Total gross floor area
        s->pdchLeedGenData = newPreDefColumn(s->pdstLeedGenInfo, "Data");

        s->pdstLeedSpaceUsageType = newPreDefSubTable(s->pdrLeed, "EAp2-1. Space Usage Type");
        s->pdchLeedSutSpArea = newPreDefColumn(s->pdstLeedSpaceUsageType, "Space Area [m2]");
        s->pdchLeedSutOcArea = newPreDefColumn(s->pdstLeedSpaceUsageType, "Regularly Occupied Area [m2]");
        s->pdchLeedSutUnArea = newPreDefColumn(s->pdstLeedSpaceUsageType, "Unconditioned Area [m2]");
        s->pdchLeedSutHrsWeek = newPreDefColumn(s->pdstLeedSpaceUsageType, "Typical Hours/Week in Operation [hr/wk]");

        s->pdstLeedAdvsMsg = newPreDefSubTable(s->pdrLeed, "EAp2-2. Advisory Messages");
        // single column with rows of:
        //    Number of hours heating loads not met
        //    Number of hours cooling loads not met
        //    Total
        //    Difference
        //    Number of warning messages
        //    Number of error messages
        //    Number of defaults overridden
        s->pdchLeedAmData = newPreDefColumn(s->pdstLeedAdvsMsg, "Data");

        s->pdstLeedEneTypSum = newPreDefSubTable(s->pdrLeed, "EAp2-3. Energy Type Summary");
        // multiple columns with rows of
        //    Electricity
        //    Natural Gas
        //    <additional fuels>
        s->pdchLeedEtsRtNm = newPreDefColumn(s->pdstLeedEneTypSum, "Utility Rate");
        s->pdchLeedEtsVirt = newPreDefColumn(s->pdstLeedEneTypSum, "Virtual Rate [$/unit energy]");
        s->pdchLeedEtsEneUnt = newPreDefColumn(s->pdstLeedEneTypSum, "Units of Energy");
        s->pdchLeedEtsDemUnt = newPreDefColumn(s->pdstLeedEneTypSum, "Units of Demand");

        s->pdstLeedPerf = newPreDefSubTable(s->pdrLeed, "EAp2-4/5. Performance Rating Method Compliance");
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
        s->pdchLeedPerfElEneUse = newPreDefColumn(s->pdstLeedPerf, "Electricity Energy Use [GJ]");
        s->pdchLeedPerfElDem = newPreDefColumn(s->pdstLeedPerf, "Electricity Demand [W]");
        s->pdchLeedPerfGasEneUse = newPreDefColumn(s->pdstLeedPerf, "Natural Gas Energy Use [GJ]");
        s->pdchLeedPerfGasDem = newPreDefColumn(s->pdstLeedPerf, "Natural Gas Demand [W]");
        s->pdchLeedPerfGasolineEneUse = newPreDefColumn(s->pdstLeedPerf, "Gasoline Use [GJ]");
        s->pdchLeedPerfGasolineDem = newPreDefColumn(s->pdstLeedPerf, "Gasoline Demand [W]");
        s->pdchLeedPerfDieselEneUse = newPreDefColumn(s->pdstLeedPerf, "Diesel Use [GJ]");
        s->pdchLeedPerfDieselDem = newPreDefColumn(s->pdstLeedPerf, "Diesel Demand [W]");
        s->pdchLeedPerfCoalEneUse = newPreDefColumn(s->pdstLeedPerf, "Coal Use [GJ]");
        s->pdchLeedPerfCoalDem = newPreDefColumn(s->pdstLeedPerf, "Coal Demand [W]");
        s->pdchLeedPerfFuelOil1EneUse = newPreDefColumn(s->pdstLeedPerf, "Fuel Oil No 1 Use [GJ]");
        s->pdchLeedPerfFuelOil1Dem = newPreDefColumn(s->pdstLeedPerf, "Fuel Oil No 1 Demand [W]");
        s->pdchLeedPerfFuelOil2EneUse = newPreDefColumn(s->pdstLeedPerf, "Fuel Oil No 2 Use [GJ]");
        s->pdchLeedPerfFuelOil2Dem = newPreDefColumn(s->pdstLeedPerf, "Fuel Oil No 2 Demand [W]");
        s->pdchLeedPerfPropaneEneUse = newPreDefColumn(s->pdstLeedPerf, "Propane Use [GJ]");
        s->pdchLeedPerfPropaneDem = newPreDefColumn(s->pdstLeedPerf, "Propane Demand [W]");
        s->pdchLeedPerfOtherFuel1EneUse = newPreDefColumn(s->pdstLeedPerf, "Other Fuel 1 Use [GJ]");
        s->pdchLeedPerfOtherFuel1Dem = newPreDefColumn(s->pdstLeedPerf, "Other Fuel 1 Demand [W]");
        s->pdchLeedPerfOtherFuel2EneUse = newPreDefColumn(s->pdstLeedPerf, "Other Fuel 2 Use [GJ]");
        s->pdchLeedPerfOtherFuel2Dem = newPreDefColumn(s->pdstLeedPerf, "Other Fuel 2 Demand [W]");
        s->pdchLeedPerfDisClEneUse = newPreDefColumn(s->pdstLeedPerf, "District Cooling Use [GJ]");
        s->pdchLeedPerfDisClDem = newPreDefColumn(s->pdstLeedPerf, "District Cooling Demand [W]");
        s->pdchLeedPerfDisHtEneUse = newPreDefColumn(s->pdstLeedPerf, "District Heating Use [GJ]");
        s->pdchLeedPerfDisHtDem = newPreDefColumn(s->pdstLeedPerf, "District Heating Demand [W]");

        s->pdstLeedEneUseSum = newPreDefSubTable(s->pdrLeed, "EAp2-6. Energy Use Summary");
        // Multiple columns with rows of:
        //    Electricity
        //    Natural Gas
        //    <additional fuels>
        //    Total
        s->pdchLeedEusProc = newPreDefColumn(s->pdstLeedEneUseSum, "Process Subtotal [GJ]");
        s->pdchLeedEusTotal = newPreDefColumn(s->pdstLeedEneUseSum, "Total Energy Use [GJ]");

        s->pdstLeedEneCostSum = newPreDefSubTable(s->pdrLeed, "EAp2-7. Energy Cost Summary");
        // Multiple columns with rows of:
        //    Electricity
        //    Natural Gas
        //    <additional fuels>
        //    Total
        s->pdchLeedEcsProc = newPreDefColumn(s->pdstLeedEneCostSum, "Process Subtotal [$]");
        s->pdchLeedEcsTotal = newPreDefColumn(s->pdstLeedEneCostSum, "Total Energy Cost [$]");

        s->pdstLeedRenewSum = newPreDefSubTable(s->pdrLeed, "L-1. Renewable Energy Source Summary");
        // Multiple columns with rows of each renewable source
        s->pdchLeedRenRatCap = newPreDefColumn(s->pdstLeedRenewSum, "Rated Capacity [kW]");
        s->pdchLeedRenAnGen = newPreDefColumn(s->pdstLeedRenewSum, "Annual Energy Generated [GJ]");

        s->pdstLeedEneUseIntEl = newPreDefSubTable(s->pdrLeed, "EAp2-17a. Energy Use Intensity - Electricity");
        // Single column with rows of:
        //    Interior lighting
        //    Space heating
        //    Space cooling
        //    Fans-interior
        //    Service water heating
        //    Receptacle equipment
        //    Miscellaneous
        //    Subtotal
        s->pdchLeedEuiElec = newPreDefColumn(s->pdstLeedEneUseIntEl, "Electricty [MJ/m2]");

        s->pdstLeedEneUseIntNatG = newPreDefSubTable(s->pdrLeed, "EAp2-17b. Energy Use Intensity - Natural Gas");
        // Single column with rows of:
        //    Space heating
        //    Service water heating
        //    Miscellaneous
        //    Subtotal
        s->pdchLeedEuiNatG = newPreDefColumn(s->pdstLeedEneUseIntNatG, "Natural Gas [MJ/m2]");

        s->pdstLeedEneUseIntOthr = newPreDefSubTable(s->pdrLeed, "EAp2-17c. Energy Use Intensity - Additional");
        // Single column with rows of:
        //    Miscellaneous
        //    Subtotal
        s->pdchLeedEuiOthr = newPreDefColumn(s->pdstLeedEneUseIntOthr, "Additional [MJ/m2]");

        s->pdstLeedEneUsePerc = newPreDefSubTable(s->pdrLeed, "EAp2-18. End Use Percentage");
        // single column with rows of:
        //    Interior Lighting
        //    Space heating
        //    Space cooling
        //    Fans-Interior
        //    Service Water Heating
        //    Receptacle Equipment
        //    Miscellaneous
        s->pdchLeedEupPerc = newPreDefColumn(s->pdstLeedEneUsePerc, "Percent [%]");

        s->pdstLeedEqFlLdHrs = newPreDefSubTable(s->pdrLeed, "Schedules-Equivalent Full Load Hours (Schedule Type=Fraction)");
        s->pdchLeedEflhEflh = newPreDefColumn(s->pdstLeedEqFlLdHrs, "Equivalent Full Load Hours of Operation Per Year [hr]");
        s->pdchLeedEflhNonZerHrs = newPreDefColumn(s->pdstLeedEqFlLdHrs, "Hours > 1% [hr]");

        s->pdstLeedSchedSetPts = newPreDefSubTable(s->pdrLeed, "Schedules-SetPoints (Schedule Type=Temperature)");
        s->pdChLeedSchStPtFirstObjUsed = newPreDefColumn(s->pdstLeedSchedSetPts, "First Object Used");
        s->pdChLeedSchStPtMonthUsed = newPreDefColumn(s->pdstLeedSchedSetPts, "Month Assumed");
        s->pdchLeedSchStPt11amWednesday = newPreDefColumn(s->pdstLeedSchedSetPts, "11am First Wednesday [C]");
        s->pdchLeedSchStPt11amWedCnt = newPreDefColumn(s->pdstLeedSchedSetPts, "Days with Same 11am Value");
        s->pdchLeedSchStPt11pmWednesday = newPreDefColumn(s->pdstLeedSchedSetPts, "11pm First Wednesday [C]");
        s->pdchLeedSchStPt11pmWedCnt = newPreDefColumn(s->pdstLeedSchedSetPts, "Days with Same 11pm Value");

        s->pdrThermalResilience = newPreDefReport("ThermalResilienceSummary", "ThermR", "Annual Thermal Resilience Summary");

        s->pdstHIHours = newPreDefSubTable(s->pdrThermalResilience, "Heat Index Hours");
        s->pdchHIHourSafe = newPreDefColumn(s->pdstHIHours, "Safe (≤ 26.7°C) [hr]");
        s->pdchHIHourCaution = newPreDefColumn(s->pdstHIHours, "Caution (> 26.7, ≤ 32.2°C) [hr]");
        s->pdchHIHourExtremeCaution = newPreDefColumn(s->pdstHIHours, "Extreme Caution (> 32.2, ≤ 39.4°C) [hr]");
        s->pdchHIHourDanger = newPreDefColumn(s->pdstHIHours, "Danger (> 39.4, ≤ 51.7°C) [hr]");
        s->pdchHIHourExtremeDanger = newPreDefColumn(s->pdstHIHours, "Extreme Danger (> 51.7°C) [hr]");

        s->pdstHIOccuHours = newPreDefSubTable(s->pdrThermalResilience, "Heat Index OccupantHours");
        s->pdchHIOccuHourSafe = newPreDefColumn(s->pdstHIOccuHours, "Safe (≤ 26.7°C) [hr]");
        s->pdchHIOccuHourCaution = newPreDefColumn(s->pdstHIOccuHours, "Caution (> 26.7, ≤ 32.2°C) [hr]");
        s->pdchHIOccuHourExtremeCaution = newPreDefColumn(s->pdstHIOccuHours, "Extreme Caution (> 32.2, ≤ 39.4°C) [hr]");
        s->pdchHIOccuHourDanger = newPreDefColumn(s->pdstHIOccuHours, "Danger (> 39.4, ≤ 51.7°C) [hr]");
        s->pdchHIOccuHourExtremeDanger = newPreDefColumn(s->pdstHIOccuHours, "Extreme Danger (> 51.7°C) [hr]");

        s->pdstHumidexHours = newPreDefSubTable(s->pdrThermalResilience, "Humidex Hours");
        s->pdchHumidexHourLittle = newPreDefColumn(s->pdstHumidexHours, "Little to no Discomfort (≤ 29) [hr]");
        s->pdchHumidexHourSome = newPreDefColumn(s->pdstHumidexHours, "Some Discomfort (> 29, ≤ 40) [hr]");
        s->pdchHumidexHourGreat = newPreDefColumn(s->pdstHumidexHours, "Great Discomfort; Avoid Exertion (> 40, ≤ 45) [hr]");
        s->pdchHumidexHourDanger = newPreDefColumn(s->pdstHumidexHours, "Dangerous (> 45, ≤ 50) [hr]");
        s->pdchHumidexHourStroke = newPreDefColumn(s->pdstHumidexHours, "Heat Stroke Quite Possible (> 50) [hr]");

        s->pdstHumidexOccuHours = newPreDefSubTable(s->pdrThermalResilience, "Humidex OccupantHours");
        s->pdchHumidexOccuHourLittle = newPreDefColumn(s->pdstHumidexOccuHours, "Little to no Discomfort (≤ 29) [hr]");
        s->pdchHumidexOccuHourSome = newPreDefColumn(s->pdstHumidexOccuHours, "Some Discomfort (> 29, ≤ 40) [hr]");
        s->pdchHumidexOccuHourGreat = newPreDefColumn(s->pdstHumidexOccuHours, "Great Discomfort; Avoid Exertion (> 40, ≤ 45) [hr]");
        s->pdchHumidexOccuHourDanger = newPreDefColumn(s->pdstHumidexOccuHours, "Dangerous (> 45, ≤ 50) [hr]");
        s->pdchHumidexOccuHourStroke = newPreDefColumn(s->pdstHumidexOccuHours, "Heat Stroke Quite Possible (> 50) [hr]");

        s->pdstHeatingSETHours = newPreDefSubTable(s->pdrThermalResilience, "Heating SET Hours");
        s->pdchHeatingSETHours = newPreDefColumn(s->pdstHeatingSETHours, "SET ≤ 12.2°C Hours (°C)");
        s->pdchHeatingSETOccuHours = newPreDefColumn(s->pdstHeatingSETHours, "SET ≤ 12.2°C OccupantHours (°C)");
        s->pdchHeatingSETUnmetDuration = newPreDefColumn(s->pdstHeatingSETHours, "Longest SET ≤ 12.2°C Duration [hr]");
        s->pdchHeatingSETUnmetTime = newPreDefColumn(s->pdstHeatingSETHours, "Start Time of the Longest SET ≤ 12.2°C Duration");

        s->pdstCoolingSETHours = newPreDefSubTable(s->pdrThermalResilience, "Cooling SET Hours");
        s->pdchCoolingSETHours = newPreDefColumn(s->pdstCoolingSETHours, "SET > 30°C Hours (°C)");
        s->pdchCoolingSETOccuHours = newPreDefColumn(s->pdstCoolingSETHours, "SET > 30°C OccupantHours (°C)");
        s->pdchCoolingSETUnmetDuration = newPreDefColumn(s->pdstCoolingSETHours, "Longest SET > 30°C Duration [hr]");
        s->pdchCoolingSETUnmetTime = newPreDefColumn(s->pdstCoolingSETHours, "Start Time of the Longest SET > 30°C Duration");

        s->pdrCO2Resilience = newPreDefReport("CO2ResilienceSummary", "CO2R", "Annual CO2 Resilience Summary");

        s->pdstCO2Hours = newPreDefSubTable(s->pdrCO2Resilience, "CO2 Level Hours");
        s->pdchCO2HourSafe = newPreDefColumn(s->pdstCO2Hours, "Safe (<= 1000 ppm) [hr]");
        s->pdchCO2HourCaution = newPreDefColumn(s->pdstCO2Hours, "Caution (> 1000, <= 5000 ppm) [hr]");
        s->pdchCO2HourHazard = newPreDefColumn(s->pdstCO2Hours, "Hazard (> 5000 ppm) [hr]");

        s->pdstCO2OccuHours = newPreDefSubTable(s->pdrCO2Resilience, "CO2 Level OccupantHours");
        s->pdchCO2OccuHourSafe = newPreDefColumn(s->pdstCO2OccuHours, "Safe (<= 1000 ppm) [hr]");
        s->pdchCO2OccuHourCaution = newPreDefColumn(s->pdstCO2OccuHours, "Caution (> 1000, <= 5000 ppm) [hr]");
        s->pdchCO2OccuHourHazard = newPreDefColumn(s->pdstCO2OccuHours, "Hazard (> 5000 ppm) [hr]");

        s->pdrVisualResilience = newPreDefReport("VisualResilienceSummary", "VisualR", "Annual Visual Resilience Summary");

        s->pdstIllumHours = newPreDefSubTable(s->pdrVisualResilience, "Illuminance Level Hours");
        s->pdchIllumHourDark = newPreDefColumn(s->pdstIllumHours, "A Bit Dark (<= 100 lux) [hr]");
        s->pdchIllumHourDim = newPreDefColumn(s->pdstIllumHours, "Dim (> 100, <= 300 lux) [hr]");
        s->pdchIllumHourAdequate = newPreDefColumn(s->pdstIllumHours, "Adequate (> 300, <= 500 lux) [hr]");
        s->pdchIllumHourBright = newPreDefColumn(s->pdstIllumHours, "Bright (>500 lux) [hr]");

        s->pdstIllumOccuHours = newPreDefSubTable(s->pdrVisualResilience, "Illuminance Level OccupantHours");
        s->pdchIllumOccuHourDark = newPreDefColumn(s->pdstIllumOccuHours, "A Bit Dark (<= 100 lux) [hr]");
        s->pdchIllumOccuHourDim = newPreDefColumn(s->pdstIllumOccuHours, "Dim (> 100, <= 300 lux) [hr]");
        s->pdchIllumOccuHourAdequate = newPreDefColumn(s->pdstIllumOccuHours, "Adequate (> 300, <= 500 lux) [hr]");
        s->pdchIllumOccuHourBright = newPreDefColumn(s->pdstIllumOccuHours, "Bright (>500 lux) [hr]");

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

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int sigDigitCount;
        std::string stringEntry;

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

        if (tableEntryReal < 1e8) { // change from 1e10 for more robust entry writing
            tableEntry(numTableEntry).charEntry = format("{:#12.{}F}", tableEntryReal, sigDigitCount);
        } else {
            tableEntry(numTableEntry).charEntry = format("{:12.{}Z}", tableEntryReal, sigDigitCount);
        }


        if (tableEntry(numTableEntry).charEntry.size() > 12) {
            tableEntry(numTableEntry).charEntry = "  Too Big";
        }

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

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        incrementTableEntry();
        // convert the integer to a string
        tableEntry(numTableEntry).charEntry = format("{:12}", tableEntryInt);
        tableEntry(numTableEntry).objectName = objName;
        tableEntry(numTableEntry).indexColumn = columnIndex;
    }

    std::string RetrievePreDefTableEntry(int const columnIndex, std::string const &objName)
    {
        for (int iTableEntry = 1; iTableEntry <= numTableEntry; ++iTableEntry) {
            if (tableEntry(iTableEntry).indexColumn == columnIndex && tableEntry(iTableEntry).objectName == objName) {
                return trimmed(ljustified(tableEntry(iTableEntry).charEntry));
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
