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

// C++ Headers
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/SQLiteProcedures.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WeatherManager.hh>

namespace EnergyPlus {

namespace ReportSizingManager {

    // Module containing the routines dealing with the <module_name>

    // MODULE INFORMATION:
    //       AUTHOR         Linda Lawrie<author>
    //       DATE WRITTEN   November 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Provide module interface for ReportSizingOutput

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // OTHER NOTES:
    // na

    // USE STATEMENTS:
    // na

    // MODULE PARAMETER DEFINITIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // MODULE VARIABLE DECLARATIONS:
    // na

    // SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

    // Functions
    bool MyOneTimeFlag(true);

    void clear_state()
    {
        MyOneTimeFlag = true;
    }

    void ReportSizingOutput(std::string const &CompType,    // the type of the component
                            std::string const &CompName,    // the name of the component
                            std::string const &VarDesc,     // the description of the input variable
                            Real64 const VarValue,          // the value from the sizing calculation
                            Optional_string_const UsrDesc,  // the description of a user-specified variable
                            Optional<Real64 const> UsrValue // the value from the user for the desc item
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   Decenber 2001
        //       MODIFIED       August 2008, Greg Stark
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine writes one item of sizing data to the "eio" file..

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataPrecisionGlobals;
        using namespace OutputReportPredefined;
        using General::RoundSigDigits;

        // Formats
        static constexpr auto Format_991(" Component Sizing Information, {}, {}, {}, {:.5R}\n");

        // to do, make this a parameter. Unfortunately this function is used in MANY
        // places so it involves touching most of E+
        auto &ioFiles = IOFiles::getSingleton();
        if (MyOneTimeFlag) {
            static constexpr auto Format_990("! <Component Sizing Information>, Component Type, Component Name, Input Field Description, Value\n");
            print(ioFiles.eio, Format_990);
            MyOneTimeFlag = false;
        }

        print(ioFiles.eio, Format_991, CompType, CompName, VarDesc, VarValue);
        // add to tabular output reports
        AddCompSizeTableEntry(CompType, CompName, VarDesc, VarValue);

        if (present(UsrDesc) && present(UsrValue)) {
            print(ioFiles.eio, Format_991, CompType, CompName, UsrDesc(), UsrValue());
            AddCompSizeTableEntry(CompType, CompName, UsrDesc, UsrValue);
        } else if (present(UsrDesc) || present(UsrValue)) {
            ShowFatalError("ReportSizingOutput: (Developer Error) - called with user-specified description or value but not both.");
        }

        // add to SQL output
        if (sqlite) sqlite->addSQLiteComponentSizingRecord(CompType, CompName, VarDesc, VarValue);
        if (present(UsrDesc) && present(UsrValue)) {
            if (sqlite) sqlite->addSQLiteComponentSizingRecord(CompType, CompName, UsrDesc, UsrValue);
        }
    }

    void RequestSizing(std::string const &CompType,       // type of component
                       std::string const &CompName,       // name of component
                       int const SizingType,              // integerized type of sizing requested (see DataHVACGlobals, e.g. CoolingCapacitySizing)
                       std::string const &SizingString,   // string containing info for eio report
                       Real64 &SizingResult,              // result of the sizing procedure
                       bool const PrintWarningFlag,       // TRUE when requesting output (eio) reporting
                       std::string const &CallingRoutine) // name of calling routine for warning messages
    {
        // SUBROUTINE INFORMATION :
        // AUTHOR         Richard Raustad, FSEC
        // DATE WRITTEN   October 2013
        // MODIFIED       na
        // RE - ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE :
        // This function returns the sizing result.

        // METHODOLOGY EMPLOYED :
        // Sizing calculations for component and parent models are now consolidated into a common routine. The parent or component will
        // specify global variables (instead of using optional arguments) to set up the input required for specific sizing calculations.
        //
        // SIZING TYPES (selects the specific sizing calculations, optional global arguments are in parentheses):
        // (other sizing types may be added as needed)
        //
        // Developer will typically provide all Data* variables in parentheses since a coil could be zone or airloop equipment
        // Caution: DataFlowUsedForSizing might be either m3/s or kg/s, check the code during model development to be sure of usage
        // Comments for ZONE or AIRLOOP coils indicates differences in calculations, OPTIONAL means 1 will otherwise be used)
        //
        // CoolingAirflowSizing( 1 ); // request sizing for cooling air flow rate
        // CoolingWaterflowSizing( 2 ); // request sizing for cooling water flow rate (DataPltSizCoolNum, DataWaterLoopNum, AIRLOOP COILS:
        // DataCapacityUsedForSizing) HeatingWaterflowSizing( 3 ); // request sizing for heating coil water flow rate (DataPltSizHeatNum,
        // DataWaterLoopNum, AIRLOOP COILS: DataCapacityUsedForSizing) CoolingWaterDesAirInletTempSizing( 4 ); // request sizing for cooling water
        // coil inlet air temp CoolingWaterDesAirInletHumRatSizing( 5 ); // request sizing for cooling water coil inlet air humidity ratio
        // CoolingWaterDesWaterInletTempSizing( 6 ); // request sizing for cooling water coil inlet water temp (DataPltSizCoolNum)
        // CoolingWaterDesAirOutletTempSizing( 7 ); // request sizing for cooling water coil outlet air temp (DataPltSizCoolNum, DataWaterLoopNum,
        // DataDesInletAirTemp, DataDesInletAirHumRat, DataAirFlowUsedForSizing, DataWaterFlowUsedForSizing, AIRLOOP COILS: DataDesInletWaterTemp)
        // CoolingWaterDesAirOutletHumRatSizing( 8 ); // request sizing for cooling water coil outlet air humidity ratio (DataDesInletAirHumRat,
        // DataDesInletWaterTemp, DataCapacityUsedForSizing, ZONE COILS: DataDesOutletAirTemp) CoolingWaterNumofTubesPerRowSizing( 9 ); // request
        // sizing for cooling water coil number of tubes per row (DataWaterFlowUsedForSizing) HeatingWaterDesAirInletTempSizing( 10 ); // request
        // sizing for heating water coil inlet air temp HeatingWaterDesAirInletHumRatSizing( 11 ); // request sizing for heating water coil inlet air
        // humidity ratio HeatingWaterDesCoilLoadUsedForUASizing( 12 ); // request sizing for heating water coil capacity used for UA sizing
        // (DataWaterLoopNum, DataPltSizHeatNum, AIRLOOP COILS: DataAirFlowUsedForSizing) HeatingWaterDesCoilWaterVolFlowUsedForUASizing( 13 ); //
        // request sizing for heating water coil volume flow rate used for UA sizing (DataWaterFlowUsedForSizing) HeatingAirflowSizing( 14 ); //
        // request sizing for heating air flow rate HeatingAirflowUASizing( 15 ); // request sizing for heating air flow rate SystemAirflowSizing( 16
        // ); // request sizing for system air flow rate CoolingCapacitySizing( 17 ); // request sizing for cooling capacity (DataFlowUsedForSizing
        // [m3/s], DataTotCapCurveIndex [DX coils only]) HeatingCapacitySizing( 18 ); // request sizing for heating capacity (DataCoolCoilCap,
        // DataFlowUsedForSizing, AIRLOOP COILS: DataCoilIsSuppHeater, OPTIONAL: DataHeatSizeRatio) WaterHeatingCapacitySizing( 19 ); // request
        // sizing for water-side heating capacity (ZONE COILS ONLY: DataWaterLoopNum, DataPltSizHeatNum, OPTIONAL: DataHeatSizeRatio)
        // WaterHeatingCoilUASizing( 20 ); // request sizing for heating coil UA (DataCapacityUsedForSizing, DataCoilNum, DataFanOpMode,
        // DataCapacityUsedForSizing, DataDesInletAirTemp, DataDesInletAirHumRat, DataFlowUsedForSizing, DataDesignCoilCapacity) SystemCapacitySizing(
        // 21 ); // request sizing for system capacity CoolingSHRSizing( 22 ); // request sizing for cooling SHR (DataFlowUsedForSizing,
        // DataCapacityUsedForSizing) HeatingDefrostSizing( 23 ); // request sizing for heating defrost capacity MaxHeaterOutletTempSizing( 24 ); //
        // request sizing for heating coil maximum outlet temperature AutoCalculateSizing ( 25 ); // identifies an autocalulate input
        //
        // GLOBAL VARIABLES (previously used as optional arguments):
        // (other global variables may be added as needed)
        //
        // int DataTotCapCurveIndex( 0 ); // index to total capacity as a function of temperature curve
        // int DataPltSizCoolNum( 0 ); // index to cooling plant sizing data
        // int DataPltSizHeatNum( 0 ); // index to heating plant sizing data
        // int DataWaterLoopNum( 0 ); // index to plant water loop
        // int DataCoilNum( 0 ); // index to coil object
        // int DataFanOpMode( 0 ); // fan operating mode (ContFanCycCoil or CycFanCycCoil)
        // int DataFanEnumType( -1 ); // Fan type used during sizing
        // int DataFanIndex( -1 ); // Fan index used during sizing
        // bool DataCoilIsSuppHeater( false ); // TRUE if heating coil used as supplemental heater
        // bool DataIsDXCoil( false ); // TRUE if direct-expansion coil
        // bool DataAutosizable( true ); // TRUE if component is autosizable
        // bool DataEMSOverrideON( false ); // boolean determines if user relies on EMS to override autosizing
        // Real64 DataDesInletWaterTemp( 0.0 ); // coil inlet water temperture used for warning messages
        // Real64 DataDesInletAirHumRat( 0.0 ); // coil inlet air humidity ratio used for warning messages
        // Real64 DataDesInletAirTemp( 0.0 ); // coil inlet air temperature used for warning messages
        // Real64 DataDesOutletAirTemp( 0.0 ); // coil outlet air temperature used for sizing
        // Real64 DataCoolCoilCap( 0.0 ); // cooling coil capacity used for sizing with scalable inputs [W]
        // Real64 DataFlowUsedForSizing( 0.0 ); // air flow rate used for sizing with scalable inputs [m3/s]
        // Real64 DataAirFlowUsedForSizing( 0.0 ); // air flow rate used for sizing with scalable inputs [m3/s]
        // Real64 DataWaterFlowUsedForSizing( 0.0 ); // water flow rate used for sizing with scalable inputs [m3/s]
        // Real64 DataCapacityUsedForSizing( 0.0 ); //capacity used for sizing with scalable inputs [W]
        // Real64 DataDesignCoilCapacity( 0.0); // calculated capacity of coil at end of UA calculation
        // Real64 DataHeatSizeRatio( 1.0 ); // heating coil size as a ratio of cooling coil capacity
        // Real64 DataEMSOverride( 0.0 ); // value of EMS variable used to override autosizing
        // Real64 DataBypassFrac( 0.0 ); // value of bypass fraction for Coil:Cooling:DX:TwoStageWithHumidityControlMode coils
        // Real64 DataConstantUsedForSizing( 0.0 ); // base value used for sizing inputs that are ratios of other inputs
        // Real64 DataFractionUsedForSizing( 0.0 ); // fractional value of base value used for sizing inputs that are ratios of other inputs
        // Real64 DataNonZoneNonAirloopValue( 0.0 ); // used when equipment is not located in a zone or airloop (rarely used, ex. HPWH fan)
        //
        // EXAMPLE setup in DXCoils:
        //			if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
        //				SizingMethod = CoolingAirflowSizing;
        //				CompName = DXCoil( DXCoilNum ).Name + ":" + DXCoil( DXCoilNum ).CoilPerformanceName( Mode );
        //				FieldNum = 4;
        //				DataBypassFrac = DXCoil ( DXCoilNum ).BypassedFlowFrac ( Mode );
        //			}
        //			else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatingEmpirical ) {
        //				SizingMethod = HeatingAirflowSizing;
        //				CompName = DXCoil( DXCoilNum ).Name;
        //				FieldNum = 3;
        //			}
        //			else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_Heating ) {
        //				SizingMethod = HeatingAirflowSizing;
        //				CompName = DXCoil( DXCoilNum ).Name;
        //				FieldNum = 2;
        //			}
        //			else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_Cooling ) {
        //				SizingMethod = CoolingAirflowSizing;
        //				CompName = DXCoil( DXCoilNum ).Name;
        //				FieldNum = 3;
        //			}
        //			else {
        //				SizingMethod = CoolingAirflowSizing;
        //				CompName = DXCoil( DXCoilNum ).Name;
        //				FieldNum = 4;
        //			}
        //			TempSize = DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode );
        //			SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [m3/s]";
        //			CompType = trim( DXCoil( DXCoilNum ).DXCoilType );
        //			DataIsDXCoil = true;
        //			DataEMSOverrideON = DXCoil ( DXCoilNum ).RatedAirVolFlowRateEMSOverrideON ( Mode );
        //			DataEMSOverride = DXCoil( DXCoilNum ).RatedAirVolFlowRateEMSOverrideValue( Mode );
        //			RequestSizing( CompType, CompName, SizingMethod, trim(SizingString ), TempSize, bPRINT, RoutineName );
        //			DXCoil ( DXCoilNum ).RatedAirVolFlowRate ( Mode ) = TempSize;
        //			DataIsDXCoil = false;
        //			DataEMSOverrideON = false;
        //			DataEMSOverride = 0.0;
        //			DataBypassFrac = 0.0;
        //
        // PARENT OBJECT OVERRIDES:
        //
        // Parent objects can override sizing calculations using a combination of boolean and Real64 variables.
        //
        // Zone Equipment (eg):
        // Boolean - ZoneEqSizing( CurZoneEqNum ).CoolingCapacity and Real64 - ZoneEqSizing ( CurZoneEqNum ).DesCoolingLoad;
        //
        // AirloopHVAC Equipment (eg):
        // Boolean - UnitarySysEqSizing(CurSysNum).HeatingAirFlow and Real64 - UnitarySysEqSizing(CurSysNum).HeatingAirVolFlow
        //
        // Outside Air System Equipment (eg):
        // Boolean - OASysEqSizing( CurOASysNum ).CoolingAirFlow and Real64 - OASysEqSizing ( CurOASysNum ).CoolingAirVolFlow

        // USE STATEMENTS :
        using CurveManager::CurveValue;
        using DataEnvironment::StdBaroPress;
        using DataGlobals::DisplayExtraWarnings;
        using namespace DataSizing;
        using namespace DataHVACGlobals;
        using Fans::FanDesHeatGain;
        using General::RoundSigDigits;
        using General::TrimSigDigits;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyHFnTdbW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using Psychrometrics::PsyTwbFnTdbWPb;
        using Psychrometrics::PsyWFnTdbRhPb;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool IsAutoSize;                           // Indicator to autosize for reporting
        bool HardSizeNoDesRun;                     // Indicator to hardsize with no sizing runs for reporting
        bool SizingDesRunThisAirSys;               // true if a particular air system had a Sizing : System object and system sizing done
        bool SizingDesRunThisZone;                 // true if a particular zone had a Sizing : Zone object and zone sizing was done
        bool SizingDesValueFromParent;             // true if the parent set the design size (whether or not there is a sizing run)
        bool OASysFlag;                            // Logical flag determines if parent object set OA Sys coil property
        bool AirLoopSysFlag;                       // Logical flag determines if parent object set air loop coil property
        bool bCheckForZero;                        // logical to flag whether or not to check for very small autosized values
        Real64 AutosizeDes;                        // autosized value
        Real64 AutosizeUser;                       // user sized value
        Real64 CoilInTemp;                         // entering coil air temperature [C]
        Real64 CoilInHumRat;                       // entering coil air humidity ratio [kg/kg]
        Real64 TotCapTempModFac;                   // DX coil total capacity as a function of temperature curve pointer
        Real64 CoilOutTemp;                        // coil outlet air temperature [C]
        Real64 CoilOutHumRat;                      // coil outlet air humidity ratio [kg/kg]
        Real64 DesVolFlow;                         // coil design air volume flow rate [m3/s]
        Real64 CpAirStd;                           // specific heat of air at standard conditions [J/kg-K]
        int SupFanNum;                             // index to supply fan
        int RetFanNum;                             // index to return fan
        Real64 SupFanDT;                           // supply air fan delta temperature [C]
        Real64 RetFanDT;                           // return air fan delta temperature [C]
        Real64 FanCoolLoad;                        // load due to fan operation added to cooling load [W]
        Array1D<Real64> Par(4);                    // array passed to RegulaFalsi
        std::string ScalableSM;                    // scalable sizing methods label for reporting

        std::string DDNameFanPeak;   // Name of the design day that produced the Peak
        std::string dateTimeFanPeak; // A String representing the DateTime of the Peak

        AutosizeDes = 0.0;
        AutosizeUser = 0.0;
        IsAutoSize = false;
        OASysFlag = false;
        AirLoopSysFlag = false;
        bCheckForZero = true;
        CpAirStd = PsyCpAirFnW(0.0);
        SupFanDT = 0.0;
        RetFanDT = 0.0;
        SupFanNum = 0;
        RetFanNum = 0;
        FanCoolLoad = 0.0;
        SizingDesValueFromParent = false;
        TotCapTempModFac = 1.0;
        CoilOutTemp = -999.0;
        CoilOutHumRat = -999.0;
        DesVolFlow = 0.0;
        CoilInTemp = -999.0;
        CoilInHumRat = -999.0;

        if (SysSizingRunDone || ZoneSizingRunDone) {
            HardSizeNoDesRun = false;
        } else {
            HardSizeNoDesRun = true;
        }

        if (CurSysNum > 0 && CurSysNum <= NumPrimaryAirSys) {
            CheckThisAirSystemForSizing(CurSysNum, SizingDesRunThisAirSys);
            AirLoopSysFlag = UnitarySysEqSizing(CurSysNum).CoolingCapacity ||
                             UnitarySysEqSizing(CurSysNum).HeatingCapacity; // logicals used when parent sizes coil
            if (CurOASysNum > 0) OASysFlag = OASysEqSizing(CurOASysNum).CoolingCapacity || OASysEqSizing(CurOASysNum).HeatingCapacity;
        } else {
            SizingDesRunThisAirSys = false;
        }

        if (CurZoneEqNum > 0) {
            SizingDesValueFromParent = ZoneEqSizing(CurZoneEqNum).DesignSizeFromParent;
            CheckThisZoneForSizing(CurZoneEqNum, SizingDesRunThisZone);
            HardSizeNoDesRun = false;
        } else {
            SizingDesRunThisZone = false;
        }

        if (SizingResult == AutoSize) {
            IsAutoSize = true;
            HardSizeNoDesRun = false;
            if (!SizingDesRunThisAirSys && CurSysNum > 0 && SizingType != AutoCalculateSizing) CheckSysSizing(CompType, CompName);
            if (!SizingDesRunThisZone && CurZoneEqNum > 0 && !SizingDesValueFromParent && SizingType != AutoCalculateSizing)
                CheckZoneSizing(CompType, CompName);
        }

        if (SizingType == AutoCalculateSizing) {
            if (DataFractionUsedForSizing > 0.0) {
                AutosizeDes = DataConstantUsedForSizing * DataFractionUsedForSizing;
                HardSizeNoDesRun = false;
            } else {
                ShowSevereError(CallingRoutine + ' ' + CompType + ' ' + CompName);
                ShowContinueError("... DataConstantUsedForSizing and DataFractionUsedForSizing used for autocalculating " + SizingString +
                                  " must both be greater than 0.");
                ShowFatalError("Preceding conditions cause termination.");
            }
            bCheckForZero = false;
        } else if (CurZoneEqNum > 0) {

            if (!IsAutoSize && !SizingDesRunThisZone && !SizingDesValueFromParent) {
                HardSizeNoDesRun = true;
                AutosizeUser = SizingResult;
                if (PrintWarningFlag && SizingResult > 0.0 && !DataScalableCapSizingON) {
                    if (UtilityRoutines::SameString(CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE") &&
                        SizingType == CoolingAirflowSizing && DataIsDXCoil) {
                        SizingResult /= (1 - DataBypassFrac); // back out bypass fraction applied in GetInput
                        ReportSizingOutput(CompType, CompName, "User-Specified " + SizingString, SizingResult);
                        SizingResult *= (1 - DataBypassFrac); // now reapply for second message and remianing simulation calcs
                        ReportSizingOutput(CompType, CompName, "User-Specified " + SizingString + " (non-bypassed)", SizingResult);
                    } else {
                        ReportSizingOutput(CompType, CompName, "User-Specified " + SizingString, SizingResult);
                    }
                }
            } else {

            }

        } else if (CurSysNum > 0) {
            if (!IsAutoSize && !SizingDesRunThisAirSys) {
                HardSizeNoDesRun = true;
                AutosizeUser = SizingResult;
                if (PrintWarningFlag && SizingResult > 0.0) {
                    if (UtilityRoutines::SameString(CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE") &&
                        SizingType == CoolingAirflowSizing && DataIsDXCoil) {
                    } else {
                        ReportSizingOutput(CompType, CompName, "User-Specified " + SizingString, SizingResult);
                    }
                }
            } else {
            }
        } else {
            // some components don't set CurZoneEqNum or CurSysNum (e.g., Plant HPWH fans)
            HardSizeNoDesRun = true;
            AutosizeDes = DataNonZoneNonAirloopValue;
            if (DataNonZoneNonAirloopValue > 0.0 && IsAutoSize) {
                SizingResult = DataNonZoneNonAirloopValue;
            }
            if (PrintWarningFlag) {
                if (IsAutoSize && SizingResult > 0.0) {
                    ReportSizingOutput(CompType, CompName, "Design Size " + SizingString, SizingResult);
                } else if (SizingResult > 0.0) {
                    AutosizeUser = SizingResult;
                    if ((std::abs(AutosizeDes - AutosizeUser) / AutosizeUser) > AutoVsHardSizingThreshold) {
                        ReportSizingOutput(
                            CompType, CompName, "Design Size " + SizingString, AutosizeDes, "User-Specified " + SizingString, AutosizeUser);
                    } else {
                        ReportSizingOutput(CompType, CompName, "User-Specified " + SizingString, AutosizeUser);
                    }
                    if (DisplayExtraWarnings) {
                        if ((std::abs(AutosizeDes - AutosizeUser) / AutosizeUser) > AutoVsHardSizingThreshold) {
                            ShowMessage(CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName);
                            ShowContinueError("User-Specified " + SizingString + " = " + RoundSigDigits(AutosizeUser, 5));
                            ShowContinueError("differs from Design Size " + SizingString + " = " + RoundSigDigits(AutosizeDes, 5));
                            ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                } else {
                    ShowSevereError(CallingRoutine + ' ' + CompType + ' ' + CompName + ", Developer Error: Component sizing incomplete.");
                    ShowContinueError("SizingString = " + SizingString + ", SizingResult = " + TrimSigDigits(SizingResult, 1));
                    // ShowFatalError( " Previous errors cause program termination" );
                }
            }
        }

        if (IsAutoSize || SizingDesRunThisAirSys || SizingDesRunThisZone) {
            if (AutosizeDes < SmallAirVolFlow && bCheckForZero) {
                AutosizeDes = 0.0;
            }
        }

        if (SizingResult == AutoSize) {
            if (DataEMSOverrideON) {
                SizingResult = DataEMSOverride;
            } else {
                SizingResult = AutosizeDes;
            }
        } else if (DataScalableSizingON || DataScalableCapSizingON) {
            if (DataEMSOverrideON) {
                SizingResult = DataEMSOverride;
            } else {
                AutosizeUser = SizingResult;
            }
        } else {
            AutosizeUser = SizingResult;
        }

        //		if ( PrintWarningFlag ) { // these special if tests inside here are only executed when PrintWarningFlag is true, but we might
        // want to get the size without printing
        if (!HardSizeNoDesRun || DataScalableSizingON || DataScalableCapSizingON) {
            if (IsAutoSize) { // Design Size values are available for both autosized and hard - sized
                // check capacity to make sure design volume flow per total capacity is within range

                // Note: the VolFlowPerRatedTotCap check is not applicable for VRF-FluidTCtrl coil model, which implements variable flow fans and
                // determines capacity using physical calculations instead of emperical curves
                bool FlagCheckVolFlowPerRatedTotCap = true;
                if (UtilityRoutines::SameString(CompType, "Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl") ||
                    UtilityRoutines::SameString(CompType, "Coil:Heating:DX:VariableRefrigerantFlow:FluidTemperatureControl"))
                    FlagCheckVolFlowPerRatedTotCap = false;

                if (DataAutosizable && AutosizeUser > 0.0 && AutosizeDes > 0.0 && PrintWarningFlag) {
                    if (UtilityRoutines::SameString(CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE") &&
                        DataIsDXCoil) {
                        ReportSizingOutput(
                            CompType, CompName, "Design Size " + SizingString, AutosizeDes, "User-Specified " + SizingString, AutosizeUser);
                        SizingResult *= (1 - DataBypassFrac); // now apply bypass fraction for second message and remaining simulation calcs
                        AutosizeUser *= (1 - DataBypassFrac); // now apply bypass fraction for second message and remaining simulation calcs
                        ReportSizingOutput(CompType,
                                           CompName,
                                           "Design Size " + SizingString + " ( non-bypassed )",
                                           AutosizeDes,
                                           "User-Specified " + SizingString + " ( non-bypassed )",
                                           AutosizeUser);
                    } else {
                        ReportSizingOutput(
                            CompType, CompName, "Design Size " + SizingString, AutosizeDes, "User-Specified " + SizingString, AutosizeUser);
                    }
                    if (DisplayExtraWarnings && PrintWarningFlag) {
                        if ((std::abs(AutosizeDes - AutosizeUser) / AutosizeUser) > AutoVsHardSizingThreshold) {
                            ShowMessage(CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName);
                            ShowContinueError("User-Specified " + SizingString + " = " + RoundSigDigits(AutosizeUser, 5));
                            ShowContinueError("differs from Design Size " + SizingString + " = " + RoundSigDigits(AutosizeDes, 5));
                            ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                } else if ((DataScalableSizingON || DataScalableCapSizingON) && AutosizeDes > 0.0 && PrintWarningFlag) {
                    if (UtilityRoutines::SameString(CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE") &&
                        SizingType == CoolingAirflowSizing && DataIsDXCoil) {
                        if (DataAutosizable) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString, SizingResult);
                        SizingResult *= (1 - DataBypassFrac); // now apply bypass fraction for second message and remaining simulation calcs
                        if (DataAutosizable) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString + " ( non-bypassed )", SizingResult);
                    } else {
                        if (DataAutosizable) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString, SizingResult);
                    }
                } else if (PrintWarningFlag) {
                    if (UtilityRoutines::SameString(CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE") &&
                        SizingType == CoolingAirflowSizing && DataIsDXCoil) {
                        if (DataAutosizable) ReportSizingOutput(CompType, CompName, "Design Size " + SizingString, SizingResult);
                        SizingResult *= (1 - DataBypassFrac); // now apply bypass fraction for second message and remaining simulation calcs
                        if (DataAutosizable)
                            ReportSizingOutput(CompType, CompName, "Design Size " + SizingString + " ( non-bypassed )", SizingResult);
                    } else {
                        if (DataAutosizable && PrintWarningFlag) ReportSizingOutput(CompType, CompName, "Design Size " + SizingString, SizingResult);
                    }
                }
            } else {
                if (DataAutosizable && AutosizeUser > 0.0 && AutosizeDes > 0.0 && PrintWarningFlag &&
                    !(DataScalableSizingON || DataScalableCapSizingON)) {
                    if ((std::abs(AutosizeDes - AutosizeUser) / AutosizeUser) > AutoVsHardSizingThreshold) {
                        ReportSizingOutput(
                            CompType, CompName, "Design Size " + SizingString, AutosizeDes, "User-Specified " + SizingString, AutosizeUser);
                    } else {
                        ReportSizingOutput(CompType, CompName, "User-Specified " + SizingString, AutosizeUser);
                    }
                    if (DisplayExtraWarnings) {
                        if ((std::abs(AutosizeDes - AutosizeUser) / AutosizeUser) > AutoVsHardSizingThreshold) {
                            ShowMessage(CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName);
                            ShowContinueError("User-Specified " + SizingString + " = " + RoundSigDigits(AutosizeUser, 5));
                            ShowContinueError("differs from Design Size " + SizingString + " = " + RoundSigDigits(AutosizeDes, 5));
                            ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                } else if (DataAutosizable && AutosizeUser > 0.0 && AutosizeDes > 0.0 && PrintWarningFlag &&
                           (DataScalableSizingON || DataScalableCapSizingON)) {
                    ReportSizingOutput(CompType, CompName, "Design Size " + SizingString, AutosizeDes, ScalableSM + SizingString, AutosizeUser);
                    if (DisplayExtraWarnings) {
                        if ((std::abs(AutosizeDes - AutosizeUser) / AutosizeUser) > AutoVsHardSizingThreshold) {
                            ShowMessage(CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName);
                            ShowContinueError(ScalableSM + SizingString + " = " + RoundSigDigits(AutosizeUser, 5));
                            ShowContinueError("differs from Design Size " + SizingString + " = " + RoundSigDigits(AutosizeDes, 5));
                            ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                } else if ((DataScalableSizingON || DataScalableCapSizingON) && AutosizeDes > 0.0 && PrintWarningFlag) {
                    if (UtilityRoutines::SameString(CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE") &&
                        SizingType == CoolingAirflowSizing && DataIsDXCoil) {
                        if (DataAutosizable) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString, SizingResult);
                        SizingResult *= (1 - DataBypassFrac); // now apply bypass fraction for second message and remaining simulation calcs
                        if (DataAutosizable) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString + " ( non-bypassed )", SizingResult);
                    } else {
                        if (DataAutosizable) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString, SizingResult);
                    }
                } else if ((DataScalableSizingON || DataScalableCapSizingON) && AutosizeUser > 0.0 && PrintWarningFlag) {
                    if (UtilityRoutines::SameString(CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE") &&
                        SizingType == CoolingAirflowSizing && DataIsDXCoil) {
                        if (DataAutosizable) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString, SizingResult);
                        SizingResult *= (1 - DataBypassFrac); // now apply bypass fraction for second message and remaining simulation calcs
                        if (DataAutosizable) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString + " ( non-bypassed )", SizingResult);
                    } else {
                        if (DataAutosizable) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString, SizingResult);
                    }
                } else if (PrintWarningFlag) {
                    if (UtilityRoutines::SameString(CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE") &&
                        SizingType == CoolingAirflowSizing && DataIsDXCoil) {
                        if (DataAutosizable) ReportSizingOutput(CompType, CompName, "User-Specified " + SizingString, SizingResult);
                        SizingResult *= (1 - DataBypassFrac); // now apply bypass fraction for second message and remaining simulation calcs
                        if (DataAutosizable)
                            ReportSizingOutput(CompType, CompName, "User-Specified " + SizingString + " (non-bypassed)", SizingResult);
                    } else {
                        if (DataAutosizable && PrintWarningFlag)
                            ReportSizingOutput(CompType, CompName, "User-Specified " + SizingString, SizingResult);
                    }
                }
            }
            //			} else {
            //				eventually move hardsize reporting here? [up in calcs, 3 places at e.g., if ( !IsAutoSize &&
            //! SizingDesRunThisAirSys
            //)]
        }
        //		}
        // Coil summary Reporting
    }

    void GetCoilDesFlowT(int SysNum,           // central air system index
                         Real64 CpAir,         // specific heat to be used in calculations [J/kgC]
                         Real64 &DesFlow,      // returned design mass flow [kg/s]
                         Real64 &DesExitTemp,  // returned design coil exit temperature [kg/s]
                         Real64 &DesExitHumRat // returned design coil exit humidity ratio [kg/kg]
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   September 2014
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function calculates the coil design air flow rate and exit temperature depending on the
        // cooling capacity control method

        // METHODOLOGY EMPLOYED:
        // energy and mass flow balance

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataSizing;
        using DataEnvironment::StdRhoAir;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int DDAtSensPeak(0);
        int TimeStepAtSensPeak(0);
        int DDAtFlowPeak(0);
        int TimeStepAtFlowPeak(0);
        int CoolCapCtrl; // type of coil capacity control
        int PeakLoadType;
        int DDAtTotPeak(0);
        int TimeStepAtTotPeak(0);
        int TimeStepAtPeak(0);
        Real64 ZoneCoolLoadSum(0); // sum of zone cooling loads at the peak [W]
        Real64 AvgZoneTemp(0);     // average zone temperature [C]
        Real64 AvgSupTemp(0.0);    // average supply temperature for bypass control [C]
        Real64 TotFlow(0.0);       // total flow for bypass control [m3/s]
        Real64 MixTemp(0.0);       // mixed air temperature at the peak [C]

        CoolCapCtrl = SysSizInput(SysNum).CoolCapControl;
        PeakLoadType = SysSizInput(SysNum).CoolingPeakLoadType;
        DDAtSensPeak = SysSizPeakDDNum(SysNum).SensCoolPeakDD;
        if (DDAtSensPeak > 0) {
            TimeStepAtSensPeak = SysSizPeakDDNum(SysNum).TimeStepAtSensCoolPk(DDAtSensPeak);
            DDAtFlowPeak = SysSizPeakDDNum(SysNum).CoolFlowPeakDD;
            TimeStepAtFlowPeak = SysSizPeakDDNum(SysNum).TimeStepAtCoolFlowPk(DDAtFlowPeak);
            DDAtTotPeak = SysSizPeakDDNum(SysNum).TotCoolPeakDD;
            TimeStepAtTotPeak = SysSizPeakDDNum(SysNum).TimeStepAtTotCoolPk(DDAtTotPeak);

            if (PeakLoadType == TotalCoolingLoad) {
                TimeStepAtPeak = TimeStepAtTotPeak;
            } else {
                TimeStepAtPeak = TimeStepAtSensPeak;
            }
        } else {
            if ((CoolCapCtrl == VT) || (CoolCapCtrl == Bypass)) {
                ShowWarningError("GetCoilDesFlow: AirLoopHVAC=" + SysSizInput(SysNum).AirPriLoopName +
                                 "has no time of peak cooling load for sizing.");
                ShowContinueError("Using Central Cooling Capacity Control Method=VAV instead of Bypass or VT.");
                CoolCapCtrl = VAV;
            }
        }

        if (CoolCapCtrl == VAV) {
            DesExitTemp = FinalSysSizing(SysNum).CoolSupTemp;
            DesFlow = FinalSysSizing(SysNum).MassFlowAtCoolPeak / StdRhoAir;
            DesExitHumRat = FinalSysSizing(SysNum).CoolSupHumRat;
        } else if (CoolCapCtrl == OnOff) {
            DesExitTemp = FinalSysSizing(SysNum).CoolSupTemp;
            DesFlow = DataAirFlowUsedForSizing;
            DesExitHumRat = FinalSysSizing(SysNum).CoolSupHumRat;
        } else if (CoolCapCtrl == VT) {
            if (FinalSysSizing(SysNum).CoolingPeakLoadType == SensibleCoolingLoad) {
                ZoneCoolLoadSum = CalcSysSizing(SysNum).SumZoneCoolLoadSeq(TimeStepAtPeak);
                AvgZoneTemp = CalcSysSizing(SysNum).CoolZoneAvgTempSeq(TimeStepAtPeak);
            } else if (FinalSysSizing(SysNum).CoolingPeakLoadType == TotalCoolingLoad) {
                ZoneCoolLoadSum = CalcSysSizing(SysNum).SumZoneCoolLoadSeq(TimeStepAtPeak);
                AvgZoneTemp = CalcSysSizing(SysNum).CoolZoneAvgTempSeq(TimeStepAtPeak);
            }
            DesExitTemp =
                max(FinalSysSizing(SysNum).CoolSupTemp, AvgZoneTemp - ZoneCoolLoadSum / (StdRhoAir * CpAir * FinalSysSizing(SysNum).DesCoolVolFlow));
            DesFlow = FinalSysSizing(SysNum).DesCoolVolFlow;
            DesExitHumRat = Psychrometrics::PsyWFnTdbRhPb(DesExitTemp, 0.9, DataEnvironment::StdBaroPress, "GetCoilDesFlowT");
        } else if (CoolCapCtrl == Bypass) {
            if (FinalSysSizing(SysNum).CoolingPeakLoadType == SensibleCoolingLoad) {
                ZoneCoolLoadSum = CalcSysSizing(SysNum).SumZoneCoolLoadSeq(TimeStepAtPeak);
                AvgZoneTemp = CalcSysSizing(SysNum).CoolZoneAvgTempSeq(TimeStepAtPeak);
            } else if (FinalSysSizing(SysNum).CoolingPeakLoadType == TotalCoolingLoad) {
                ZoneCoolLoadSum = CalcSysSizing(SysNum).SumZoneCoolLoadSeq(TimeStepAtPeak);
                AvgZoneTemp = CalcSysSizing(SysNum).CoolZoneAvgTempSeq(TimeStepAtPeak);
            }
            AvgSupTemp = AvgZoneTemp - ZoneCoolLoadSum / (StdRhoAir * CpAir * FinalSysSizing(SysNum).DesCoolVolFlow);
            TotFlow = FinalSysSizing(SysNum).DesCoolVolFlow;
            MixTemp = CalcSysSizing(SysNum).MixTempAtCoolPeak;
            DesExitTemp = FinalSysSizing(SysNum).CoolSupTemp;
            if (MixTemp > DesExitTemp) {
                DesFlow = TotFlow * max(0.0, min(1.0, ((MixTemp - AvgSupTemp) / (MixTemp - DesExitTemp))));
            } else {
                DesFlow = TotFlow;
            }
            DesExitHumRat = Psychrometrics::PsyWFnTdbRhPb(DesExitTemp, 0.9, DataEnvironment::StdBaroPress, "GetCoilDesFlowT");
        }
    }

    Real64 setOAFracForZoneEqSizing(Real64 const &desMassFlow, DataSizing::ZoneEqSizingData const &zoneEqSizing)
    {
        Real64 outAirFrac = 0.0;
        if (desMassFlow <= 0.0) return outAirFrac;

        if (zoneEqSizing.ATMixerVolFlow > 0.0) {
            // set central DOAS AT mixer OA fraction
            outAirFrac = min(DataEnvironment::StdRhoAir * zoneEqSizing.ATMixerVolFlow / desMassFlow, 1.0);
        } else if (zoneEqSizing.OAVolFlow > 0.0) { // set zone equipment OA fraction
            outAirFrac = min(DataEnvironment::StdRhoAir * zoneEqSizing.OAVolFlow / desMassFlow, 1.0);
        }
        return outAirFrac;
    }

    Real64 setHeatCoilInletTempForZoneEqSizing(Real64 const &outAirFrac,
                                               DataSizing::ZoneEqSizingData const &zoneEqSizing,
                                               DataSizing::ZoneSizingData const &finalZoneSizing)
    {
        Real64 coilInTemp = 0.0;
        if (zoneEqSizing.ATMixerVolFlow > 0.0) {
            // adjust for central DOAS AT mixer mixed inlet temp
            coilInTemp = (1.0 - outAirFrac) * finalZoneSizing.ZoneRetTempAtHeatPeak + outAirFrac * zoneEqSizing.ATMixerHeatPriDryBulb;
        } else if (zoneEqSizing.OAVolFlow > 0.0) {
            // adjust for raw OA mixed inlet temp
            coilInTemp = (1.0 - outAirFrac) * finalZoneSizing.ZoneTempAtHeatPeak + outAirFrac * finalZoneSizing.OutTempAtHeatPeak;
        } else {
            // use zone condition for sizing zone equipment
            coilInTemp = finalZoneSizing.ZoneTempAtHeatPeak;
        }
        return coilInTemp;
    }

    Real64 setHeatCoilInletHumRatForZoneEqSizing(Real64 const &outAirFrac,
                                                 DataSizing::ZoneEqSizingData const &zoneEqSizing,
                                                 DataSizing::ZoneSizingData const &finalZoneSizing)
    {
        Real64 coilInHumRat = 0.0;
        if (zoneEqSizing.ATMixerVolFlow > 0.0) {
            // adjust for central DOAS AT mixer mixed inlet humrat
            coilInHumRat = (1.0 - outAirFrac) * finalZoneSizing.ZoneHumRatAtHeatPeak + outAirFrac * zoneEqSizing.ATMixerHeatPriHumRat;
        } else if (zoneEqSizing.OAVolFlow > 0.0) { // adjust for raw OA mixed inlet humrat
            coilInHumRat = (1.0 - outAirFrac) * finalZoneSizing.ZoneHumRatAtHeatPeak + outAirFrac * finalZoneSizing.OutHumRatAtHeatPeak;
        } else {
            coilInHumRat = finalZoneSizing.ZoneHumRatAtHeatPeak;
        }
        return coilInHumRat;
    }

    Real64 setCoolCoilInletTempForZoneEqSizing(Real64 const &outAirFrac,
                                               DataSizing::ZoneEqSizingData const &zoneEqSizing,
                                               DataSizing::ZoneSizingData const &finalZoneSizing)
    {
        Real64 coilInTemp = 0.0;
        if (zoneEqSizing.ATMixerVolFlow > 0.0) {
            // adjust for central DOAS AT mixer mixed inlet temp
            coilInTemp = (1.0 - outAirFrac) * finalZoneSizing.ZoneRetTempAtCoolPeak + outAirFrac * zoneEqSizing.ATMixerCoolPriDryBulb;
        } else if (zoneEqSizing.OAVolFlow > 0.0) {
            // adjust for raw OA mixed inlet temp
            coilInTemp = (1.0 - outAirFrac) * finalZoneSizing.ZoneTempAtCoolPeak + outAirFrac * finalZoneSizing.OutTempAtCoolPeak;
        } else {
            // use zone condition for sizing zone equipment
            coilInTemp = finalZoneSizing.ZoneTempAtCoolPeak;
        }
        return coilInTemp;
    }

    Real64 setCoolCoilInletHumRatForZoneEqSizing(Real64 const &outAirFrac,
                                                 DataSizing::ZoneEqSizingData const &zoneEqSizing,
                                                 DataSizing::ZoneSizingData const &finalZoneSizing)
    {
        Real64 coilInHumRat = 0.0;
        if (zoneEqSizing.ATMixerVolFlow > 0.0) {
            // adjust for central DOAS AT mixer mixed inlet humrat
            coilInHumRat = (1.0 - outAirFrac) * finalZoneSizing.ZoneHumRatAtCoolPeak + outAirFrac * zoneEqSizing.ATMixerCoolPriHumRat;
        } else if (zoneEqSizing.OAVolFlow > 0.0) { // adjust for raw OA mixed inlet humrat
            coilInHumRat = (1.0 - outAirFrac) * finalZoneSizing.ZoneHumRatAtCoolPeak + outAirFrac * finalZoneSizing.OutHumRatAtCoolPeak;
        } else {
            coilInHumRat = finalZoneSizing.ZoneHumRatAtCoolPeak;
        }
        return coilInHumRat;
    }

} // namespace ReportSizingManager

} // namespace EnergyPlus
