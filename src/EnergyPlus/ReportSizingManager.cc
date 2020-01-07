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
#include <EnergyPlus/AirLoopHVACDOAS.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DesiccantDehumidifiers.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/SQLiteProcedures.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>
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
        using DataGlobals::OutputFileInits;
        using namespace OutputReportPredefined;
        using General::RoundSigDigits;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static bool MyOneTimeFlag(true);

        // Formats
        static ObjexxFCL::gio::Fmt Format_990(
            "('! <Component Sizing Information>, Component Type, Component Name, ','Input Field Description, Value')");
        static ObjexxFCL::gio::Fmt Format_991("(' Component Sizing Information, ',A,', ',A,', ',A,', ',A)");

        if (MyOneTimeFlag) {
            ObjexxFCL::gio::write(OutputFileInits, Format_990);
            MyOneTimeFlag = false;
        }

        ObjexxFCL::gio::write(OutputFileInits, Format_991) << CompType << CompName << VarDesc << RoundSigDigits(VarValue, 5);
        // add to tabular output reports
        AddCompSizeTableEntry(CompType, CompName, VarDesc, VarValue);

        if (present(UsrDesc) && present(UsrValue)) {
            ObjexxFCL::gio::write(OutputFileInits, Format_991) << CompType << CompName << UsrDesc << RoundSigDigits(UsrValue, 5);
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

    void RequestSizing(std::string const &CompType,      // type of component
                       std::string const &CompName,      // name of component
                       int const SizingType,             // integerized type of sizing requested (see DataHVACGlobals, e.g. CoolingCapacitySizing)
                       std::string const &SizingString,  // string containing info for eio report
                       Real64 &SizingResult,             // result of the sizing procedure
                       bool const PrintWarningFlag,      // TRUE when requesting output (eio) reporting
                       std::string const &CallingRoutine // name of calling routine for warning messages
    )
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
        using DataAirLoop::AirLoopControlInfo;
        using DataAirSystems::PrimaryAirSystem;
        using DataEnvironment::StdBaroPress;
        using DataEnvironment::StdRhoAir;
        using DataGlobals::DisplayExtraWarnings;
        using namespace DataSizing;
        using namespace DataHVACGlobals;
        using DataPlant::PlantLoop;
        using DesiccantDehumidifiers::DesicDehum;
        using DXCoils::ValidateADP;
        using Fans::FanDesDT;
        using Fans::FanDesHeatGain;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using General::RoundSigDigits;
        using General::SolveRoot;
        using General::TrimSigDigits;
        using Psychrometrics::PsyCpAirFnWTdb;
        using Psychrometrics::PsyHFnTdbW;
        using Psychrometrics::PsyRhFnTdbWPb;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using Psychrometrics::PsyTdbFnHW;
        using Psychrometrics::PsyTdpFnWPb;
        using Psychrometrics::PsyTwbFnTdbWPb;
        using Psychrometrics::PsyWFnTdbH;
        using Psychrometrics::PsyWFnTdbRhPb;
        using Psychrometrics::PsyWFnTdpPb;
        using WaterCoils::SimpleHeatingCoilUAResidual;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const Acc(0.0001); // Accuracy of result
        int const MaxIte(500);    // Maximum number of iterations

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DDNum;                                 // design day number corresponding to TimeStepNumAtMax
        int SolFla;                                // Flag of solver
        int TimeStepNumAtMax;                      // time step number at max load
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
        Real64 OutTemp;                            // outdoor air temperature [C]
        Real64 CoilInTemp;                         // entering coil air temperature [C]
        Real64 CoilInHumRat;                       // entering coil air humidity ratio [kg/kg]
        Real64 CoilInWetBulb;                      // entering coil air wet-bulb temperature [C]
        Real64 TotCapTempModFac;                   // DX coil total capacity as a function of temperature curve pointer
        Real64 CoilInEnth;                         // entering coil air enthalpy [J/kg]
        Real64 CoilOutTemp;                        // coil outlet air temperature [C]
        Real64 CoilOutHumRat;                      // coil outlet air humidity ratio [kg/kg]
        Real64 CoilOutEnth;                        // coil outlet air enthalpy [J/kg]
        Real64 DesCoilLoad;                        // design coil load based on sizing inputs and entering air conditions [W]
        Real64 PeakCoilLoad;                       // adjusted coil size based on TotCapTempModFac [W]
        Real64 DesVolFlow;                         // coil design air volume flow rate [m3/s]
        Real64 DesMassFlow;                        // coil design mass flow rate [kg/s]
        Real64 CpAir;                              // specific heat of air [J/kg-K]
        Real64 rhoair;                             // density of air [kg/m3]
        Real64 OutAirFrac;                         // outdoor air fraction
        Real64 MinPriFlowFrac;                     // minimum primary air flow fraction for induction units
        Real64 CpAirStd;                           // specific heat of air at standard conditions [J/kg-K]
        Real64 NominalCapacityDes;                 // Autosized nominal capacity for reporting [W]
        Real64 RatedVolFlowPerRatedTotCap;         // ratio of volume flow rate to capacity [m3/W]
        Real64 CoilDesWaterDeltaT;                 // water coil delta T used for sizing [C]
        Real64 Cp;                                 // water loop fluid specific heat [J/kgK]
        Real64 rho;                                // water loop fluid density [kg/m3]
        Real64 DesSatEnthAtWaterInTemp;            // temp variable used for warning messages
        Real64 DesHumRatAtWaterInTemp;             // temp variable used for warning messages
        Real64 T1Out;                              // water coil air outlet temperature [C]
        Real64 T2Out;                              // water coil water outlet temperature [C]
        Real64 UA0;                                // lower bound of UA for autosizing
        Real64 UA1;                                // upper bound of UA for autosizing
        Real64 MinFlowFrac;                        // minimum flow fraction from terminal unit []
        Real64 TDpIn;                              // coil inlet air dew point temperature [C]
        int SupFanNum;                             // index to supply fan
        int RetFanNum;                             // index to return fan
        Real64 SupFanDT;                           // supply air fan delta temperature [C]
        Real64 RetFanDT;                           // return air fan delta temperature [C]
        Real64 FanCoolLoad;                        // load due to fan operation added to cooling load [W]
        Array1D<Real64> Par(4);                    // array passed to RegulaFalsi
        std::string ScalableSM;                    // scalable sizing methods label for reporting
        Real64 const RatedInletAirTemp(26.6667);   // 26.6667C or 80F
        Real64 const RatedInletAirHumRat(0.01125); // Humidity ratio corresponding to 80F dry bulb/67F wet bulb

        std::string DDNameFanPeak;   // Name of the design day that produced the Peak
        std::string dateTimeFanPeak; // A String representing the DateTime of the Peak
        Real64 DXFlowPerCapMinRatio(1.0);
        Real64 DXFlowPerCapMaxRatio(1.0);

        AutosizeDes = 0.0;
        AutosizeUser = 0.0;
        IsAutoSize = false;
        OASysFlag = false;
        AirLoopSysFlag = false;
        bCheckForZero = true;
        CpAirStd = PsyCpAirFnWTdb(0.0, 20.0);
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

                int tempZoneNum = CurZoneEqNum;
                bool FinalZoneSizingNotAllocated = false;
                if (!allocated(DataSizing::FinalZoneSizing)) {
                    DataSizing::FinalZoneSizing.allocate(1);
                    tempZoneNum = 1;
                    FinalZoneSizingNotAllocated = true;
                }
                DataSizing::ZoneEqSizingData &zoneEqSizing = DataSizing::ZoneEqSizing(CurZoneEqNum);
                DataSizing::ZoneSizingData &finalZoneSizing = DataSizing::FinalZoneSizing(tempZoneNum);

                if (SizingType == SystemAirflowSizing) {

                    {
                        auto const SELECT_CASE_var(ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingType));
                        if ((SELECT_CASE_var == SupplyAirFlowRate) || (SELECT_CASE_var == None)) {

                            if (ZoneEqSizing(CurZoneEqNum).SystemAirFlow) {
                                AutosizeDes = max(ZoneEqSizing(CurZoneEqNum).AirVolFlow,
                                                  FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow,
                                                  FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow);
                                if (AutosizeDes == FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                } else if (AutosizeDes == ZoneEqSizing(CurZoneEqNum).AirVolFlow) {
                                    DDNameFanPeak = "Unknown";
                                }
                            } else {
                                if (ZoneCoolingOnlyFan) {
                                    AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (ZoneHeatingOnlyFan) {
                                    AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                    AutosizeDes = ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow;
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                    AutosizeDes = ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow;
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                    AutosizeDes = max(ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow, ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow);
                                    if (AutosizeDes == ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow) {
                                        if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                            FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                            DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                            dateTimeFanPeak =
                                                General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) +
                                                "/" +
                                                General::TrimSigDigits(
                                                    WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                                " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                        }
                                    } else if (AutosizeDes == ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow) {
                                        if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                            FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                            DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                            dateTimeFanPeak =
                                                General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) +
                                                "/" +
                                                General::TrimSigDigits(
                                                    WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                                " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                        }
                                    }
                                } else {
                                    AutosizeDes = max(FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow, FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow);
                                    if (AutosizeDes == FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow) {
                                        if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                            FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                            // This block of code is entered in Test ReportSizingManager.unit.cc::ReportSizingManager_FanPeak
                                            DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                            dateTimeFanPeak =
                                                General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) +
                                                "/" +
                                                General::TrimSigDigits(
                                                    WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                                " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                        }
                                    } else if (AutosizeDes == FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow) {
                                        if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                            FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                            DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                            dateTimeFanPeak =
                                                General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) +
                                                "/" +
                                                General::TrimSigDigits(
                                                    WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                                " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                        }
                                    }
                                }
                            }
                        } else if (SELECT_CASE_var == FractionOfAutosizedCoolingAirflow) {
                            if (ZoneCoolingOnlyFan) {
                                AutosizeDes = DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneHeatingOnlyFan) {
                                AutosizeDes = DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                AutosizeDes = DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = max(DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow,
                                                  DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow);
                                if (AutosizeDes == DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            } else {
                                AutosizeDes = max(DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow,
                                                  DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow);
                                if (AutosizeDes == DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            }
                        } else if (SELECT_CASE_var == FractionOfAutosizedHeatingAirflow) {
                            if (ZoneCoolingOnlyFan) {
                                AutosizeDes = DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneHeatingOnlyFan) {
                                AutosizeDes = DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                AutosizeDes = DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = max(DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow,
                                                  DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow);
                                if (AutosizeDes == DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            } else {
                                AutosizeDes = max(DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow,
                                                  DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow);
                                if (AutosizeDes == DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            }
                        } else if (SELECT_CASE_var == FlowPerCoolingCapacity) {
                            if (ZoneCoolingOnlyFan) {
                                AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneHeatingOnlyFan) {
                                AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = max(DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity,
                                                  DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity);
                                if (AutosizeDes == DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            } else {
                                AutosizeDes = max(DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity,
                                                  DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity);
                                if (AutosizeDes == DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            }
                        } else if (SELECT_CASE_var == FlowPerHeatingCapacity) {
                            if (ZoneCoolingOnlyFan) {
                                AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneHeatingOnlyFan) {
                                AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = max(DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity,
                                                  DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity);
                                if (AutosizeDes == DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            } else {
                                AutosizeDes = max(DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity,
                                                  DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity);
                                if (AutosizeDes == DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            }
                        } else {
                            if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                AutosizeDes = ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = max(ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow, ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow);
                                if (AutosizeDes == ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            } else {
                                if (ZoneEqSizing(CurZoneEqNum).SystemAirFlow) {
                                    AutosizeDes = max(ZoneEqSizing(CurZoneEqNum).AirVolFlow,
                                                      ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow,
                                                      ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow);
                                } else if (ZoneCoolingOnlyFan) {
                                    AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (ZoneHeatingOnlyFan) {
                                    AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                } else {
                                    AutosizeDes = max(FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow, FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow);
                                    if (AutosizeDes == FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow) {
                                        if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                            FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                            DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                            dateTimeFanPeak =
                                                General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) +
                                                "/" +
                                                General::TrimSigDigits(
                                                    WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                                " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                        }
                                    } else if (AutosizeDes == FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow) {
                                        if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                            FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                            DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                            dateTimeFanPeak =
                                                General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) +
                                                "/" +
                                                General::TrimSigDigits(
                                                    WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                                " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                        }
                                    }
                                }
                            }
                            if (DataFractionUsedForSizing > 0.0) AutosizeDes = AutosizeDes * DataFractionUsedForSizing;
                        }
                    }
                } else if (SizingType == CoolingAirflowSizing) {
                    {
                        auto const SELECT_CASE_var(ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingType));
                        if ((SELECT_CASE_var == SupplyAirFlowRate) || (SELECT_CASE_var == None) || (SELECT_CASE_var == FlowPerFloorArea)) {
                            if (ZoneEqSizing(CurZoneEqNum).SystemAirFlow) {
                                AutosizeDes = max(ZoneEqSizing(CurZoneEqNum).AirVolFlow,
                                                  ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow,
                                                  ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow);
                                // what is this IF block for? Already inside of (SizingType == CoolingAirFlowSizing).
                                if (SizingType == CoolingAirflowSizing) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (SizingType == HeatingAirflowSizing) {

                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                } else {

                                    if (AutosizeDes == ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow) {
                                        if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                            FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                            DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                            dateTimeFanPeak =
                                                General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) +
                                                "/" +
                                                General::TrimSigDigits(
                                                    WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                                " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                        }
                                    } else if (AutosizeDes == ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow) {
                                        if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                            FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                            DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                            dateTimeFanPeak =
                                                General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) +
                                                "/" +
                                                General::TrimSigDigits(
                                                    WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                                " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                        }
                                    } else if (AutosizeDes == ZoneEqSizing(CurZoneEqNum).AirVolFlow) {
                                        DDNameFanPeak = "Unknown";
                                    }
                                }
                            } else {
                                if (ZoneCoolingOnlyFan) {
                                    AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (ZoneHeatingOnlyFan) {
                                    AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                    AutosizeDes = ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow;
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                    AutosizeDes = ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow;
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                    AutosizeDes = max(ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow, ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow);
                                    if (AutosizeDes == ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow) {
                                        if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                            FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                            DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                            dateTimeFanPeak =
                                                General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) +
                                                "/" +
                                                General::TrimSigDigits(
                                                    WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                                " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                        }
                                    } else if (AutosizeDes == ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow) {
                                        if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                            FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                            DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                            dateTimeFanPeak =
                                                General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) +
                                                "/" +
                                                General::TrimSigDigits(
                                                    WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                                " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                        }
                                    }
                                } else {
                                    AutosizeDes = max(FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow, FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow);
                                    if (AutosizeDes == FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow) {
                                        if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                            FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                            DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                            dateTimeFanPeak =
                                                General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) +
                                                "/" +
                                                General::TrimSigDigits(
                                                    WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                                " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                        }
                                    } else if (AutosizeDes == FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow) {
                                        if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                            FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                            DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                            dateTimeFanPeak =
                                                General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) +
                                                "/" +
                                                General::TrimSigDigits(
                                                    WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                                " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                        }
                                    }
                                }
                            }
                        } else if (SELECT_CASE_var == FractionOfAutosizedCoolingAirflow) {
                            if (ZoneCoolingOnlyFan) {
                                AutosizeDes = DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneHeatingOnlyFan) {
                                AutosizeDes = DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                AutosizeDes = DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = max(DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow,
                                                  DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow);
                                if (AutosizeDes == DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            } else {
                                AutosizeDes = max(DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow,
                                                  DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow);
                                if (AutosizeDes == DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            }
                        } else if (SELECT_CASE_var == FractionOfAutosizedHeatingAirflow) {
                            if (ZoneCoolingOnlyFan) {
                                AutosizeDes = DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneHeatingOnlyFan) {
                                AutosizeDes = DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                AutosizeDes = DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = max(DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow,
                                                  DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow);
                                if (AutosizeDes == DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            } else {
                                AutosizeDes = max(DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow,
                                                  DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow);
                                if (AutosizeDes == DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            }
                        } else if (SELECT_CASE_var == FlowPerCoolingCapacity) {
                            if (ZoneCoolingOnlyFan) {
                                AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneHeatingOnlyFan) {
                                AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = max(DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity,
                                                  DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity);
                                if (AutosizeDes == DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            } else {
                                AutosizeDes = max(DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity,
                                                  DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity);
                                if (AutosizeDes == DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            }
                        } else if (SELECT_CASE_var == FlowPerHeatingCapacity) {
                            if (ZoneCoolingOnlyFan) {
                                AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneHeatingOnlyFan) {
                                AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
                                if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                    FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                        " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                }
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = max(DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity,
                                                  DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity);
                                if (AutosizeDes == DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            } else {
                                AutosizeDes = max(DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity,
                                                  DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity);
                                if (AutosizeDes == DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).CoolDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).CoolDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax);
                                    }
                                } else if (AutosizeDes == DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity) {
                                    if (FinalZoneSizing(CurZoneEqNum).HeatDDNum > 0 &&
                                        FinalZoneSizing(CurZoneEqNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtHeatMax);
                                    }
                                }
                            }
                        } else {
                            if (ZoneCoolingOnlyFan) {
                                AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                            } else if (TermUnitIU && (CurTermUnitSizingNum > 0)) {
                                AutosizeDes = TermUnitSizing(CurTermUnitSizingNum).AirVolFlow;
                            } else if (ZoneEqFanCoil) {
                                AutosizeDes = ZoneEqSizing(CurZoneEqNum).AirVolFlow;
                            } else if (ZoneHeatingOnlyFan) {
                                AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                            } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow;
                            } else {
                                AutosizeDes = max(FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow, FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow);
                            }
                        }
                    }
                } else if (SizingType == HeatingAirflowSizing) {
                    {
                        auto const SELECT_CASE_var(ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingType));
                        if ((SELECT_CASE_var == SupplyAirFlowRate) || (SELECT_CASE_var == None) || (SELECT_CASE_var == FlowPerFloorArea)) {
                            if (ZoneEqSizing(CurZoneEqNum).SystemAirFlow) {
                                AutosizeDes = max(ZoneEqSizing(CurZoneEqNum).AirVolFlow,
                                                  ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow,
                                                  ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow);
                            } else {
                                if (ZoneCoolingOnlyFan) {
                                    AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                                } else if (ZoneHeatingOnlyFan) {
                                    AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                    AutosizeDes = ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow;
                                } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                    AutosizeDes = ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow;
                                } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                    AutosizeDes = max(ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow, ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow);
                                } else {
                                    AutosizeDes = max(FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow, FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow);
                                }
                            }
                        } else if (SELECT_CASE_var == FractionOfAutosizedCoolingAirflow) {
                            if (ZoneCoolingOnlyFan) {
                                AutosizeDes = DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                            } else if (ZoneHeatingOnlyFan) {
                                AutosizeDes = DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                            } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                AutosizeDes = DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow;
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow;
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = max(DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow,
                                                  DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow);
                            } else {
                                AutosizeDes = max(DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow,
                                                  DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow);
                            }
                        } else if (SELECT_CASE_var == FractionOfAutosizedHeatingAirflow) {
                            if (ZoneCoolingOnlyFan) {
                                AutosizeDes = DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                            } else if (ZoneHeatingOnlyFan) {
                                AutosizeDes = DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                            } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                AutosizeDes = DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow;
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow;
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = max(DataFracOfAutosizedCoolingAirflow * ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow,
                                                  DataFracOfAutosizedHeatingAirflow * ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow);
                            } else {
                                AutosizeDes = max(DataFracOfAutosizedCoolingAirflow * FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow,
                                                  DataFracOfAutosizedHeatingAirflow * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow);
                            }
                        } else if (SELECT_CASE_var == FlowPerCoolingCapacity) {
                            if (ZoneCoolingOnlyFan) {
                                AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
                            } else if (ZoneHeatingOnlyFan) {
                                AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
                            } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = max(DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity,
                                                  DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity);
                            } else {
                                AutosizeDes = max(DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity,
                                                  DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity);
                            }
                        } else if (SELECT_CASE_var == FlowPerHeatingCapacity) {
                            if (ZoneCoolingOnlyFan) {
                                AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
                            } else if (ZoneHeatingOnlyFan) {
                                AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
                            } else if (ZoneEqSizing(CurZoneEqNum).CoolingAirFlow && !ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && !ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow && ZoneEqSizing(CurZoneEqNum).CoolingAirFlow) {
                                AutosizeDes = max(DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity,
                                                  DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity);
                            } else {
                                AutosizeDes = max(DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity,
                                                  DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity);
                            }
                        } else {
                            if (ZoneCoolingOnlyFan) {
                                AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                            } else if (TermUnitIU && (CurTermUnitSizingNum > 0)) {
                                AutosizeDes = TermUnitSizing(CurTermUnitSizingNum).AirVolFlow;
                            } else if (ZoneEqFanCoil) {
                                AutosizeDes = ZoneEqSizing(CurZoneEqNum).AirVolFlow;
                            } else if (ZoneHeatingOnlyFan) {
                                AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                            } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                                AutosizeDes = ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow;
                            } else {
                                AutosizeDes = max(FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow, FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow);
                            }
                        }
                    }
                } else if (SizingType == CoolingWaterflowSizing) {
                    CoilDesWaterDeltaT = DataWaterCoilSizCoolDeltaT;
                    Cp = GetSpecificHeatGlycol(
                        PlantLoop(DataWaterLoopNum).FluidName, DataGlobals::CWInitConvTemp, PlantLoop(DataWaterLoopNum).FluidIndex, CallingRoutine);
                    rho = GetDensityGlycol(
                        PlantLoop(DataWaterLoopNum).FluidName, DataGlobals::CWInitConvTemp, PlantLoop(DataWaterLoopNum).FluidIndex, CallingRoutine);
                    if (TermUnitIU && (CurTermUnitSizingNum > 0)) {
                        AutosizeDes = TermUnitSizing(CurTermUnitSizingNum).MaxCWVolFlow;
                    } else if (ZoneEqFanCoil || ZoneEqUnitVent || ZoneEqVentedSlab) {
                        AutosizeDes = ZoneEqSizing(CurZoneEqNum).MaxCWVolFlow;
                    } else {
                        CoilInTemp = FinalZoneSizing(CurZoneEqNum).DesCoolCoilInTemp;
                        CoilOutTemp = FinalZoneSizing(CurZoneEqNum).CoolDesTemp;
                        CoilOutHumRat = FinalZoneSizing(CurZoneEqNum).CoolDesHumRat;
                        CoilInHumRat = FinalZoneSizing(CurZoneEqNum).DesCoolCoilInHumRat;
                        DesCoilLoad = FinalZoneSizing(CurZoneEqNum).DesCoolMassFlow *
                                      (PsyHFnTdbW(CoilInTemp, CoilInHumRat) - PsyHFnTdbW(CoilOutTemp, CoilOutHumRat));
                        DesVolFlow = FinalZoneSizing(CurZoneEqNum).DesCoolMassFlow / StdRhoAir;
                        // add fan heat to coil load
                        DesCoilLoad += DataAirSystems::calcFanDesignHeatGain(DataFanEnumType, DataFanIndex, DesVolFlow);
                        if (DesCoilLoad >= SmallLoad) {
                            AutosizeDes = DesCoilLoad / (CoilDesWaterDeltaT * Cp * rho);
                        } else {
                            AutosizeDes = 0.0;
                        }
                    }
                    bCheckForZero = false;
                } else if (SizingType == HeatingWaterflowSizing) {
                    if ((TermUnitSingDuct || TermUnitPIU || TermUnitIU) && (CurTermUnitSizingNum > 0)) {
                        AutosizeDes = TermUnitSizing(CurTermUnitSizingNum).MaxHWVolFlow;
                        Cp = GetSpecificHeatGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                                   DataGlobals::HWInitConvTemp,
                                                   PlantLoop(DataWaterLoopNum).FluidIndex,
                                                   CallingRoutine);
                        rho = GetDensityGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                               DataGlobals::HWInitConvTemp,
                                               PlantLoop(DataWaterLoopNum).FluidIndex,
                                               CallingRoutine);
                        DesCoilLoad = AutosizeDes * DataWaterCoilSizHeatDeltaT * Cp * rho;
                    } else if (ZoneEqFanCoil) {
                        AutosizeDes = ZoneEqSizing(CurZoneEqNum).MaxHWVolFlow;
                        Cp = GetSpecificHeatGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                                   DataGlobals::HWInitConvTemp,
                                                   PlantLoop(DataWaterLoopNum).FluidIndex,
                                                   CallingRoutine);
                        rho = GetDensityGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                               DataGlobals::HWInitConvTemp,
                                               PlantLoop(DataWaterLoopNum).FluidIndex,
                                               CallingRoutine);
                        DesCoilLoad = AutosizeDes * DataWaterCoilSizHeatDeltaT * Cp * rho;
                    } else if (ZoneEqUnitHeater || ZoneEqVentedSlab) { // for unit ventilator the cp value is calculated at 5.05(InitConvTemp) for the
                                                                       // child and 60.0C for the unit ventilator //|| ZoneEqUnitVent
                        AutosizeDes = ZoneEqSizing(CurZoneEqNum).MaxHWVolFlow;
                    } else {
                        if (ZoneEqSizing(CurZoneEqNum).SystemAirFlow) {
                            DesMassFlow = ZoneEqSizing(CurZoneEqNum).AirVolFlow * StdRhoAir;
                        } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                            DesMassFlow = ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow * StdRhoAir;
                        } else {
                            DesMassFlow = FinalZoneSizing(CurZoneEqNum).DesHeatMassFlow;
                        }
                        CoilInTemp =
                            setHeatCoilInletTempForZoneEqSizing(setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                        CoilOutTemp = FinalZoneSizing(CurZoneEqNum).HeatDesTemp;
                        CoilOutHumRat = FinalZoneSizing(CurZoneEqNum).HeatDesHumRat;
                        DesCoilLoad = PsyCpAirFnWTdb(CoilOutHumRat, 0.5 * (CoilInTemp + CoilOutTemp)) * DesMassFlow * (CoilOutTemp - CoilInTemp);
                        if (DesCoilLoad >= SmallLoad) {
                            Cp = GetSpecificHeatGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                                       DataGlobals::HWInitConvTemp,
                                                       PlantLoop(DataWaterLoopNum).FluidIndex,
                                                       CallingRoutine);
                            rho = GetDensityGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                                   DataGlobals::HWInitConvTemp,
                                                   PlantLoop(DataWaterLoopNum).FluidIndex,
                                                   CallingRoutine);
                            AutosizeDes = DesCoilLoad / (DataWaterCoilSizHeatDeltaT * Cp * rho);
                        } else {
                            AutosizeDes = 0.0;
                        }
                    }
                    bCheckForZero = false;
                } else if (SizingType == HeatingWaterDesAirInletTempSizing) {
                    if (TermUnitPIU && (CurTermUnitSizingNum > 0)) {
                        MinFlowFrac = TermUnitSizing(CurTermUnitSizingNum).MinFlowFrac;
                        if (TermUnitSizing(CurTermUnitSizingNum).InducesPlenumAir) {
                            AutosizeDes = (TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU * MinFlowFrac) +
                                          (TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneRetTempAtHeatPeak * (1.0 - MinFlowFrac));
                        } else {
                            AutosizeDes = TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU * MinFlowFrac +
                                          FinalZoneSizing(CurZoneEqNum).ZoneTempAtHeatPeak * (1.0 - MinFlowFrac);
                        }
                    } else if (TermUnitIU && (CurTermUnitSizingNum > 0)) {
                        AutosizeDes = TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneTempAtHeatPeak;
                    } else if (TermUnitSingDuct && (CurTermUnitSizingNum > 0)) {
                        AutosizeDes = TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU;
                    } else {
                        if (ZoneEqSizing(CurZoneEqNum).SystemAirFlow) {
                            DesMassFlow = ZoneEqSizing(CurZoneEqNum).AirVolFlow * StdRhoAir;
                        } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                            DesMassFlow = ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow * StdRhoAir;
                        } else {
                            DesMassFlow = FinalZoneSizing(CurZoneEqNum).DesHeatMassFlow;
                        }
                        AutosizeDes =
                            setHeatCoilInletTempForZoneEqSizing(setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                    }
                    bCheckForZero = false;
                } else if (SizingType == HeatingWaterDesAirInletHumRatSizing) {
                    if (TermUnitPIU && (CurTermUnitSizingNum > 0)) {
                        MinFlowFrac = TermUnitSizing(CurTermUnitSizingNum).MinFlowFrac;
                        AutosizeDes = TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInHumRatTU * MinFlowFrac +
                                      FinalZoneSizing(CurZoneEqNum).ZoneHumRatAtHeatPeak * (1.0 - MinFlowFrac);
                    } else if (TermUnitIU && (CurTermUnitSizingNum > 0)) {
                        AutosizeDes = TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneHumRatAtHeatPeak;
                    } else if (TermUnitSingDuct && (CurTermUnitSizingNum > 0)) {
                        AutosizeDes = TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInHumRatTU;
                    } else {
                        if (ZoneEqSizing(CurZoneEqNum).SystemAirFlow) {
                            DesMassFlow = ZoneEqSizing(CurZoneEqNum).AirVolFlow * StdRhoAir;
                        } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                            DesMassFlow = ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow * StdRhoAir;
                        } else {
                            DesMassFlow = FinalZoneSizing(CurZoneEqNum).DesHeatMassFlow;
                        }
                        AutosizeDes =
                            setHeatCoilInletHumRatForZoneEqSizing(setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                    }
                    bCheckForZero = false;
                } else if (SizingType == CoolingWaterDesAirInletTempSizing) {
                    if (TermUnitIU) {
                        AutosizeDes = FinalZoneSizing(CurZoneEqNum).ZoneTempAtCoolPeak;
                    } else if (ZoneEqFanCoil) {
                        DesMassFlow = FinalZoneSizing(CurZoneEqNum).DesCoolMassFlow;
                        AutosizeDes =
                            setCoolCoilInletTempForZoneEqSizing(setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                    } else {
                        AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesCoolCoilInTemp;
                    }
                    Real64 fanDeltaT = 0.0;
                    if (DataSizing::DataFanPlacement == DataSizing::zoneFanPlacement::zoneBlowThru) {
                        // calculate fan heat to get fan air-side delta T
                        FanCoolLoad = DataAirSystems::calcFanDesignHeatGain(DataFanEnumType, DataFanIndex, DataAirFlowUsedForSizing);
                        if (DataDesInletAirHumRat > 0.0 && DataAirFlowUsedForSizing > 0.0) {
                            CpAir = PsyCpAirFnWTdb(DataDesInletAirHumRat, AutosizeDes);
                            fanDeltaT = FanCoolLoad / (CpAir * StdRhoAir * DataAirFlowUsedForSizing);
                        }
                    }
                    AutosizeDes += fanDeltaT;
                    bCheckForZero = false;
                } else if (SizingType == CoolingWaterDesWaterInletTempSizing) {
                    AutosizeDes = PlantSizData(DataPltSizCoolNum).ExitTemp;
                    bCheckForZero = false;
                } else if (SizingType == CoolingWaterNumofTubesPerRowSizing) {
                    // result will be integerized external to this routine , add 0.5 to existing calc to round the result
                    AutosizeDes = int(max(3.0, 13750.0 * DataWaterFlowUsedForSizing + 1.0));
                    bCheckForZero = false;
                } else if (SizingType == CoolingWaterDesAirOutletTempSizing) {
                    if (TermUnitIU) {
                        Cp = GetSpecificHeatGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                                   DataGlobals::CWInitConvTemp,
                                                   PlantLoop(DataWaterLoopNum).FluidIndex,
                                                   CallingRoutine);
                        rho = GetDensityGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                               DataGlobals::CWInitConvTemp,
                                               PlantLoop(DataWaterLoopNum).FluidIndex,
                                               CallingRoutine);
                        DesCoilLoad = DataWaterFlowUsedForSizing * DataWaterCoilSizCoolDeltaT * Cp * rho;
                        T1Out = DataDesInletAirTemp -
                                DesCoilLoad / (StdRhoAir * PsyCpAirFnWTdb(DataDesInletAirHumRat, DataDesInletAirTemp) * DataAirFlowUsedForSizing);
                        T2Out = PlantSizData(DataPltSizCoolNum).ExitTemp + 2.0;
                        AutosizeDes = max(T1Out, T2Out);
                    } else {
                        AutosizeDes = FinalZoneSizing(CurZoneEqNum).CoolDesTemp;
                    }
                    Real64 fanDeltaT = 0.0;
                    if (DataSizing::DataFanPlacement == DataSizing::zoneFanPlacement::zoneDrawThru) {
                        // calculate fan heat to get fan air-side delta T
                        FanCoolLoad = DataAirSystems::calcFanDesignHeatGain(DataFanEnumType, DataFanIndex, DataAirFlowUsedForSizing);
                        if (DataDesInletAirHumRat > 0.0 && DataAirFlowUsedForSizing > 0.0) {
                            CpAir = PsyCpAirFnWTdb(DataDesInletAirHumRat, AutosizeDes);
                            fanDeltaT = FanCoolLoad / (CpAir * StdRhoAir * DataAirFlowUsedForSizing);
                            DataDesAccountForFanHeat = false; // used in CoolingCapacitySizing calculations to avoid double counting fan heat
                        }
                    }
                    AutosizeDes -= fanDeltaT;

                    if (AutosizeDes < DataDesInletWaterTemp && DataWaterFlowUsedForSizing > 0.0) { // flow here is water vol flow rate
                        ShowWarningError(CallingRoutine + ":" + " Coil=\"" + CompName +
                                         "\", Cooling Coil has leaving air temperature < entering water temperature.");
                        ShowContinueError("    Tair,out  =  " + RoundSigDigits(AutosizeDes, 3));
                        ShowContinueError("    Twater,in = " + RoundSigDigits(DataDesInletWaterTemp, 3));
                        AutosizeDes = DataDesInletWaterTemp + 0.5;
                        ShowContinueError("....coil leaving air temperature will be reset to:");
                        ShowContinueError("    Tair,out = " + RoundSigDigits(AutosizeDes, 3));
                    }
                    bCheckForZero = false;
                } else if (SizingType == CoolingWaterDesAirInletHumRatSizing) {
                    if (TermUnitIU) {
                        AutosizeDes = FinalZoneSizing(CurZoneEqNum).ZoneHumRatAtCoolPeak;
                    } else if (ZoneEqFanCoil) {
                        DesMassFlow = FinalZoneSizing(CurZoneEqNum).DesCoolMassFlow;
                        AutosizeDes =
                            setCoolCoilInletHumRatForZoneEqSizing(setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                    } else {
                        AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesCoolCoilInHumRat;
                    }
                    bCheckForZero = false;
                } else if (SizingType == CoolingWaterDesAirOutletHumRatSizing) {
                    if (TermUnitIU) {
                        TDpIn = PsyTdpFnWPb(DataDesInletAirHumRat, StdBaroPress);
                        if (TDpIn <= DataDesInletWaterTemp) {
                            AutosizeDes = DataDesInletAirHumRat;
                        } else {
                            AutosizeDes = min(PsyWFnTdbRhPb(DataDesOutletAirTemp, 0.9, StdBaroPress), DataDesInletAirHumRat);
                        }
                    } else {
                        AutosizeDes = FinalZoneSizing(CurZoneEqNum).CoolDesHumRat;
                    }
                    if (AutosizeDes > DataDesInletAirHumRat && (UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER") ||
                                                                UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER:DETAILEDGEOMETRY"))) {
                        ShowWarningError(CallingRoutine + ":" + " Coil=\"" + CompName +
                                         "\", Cooling Coil has leaving humidity ratio > entering humidity ratio.");
                        ShowContinueError("    Wair,in =  " + RoundSigDigits(DataDesInletAirHumRat, 6));
                        ShowContinueError("    Wair,out = " + RoundSigDigits(AutosizeDes, 6));
                        if (DataDesInletAirHumRat > 0.016) {
                            AutosizeDes = 0.5 * DataDesInletAirHumRat;
                        } else {
                            AutosizeDes = DataDesInletAirHumRat;
                        }
                        ShowContinueError("....coil leaving humidity ratio will be reset to:");
                        ShowContinueError("    Wair,out = " + RoundSigDigits(AutosizeDes, 6));
                    }

                    // check for dry coil and reset outlet humrat if needed
                    DesSatEnthAtWaterInTemp = PsyHFnTdbW(DataDesInletWaterTemp, PsyWFnTdpPb(DataDesInletWaterTemp, StdBaroPress));
                    DesHumRatAtWaterInTemp = PsyWFnTdbH(DataDesInletWaterTemp, DesSatEnthAtWaterInTemp, CallingRoutine);
                    if (AutosizeDes < DataDesInletAirHumRat && DesHumRatAtWaterInTemp > DataDesInletAirHumRat) {
                        if (AutosizeDes < DataDesInletAirHumRat && (UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER") ||
                                                                    UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER:DETAILEDGEOMETRY"))) {
                            ShowWarningError(
                                CallingRoutine + ":" + " Coil=\"" + CompName +
                                "\", Cooling Coil is running dry for sizing and has minimum humidity ratio at saturation for inlet chilled water "
                                "temperature > coil entering air humidity ratio.");
                            ShowContinueError("    Wair,in =  " + RoundSigDigits(DataDesInletAirHumRat, 6));
                            ShowContinueError("    Wair,out = " + RoundSigDigits(AutosizeDes, 6));
                            ShowContinueError("    Inlet chilled water temperature = " + RoundSigDigits(DataDesInletWaterTemp, 3) + " [C]");
                            ShowContinueError("    Minimum humidity ratio at saturation for inlet chilled water temperature = " +
                                              RoundSigDigits(DesHumRatAtWaterInTemp, 6) + " [KGWATER/KGDRYAIR]");
                            AutosizeDes = DataDesInletAirHumRat;
                            ShowContinueError("....coil leaving humidity ratio will be reset to:");
                            ShowContinueError("    Wair,out = " + RoundSigDigits(AutosizeDes, 6));
                        }
                    }
                    bCheckForZero = false;
                } else if (SizingType == CoolingSHRSizing) {
                    if (DataFlowUsedForSizing >= SmallAirVolFlow && DataCapacityUsedForSizing > 0.0) {
                        // For autosizing the rated SHR, we set a minimum SHR of 0.676 and a maximum of 0.798. The min SHR occurs occurs at the
                        // minimum flow / capacity ratio = MinRatedVolFlowPerRatedTotCap = 0.00004027 [m3/s / W] = 300 [cfm/ton].
                        // The max SHR occurs at maximum flow / capacity ratio = MaxRatedVolFlowPerRatedTotCap = 0.00006041 [m3/s / W] = 450
                        // [cfm/ton]. For flow / capacity ratios between the min and max we linearly interpolate between min and max SHR. Thus
                        // rated SHR is a linear function of the rated flow / capacity ratio. This linear function (see below) is the result of a
                        // regression of flow/capacity ratio vs SHR for several actual coils.
                        RatedVolFlowPerRatedTotCap = DataFlowUsedForSizing / DataCapacityUsedForSizing;
                        if (DXCT == RegularDXCoil) {
                            if (RatedVolFlowPerRatedTotCap > MaxRatedVolFlowPerRatedTotCap(DXCT)) {
                                AutosizeDes = 0.431 + 6086.0 * MaxRatedVolFlowPerRatedTotCap(DXCT);
                            } else if (RatedVolFlowPerRatedTotCap < MinRatedVolFlowPerRatedTotCap(DXCT)) {
                                AutosizeDes = 0.431 + 6086.0 * MinRatedVolFlowPerRatedTotCap(DXCT);
                            } else {
                                AutosizeDes = 0.431 + 6086.0 * RatedVolFlowPerRatedTotCap;
                            }
                        } else { // DOASDXCoil, or DXCT = 2
                            if (RatedVolFlowPerRatedTotCap > MaxRatedVolFlowPerRatedTotCap(DXCT)) {
                                AutosizeDes = 0.389 + 7684.0 * MaxRatedVolFlowPerRatedTotCap(DXCT);
                            } else if (RatedVolFlowPerRatedTotCap < MinRatedVolFlowPerRatedTotCap(DXCT)) {
                                AutosizeDes = 0.389 + 7684.0 * MinRatedVolFlowPerRatedTotCap(DXCT);
                            } else {
                                AutosizeDes = 0.389 + 7684.0 * RatedVolFlowPerRatedTotCap;
                            }
                        }

                        // check that the autosized SHR corresponds to a valid apperatus dew point (ADP) temperature
                        AutosizeDes = ValidateADP(CompType,
                                                  CompName,
                                                  RatedInletAirTemp,
                                                  RatedInletAirHumRat,
                                                  DataCapacityUsedForSizing,
                                                  DataFlowUsedForSizing,
                                                  AutosizeDes,
                                                  CallingRoutine);

                    } else {
                        AutosizeDes = 1.0;
                    }
                } else if (SizingType == CoolingCapacitySizing) {
                    if (ZoneEqSizing(CurZoneEqNum).CoolingCapacity) { // Parent object calculated capacity
                        AutosizeDes = ZoneEqSizing(CurZoneEqNum).DesCoolingLoad;
                        DesVolFlow = DataFlowUsedForSizing;
                        CoilInTemp = DataSizing::DataCoilSizingAirInTemp;
                        CoilInHumRat = DataSizing::DataCoilSizingAirInHumRat;
                        CoilOutTemp = DataSizing::DataCoilSizingAirOutTemp;
                        CoilOutHumRat = DataSizing::DataCoilSizingAirOutHumRat;
                        FanCoolLoad = DataSizing::DataCoilSizingFanCoolLoad;
                        TotCapTempModFac = DataSizing::DataCoilSizingCapFT;
                    } else {
                        if (UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER") ||
                            UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER:DETAILEDGEOMETRY") ||
                            UtilityRoutines::SameString(CompType, "ZONEHVAC:IDEALLOADSAIRSYSTEM")) {
                            if (TermUnitIU && (CurTermUnitSizingNum > 0)) {
                                AutosizeDes = TermUnitSizing(CurTermUnitSizingNum).DesCoolingLoad;
                            } else if (ZoneEqFanCoil) {
                                AutosizeDes = ZoneEqSizing(CurZoneEqNum).DesCoolingLoad;
                            } else {
                                CoilInTemp = FinalZoneSizing(CurZoneEqNum).DesCoolCoilInTemp;
                                CoilInHumRat = FinalZoneSizing(CurZoneEqNum).DesCoolCoilInHumRat;
                                CoilOutTemp = min(CoilInTemp, FinalZoneSizing(CurZoneEqNum).CoolDesTemp);
                                CoilOutHumRat = min(CoilInHumRat, FinalZoneSizing(CurZoneEqNum).CoolDesHumRat);
                                AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesCoolMassFlow *
                                              (PsyHFnTdbW(CoilInTemp, CoilInHumRat) - PsyHFnTdbW(CoilOutTemp, CoilOutHumRat));
                                DesVolFlow = FinalZoneSizing(CurZoneEqNum).DesCoolMassFlow / StdRhoAir;
                                // add fan heat to coil load
                                FanCoolLoad += DataAirSystems::calcFanDesignHeatGain(DataFanEnumType, DataFanIndex, DesVolFlow);
                                AutosizeDes += FanCoolLoad;
                            }
                        } else {
                            DesVolFlow = DataFlowUsedForSizing;
                            if (DesVolFlow >= SmallAirVolFlow) {
                                // each of these IFs now seem the same and can be condensed to just CoilInTemp = set() and CoilInHumRat = set()
                                if (ZoneEqDXCoil) {
                                    // ATMixer has priority over Equipment OA vol flow
                                    if (ZoneEqSizing(CurZoneEqNum).ATMixerVolFlow > 0.0) { // NEW ATMixer coil sizing method
                                        DesMassFlow = DesVolFlow * StdRhoAir;
                                        CoilInTemp = setCoolCoilInletTempForZoneEqSizing(
                                            setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                                        CoilInHumRat = setCoolCoilInletHumRatForZoneEqSizing(
                                            setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                                    } else if (ZoneEqSizing(CurZoneEqNum).OAVolFlow > 0.0) {
                                        CoilInTemp = FinalZoneSizing(CurZoneEqNum).DesCoolCoilInTemp;
                                        CoilInHumRat = FinalZoneSizing(CurZoneEqNum).DesCoolCoilInHumRat;
                                    } else {
                                        CoilInTemp = FinalZoneSizing(CurZoneEqNum)
                                                         .ZoneRetTempAtCoolPeak; // Question whether zone equipment should use return temp for sizing
                                        CoilInHumRat = FinalZoneSizing(CurZoneEqNum).ZoneHumRatAtCoolPeak;
                                    }
                                } else if (ZoneEqFanCoil) {
                                    // use fan coil flow (i.e., set by parent) or flow used during sizing?
                                    DesMassFlow = FinalZoneSizing(CurZoneEqNum).DesCoolMassFlow;
                                    CoilInTemp = setCoolCoilInletTempForZoneEqSizing(
                                        setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                                    CoilInHumRat = setCoolCoilInletHumRatForZoneEqSizing(
                                        setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                                } else {
                                    CoilInTemp = FinalZoneSizing(CurZoneEqNum).DesCoolCoilInTemp;
                                    CoilInHumRat = FinalZoneSizing(CurZoneEqNum).DesCoolCoilInHumRat;
                                }
                                CoilOutTemp = min(CoilInTemp, FinalZoneSizing(CurZoneEqNum).CoolDesTemp);
                                CoilOutHumRat = min(CoilInHumRat, FinalZoneSizing(CurZoneEqNum).CoolDesHumRat);
                                TimeStepNumAtMax = FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax;
                                DDNum = FinalZoneSizing(CurZoneEqNum).CoolDDNum;
                                if (DDNum > 0 && TimeStepNumAtMax > 0) {
                                    OutTemp = DesDayWeath(DDNum).Temp(TimeStepNumAtMax);
                                } else {
                                    OutTemp = 0.0;
                                }
                                rhoair = PsyRhoAirFnPbTdbW(StdBaroPress, CoilInTemp, CoilInHumRat, CallingRoutine);
                                CoilInEnth = PsyHFnTdbW(CoilInTemp, CoilInHumRat);
                                CoilOutEnth = PsyHFnTdbW(CoilOutTemp, CoilOutHumRat);
                                if (ZoneEqFanCoil) {
                                    PeakCoilLoad = max(0.0, (StdRhoAir * DesVolFlow * (CoilInEnth - CoilOutEnth)));
                                } else if (ZoneEqUnitVent) {
                                    PeakCoilLoad = max(0.0, (StdRhoAir * DesVolFlow * (CoilInEnth - CoilOutEnth)));
                                } else {
                                    PeakCoilLoad = max(0.0, (rhoair * DesVolFlow * (CoilInEnth - CoilOutEnth)));
                                }
                                // add fan heat to coil load
                                FanCoolLoad += DataAirSystems::calcFanDesignHeatGain(DataFanEnumType, DataFanIndex, DesVolFlow);
                                PeakCoilLoad += FanCoolLoad;
                                CpAir = PsyCpAirFnWTdb(CoilInHumRat, CoilInTemp);
                                // adjust coil inlet/outlet temp with fan temperature rise
                                if (DataDesAccountForFanHeat) {
                                    if (DataSizing::DataFanPlacement == DataSizing::zoneFanPlacement::zoneBlowThru) {
                                        CoilInTemp += FanCoolLoad / (CpAir * StdRhoAir * DesVolFlow);
                                    } else if (DataSizing::DataFanPlacement == DataSizing::zoneFanPlacement::zoneDrawThru) {
                                        CoilOutTemp -= FanCoolLoad / (CpAir * StdRhoAir * DesVolFlow);
                                    }
                                }
                                CoilInWetBulb = PsyTwbFnTdbWPb(CoilInTemp, CoilInHumRat, StdBaroPress, CallingRoutine);
                                if (DataTotCapCurveIndex > 0) {
                                    TotCapTempModFac = CurveValue(DataTotCapCurveIndex, CoilInWetBulb, OutTemp);
                                } else if (DataTotCapCurveValue > 0) {
                                    TotCapTempModFac = DataTotCapCurveValue;
                                } else {
                                    TotCapTempModFac = 1.0;
                                }
                                if (TotCapTempModFac > 0.0) {
                                    AutosizeDes = PeakCoilLoad / TotCapTempModFac;
                                } else {
                                    AutosizeDes = PeakCoilLoad;
                                }
                                // save these conditions to use when ZoneEqSizing(CurZoneEqNum).CoolingCapacity = true
                                DataSizing::DataCoilSizingAirInTemp = CoilInTemp;
                                DataSizing::DataCoilSizingAirInHumRat = CoilInHumRat;
                                DataSizing::DataCoilSizingAirOutTemp = CoilOutTemp;
                                DataSizing::DataCoilSizingAirOutHumRat = CoilOutHumRat;
                                DataSizing::DataCoilSizingFanCoolLoad = FanCoolLoad;
                                DataSizing::DataCoilSizingCapFT = TotCapTempModFac;
                            } else {
                                AutosizeDes = 0.0;
                                CoilOutTemp = -999.0;
                            }
                        }
                    }
                    AutosizeDes = AutosizeDes * DataFracOfAutosizedCoolingCapacity;
                    DataDesAccountForFanHeat = true; // reset for next water coil
                    if (DisplayExtraWarnings && AutosizeDes <= 0.0) {
                        ShowWarningMessage(CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName);
                        ShowContinueError("...Rated Total Cooling Capacity = " + TrimSigDigits(AutosizeDes, 2) + " [W]");
                        if (ZoneEqSizing(CurZoneEqNum).CoolingCapacity) {
                            ShowContinueError("...Capacity passed by parent object to size child component = " + TrimSigDigits(AutosizeDes, 2) +
                                              " [W]");
                        } else {
                            if (UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER") ||
                                UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER:DETAILEDGEOMETRY") ||
                                UtilityRoutines::SameString(CompType, "ZONEHVAC:IDEALLOADSAIRSYSTEM")) {
                                if (TermUnitIU || ZoneEqFanCoil) {
                                    ShowContinueError(
                                        "...Capacity passed by parent object to size child component = " + TrimSigDigits(AutosizeDes, 2) + " [W]");
                                } else {
                                    ShowContinueError("...Air flow rate used for sizing = " + TrimSigDigits(DesVolFlow, 5) + " [m3/s]");
                                    ShowContinueError("...Coil inlet air temperature used for sizing = " + TrimSigDigits(CoilInTemp, 2) + " [C]");
                                    ShowContinueError("...Coil outlet air temperature used for sizing = " + TrimSigDigits(CoilOutTemp, 2) + " [C]");
                                }
                            } else {
                                if (CoilOutTemp > -999.0) {
                                    ShowContinueError("...Air flow rate used for sizing = " + TrimSigDigits(DesVolFlow, 5) + " [m3/s]");
                                    ShowContinueError("...Coil inlet air temperature used for sizing = " + TrimSigDigits(CoilInTemp, 2) + " [C]");
                                    ShowContinueError("...Coil outlet air temperature used for sizing = " + TrimSigDigits(CoilOutTemp, 2) + " [C]");
                                } else {
                                    ShowContinueError("...Capacity used to size child component set to 0 [W]");
                                }
                            }
                        }
                    }
                } else if (SizingType == HeatingCapacitySizing) {
                    if (ZoneEqSizing(CurZoneEqNum).HeatingCapacity) {
                        NominalCapacityDes = ZoneEqSizing(CurZoneEqNum).DesHeatingLoad;
                        if (DataFlowUsedForSizing > 0.0) {
                            DesVolFlow = DataFlowUsedForSizing;
                        }
                    } else if (DataCoolCoilCap > 0.0 && DataFlowUsedForSizing > 0.0) {
                        NominalCapacityDes = DataCoolCoilCap;
                        DesVolFlow = DataFlowUsedForSizing;
                    } else if (FinalZoneSizing(CurZoneEqNum).DesHeatMassFlow >= SmallMassFlow) {
                        if (DataFlowUsedForSizing > 0.0) {
                            DesVolFlow = DataFlowUsedForSizing;
                        }
                        if (TermUnitPIU && (CurTermUnitSizingNum > 0)) {
                            MinPriFlowFrac = TermUnitSizing(CurTermUnitSizingNum).MinFlowFrac;
                            if (TermUnitSizing(CurTermUnitSizingNum).InducesPlenumAir) {
                                CoilInTemp = (TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU * MinPriFlowFrac) +
                                             (TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneRetTempAtHeatPeak * (1.0 - MinPriFlowFrac));
                            } else {
                                CoilInTemp = (TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU * MinPriFlowFrac) +
                                             (TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneTempAtHeatPeak * (1.0 - MinPriFlowFrac));
                            }
                        } else if (ZoneEqFanCoil) {
                            // use fan coil flow (i.e., set by parent) or flow used during sizing?
                            if (DesVolFlow > 0.0) {
                                DesMassFlow = DesVolFlow * StdRhoAir;
                            } else {
                                DesMassFlow = FinalZoneSizing(CurZoneEqNum).DesHeatMassFlow;
                            }
                            CoilInTemp = setHeatCoilInletTempForZoneEqSizing(
                                setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                            CoilInHumRat = setHeatCoilInletHumRatForZoneEqSizing(
                                setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                        } else if (TermUnitIU && (CurTermUnitSizingNum > 0)) {
                            CoilInTemp = TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneTempAtHeatPeak;
                            CoilInHumRat = TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneHumRatAtHeatPeak;
                        } else if (TermUnitSingDuct && (CurTermUnitSizingNum > 0)) {
                            CoilInTemp = TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU;
                            CoilInHumRat = TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInHumRatTU;
                        } else {
                            if (DesVolFlow > 0.0) {
                                DesMassFlow = DesVolFlow * StdRhoAir;
                            } else {
                                DesMassFlow = FinalZoneSizing(CurZoneEqNum).DesHeatMassFlow;
                            }
                            CoilInTemp = setHeatCoilInletTempForZoneEqSizing(
                                setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                            CoilInHumRat = setHeatCoilInletHumRatForZoneEqSizing(
                                setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                        }
                        if ((TermUnitSingDuct || TermUnitPIU) && (CurTermUnitSizingNum > 0)) {
                            CoilOutTemp = TermUnitFinalZoneSizing(CurTermUnitSizingNum).HeatDesTemp;
                            CoilOutHumRat = TermUnitFinalZoneSizing(CurTermUnitSizingNum).HeatDesHumRat;
                            CpAir = PsyCpAirFnWTdb(CoilOutHumRat, 0.5 * (CoilInTemp + CoilOutTemp));
                            DesCoilLoad = CpAir * StdRhoAir * TermUnitSizing(CurTermUnitSizingNum).AirVolFlow * (CoilOutTemp - CoilInTemp);
                            DesVolFlow = TermUnitSizing(CurTermUnitSizingNum).AirVolFlow;
                        } else if (TermUnitIU && (CurTermUnitSizingNum > 0)) {
                            if (TermUnitSizing(CurTermUnitSizingNum).InducRat > 0.01) {
                                DesVolFlow = TermUnitSizing(CurTermUnitSizingNum).AirVolFlow / TermUnitSizing(CurTermUnitSizingNum).InducRat;
                                CpAir = PsyCpAirFnWTdb(TermUnitFinalZoneSizing(CurTermUnitSizingNum).HeatDesHumRat,
                                                       TermUnitFinalZoneSizing(CurTermUnitSizingNum).HeatDesTemp);
                                // the design heating coil load is the zone load minus whatever the central system does.Note that
                                // DesHeatCoilInTempTU is really the primary air inlet temperature for the unit.
                                DesCoilLoad = TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatLoad -
                                              (CpAir * StdRhoAir * DesVolFlow *
                                               (TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU -
                                                TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneTempAtHeatPeak));
                            } else {
                                DesCoilLoad = 0.0;
                            }
                        } else {
                            CoilOutTemp = FinalZoneSizing(CurZoneEqNum).HeatDesTemp;
                            CoilOutHumRat = FinalZoneSizing(CurZoneEqNum).HeatDesHumRat;
                            CpAir = PsyCpAirFnWTdb(CoilOutHumRat, 0.5 * (CoilInTemp + CoilOutTemp));
                            DesCoilLoad = CpAir * FinalZoneSizing(CurZoneEqNum).DesHeatMassFlow * (CoilOutTemp - CoilInTemp);
                            DesVolFlow = FinalZoneSizing(CurZoneEqNum).DesHeatMassFlow / StdRhoAir;
                        }
                        NominalCapacityDes = max(0.0, DesCoilLoad);
                    } else {
                        NominalCapacityDes = 0.0;
                        CoilOutTemp = -999.0;
                    }
                    if (DataCoolCoilCap > 0.0) {
                        AutosizeDes = NominalCapacityDes * DataHeatSizeRatio;
                    } else {
                        AutosizeDes = NominalCapacityDes * DataHeatSizeRatio * DataFracOfAutosizedHeatingCapacity;
                    }
                    if (DisplayExtraWarnings && AutosizeDes <= 0.0) {
                        ShowWarningMessage(CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName);
                        ShowContinueError("...Rated Total Heating Capacity = " + TrimSigDigits(AutosizeDes, 2) + " [W]");
                        if (ZoneEqSizing(CurZoneEqNum).HeatingCapacity || (DataCoolCoilCap > 0.0 && DataFlowUsedForSizing > 0.0)) {
                            ShowContinueError(
                                "...Capacity passed by parent object to size child component = " + TrimSigDigits(NominalCapacityDes, 2) + " [W]");
                        } else {
                            if (CoilOutTemp > -999.0) {
                                ShowContinueError("...Air flow rate used for sizing = " + TrimSigDigits(DesVolFlow, 5) + " [m3/s]");
                                ShowContinueError("...Coil inlet air temperature used for sizing = " + TrimSigDigits(CoilInTemp, 2) + " [C]");
                                ShowContinueError("...Coil outlet air temperature used for sizing = " + TrimSigDigits(CoilOutTemp, 2) + " [C]");
                            } else {
                                ShowContinueError("...Capacity used to size child component set to 0 [W]");
                            }
                        }
                    }
                } else if (SizingType == WaterHeatingCapacitySizing) {
                    if ((TermUnitSingDuct || TermUnitPIU || TermUnitIU) && (CurTermUnitSizingNum > 0)) {
                        DesMassFlow = TermUnitSizing(CurTermUnitSizingNum).MaxHWVolFlow;
                        Cp = GetSpecificHeatGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                                   DataGlobals::HWInitConvTemp,
                                                   PlantLoop(DataWaterLoopNum).FluidIndex,
                                                   CallingRoutine);
                        rho = GetDensityGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                               DataGlobals::HWInitConvTemp,
                                               PlantLoop(DataWaterLoopNum).FluidIndex,
                                               CallingRoutine);
                        NominalCapacityDes = DesMassFlow * DataWaterCoilSizHeatDeltaT * Cp * rho;
                    } else if (ZoneEqFanCoil || ZoneEqUnitHeater) {
                        DesMassFlow = ZoneEqSizing(CurZoneEqNum).MaxHWVolFlow;
                        Cp = GetSpecificHeatGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                                   DataGlobals::HWInitConvTemp,
                                                   PlantLoop(DataWaterLoopNum).FluidIndex,
                                                   CallingRoutine);
                        rho = GetDensityGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                               DataGlobals::HWInitConvTemp,
                                               PlantLoop(DataWaterLoopNum).FluidIndex,
                                               CallingRoutine);
                        NominalCapacityDes = DesMassFlow * DataWaterCoilSizHeatDeltaT * Cp * rho;
                        // if coil is part of a zonal unit, calc coil load to get hot water flow rate
                    } else {
                        if (ZoneEqSizing(CurZoneEqNum).SystemAirFlow) {
                            DesMassFlow = ZoneEqSizing(CurZoneEqNum).AirVolFlow * StdRhoAir;
                        } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                            DesMassFlow = ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow * StdRhoAir;
                        } else {
                            DesMassFlow = FinalZoneSizing(CurZoneEqNum).DesHeatMassFlow;
                        }
                        CoilInTemp =
                            setHeatCoilInletTempForZoneEqSizing(setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                        CoilInHumRat =
                            setHeatCoilInletHumRatForZoneEqSizing(setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                        CoilOutTemp = FinalZoneSizing(CurZoneEqNum).HeatDesTemp;
                        CoilOutHumRat = FinalZoneSizing(CurZoneEqNum).HeatDesHumRat;
                        NominalCapacityDes =
                            PsyCpAirFnWTdb(CoilOutHumRat, 0.5 * (CoilInTemp + CoilOutTemp)) * DesMassFlow * (CoilOutTemp - CoilInTemp);
                    }
                    AutosizeDes = NominalCapacityDes * DataHeatSizeRatio;
                    if (DisplayExtraWarnings && AutosizeDes <= 0.0) {
                        ShowWarningMessage(CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName);
                        ShowContinueError("...Rated Total Heating Capacity = " + TrimSigDigits(AutosizeDes, 2) + " [W]");
                        ShowContinueError("...Air flow rate used for sizing = " + TrimSigDigits(DesMassFlow / StdRhoAir, 5) + " [m3/s]");
                        if (TermUnitSingDuct || TermUnitPIU || TermUnitIU || ZoneEqFanCoil || ZoneEqUnitHeater) {
                            ShowContinueError("...Air flow rate used for sizing = " + TrimSigDigits(DesMassFlow / StdRhoAir, 5) + " [m3/s]");
                            ShowContinueError("...Plant loop temperature difference = " + TrimSigDigits(DataWaterCoilSizHeatDeltaT, 2) + " [C]");
                        } else {
                            ShowContinueError("...Coil inlet air temperature used for sizing = " + TrimSigDigits(CoilInTemp, 2) + " [C]");
                            ShowContinueError("...Coil outlet air temperature used for sizing = " + TrimSigDigits(CoilOutTemp, 2) + " [C]");
                            ShowContinueError("...Coil outlet air humidity ratio used for sizing = " + TrimSigDigits(CoilOutHumRat, 2) +
                                              " [kgWater/kgDryAir]");
                        }
                    }
                } else if (SizingType == HeatingWaterDesCoilLoadUsedForUASizing) {
                    if (TermUnitSingDuct && (CurTermUnitSizingNum > 0)) {
                        DesMassFlow =
                            StdRhoAir * TermUnitSizing(CurTermUnitSizingNum).AirVolFlow * TermUnitSizing(CurTermUnitSizingNum).ReheatAirFlowMult;
                        Cp = GetSpecificHeatGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                                   DataGlobals::HWInitConvTemp,
                                                   PlantLoop(DataWaterLoopNum).FluidIndex,
                                                   CallingRoutine);
                        rho = GetDensityGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                               DataGlobals::HWInitConvTemp,
                                               PlantLoop(DataWaterLoopNum).FluidIndex,
                                               CallingRoutine);
                        AutosizeDes = DataWaterFlowUsedForSizing * DataWaterCoilSizHeatDeltaT * Cp * rho;
                        coilSelectionReportObj->setCoilReheatMultiplier(CompName, CompType, 1.0);
                    } else if ((TermUnitPIU || TermUnitIU) && (CurTermUnitSizingNum > 0)) {
                        DesMassFlow =
                            StdRhoAir * TermUnitSizing(CurTermUnitSizingNum).AirVolFlow * TermUnitSizing(CurTermUnitSizingNum).ReheatAirFlowMult;
                        Cp = GetSpecificHeatGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                                   DataGlobals::HWInitConvTemp,
                                                   PlantLoop(DataWaterLoopNum).FluidIndex,
                                                   CallingRoutine);
                        rho = GetDensityGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                               DataGlobals::HWInitConvTemp,
                                               PlantLoop(DataWaterLoopNum).FluidIndex,
                                               CallingRoutine);
                        AutosizeDes =
                            DataWaterFlowUsedForSizing * DataWaterCoilSizHeatDeltaT * Cp * rho * TermUnitSizing(CurTermUnitSizingNum).ReheatLoadMult;
                    } else if (ZoneEqFanCoil || ZoneEqUnitHeater) {
                        Cp = GetSpecificHeatGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                                   DataGlobals::HWInitConvTemp,
                                                   PlantLoop(DataWaterLoopNum).FluidIndex,
                                                   CallingRoutine);
                        rho = GetDensityGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                               DataGlobals::HWInitConvTemp,
                                               PlantLoop(DataWaterLoopNum).FluidIndex,
                                               CallingRoutine);
                        AutosizeDes = DataWaterFlowUsedForSizing * DataWaterCoilSizHeatDeltaT * Cp * rho;
                        coilSelectionReportObj->setCoilReheatMultiplier(CompName, CompType, 1.0);
                    } else {
                        if (ZoneEqSizing(CurZoneEqNum).SystemAirFlow) {
                            DesMassFlow = ZoneEqSizing(CurZoneEqNum).AirVolFlow * StdRhoAir;
                        } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                            DesMassFlow = ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow * StdRhoAir;
                        } else {
                            DesMassFlow = FinalZoneSizing(CurZoneEqNum).DesHeatMassFlow;
                        }
                        CoilInTemp =
                            setHeatCoilInletTempForZoneEqSizing(setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                        CoilInHumRat =
                            setHeatCoilInletHumRatForZoneEqSizing(setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing), zoneEqSizing, finalZoneSizing);
                        CoilOutTemp = FinalZoneSizing(CurZoneEqNum).HeatDesTemp;
                        CoilOutHumRat = FinalZoneSizing(CurZoneEqNum).HeatDesHumRat;
                        AutosizeDes = PsyCpAirFnWTdb(CoilOutHumRat, 0.5 * (CoilInTemp + CoilOutTemp)) * DesMassFlow * (CoilOutTemp - CoilInTemp);
                    }
                } else if (SizingType == HeatingWaterDesCoilWaterVolFlowUsedForUASizing) {
                    if (TermUnitSingDuct) {
                        AutosizeDes = DataWaterFlowUsedForSizing;
                        coilSelectionReportObj->setCoilReheatMultiplier(CompName, CompType, 1.0);
                    } else if ((TermUnitPIU || TermUnitIU) && (CurTermUnitSizingNum > 0)) {
                        AutosizeDes = DataWaterFlowUsedForSizing * TermUnitSizing(CurTermUnitSizingNum).ReheatLoadMult;
                        coilSelectionReportObj->setCoilReheatMultiplier(CompName, CompType, TermUnitSizing(CurTermUnitSizingNum).ReheatLoadMult);

                    } else if (ZoneEqFanCoil) {
                        AutosizeDes = DataWaterFlowUsedForSizing;
                        coilSelectionReportObj->setCoilReheatMultiplier(CompName, CompType, 1.0);
                    } else {
                        AutosizeDes = DataWaterFlowUsedForSizing;
                    }
                    bCheckForZero = false;
                } else if (SizingType == HeatingAirflowUASizing) {
                    if (TermUnitSingDuct && (CurTermUnitSizingNum > 0)) {
                        AutosizeDes = StdRhoAir * TermUnitSizing(CurTermUnitSizingNum).AirVolFlow;
                    } else if ((TermUnitPIU || TermUnitIU) && (CurTermUnitSizingNum > 0)) {
                        AutosizeDes =
                            StdRhoAir * TermUnitSizing(CurTermUnitSizingNum).AirVolFlow * TermUnitSizing(CurTermUnitSizingNum).ReheatAirFlowMult;
                    } else if (ZoneEqFanCoil) {
                        AutosizeDes = StdRhoAir * FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                    } else if (ZoneEqSizing(CurZoneEqNum).SystemAirFlow) {
                        AutosizeDes = ZoneEqSizing(CurZoneEqNum).AirVolFlow * StdRhoAir;
                    } else if (ZoneEqSizing(CurZoneEqNum).HeatingAirFlow) {
                        AutosizeDes = ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow * StdRhoAir;
                    } else {
                        AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesHeatMassFlow;
                    }
                } else if (SizingType == WaterHeatingCoilUASizing) {
                    if (DataCapacityUsedForSizing > 0.0 && DataWaterFlowUsedForSizing > 0.0 && DataFlowUsedForSizing > 0.0) {
                        Par(1) = DataCapacityUsedForSizing;
                        Par(2) = double(DataCoilNum);
                        Par(3) = double(DataFanOpMode); // fan operating mode
                        Par(4) = 1.0;                   // part-load ratio
                        UA0 = 0.001 * DataCapacityUsedForSizing;
                        UA1 = DataCapacityUsedForSizing;
                        // Invert the simple heating coil model: given the design inlet conditions and the design load,
                        // find the design UA.
                        SolveRoot(Acc, MaxIte, SolFla, AutosizeDes, SimpleHeatingCoilUAResidual, UA0, UA1, Par);
                        if (SolFla == -1) {
                            ShowSevereError("Autosizing of heating coil UA failed for Coil:Heating:Water \"" + CompName + "\"");
                            ShowContinueError("  Iteration limit exceeded in calculating coil UA");
                            ShowContinueError("  Lower UA estimate = " + TrimSigDigits(UA0, 6) + " W/m2-K (0.1% of Design Coil Load)");
                            ShowContinueError("  Upper UA estimate = " + TrimSigDigits(UA1, 6) + " W/m2-K (100% of Design Coil Load)");
                            ShowContinueError("  Final UA estimate when iterations exceeded limit = " + TrimSigDigits(AutosizeDes, 6) + " W/m2-K");
                            ShowContinueError("  Zone \"" + FinalZoneSizing(CurZoneEqNum).ZoneName +
                                              "\" coil sizing conditions (may be different than Sizing inputs):");
                            ShowContinueError("  Coil inlet air temperature     = " + TrimSigDigits(DataDesInletAirTemp, 3) + " C");
                            ShowContinueError("  Coil inlet air humidity ratio  = " + TrimSigDigits(DataDesInletAirHumRat, 3) + " kgWater/kgDryAir");
                            ShowContinueError("  Coil inlet air mass flow rate  = " + TrimSigDigits(DataFlowUsedForSizing, 6) + " kg/s");
                            // TotWaterHeatingCoilRate is set in CALL to CalcSimpleHeatingCoil
                            ShowContinueError("  Design Coil Capacity           = " + TrimSigDigits(DataDesignCoilCapacity, 3) + " W");
                            if (DataNomCapInpMeth) {
                                ShowContinueError("  Design Coil Load               = " + TrimSigDigits(DataCapacityUsedForSizing, 3) + " W");
                                ShowContinueError("  Coil outlet air temperature    = " + TrimSigDigits(DataDesOutletAirTemp, 3) + " C");
                                ShowContinueError("  Coil outlet air humidity ratio = " + TrimSigDigits(DataDesOutletAirHumRat, 3) +
                                                  " kgWater/kgDryAir");
                            } else if (TermUnitSingDuct || TermUnitPIU || TermUnitIU || ZoneEqFanCoil) {
                                ShowContinueError("  Design Coil Load               = " + TrimSigDigits(DataCapacityUsedForSizing, 3) + " W");
                            } else {
                                ShowContinueError("  Design Coil Load               = " + TrimSigDigits(DataCapacityUsedForSizing, 3) + " W");
                                ShowContinueError(
                                    "  Coil outlet air temperature    = " + TrimSigDigits(FinalZoneSizing(CurZoneEqNum).HeatDesTemp, 3) + " C");
                                ShowContinueError("  Coil outlet air humidity ratio = " +
                                                  TrimSigDigits(FinalZoneSizing(CurZoneEqNum).HeatDesHumRat, 3) + " kgWater/kgDryAir");
                            }
                            DataErrorsFound = true;
                        } else if (SolFla == -2) {
                            ShowSevereError("Autosizing of heating coil UA failed for Coil:Heating:Water \"" + CompName + "\"");
                            ShowContinueError("  Bad starting values for UA");
                            ShowContinueError("  Lower UA estimate = " + TrimSigDigits(UA0, 6) + " W/m2-K (0.1% of Design Coil Load)");
                            ShowContinueError("  Upper UA estimate = " + TrimSigDigits(UA1, 6) + " W/m2-K (100% of Design Coil Load)");
                            ShowContinueError("  Zone \"" + FinalZoneSizing(CurZoneEqNum).ZoneName +
                                              "\" coil sizing conditions (may be different than Sizing inputs):");
                            ShowContinueError("  Coil inlet air temperature     = " + TrimSigDigits(DataDesInletAirTemp, 3) + " C");
                            ShowContinueError("  Coil inlet air humidity ratio  = " + TrimSigDigits(DataDesInletAirHumRat, 3) + " kgWater/kgDryAir");
                            ShowContinueError("  Coil inlet air mass flow rate  = " + TrimSigDigits(DataFlowUsedForSizing, 6) + " kg/s");
                            ShowContinueError("  Design Coil Capacity           = " + TrimSigDigits(DataDesignCoilCapacity, 3) + " W");
                            if (DataNomCapInpMeth) {
                                ShowContinueError("  Design Coil Load               = " + TrimSigDigits(DataCapacityUsedForSizing, 3) + " W");
                                ShowContinueError("  Coil outlet air temperature    = " + TrimSigDigits(DataDesOutletAirTemp, 3) + " C");
                                ShowContinueError("  Coil outlet air humidity ratio = " + TrimSigDigits(DataDesOutletAirHumRat, 3) +
                                                  " kgWater/kgDryAir");
                            } else if (TermUnitSingDuct || TermUnitPIU || TermUnitIU || ZoneEqFanCoil) {
                                ShowContinueError("  Design Coil Load               = " + TrimSigDigits(DataCapacityUsedForSizing, 3) + " W");
                            } else {
                                ShowContinueError("  Design Coil Load               = " + TrimSigDigits(DataCapacityUsedForSizing, 3) + " W");
                                ShowContinueError(
                                    "  Coil outlet air temperature    = " + TrimSigDigits(FinalZoneSizing(CurZoneEqNum).HeatDesTemp, 3) + " C");
                                ShowContinueError("  Coil outlet air humidity ratio = " +
                                                  TrimSigDigits(FinalZoneSizing(CurZoneEqNum).HeatDesHumRat, 3) + " kgWater/kgDryAir");
                            }
                            // TotWaterHeatingCoilRate is set in CALL to CalcSimpleHeatingCoil
                            if (DataDesignCoilCapacity < DataCapacityUsedForSizing) {
                                ShowContinueError("  Inadequate water side capacity: in Plant Sizing for this hot water loop");
                                ShowContinueError("  increase design loop exit temperature and/or decrease design loop delta T");
                                ShowContinueError("  Plant Sizing object = " + PlantSizData(DataPltSizHeatNum).PlantLoopName);
                                ShowContinueError(
                                    "  Plant design loop exit temperature = " + TrimSigDigits(PlantSizData(DataPltSizHeatNum).ExitTemp, 3) + " C");
                                ShowContinueError("  Plant design loop delta T          = " + TrimSigDigits(DataWaterCoilSizHeatDeltaT, 3) + " C");
                            }
                            DataErrorsFound = true;
                        }
                    } else {
                        AutosizeDes = 1.0;
                        if (DataWaterFlowUsedForSizing > 0.0 && DataCapacityUsedForSizing == 0.0) {
                            ShowWarningError("The design coil load used for UA sizing is zero for Coil:Heating:Water " + CompName);
                            ShowContinueError("An autosize value for UA cannot be calculated");
                            ShowContinueError("Input a value for UA, change the heating design day, or raise");
                            ShowContinueError("  the zone heating design supply air temperature");
                            ShowContinueError("Water coil UA is set to 1 and the simulation continues.");
                        }
                    }
                } else if (SizingType == MaxHeaterOutletTempSizing) {
                    AutosizeDes = FinalZoneSizing(CurZoneEqNum).HeatDesTemp;
                } else if (SizingType == ZoneCoolingLoadSizing) {
                    AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesCoolLoad;
                } else if (SizingType == ZoneHeatingLoadSizing) {
                    AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesHeatLoad;
                } else if (SizingType == MinSATempCoolingSizing) {
                    if (DataCapacityUsedForSizing > 0.0 && DataFlowUsedForSizing > 0.0) {
                        AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesCoolCoilInTemp -
                                      (DataCapacityUsedForSizing / (DataFlowUsedForSizing * StdRhoAir *
                                                                    PsyCpAirFnWTdb(FinalZoneSizing(CurZoneEqNum).DesCoolCoilInHumRat,
                                                                                   FinalZoneSizing(CurZoneEqNum).DesCoolCoilInTemp)));
                    } else {
                        ShowSevereError(CallingRoutine + ' ' + CompType + ' ' + CompName + ", Developer Error: Component sizing incomplete.");
                        ShowContinueError("SizingString = " + SizingString +
                                          ", DataCapacityUsedForSizing = " + TrimSigDigits(DataCapacityUsedForSizing, 1));
                        ShowContinueError("SizingString = " + SizingString + ", DataFlowUsedForSizing = " + TrimSigDigits(DataFlowUsedForSizing, 1));
                    }
                } else if (SizingType == MaxSATempHeatingSizing) {
                    if (DataCapacityUsedForSizing > 0.0 && DataFlowUsedForSizing > 0.0) {
                        AutosizeDes = FinalZoneSizing(CurZoneEqNum).DesHeatCoilInTemp +
                                      (DataCapacityUsedForSizing / (DataFlowUsedForSizing * StdRhoAir *
                                                                    PsyCpAirFnWTdb(FinalZoneSizing(CurZoneEqNum).DesHeatCoilInHumRat,
                                                                                   FinalZoneSizing(CurZoneEqNum).DesHeatCoilInTemp)));
                    } else {
                        ShowSevereError(CallingRoutine + ' ' + CompType + ' ' + CompName + ", Developer Error: Component sizing incomplete.");
                        ShowContinueError("SizingString = " + SizingString +
                                          ", DataCapacityUsedForSizing = " + TrimSigDigits(DataCapacityUsedForSizing, 1));
                        ShowContinueError("SizingString = " + SizingString + ", DataFlowUsedForSizing = " + TrimSigDigits(DataFlowUsedForSizing, 1));
                    }
                } else if (SizingType == ASHRAEMinSATCoolingSizing) {
                    if (DataCapacityUsedForSizing > 0.0 && DataFlowUsedForSizing > 0.0) {
                        AutosizeDes = FinalZoneSizing(CurZoneEqNum).ZoneTempAtCoolPeak -
                                      (DataCapacityUsedForSizing / (DataFlowUsedForSizing * StdRhoAir *
                                                                    PsyCpAirFnWTdb(FinalZoneSizing(CurZoneEqNum).ZoneHumRatAtCoolPeak,
                                                                                   FinalZoneSizing(CurZoneEqNum).ZoneTempAtCoolPeak)));
                    } else {
                        ShowSevereError(CallingRoutine + ' ' + CompType + ' ' + CompName + ", Developer Error: Component sizing incomplete.");
                        ShowContinueError("SizingString = " + SizingString +
                                          ", DataCapacityUsedForSizing = " + TrimSigDigits(DataCapacityUsedForSizing, 1));
                        ShowContinueError("SizingString = " + SizingString + ", DataFlowUsedForSizing = " + TrimSigDigits(DataFlowUsedForSizing, 1));
                    }
                } else if (SizingType == ASHRAEMaxSATHeatingSizing) {
                    if (DataCapacityUsedForSizing > 0.0 && DataFlowUsedForSizing > 0.0) {
                        AutosizeDes = FinalZoneSizing(CurZoneEqNum).ZoneTempAtHeatPeak +
                                      (DataCapacityUsedForSizing / (DataFlowUsedForSizing * StdRhoAir *
                                                                    PsyCpAirFnWTdb(FinalZoneSizing(CurZoneEqNum).ZoneHumRatAtHeatPeak,
                                                                                   FinalZoneSizing(CurZoneEqNum).ZoneTempAtHeatPeak)));
                    } else {
                        ShowSevereError(CallingRoutine + ' ' + CompType + ' ' + CompName + ", Developer Error: Component sizing incomplete.");
                        ShowContinueError("SizingString = " + SizingString +
                                          ", DataCapacityUsedForSizing = " + TrimSigDigits(DataCapacityUsedForSizing, 1));
                        ShowContinueError("SizingString = " + SizingString + ", DataFlowUsedForSizing = " + TrimSigDigits(DataFlowUsedForSizing, 1));
                    }
                } else {
                    // should never happen
                }

                // get rid of temporary sizing array so next pass through will know there was no sizing data available
                if (FinalZoneSizingNotAllocated) DataSizing::FinalZoneSizing.deallocate();
            }

        } else if (CurSysNum > 0) {
            if (!IsAutoSize && !SizingDesRunThisAirSys) {
                HardSizeNoDesRun = true;
                AutosizeUser = SizingResult;
                if (PrintWarningFlag && SizingResult > 0.0) {
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
                if (SizingType == CoolingAirflowSizing) {
                    if (CurOASysNum > 0) {
                        if (OASysEqSizing(CurOASysNum).AirFlow) {
                            // Parent object sets flow rate
                            AutosizeDes = OASysEqSizing(CurOASysNum).AirVolFlow;
                        } else if (OASysEqSizing(CurOASysNum).CoolingAirFlow) {
                            // Parent object sets flow rate
                            AutosizeDes = OASysEqSizing(CurOASysNum).CoolingAirVolFlow;
                        } else if (DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                            AutosizeDes =
                                AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].SizingMassFlow / StdRhoAir;
                        } else {
                            AutosizeDes = FinalSysSizing(CurSysNum).DesOutAirVolFlow;
                        }
                    } else if (DataAirFlowUsedForSizing > 0.0) {
                        AutosizeDes = DataAirFlowUsedForSizing;
                    } else {
                        if (UnitarySysEqSizing(CurSysNum).AirFlow) {
                            AutosizeDes = UnitarySysEqSizing(CurSysNum).AirVolFlow;
                        } else if (UnitarySysEqSizing(CurSysNum).CoolingAirFlow) {
                            AutosizeDes = UnitarySysEqSizing(CurSysNum).CoolingAirVolFlow;
                        } else {
                            if (CurDuctType == Main) {
                                AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                            } else if (CurDuctType == Cooling) {
                                AutosizeDes = FinalSysSizing(CurSysNum).DesCoolVolFlow;
                            } else if (CurDuctType == Heating) {
                                AutosizeDes = FinalSysSizing(CurSysNum).DesHeatVolFlow;
                            } else if (CurDuctType == Other) {
                                AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                            } else {
                                AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                            }
                        }
                    }

                } else if (SizingType == HeatingAirflowSizing) {
                    if (CurOASysNum > 0) {
                        if (OASysEqSizing(CurOASysNum).AirFlow) {
                            // Parent object sets system flow rate
                            AutosizeDes = OASysEqSizing(CurOASysNum).AirVolFlow;
                        } else if (OASysEqSizing(CurOASysNum).HeatingAirFlow) {
                            // Parent object sets heating flow rate
                            AutosizeDes = OASysEqSizing(CurOASysNum).HeatingAirVolFlow;
                        } else if (DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                            AutosizeDes =
                                AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].SizingMassFlow / StdRhoAir;
                        } else {
                            AutosizeDes = FinalSysSizing(CurSysNum).DesOutAirVolFlow;
                        }
                    } else {
                        if (UnitarySysEqSizing(CurSysNum).AirFlow) {
                            AutosizeDes = UnitarySysEqSizing(CurSysNum).AirVolFlow;
                        } else if (UnitarySysEqSizing(CurSysNum).HeatingAirFlow) {
                            AutosizeDes = UnitarySysEqSizing(CurSysNum).HeatingAirVolFlow;
                        } else {
                            if (CurDuctType == Main) {
                                if (UtilityRoutines::SameString(CompType, "COIL:HEATING:WATER")) {
                                    if (FinalSysSizing(CurSysNum).SysAirMinFlowRat > 0.0 && !DataDesicRegCoil) {
                                        AutosizeDes = FinalSysSizing(CurSysNum).SysAirMinFlowRat * FinalSysSizing(CurSysNum).DesMainVolFlow;
                                    } else {
                                        AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                                    }
                                } else {
                                    AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                                }
                            } else if (CurDuctType == Cooling) {
                                if (UtilityRoutines::SameString(CompType, "COIL:HEATING:WATER")) {
                                    if (FinalSysSizing(CurSysNum).SysAirMinFlowRat > 0.0 && !DataDesicRegCoil) {
                                        AutosizeDes = FinalSysSizing(CurSysNum).SysAirMinFlowRat * FinalSysSizing(CurSysNum).DesCoolVolFlow;
                                    } else {
                                        AutosizeDes = FinalSysSizing(CurSysNum).DesCoolVolFlow;
                                    }
                                } else {
                                    AutosizeDes = FinalSysSizing(CurSysNum).DesCoolVolFlow;
                                }
                            } else if (CurDuctType == Heating) {
                                AutosizeDes = FinalSysSizing(CurSysNum).DesHeatVolFlow;
                            } else if (CurDuctType == Other) {
                                AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                            } else {
                                AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                            }
                        }
                    }
                } else if (SizingType == SystemAirflowSizing) {
                    if (HRFlowSizingFlag) { // HX sizing
                        if (CurOASysNum) {
                            if (FinalSysSizing(CurSysNum).DesOutAirVolFlow > 0.0) {
                                AutosizeDes = FinalSysSizing(CurSysNum).DesOutAirVolFlow;
                            } else {
                                // ELSE size to supply air duct flow rate
                                {
                                    auto const SELECT_CASE_var(CurDuctType);
                                    if (SELECT_CASE_var == Main) {
                                        AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                                    } else if (SELECT_CASE_var == Cooling) {
                                        AutosizeDes = FinalSysSizing(CurSysNum).DesCoolVolFlow;
                                    } else if (SELECT_CASE_var == Heating) {
                                        AutosizeDes = FinalSysSizing(CurSysNum).DesHeatVolFlow;
                                    } else if (SELECT_CASE_var == Other) {
                                        AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                                    } else {
                                        AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                                    }
                                }
                            }

                        } else {
                            {
                                auto const SELECT_CASE_var(CurDuctType);
                                if (SELECT_CASE_var == Main) {
                                    AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                                } else if (SELECT_CASE_var == Cooling) {
                                    AutosizeDes = FinalSysSizing(CurSysNum).DesCoolVolFlow;
                                } else if (SELECT_CASE_var == Heating) {
                                    AutosizeDes = FinalSysSizing(CurSysNum).DesHeatVolFlow;
                                } else if (SELECT_CASE_var == Other) {
                                    AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                                } else {
                                    AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                                }
                            }
                        }

                    } else {
                        if (AirLoopSysFlag) {
                            if (UnitarySysEqSizing(CurSysNum).CoolingAirFlow && UnitarySysEqSizing(CurSysNum).HeatingAirFlow) {
                                AutosizeDes =
                                    std::max(UnitarySysEqSizing(CurSysNum).CoolingAirVolFlow, UnitarySysEqSizing(CurSysNum).HeatingAirVolFlow);
                                if (AutosizeDes == UnitarySysEqSizing(CurSysNum).CoolingAirVolFlow) {
                                    if (DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD > 0 &&
                                        DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(
                                                WeatherManager::DesDayInput(DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD).Month) +
                                            "/" +
                                            General::TrimSigDigits(
                                                WeatherManager::DesDayInput(DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD).DayOfMonth) +
                                            " " +
                                            coilSelectionReportObj->getTimeText(DataSizing::SysSizPeakDDNum(CurSysNum).TimeStepAtCoolFlowPk(
                                                DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD));
                                    }

                                } else if (AutosizeDes == UnitarySysEqSizing(CurSysNum).HeatingAirVolFlow) {
                                    if (FinalSysSizing(CurSysNum).HeatDDNum > 0 &&
                                        FinalSysSizing(CurSysNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalSysSizing(CurSysNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalSysSizing(CurSysNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalSysSizing(CurSysNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalSysSizing(CurSysNum).SysHeatAirTimeStepPk);
                                    }
                                }
                            } else if (UnitarySysEqSizing(CurSysNum).CoolingAirFlow) {
                                AutosizeDes = UnitarySysEqSizing(CurSysNum).CoolingAirVolFlow;
                                if (DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD > 0 &&
                                    DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(
                                            WeatherManager::DesDayInput(DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD).Month) +
                                        "/" +
                                        General::TrimSigDigits(
                                            WeatherManager::DesDayInput(DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD).DayOfMonth) +
                                        " " +
                                        coilSelectionReportObj->getTimeText(DataSizing::SysSizPeakDDNum(CurSysNum).TimeStepAtCoolFlowPk(
                                            DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD));
                                }
                            } else if (UnitarySysEqSizing(CurSysNum).HeatingAirFlow) {
                                AutosizeDes = UnitarySysEqSizing(CurSysNum).HeatingAirVolFlow;
                                if (FinalSysSizing(CurSysNum).HeatDDNum > 0 && FinalSysSizing(CurSysNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalSysSizing(CurSysNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalSysSizing(CurSysNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalSysSizing(CurSysNum).HeatDDNum).DayOfMonth) + " " +
                                        coilSelectionReportObj->getTimeText(FinalSysSizing(CurSysNum).SysHeatAirTimeStepPk);
                                }

                            } else {
                                AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                                if (AutosizeDes == FinalSysSizing(CurSysNum).DesHeatVolFlow) {
                                    if (FinalSysSizing(CurSysNum).HeatDDNum > 0 &&
                                        FinalSysSizing(CurSysNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(FinalSysSizing(CurSysNum).HeatDDNum).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalSysSizing(CurSysNum).HeatDDNum).Month) + "/" +
                                            General::TrimSigDigits(WeatherManager::DesDayInput(FinalSysSizing(CurSysNum).HeatDDNum).DayOfMonth) +
                                            " " + coilSelectionReportObj->getTimeText(FinalSysSizing(CurSysNum).SysHeatAirTimeStepPk);
                                    }
                                } else if (AutosizeDes == FinalSysSizing(CurSysNum).DesCoolVolFlow) {
                                    if (DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD > 0 &&
                                        DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD <= DataEnvironment::TotDesDays) {
                                        DDNameFanPeak = WeatherManager::DesDayInput(DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD).Title;
                                        dateTimeFanPeak =
                                            General::TrimSigDigits(
                                                WeatherManager::DesDayInput(DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD).Month) +
                                            "/" +
                                            General::TrimSigDigits(
                                                WeatherManager::DesDayInput(DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD).DayOfMonth) +
                                            " " +
                                            coilSelectionReportObj->getTimeText(DataSizing::SysSizPeakDDNum(CurSysNum).TimeStepAtCoolFlowPk(
                                                DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD));
                                    }
                                }
                            }
                        } else {
                            AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                            if (AutosizeDes == FinalSysSizing(CurSysNum).DesHeatVolFlow) {
                                if (FinalSysSizing(CurSysNum).HeatDDNum > 0 && FinalSysSizing(CurSysNum).HeatDDNum <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(FinalSysSizing(CurSysNum).HeatDDNum).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalSysSizing(CurSysNum).HeatDDNum).Month) + "/" +
                                        General::TrimSigDigits(WeatherManager::DesDayInput(FinalSysSizing(CurSysNum).HeatDDNum).DayOfMonth) + " " +
                                        coilSelectionReportObj->getTimeText(FinalSysSizing(CurSysNum).SysHeatAirTimeStepPk);
                                }
                            } else if (AutosizeDes == FinalSysSizing(CurSysNum).DesCoolVolFlow) {
                                if (DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD > 0 &&
                                    DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD <= DataEnvironment::TotDesDays) {
                                    DDNameFanPeak = WeatherManager::DesDayInput(DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD).Title;
                                    dateTimeFanPeak =
                                        General::TrimSigDigits(
                                            WeatherManager::DesDayInput(DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD).Month) +
                                        "/" +
                                        General::TrimSigDigits(
                                            WeatherManager::DesDayInput(DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD).DayOfMonth) +
                                        " " +
                                        coilSelectionReportObj->getTimeText(DataSizing::SysSizPeakDDNum(CurSysNum).TimeStepAtCoolFlowPk(
                                            DataSizing::SysSizPeakDDNum(CurSysNum).CoolFlowPeakDD));
                                }
                            }
                        }
                        if (DataFractionUsedForSizing > 0.0) AutosizeDes = AutosizeDes * DataFractionUsedForSizing;
                    }
                } else if (SizingType == CoolingWaterflowSizing) {
                    if (CurOASysNum > 0) {
                        CoilDesWaterDeltaT = 0.5 * DataWaterCoilSizCoolDeltaT;
                    } else {
                        CoilDesWaterDeltaT = DataWaterCoilSizCoolDeltaT;
                    }
                    if (DataCapacityUsedForSizing >= SmallLoad) {
                        Cp = GetSpecificHeatGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                                   DataGlobals::CWInitConvTemp,
                                                   PlantLoop(DataWaterLoopNum).FluidIndex,
                                                   CallingRoutine);
                        rho = GetDensityGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                               DataGlobals::CWInitConvTemp,
                                               PlantLoop(DataWaterLoopNum).FluidIndex,
                                               CallingRoutine);
                        AutosizeDes = DataCapacityUsedForSizing / (CoilDesWaterDeltaT * Cp * rho);
                    } else {
                        AutosizeDes = 0.0;
                        // Warning about zero design coil load is issued elsewhere.
                    }
                    bCheckForZero = false;
                } else if (SizingType == HeatingWaterflowSizing) {
                    if (DataCapacityUsedForSizing >= SmallLoad) {
                        Cp = GetSpecificHeatGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                                   DataGlobals::HWInitConvTemp,
                                                   PlantLoop(DataWaterLoopNum).FluidIndex,
                                                   CallingRoutine);
                        rho = GetDensityGlycol(PlantLoop(DataWaterLoopNum).FluidName,
                                               DataGlobals::HWInitConvTemp,
                                               PlantLoop(DataWaterLoopNum).FluidIndex,
                                               CallingRoutine);
                        AutosizeDes = DataCapacityUsedForSizing / (DataWaterCoilSizHeatDeltaT * Cp * rho);
                    } else {
                        AutosizeDes = 0.0;
                        // Warning about zero design coil load is issued elsewhere.
                    }
                    bCheckForZero = false;
                } else if (SizingType == HeatingWaterDesAirInletTempSizing) {
                    if (CurOASysNum > 0) {
                        OutAirFrac = 1.0;
                    } else if (FinalSysSizing(CurSysNum).HeatOAOption == MinOA) {
                        if (DataFlowUsedForSizing > 0.0) {
                            OutAirFrac = FinalSysSizing(CurSysNum).DesOutAirVolFlow / DataFlowUsedForSizing;
                        } else {
                            OutAirFrac = 1.0;
                        }
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                    } else {
                        OutAirFrac = 1.0;
                    }
                    // coil inlet temperature
                    if (CurOASysNum == 0 && PrimaryAirSystem(CurSysNum).NumOAHeatCoils > 0) {
                        AutosizeDes = OutAirFrac * FinalSysSizing(CurSysNum).PreheatTemp + (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).HeatRetTemp;
                    } else if (CurOASysNum > 0 && DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                        AutosizeDes = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].HeatOutTemp;
                    } else {
                        AutosizeDes = OutAirFrac * FinalSysSizing(CurSysNum).HeatOutTemp + (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).HeatRetTemp;
                    }
                    bCheckForZero = false;
                } else if (SizingType == HeatingWaterDesAirInletHumRatSizing) {
                    if (CurOASysNum > 0) {
                        OutAirFrac = 1.0;
                    } else if (FinalSysSizing(CurSysNum).HeatOAOption == MinOA) {
                        if (DataFlowUsedForSizing > 0.0) {
                            OutAirFrac = FinalSysSizing(CurSysNum).DesOutAirVolFlow / DataFlowUsedForSizing;
                        } else {
                            OutAirFrac = 1.0;
                        }
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                    } else {
                        OutAirFrac = 1.0;
                    }
                    // coil inlet humidity ratio
                    if (CurOASysNum == 0 && PrimaryAirSystem(CurSysNum).NumOAHeatCoils > 0) {
                        AutosizeDes =
                            OutAirFrac * FinalSysSizing(CurSysNum).PreheatHumRat + (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).HeatRetHumRat;
                    } else if (CurOASysNum > 0 && DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                        AutosizeDes = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].HeatOutHumRat;
                    } else {
                        AutosizeDes =
                            OutAirFrac * FinalSysSizing(CurSysNum).HeatOutHumRat + (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).HeatRetHumRat;
                    }
                    bCheckForZero = false;
                } else if (SizingType == CoolingWaterDesAirInletTempSizing) {
                    if (CurOASysNum > 0) { // coil is in OA stream
                        if (DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                            AutosizeDes = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].SizingCoolOATemp;
                        } else {
                            AutosizeDes = FinalSysSizing(CurSysNum).OutTempAtCoolPeak;
                        }
                    } else {                                                   // coil is in main air loop
                        if (PrimaryAirSystem(CurSysNum).NumOACoolCoils == 0) { // there is no precooling of the OA stream
                            AutosizeDes = FinalSysSizing(CurSysNum).MixTempAtCoolPeak;
                        } else if (DataDesInletAirTemp > 0.0) {
                            AutosizeDes = DataDesInletAirTemp;
                        } else { // there is precooling of the OA stream
                            if (DataFlowUsedForSizing > 0.0) {
                                OutAirFrac = FinalSysSizing(CurSysNum).DesOutAirVolFlow / DataFlowUsedForSizing;
                            } else {
                                OutAirFrac = 1.0;
                            }
                            OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                            AutosizeDes =
                                OutAirFrac * FinalSysSizing(CurSysNum).PrecoolTemp + (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).RetTempAtCoolPeak;
                        }
                        Real64 fanDeltaT = 0.0;
                        if (PrimaryAirSystem(CurSysNum).supFanLocation == DataAirSystems::fanPlacement::BlowThru) {
                            // water coils on main branch have no parent object to set DataFan* variables
                            if (DataFanIndex == -1) {
                                if (PrimaryAirSystem(CurSysNum).supFanModelTypeEnum == DataAirSystems::structArrayLegacyFanModels) {
                                    DataFanEnumType = DataAirSystems::structArrayLegacyFanModels;
                                    DataFanIndex = PrimaryAirSystem(CurSysNum).SupFanNum;
                                } else if (PrimaryAirSystem(CurSysNum).supFanModelTypeEnum == DataAirSystems::objectVectorOOFanSystemModel) {
                                    DataFanEnumType = DataAirSystems::objectVectorOOFanSystemModel;
                                    DataFanIndex = PrimaryAirSystem(CurSysNum).supFanVecIndex;
                                }
                            }
                            // calculate fan heat to get fan air-side delta T
                            FanCoolLoad = DataAirSystems::calcFanDesignHeatGain(DataFanEnumType, DataFanIndex, DataAirFlowUsedForSizing);
                            if (DataDesInletAirHumRat > 0.0 && DataAirFlowUsedForSizing > 0.0) {
                                CpAir = PsyCpAirFnWTdb(DataDesInletAirHumRat, AutosizeDes);
                                fanDeltaT = FanCoolLoad / (CpAir * StdRhoAir * DataAirFlowUsedForSizing);
                                DataDesAccountForFanHeat = false; // used in CoolingCapacitySizing calculations to avoid double counting fan heat
                            }
                        }
                        AutosizeDes += fanDeltaT;
                    }
                    bCheckForZero = false;
                } else if (SizingType == CoolingWaterDesWaterInletTempSizing) {
                    AutosizeDes = PlantSizData(DataPltSizCoolNum).ExitTemp;
                    bCheckForZero = false;
                } else if (SizingType == CoolingWaterNumofTubesPerRowSizing) {
                    AutosizeDes = int(max(3.0, 13750.0 * DataWaterFlowUsedForSizing + 1.0));
                    bCheckForZero = false;
                } else if (SizingType == CoolingWaterDesAirOutletTempSizing) {
                    if (CurOASysNum > 0) {
                        if (DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                            AutosizeDes = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].PrecoolTemp;
                        } else {
                            AutosizeDes = FinalSysSizing(CurSysNum).PrecoolTemp;
                        }
                    } else if (DataDesOutletAirTemp > 0.0) {
                        AutosizeDes = DataDesOutletAirTemp;
                        Real64 fanDeltaT = 0.0;
                        if (PrimaryAirSystem(CurSysNum).supFanLocation == DataAirSystems::fanPlacement::DrawThru) {
                            // water coils on main branch have no parent object to set DataFan* variables
                            if (DataFanIndex == -1) {
                                if (PrimaryAirSystem(CurSysNum).supFanModelTypeEnum == DataAirSystems::structArrayLegacyFanModels) {
                                    DataFanEnumType = DataAirSystems::structArrayLegacyFanModels;
                                    DataFanIndex = PrimaryAirSystem(CurSysNum).SupFanNum;
                                } else if (PrimaryAirSystem(CurSysNum).supFanModelTypeEnum == DataAirSystems::objectVectorOOFanSystemModel) {
                                    DataFanEnumType = DataAirSystems::objectVectorOOFanSystemModel;
                                    DataFanIndex = PrimaryAirSystem(CurSysNum).supFanVecIndex;
                                }
                            }
                            // calculate fan heat to get fan air-side delta T
                            FanCoolLoad = DataAirSystems::calcFanDesignHeatGain(DataFanEnumType, DataFanIndex, DataAirFlowUsedForSizing);
                            if (DataDesInletAirHumRat > 0.0 && DataAirFlowUsedForSizing > 0.0) {
                                CpAir = PsyCpAirFnWTdb(DataDesInletAirHumRat, AutosizeDes);
                                fanDeltaT = FanCoolLoad / (CpAir * StdRhoAir * DataAirFlowUsedForSizing);
                                DataDesAccountForFanHeat = false; // used in CoolingCapacitySizing calculations to avoid double counting fan heat
                            }
                        }
                        AutosizeDes -= fanDeltaT;
                    } else {
                        AutosizeDes = FinalSysSizing(CurSysNum).CoolSupTemp;
                        Real64 fanDeltaT = 0.0;
                        if (PrimaryAirSystem(CurSysNum).supFanLocation == DataAirSystems::fanPlacement::DrawThru) {
                            // water coils on main branch have no parent object to set DataFan* variables
                            if (DataFanIndex == -1) {
                                if (PrimaryAirSystem(CurSysNum).supFanModelTypeEnum == DataAirSystems::structArrayLegacyFanModels) {
                                    DataFanEnumType = DataAirSystems::structArrayLegacyFanModels;
                                    DataFanIndex = PrimaryAirSystem(CurSysNum).SupFanNum;
                                } else if (PrimaryAirSystem(CurSysNum).supFanModelTypeEnum == DataAirSystems::objectVectorOOFanSystemModel) {
                                    DataFanEnumType = DataAirSystems::objectVectorOOFanSystemModel;
                                    DataFanIndex = PrimaryAirSystem(CurSysNum).supFanVecIndex;
                                }
                            }
                            // calculate fan heat to get fan air-side delta T
                            FanCoolLoad = DataAirSystems::calcFanDesignHeatGain(DataFanEnumType, DataFanIndex, DataAirFlowUsedForSizing);
                            if (DataDesInletAirHumRat > 0.0 && DataAirFlowUsedForSizing > 0.0) {
                                CpAir = PsyCpAirFnWTdb(DataDesInletAirHumRat, AutosizeDes);
                                fanDeltaT = FanCoolLoad / (CpAir * StdRhoAir * DataAirFlowUsedForSizing);
                                DataDesAccountForFanHeat = false; // used in CoolingCapacitySizing calculations to avoid double counting fan heat
                            }
                        }
                        AutosizeDes -= fanDeltaT;
                    }
                    if (AutosizeDes < DataDesInletWaterTemp && DataWaterFlowUsedForSizing > 0.0) {
                        ShowWarningError(CallingRoutine + ":" + " Coil=\"" + CompName +
                                         "\", Cooling Coil has leaving air temperature < entering water temperature.");
                        ShowContinueError("    Tair,out  =  " + RoundSigDigits(AutosizeDes, 3));
                        ShowContinueError("    Twater,in = " + RoundSigDigits(DataDesInletWaterTemp, 3));
                        AutosizeDes = DataDesInletWaterTemp + 0.5;
                        ShowContinueError("....coil leaving air temperature will be reset to:");
                        ShowContinueError("    Tair,out = " + RoundSigDigits(AutosizeDes, 3));
                    }
                    bCheckForZero = false;
                } else if (SizingType == CoolingWaterDesAirInletHumRatSizing) {
                    if (CurOASysNum > 0) { // coil is in OA stream
                        if (DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                            AutosizeDes = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].SizingCoolOAHumRat;
                        } else {
                            AutosizeDes = FinalSysSizing(CurSysNum).OutHumRatAtCoolPeak;
                        }
                    } else if (DataDesInletAirHumRat > 0.0) {
                        AutosizeDes = DataDesInletAirHumRat;
                    } else {                                                   // coil is in main air loop
                        if (PrimaryAirSystem(CurSysNum).NumOACoolCoils == 0) { // there is no precooling of the OA stream
                            AutosizeDes = FinalSysSizing(CurSysNum).MixHumRatAtCoolPeak;
                        } else { // there is precooling of the OA stream
                            if (DataFlowUsedForSizing > 0.0) {
                                OutAirFrac = FinalSysSizing(CurSysNum).DesOutAirVolFlow / DataFlowUsedForSizing;
                            } else {
                                OutAirFrac = 1.0;
                            }
                            OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                            AutosizeDes = OutAirFrac * FinalSysSizing(CurSysNum).PrecoolHumRat +
                                          (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).RetHumRatAtCoolPeak;
                        }
                    }
                    bCheckForZero = false;
                } else if (SizingType == CoolingWaterDesAirOutletHumRatSizing) {
                    if (CurOASysNum > 0) {
                        if (DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                            AutosizeDes = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].PrecoolHumRat;
                        } else {
                            AutosizeDes = FinalSysSizing(CurSysNum).PrecoolHumRat;
                        }
                    } else if (DataDesOutletAirHumRat > 0.0) {
                        AutosizeDes = DataDesOutletAirHumRat;
                    } else {
                        AutosizeDes = FinalSysSizing(CurSysNum).CoolSupHumRat;
                    }
                    if (AutosizeDes > DataDesInletAirHumRat &&
                        (UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER") ||
                         UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER:DETAILEDGEOMETRY"))) { // flow here is water vol flow rate
                        ShowWarningError(CallingRoutine + ":" + " Coil=\"" + CompName +
                                         "\", Cooling Coil has leaving humidity ratio > entering humidity ratio.");
                        ShowContinueError("    Wair,in =  " + RoundSigDigits(DataDesInletAirHumRat, 6) + " [KGWATER/KGDRYAIR]");
                        ShowContinueError("    Wair,out = " + RoundSigDigits(AutosizeDes, 6) + " [KGWATER/KGDRYAIR]");
                        if (DataDesInletAirHumRat > 0.016) {
                            AutosizeDes = 0.5 * DataDesInletAirHumRat;
                        } else {
                            AutosizeDes = DataDesInletAirHumRat;
                        }
                        ShowContinueError("....coil leaving humidity ratio will be reset to:");
                        ShowContinueError("    Wair,out = " + RoundSigDigits(AutosizeDes, 6) + " [KGWATER/KGDRYAIR]");
                    }

                    // check for dry coil and reset outlet humrat if needed
                    DesSatEnthAtWaterInTemp = PsyHFnTdbW(DataDesInletWaterTemp, PsyWFnTdpPb(DataDesInletWaterTemp, StdBaroPress));
                    DesHumRatAtWaterInTemp = PsyWFnTdbH(DataDesInletWaterTemp, DesSatEnthAtWaterInTemp, CallingRoutine);
                    if (AutosizeDes < DataDesInletAirHumRat && DesHumRatAtWaterInTemp > DataDesInletAirHumRat) {
                        if (UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER") ||
                            UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER:DETAILEDGEOMETRY")) {
                            ShowWarningError(CallingRoutine + ":" + " Coil=\"" + CompName +
                                             "\", Cooling Coil is running dry for sizing because minimum humidity ratio at saturation for inlet "
                                             "chilled water temperature > design air entering humidity ratio.");
                            ShowContinueError("    Wair,in =  " + RoundSigDigits(DataDesInletAirHumRat, 6) + " [KGWATER/KGDRYAIR]");
                            ShowContinueError("    Wair,out = " + RoundSigDigits(AutosizeDes, 6) + " [KGWATER/KGDRYAIR]");
                            ShowContinueError("    Inlet chilled water temperature = " + RoundSigDigits(DataDesInletWaterTemp, 3) + " [C]");
                            ShowContinueError("    Minimum humidity ratio at saturation for inlet chilled water temperature = " +
                                              RoundSigDigits(DesHumRatAtWaterInTemp, 6) + " [KGWATER/KGDRYAIR]");
                            AutosizeDes = DataDesInletAirHumRat;
                            ShowContinueError("....coil leaving humidity ratio will be reset to:");
                            ShowContinueError("    Wair,out = " + RoundSigDigits(AutosizeDes, 6) + " [KGWATER/KGDRYAIR]");
                        }
                    }
                    bCheckForZero = false;
                } else if (SizingType == HeatingCoilDesAirInletTempSizing) {
                    if (DataDesicRegCoil) {
                        if (DesicDehum(DataDesicDehumNum).RegenInletIsOutsideAirNode) {
                            AutosizeDes = FinalSysSizing(CurSysNum).HeatOutTemp;
                        } else {
                            AutosizeDes = FinalSysSizing(CurSysNum).HeatRetTemp;
                        }
                    }
                    bCheckForZero = false;
                } else if (SizingType == HeatingCoilDesAirOutletTempSizing) {
                    if (DataDesicRegCoil) {
                        AutosizeDes = DesicDehum(DataDesicDehumNum).RegenSetPointTemp;
                    }

                    bCheckForZero = false;
                } else if (SizingType == HeatingCoilDesAirInletHumRatSizing) {
                    if (DataDesicRegCoil) {
                        if (DesicDehum(DataDesicDehumNum).RegenInletIsOutsideAirNode) {
                            AutosizeDes = FinalSysSizing(CurSysNum).HeatOutHumRat;
                        } else {
                            AutosizeDes = FinalSysSizing(CurSysNum).HeatRetHumRat;
                        }
                    }

                    bCheckForZero = false;
                } else if (SizingType == CoolingSHRSizing) {
                    if (DataFlowUsedForSizing >= SmallAirVolFlow && DataCapacityUsedForSizing > 0.0) {
                        // For autosizing the rated SHR, we set a minimum SHR of 0.676 and a maximum of 0.798. The min SHR occurs occurs at the
                        // minimum flow / capacity ratio = MinRatedVolFlowPerRatedTotCap = 0.00004027 [m3/s / W] = 300 [cfm/ton].
                        // The max SHR occurs at maximum flow / capacity ratio = MaxRatedVolFlowPerRatedTotCap = 0.00006041 [m3/s / W] = 450
                        // [cfm/ton]. For flow / capacity ratios between the min and max we linearly interpolate between min and max SHR. Thus
                        // rated SHR is a linear function of the rated flow / capacity ratio. This linear function (see below) is the result of a
                        // regression of flow/capacity ratio vs SHR for several actual coils.
                        RatedVolFlowPerRatedTotCap = DataFlowUsedForSizing / DataCapacityUsedForSizing;
                        if (DXCT == RegularDXCoil) {
                            if (RatedVolFlowPerRatedTotCap > MaxRatedVolFlowPerRatedTotCap(DXCT)) {
                                AutosizeDes = 0.431 + 6086.0 * MaxRatedVolFlowPerRatedTotCap(DXCT);
                            } else if (RatedVolFlowPerRatedTotCap < MinRatedVolFlowPerRatedTotCap(DXCT)) {
                                AutosizeDes = 0.431 + 6086.0 * MinRatedVolFlowPerRatedTotCap(DXCT);
                            } else if (CurOASysNum > 0 && DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                                AutosizeDes = 0.431 + 6086.0 * RatedVolFlowPerRatedTotCap;
                            } else {
                                AutosizeDes = 0.431 + 6086.0 * RatedVolFlowPerRatedTotCap;
                            }
                        } else { // DOASDXCoil, or DXCT = 2
                            if (RatedVolFlowPerRatedTotCap > MaxRatedVolFlowPerRatedTotCap(DXCT)) {
                                AutosizeDes = 0.389 + 7684.0 * MaxRatedVolFlowPerRatedTotCap(DXCT);
                            } else if (RatedVolFlowPerRatedTotCap < MinRatedVolFlowPerRatedTotCap(DXCT)) {
                                AutosizeDes = 0.389 + 7684.0 * MinRatedVolFlowPerRatedTotCap(DXCT);
                            } else {
                                AutosizeDes = 0.389 + 7684.0 * RatedVolFlowPerRatedTotCap;
                            }
                        }

                        // check that the autosized SHR corresponds to a valid apperatus dew point (ADP) temperature
                        AutosizeDes = ValidateADP(CompType,
                                                  CompName,
                                                  RatedInletAirTemp,
                                                  RatedInletAirHumRat,
                                                  DataCapacityUsedForSizing,
                                                  DataFlowUsedForSizing,
                                                  AutosizeDes,
                                                  CallingRoutine);

                    } else {
                        ShowSevereError(CallingRoutine + ' ' + CompType + ' ' + CompName);
                        ShowContinueError("... DataFlowUsedForSizing and DataCapacityUsedForSizing " + SizingString +
                                          " must both be greater than 0.");
                        ShowFatalError("Preceding conditions cause termination.");
                    }
                } else if (SizingType == CoolingCapacitySizing) {
                    DataFracOfAutosizedCoolingCapacity = 1.0;
                    if (OASysFlag) {
                        AutosizeDes = OASysEqSizing(CurOASysNum).DesCoolingLoad;
                        DesVolFlow = DataFlowUsedForSizing;
                    } else if (AirLoopSysFlag) {
                        AutosizeDes = UnitarySysEqSizing(CurSysNum).DesCoolingLoad;
                        DesVolFlow = DataFlowUsedForSizing;
                        CoilInTemp = DataSizing::DataCoilSizingAirInTemp;
                        CoilInHumRat = DataSizing::DataCoilSizingAirInHumRat;
                        CoilOutTemp = DataSizing::DataCoilSizingAirOutTemp;
                        CoilOutHumRat = DataSizing::DataCoilSizingAirOutHumRat;
                        FanCoolLoad = DataSizing::DataCoilSizingFanCoolLoad;
                        TotCapTempModFac = DataSizing::DataCoilSizingCapFT;
                        if (coilSelectionReportObj->isCompTypeCoil(CompType)) {
                            coilSelectionReportObj->setCoilEntAirHumRat(CompName, CompType, CoilInHumRat);
                            coilSelectionReportObj->setCoilEntAirTemp(CompName, CompType, CoilInTemp, CurSysNum, CurZoneEqNum);
                            coilSelectionReportObj->setCoilLvgAirTemp(CompName, CompType, CoilOutTemp);
                            coilSelectionReportObj->setCoilLvgAirHumRat(CompName, CompType, CoilOutHumRat);
                        }
                    } else if (CurOASysNum > 0 && DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                        DesVolFlow = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].SizingMassFlow / StdRhoAir;
                        if (AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].DXCoilFlag) {
                            AutosizeDes = DesVolFlow / 0.00005;
                        } else {
                            CoilInTemp = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].SizingCoolOATemp;
                            if (AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].m_FanIndex > -1 &&
                                AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].FanBlowTroughFlag &&
                                AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].m_FanTypeNum ==
                                    SimAirServingZones::Fan_System_Object) {
                                int FanIndex = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].m_FanIndex;
                                Real64 DeltaT = HVACFan::fanObjs[FanIndex]->getFanDesignTemperatureRise();
                                CoilInTemp += DeltaT;
                            }
                            CoilInHumRat = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].SizingCoolOAHumRat;
                            CoilOutTemp = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].PrecoolTemp;
                            CoilOutHumRat = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].PrecoolHumRat;
                            AutosizeDes =
                                DesVolFlow * StdRhoAir *
                                (Psychrometrics::PsyHFnTdbW(CoilInTemp, CoilInHumRat) - Psychrometrics::PsyHFnTdbW(CoilOutTemp, CoilOutHumRat));
                        }
                    } else {
                        CheckSysSizing(CompType, CompName);
                        DesVolFlow = DataFlowUsedForSizing;
                        if (FinalSysSizing(CurSysNum).CoolingCapMethod == FractionOfAutosizedCoolingCapacity) {
                            DataFracOfAutosizedCoolingCapacity = FinalSysSizing(CurSysNum).FractionOfAutosizedCoolingCapacity;
                        }
                        if (FinalSysSizing(CurSysNum).CoolingCapMethod == CapacityPerFloorArea) {
                            NominalCapacityDes = FinalSysSizing(CurSysNum).CoolingTotalCapacity;
                            AutosizeDes = NominalCapacityDes;
                        } else if (FinalSysSizing(CurSysNum).CoolingCapMethod == CoolingDesignCapacity &&
                                   FinalSysSizing(CurSysNum).CoolingTotalCapacity > 0.0) {
                            NominalCapacityDes = FinalSysSizing(CurSysNum).CoolingTotalCapacity;
                            AutosizeDes = NominalCapacityDes;
                        } else if (DesVolFlow >= SmallAirVolFlow) {
                            OutAirFrac = 0.0;
                            if (DesVolFlow > 0.0) {
                                OutAirFrac = FinalSysSizing(CurSysNum).DesOutAirVolFlow / DesVolFlow;
                            } else {
                                OutAirFrac = 1.0;
                            }
                            OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                            if (CurOASysNum > 0) { // coil is in the OA stream
                                CoilInTemp = FinalSysSizing(CurSysNum).OutTempAtCoolPeak;
                                CoilInHumRat = FinalSysSizing(CurSysNum).OutHumRatAtCoolPeak;
                                CoilOutTemp = FinalSysSizing(CurSysNum).PrecoolTemp;
                                CoilOutHumRat = FinalSysSizing(CurSysNum).PrecoolHumRat;
                            } else { // coil is on the main air loop
                                if (DataAirFlowUsedForSizing > 0.0) {
                                    DesVolFlow = DataAirFlowUsedForSizing;
                                }
                                if (DataDesOutletAirTemp > 0.0) {
                                    CoilOutTemp = DataDesOutletAirTemp;
                                } else {
                                    CoilOutTemp = FinalSysSizing(CurSysNum).CoolSupTemp;
                                }
                                if (DataDesOutletAirHumRat > 0.0) {
                                    CoilOutHumRat = DataDesOutletAirHumRat;
                                } else {
                                    CoilOutHumRat = FinalSysSizing(CurSysNum).CoolSupHumRat;
                                }

                                if (PrimaryAirSystem(CurSysNum).NumOACoolCoils == 0) { // there is no precooling of the OA stream
                                    CoilInTemp = FinalSysSizing(CurSysNum).MixTempAtCoolPeak;
                                    CoilInHumRat = FinalSysSizing(CurSysNum).MixHumRatAtCoolPeak;
                                } else { // there is precooling of OA stream
                                    if (DesVolFlow > 0.0) {
                                        OutAirFrac = FinalSysSizing(CurSysNum).DesOutAirVolFlow / DesVolFlow;
                                    } else {
                                        OutAirFrac = 1.0;
                                    }
                                    OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                                    CoilInTemp = OutAirFrac * FinalSysSizing(CurSysNum).PrecoolTemp +
                                                 (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).RetTempAtCoolPeak;
                                    CoilInHumRat = OutAirFrac * FinalSysSizing(CurSysNum).PrecoolHumRat +
                                                   (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).RetHumRatAtCoolPeak;
                                }
                                if (DataDesInletAirTemp > 0.0) CoilInTemp = DataDesInletAirTemp;
                                if (DataDesInletAirHumRat > 0.0) CoilInHumRat = DataDesInletAirHumRat;
                            }
                            OutTemp = FinalSysSizing(CurSysNum).OutTempAtCoolPeak;
                            if (UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER") ||
                                UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER:DETAILEDGEOMETRY")) {
                                rhoair = StdRhoAir;
                            } else {
                                rhoair = PsyRhoAirFnPbTdbW(StdBaroPress, CoilInTemp, CoilInHumRat, CallingRoutine);
                            }
                            CoilOutTemp = min(CoilInTemp, CoilOutTemp);
                            CoilOutHumRat = min(CoilInHumRat, CoilOutHumRat);
                            CoilInEnth = PsyHFnTdbW(CoilInTemp, CoilInHumRat);
                            CoilInWetBulb = PsyTwbFnTdbWPb(CoilInTemp, CoilInHumRat, StdBaroPress, CallingRoutine);
                            CoilOutEnth = PsyHFnTdbW(CoilOutTemp, CoilOutHumRat);
                            SupFanNum = PrimaryAirSystem(CurSysNum).SupFanNum;
                            RetFanNum = PrimaryAirSystem(CurSysNum).RetFanNum;
                            switch (PrimaryAirSystem(CurSysNum).supFanModelTypeEnum) {
                            case DataAirSystems::structArrayLegacyFanModels: {
                                FanCoolLoad = FanDesHeatGain(PrimaryAirSystem(CurSysNum).SupFanNum, DesVolFlow);
                                if (coilSelectionReportObj->isCompTypeCoil(CompType) && (SupFanNum > 0)) {
                                    coilSelectionReportObj->setCoilSupplyFanInfo(CompName,
                                                                                 CompType,
                                                                                 Fans::Fan(PrimaryAirSystem(CurSysNum).SupFanNum).FanName,
                                                                                 DataAirSystems::structArrayLegacyFanModels,
                                                                                 PrimaryAirSystem(CurSysNum).SupFanNum);
                                }

                                break;
                            }
                            case DataAirSystems::objectVectorOOFanSystemModel: {
                                FanCoolLoad = HVACFan::fanObjs[PrimaryAirSystem(CurSysNum).supFanVecIndex]->getFanDesignHeatGain(DesVolFlow);
                                if (coilSelectionReportObj->isCompTypeCoil(CompType) && (PrimaryAirSystem(CurSysNum).supFanVecIndex >= 0)) {
                                    coilSelectionReportObj->setCoilSupplyFanInfo(CompName,
                                                                                 CompType,
                                                                                 HVACFan::fanObjs[PrimaryAirSystem(CurSysNum).supFanVecIndex]->name,
                                                                                 DataAirSystems::objectVectorOOFanSystemModel,
                                                                                 PrimaryAirSystem(CurSysNum).supFanVecIndex);
                                }
                                break;
                            }
                            case DataAirSystems::fanModelTypeNotYetSet: {
                                // do nothing
                                break;
                            }
                            } // end switch

                            switch (PrimaryAirSystem(CurSysNum).retFanModelTypeEnum) {
                            case DataAirSystems::structArrayLegacyFanModels: {
                                FanCoolLoad += (1.0 - OutAirFrac) * FanDesHeatGain(PrimaryAirSystem(CurSysNum).RetFanNum, DesVolFlow);
                                break;
                            }
                            case DataAirSystems::objectVectorOOFanSystemModel: {
                                FanCoolLoad += (1.0 - OutAirFrac) *
                                               HVACFan::fanObjs[PrimaryAirSystem(CurSysNum).retFanVecIndex]->getFanDesignHeatGain(DesVolFlow);
                                break;
                            }
                            case DataAirSystems::fanModelTypeNotYetSet: {
                                // do nothing
                                break;
                            }
                            } // end switch

                            PrimaryAirSystem(CurSysNum).FanDesCoolLoad = FanCoolLoad;
                            PeakCoilLoad = max(0.0, (rhoair * DesVolFlow * (CoilInEnth - CoilOutEnth)));
                            CpAir = PsyCpAirFnWTdb(CoilInHumRat, CoilInTemp);
                            // adjust coil inlet/outlet temp with fan temperature rise
                            if (DataDesAccountForFanHeat) {
                                PeakCoilLoad += FanCoolLoad;
                                if (PrimaryAirSystem(CurSysNum).supFanLocation == DataAirSystems::fanPlacement::BlowThru) {
                                    CoilInTemp += FanCoolLoad / (CpAir * StdRhoAir * DesVolFlow);
                                    // include change in inlet condition in TotCapTempModFac
                                    CoilInWetBulb = PsyTwbFnTdbWPb(CoilInTemp, CoilInHumRat, StdBaroPress, CallingRoutine);
                                } else if (PrimaryAirSystem(CurSysNum).supFanLocation == DataAirSystems::fanPlacement::DrawThru) {
                                    CoilOutTemp -= FanCoolLoad / (CpAir * StdRhoAir * DesVolFlow);
                                }
                            }
                            if (DataTotCapCurveIndex > 0) {
                                TotCapTempModFac = CurveValue(DataTotCapCurveIndex, CoilInWetBulb, OutTemp);
                            } else {
                                TotCapTempModFac = 1.0;
                            }
                            if (TotCapTempModFac > 0.0) {
                                NominalCapacityDes = PeakCoilLoad / TotCapTempModFac;
                            } else {
                                NominalCapacityDes = PeakCoilLoad;
                            }
                            DataSizing::DataCoilSizingAirInTemp = CoilInTemp;
                            DataSizing::DataCoilSizingAirInHumRat = CoilInHumRat;
                            DataSizing::DataCoilSizingAirOutTemp = CoilOutTemp;
                            DataSizing::DataCoilSizingAirOutHumRat = CoilOutHumRat;
                            DataSizing::DataCoilSizingFanCoolLoad = FanCoolLoad;
                            DataSizing::DataCoilSizingCapFT = TotCapTempModFac;
                        } else {
                            NominalCapacityDes = 0.0;
                        }
                        AutosizeDes =
                            NominalCapacityDes * DataFracOfAutosizedCoolingCapacity; // Fixed Moved up 1 line inside block per Richard Raustad
                    }                                                                // IF(OASysFlag) THEN or ELSE IF(AirLoopSysFlag) THEN
                    DataDesAccountForFanHeat = true;                                 // reset for next water coil
                    if (DisplayExtraWarnings && AutosizeDes <= 0.0) {
                        ShowWarningMessage(CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName);
                        ShowContinueError("...Rated Total Cooling Capacity = " + TrimSigDigits(AutosizeDes, 2) + " [W]");
                        if (OASysFlag || AirLoopSysFlag || FinalSysSizing(CurSysNum).CoolingCapMethod == CapacityPerFloorArea ||
                            (FinalSysSizing(CurSysNum).CoolingCapMethod == CoolingDesignCapacity && FinalSysSizing(CurSysNum).CoolingTotalCapacity)) {
                            ShowContinueError("...Capacity passed by parent object to size child component = " + TrimSigDigits(AutosizeDes, 2) +
                                              " [W]");
                        } else {
                            ShowContinueError("...Air flow rate used for sizing = " + TrimSigDigits(DesVolFlow, 5) + " [m3/s]");
                            ShowContinueError("...Outdoor air fraction used for sizing = " + TrimSigDigits(OutAirFrac, 2));
                            ShowContinueError("...Coil inlet air temperature used for sizing = " + TrimSigDigits(CoilInTemp, 2) + " [C]");
                            ShowContinueError("...Coil outlet air temperature used for sizing = " + TrimSigDigits(CoilOutTemp, 2) + " [C]");
                        }
                    }
                } else if (SizingType == HeatingCapacitySizing) {
                    DataFracOfAutosizedHeatingCapacity = 1.0;
                    if (CurOASysNum > 0) {
                        if (OASysEqSizing(CurOASysNum).AirFlow) {
                            DesVolFlow = OASysEqSizing(CurOASysNum).AirVolFlow;
                        } else if (OASysEqSizing(CurOASysNum).HeatingAirFlow) {
                            DesVolFlow = OASysEqSizing(CurOASysNum).HeatingAirVolFlow;
                        } else if (DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                            DesVolFlow =
                                AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].SizingMassFlow / StdRhoAir;
                        } else {
                            DesVolFlow = FinalSysSizing(CurSysNum).DesOutAirVolFlow;
                        }
                    } else {
                        if (FinalSysSizing(CurSysNum).HeatingCapMethod == FractionOfAutosizedHeatingCapacity) {
                            DataFracOfAutosizedHeatingCapacity = FinalSysSizing(CurSysNum).FractionOfAutosizedHeatingCapacity;
                        }
                        if (DataFlowUsedForSizing > 0.0) {
                            DesVolFlow = DataFlowUsedForSizing;
                        } else if (UnitarySysEqSizing(CurSysNum).AirFlow) {
                            DesVolFlow = UnitarySysEqSizing(CurSysNum).AirVolFlow;
                        } else if (UnitarySysEqSizing(CurSysNum).HeatingAirFlow) {
                            DesVolFlow = UnitarySysEqSizing(CurSysNum).HeatingAirVolFlow;
                        } else {
                            if (CurDuctType == Main) {
                                if (FinalSysSizing(CurSysNum).SysAirMinFlowRat > 0.0 && !DataDesicRegCoil) {
                                    DesVolFlow = FinalSysSizing(CurSysNum).SysAirMinFlowRat * FinalSysSizing(CurSysNum).DesMainVolFlow;
                                } else {
                                    DesVolFlow = FinalSysSizing(CurSysNum).DesMainVolFlow;
                                }
                            } else if (CurDuctType == Cooling) {
                                if (FinalSysSizing(CurSysNum).SysAirMinFlowRat > 0.0 && !DataDesicRegCoil) {
                                    DesVolFlow = FinalSysSizing(CurSysNum).SysAirMinFlowRat * FinalSysSizing(CurSysNum).DesCoolVolFlow;
                                } else {
                                    DesVolFlow = FinalSysSizing(CurSysNum).DesCoolVolFlow;
                                }
                            } else if (CurDuctType == Heating) {
                                DesVolFlow = FinalSysSizing(CurSysNum).DesHeatVolFlow;
                            } else if (CurDuctType == Other) {
                                DesVolFlow = FinalSysSizing(CurSysNum).DesMainVolFlow;
                            } else {
                                DesVolFlow = FinalSysSizing(CurSysNum).DesMainVolFlow;
                            }
                        }
                    }
                    DesMassFlow = StdRhoAir * DesVolFlow;
                    // get the outside air fraction
                    if (CurOASysNum > 0) {
                        OutAirFrac = 1.0;
                    } else if (FinalSysSizing(CurSysNum).HeatOAOption == MinOA) {
                        if (DesVolFlow > 0.0) {
                            OutAirFrac = FinalSysSizing(CurSysNum).DesOutAirVolFlow / DesVolFlow;
                        } else {
                            OutAirFrac = 1.0;
                        }
                        OutAirFrac = std::min(1.0, std::max(0.0, OutAirFrac));
                    } else {
                        OutAirFrac = 1.0;
                    }
                    // coil inlet temperature
                    if (CurOASysNum == 0 && PrimaryAirSystem(CurSysNum).NumOAHeatCoils > 0) {
                        CoilInTemp = OutAirFrac * FinalSysSizing(CurSysNum).PreheatTemp + (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).HeatRetTemp;
                        CoilInHumRat = OutAirFrac * FinalSysSizing(CurSysNum).PreheatHumRat +
                                       (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).HeatRetHumRat; // include humrat for coil sizing reports
                    } else if (CurOASysNum > 0 && DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                        CoilInTemp = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].HeatOutTemp;
                        if (AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].m_FanIndex > -1 &&
                            AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].FanBlowTroughFlag &&
                            AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].m_FanTypeNum ==
                                SimAirServingZones::Fan_System_Object) {
                            int FanIndex = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].m_FanIndex;
                            Real64 DeltaT = HVACFan::fanObjs[FanIndex]->getFanDesignTemperatureRise();
                            CoilInTemp += DeltaT;
                        }
                    } else {
                        CoilInTemp = OutAirFrac * FinalSysSizing(CurSysNum).HeatOutTemp + (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).HeatRetTemp;
                        CoilInHumRat = OutAirFrac * FinalSysSizing(CurSysNum).HeatOutHumRat +
                                       (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).HeatRetHumRat; // include humrat for coil sizing reports
                    }
                    // coil load
                    if (CurOASysNum > 0) {
                        if (OASysEqSizing(CurOASysNum).HeatingCapacity) {
                            DesCoilLoad = OASysEqSizing(CurOASysNum).DesHeatingLoad;
                            // CoilOutTemp = -999.0; // , initialized at top
                        } else if (DataDesicRegCoil) {
                            DesCoilLoad = CpAirStd * DesMassFlow * (DataDesOutletAirTemp - DataDesInletAirTemp);
                            CoilOutTemp = DataDesOutletAirTemp;
                        } else if (DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                            DesCoilLoad =
                                CpAirStd * DesMassFlow *
                                (AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].PreheatTemp - CoilInTemp);
                            CoilOutTemp = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].PreheatTemp;
                        } else {
                            DesCoilLoad = CpAirStd * DesMassFlow * (FinalSysSizing(CurSysNum).PreheatTemp - CoilInTemp);
                            CoilOutTemp = FinalSysSizing(CurSysNum).PreheatTemp;
                            CoilOutHumRat = FinalSysSizing(CurSysNum).PreheatHumRat;
                        }
                    } else {
                        if (UnitarySysEqSizing(CurSysNum).HeatingCapacity) {
                            DesCoilLoad = UnitarySysEqSizing(CurSysNum).DesHeatingLoad;
                            // CoilOutTemp = -999.0; // initialized at top
                            CoilOutTemp = FinalSysSizing(CurSysNum).HeatSupTemp;
                            CoilOutHumRat = FinalSysSizing(CurSysNum).HeatSupHumRat;
                        } else if (DataDesicRegCoil) {
                            DesCoilLoad = CpAirStd * DesMassFlow * (DataDesOutletAirTemp - DataDesInletAirTemp);
                            CoilOutTemp = DataDesOutletAirTemp;
                        } else {
                            DesCoilLoad = CpAirStd * DesMassFlow * (FinalSysSizing(CurSysNum).HeatSupTemp - CoilInTemp);
                            CoilOutTemp = FinalSysSizing(CurSysNum).HeatSupTemp;
                            CoilOutHumRat = FinalSysSizing(CurSysNum).HeatSupHumRat;
                        }
                    }
                    if (CurSysNum <= NumPrimaryAirSys && AirLoopControlInfo(CurSysNum).UnitarySys) {
                        if (DataCoilIsSuppHeater) {
                            NominalCapacityDes = SuppHeatCap;
                        } else if (DataCoolCoilCap > 0.0) {
                            NominalCapacityDes = DataCoolCoilCap;
                        } else {
                            // TRUE for all air loop parent equipment except UnitarySystem where flag is reset to FALSE after simulating
                            // This method allows downstream heating coils to size individually.Probably should do this for all air loop equipment
                            // ChangoverBypass model always sets AirLoopControlInfo%UnitarySys to FALSE so heating coil can individually size
                            if (AirLoopControlInfo(CurSysNum).UnitarySysSimulating && !UtilityRoutines::SameString(CompType, "COIL:HEATING:WATER")) {
                                NominalCapacityDes = UnitaryHeatCap;
                            } else {
                                if (DesCoilLoad >= SmallLoad) {
                                    NominalCapacityDes = DesCoilLoad;
                                } else {
                                    NominalCapacityDes = 0.0;
                                }
                            }
                        }
                        DesCoilLoad = NominalCapacityDes;
                    } else if (CurSysNum <= NumPrimaryAirSys && FinalSysSizing(CurSysNum).HeatingCapMethod == CapacityPerFloorArea) {
                        NominalCapacityDes = FinalSysSizing(CurSysNum).HeatingTotalCapacity;
                    } else if (CurSysNum <= NumPrimaryAirSys && FinalSysSizing(CurSysNum).HeatingCapMethod == HeatingDesignCapacity &&
                               FinalSysSizing(CurSysNum).HeatingTotalCapacity > 0.0) {
                        NominalCapacityDes = FinalSysSizing(CurSysNum).HeatingTotalCapacity;
                    } else {
                        if (DataCoolCoilCap > 0.0) {
                            NominalCapacityDes = DataCoolCoilCap;
                        } else if (DesCoilLoad >= SmallLoad) {
                            NominalCapacityDes = DesCoilLoad;
                        } else {
                            NominalCapacityDes = 0.0;
                        }
                    }
                    AutosizeDes = NominalCapacityDes * DataHeatSizeRatio * DataFracOfAutosizedHeatingCapacity;
                    if (DisplayExtraWarnings && AutosizeDes <= 0.0) {
                        ShowWarningMessage(CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName);
                        ShowContinueError("...Rated Total Heating Capacity = " + TrimSigDigits(AutosizeDes, 2) + " [W]");
                        if (CoilOutTemp > -999.0) {
                            ShowContinueError("...Air flow rate used for sizing = " + TrimSigDigits(DesVolFlow, 5) + " [m3/s]");
                            ShowContinueError("...Outdoor air fraction used for sizing = " + TrimSigDigits(OutAirFrac, 2));
                            ShowContinueError("...Coil inlet air temperature used for sizing = " + TrimSigDigits(CoilInTemp, 2) + " [C]");
                            ShowContinueError("...Coil outlet air temperature used for sizing = " + TrimSigDigits(CoilOutTemp, 2) + " [C]");
                        } else {
                            ShowContinueError("...Capacity passed by parent object to size child component = " + TrimSigDigits(DesCoilLoad, 2) +
                                              " [W]");
                        }
                    }

                    if (CurSysNum <= NumPrimaryAirSys) {

                        switch (PrimaryAirSystem(CurSysNum).supFanModelTypeEnum) {
                        case DataAirSystems::structArrayLegacyFanModels: {
                            if (coilSelectionReportObj->isCompTypeCoil(CompType) && (PrimaryAirSystem(CurSysNum).SupFanNum > 0)) {
                                coilSelectionReportObj->setCoilSupplyFanInfo(CompName,
                                                                             CompType,
                                                                             Fans::Fan(PrimaryAirSystem(CurSysNum).SupFanNum).FanName,
                                                                             DataAirSystems::structArrayLegacyFanModels,
                                                                             PrimaryAirSystem(CurSysNum).SupFanNum);
                            }
                            break;
                        }
                        case DataAirSystems::objectVectorOOFanSystemModel: {
                            if (coilSelectionReportObj->isCompTypeCoil(CompType) && (PrimaryAirSystem(CurSysNum).supFanVecIndex >= 0)) {
                                coilSelectionReportObj->setCoilSupplyFanInfo(CompName,
                                                                             CompType,
                                                                             HVACFan::fanObjs[PrimaryAirSystem(CurSysNum).supFanVecIndex]->name,
                                                                             DataAirSystems::objectVectorOOFanSystemModel,
                                                                             PrimaryAirSystem(CurSysNum).supFanVecIndex);
                            }
                            break;
                        }
                        case DataAirSystems::fanModelTypeNotYetSet: {
                            // do nothing
                            break;
                        }
                        } // end switch
                    }

                } else if (SizingType == HeatingWaterDesCoilLoadUsedForUASizing) {
                    if (CurOASysNum > 0) {
                        OutAirFrac = 1.0;
                    } else if (FinalSysSizing(CurSysNum).HeatOAOption == MinOA) {
                        if (DataAirFlowUsedForSizing > 0.0) {
                            OutAirFrac = FinalSysSizing(CurSysNum).DesOutAirVolFlow / DataAirFlowUsedForSizing;
                        } else {
                            OutAirFrac = 1.0;
                        }
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                    } else {
                        OutAirFrac = 1.0;
                    }
                    if (CurOASysNum == 0 && PrimaryAirSystem(CurSysNum).NumOAHeatCoils > 0) {
                        CoilInTemp = OutAirFrac * FinalSysSizing(CurSysNum).PreheatTemp + (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).HeatRetTemp;
                    } else if (CurOASysNum > 0 && DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                        CoilInTemp = AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].HeatOutTemp;
                    } else {
                        CoilInTemp = OutAirFrac * FinalSysSizing(CurSysNum).HeatOutTemp + (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).HeatRetTemp;
                    }
                    // coil load
                    CpAirStd = PsyCpAirFnWTdb(0.0, 20.0);
                    if (CurOASysNum > 0) {
                        if (DataDesicRegCoil) {
                            AutosizeDes = CpAirStd * StdRhoAir * DataAirFlowUsedForSizing * (DataDesOutletAirTemp - DataDesInletAirTemp);
                        } else if (DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                            AutosizeDes =
                                CpAirStd * StdRhoAir * DataAirFlowUsedForSizing *
                                (AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].PreheatTemp - CoilInTemp);
                        } else {
                            AutosizeDes = CpAirStd * StdRhoAir * DataAirFlowUsedForSizing * (FinalSysSizing(CurSysNum).PreheatTemp - CoilInTemp);
                        }
                    } else {
                        if (DataDesicRegCoil) {
                            AutosizeDes = CpAirStd * StdRhoAir * DataAirFlowUsedForSizing * (DataDesOutletAirTemp - DataDesInletAirTemp);
                        } else {
                            AutosizeDes = CpAirStd * StdRhoAir * DataAirFlowUsedForSizing * (FinalSysSizing(CurSysNum).HeatSupTemp - CoilInTemp);
                        }
                    }
                } else if (SizingType == HeatingWaterDesCoilWaterVolFlowUsedForUASizing) {
                    AutosizeDes = DataWaterFlowUsedForSizing;
                    bCheckForZero = false;
                } else if (SizingType == HeatingAirflowUASizing) {
                    if (CurOASysNum > 0) {
                        if (DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum > -1) {
                            AutosizeDes =
                                AirLoopHVACDOAS::airloopDOAS[DataAirLoop::OutsideAirSys(CurOASysNum).AirLoopDOASNum].SizingMassFlow / StdRhoAir;
                        } else {
                            AutosizeDes = FinalSysSizing(CurSysNum).DesOutAirVolFlow;
                        }
                    } else {
                        if (CurDuctType == Main) {
                            if (FinalSysSizing(CurSysNum).SysAirMinFlowRat > 0.0) {
                                AutosizeDes = FinalSysSizing(CurSysNum).SysAirMinFlowRat * FinalSysSizing(CurSysNum).DesMainVolFlow;
                            } else {
                                AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                            }
                        } else if (CurDuctType == Cooling) {
                            if (FinalSysSizing(CurSysNum).SysAirMinFlowRat > 0.0) {
                                AutosizeDes = FinalSysSizing(CurSysNum).SysAirMinFlowRat * FinalSysSizing(CurSysNum).DesCoolVolFlow;
                            } else {
                                AutosizeDes = FinalSysSizing(CurSysNum).DesCoolVolFlow;
                            }
                        } else if (CurDuctType == Heating) {
                            AutosizeDes = FinalSysSizing(CurSysNum).DesHeatVolFlow;
                        } else if (CurDuctType == Other) {
                            AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                        } else {
                            AutosizeDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                        }
                    }
                    AutosizeDes *= StdRhoAir;
                } else if (SizingType == WaterHeatingCoilUASizing) {
                    if (DataCapacityUsedForSizing >= SmallLoad && DataWaterFlowUsedForSizing > 0.0 && DataFlowUsedForSizing > 0.0) {
                        Par(1) = DataCapacityUsedForSizing;
                        Par(2) = double(DataCoilNum);
                        Par(3) = double(DataFanOpMode); // fan operating mode
                        Par(4) = 1.0;                   // part-load ratio
                        UA0 = 0.001 * DataCapacityUsedForSizing;
                        UA1 = DataCapacityUsedForSizing;
                        // Invert the simple heating coil model: given the design inlet conditions and the design load,
                        // find the design UA.
                        SolveRoot(Acc, MaxIte, SolFla, AutosizeDes, SimpleHeatingCoilUAResidual, UA0, UA1, Par);
                        if (SolFla == -1) {
                            ShowSevereError("Autosizing of heating coil UA failed for Coil:Heating:Water \"" + CompName + "\"");
                            ShowContinueError("  Iteration limit exceeded in calculating coil UA");
                            ShowContinueError("  Lower UA estimate = " + TrimSigDigits(UA0, 6) + " W/m2-K (1% of Design Coil Load)");
                            ShowContinueError("  Upper UA estimate = " + TrimSigDigits(UA1, 6) + " W/m2-K (100% of Design Coil Load)");
                            ShowContinueError("  Final UA estimate when iterations exceeded limit = " + TrimSigDigits(AutosizeDes, 6) + " W/m2-K");
                            ShowContinueError("  AirloopHVAC \"" + FinalSysSizing(CurSysNum).AirPriLoopName +
                                              "\" coil sizing conditions (may be different than Sizing inputs):");
                            ShowContinueError("  Coil inlet air temperature     = " + TrimSigDigits(DataDesInletAirTemp, 3) + " C");
                            ShowContinueError("  Coil inlet air humidity ratio  = " + TrimSigDigits(DataDesInletAirHumRat, 3) + " kgWater/kgDryAir");
                            ShowContinueError("  Coil inlet air mass flow rate  = " + TrimSigDigits(DataFlowUsedForSizing, 6) + " kg/s");
                            ShowContinueError("  Design Coil Capacity           = " + TrimSigDigits(DataDesignCoilCapacity, 3) + " W");
                            ShowContinueError("  Design Coil Load               = " + TrimSigDigits(DataCapacityUsedForSizing, 3) + " W");
                            if (DataNomCapInpMeth) {
                                ShowContinueError("  Coil outlet air temperature    = " + TrimSigDigits(DataDesOutletAirTemp, 3) + " C");
                                ShowContinueError("  Coil outlet air humidity ratio = " + TrimSigDigits(DataDesOutletAirHumRat, 3) +
                                                  " kgWater/kgDryAir");
                            }
                            DataErrorsFound = true;
                        } else if (SolFla == -2) {
                            ShowSevereError("Autosizing of heating coil UA failed for Coil:Heating:Water \"" + CompName + "\"");
                            ShowContinueError("  Bad starting values for UA");
                            ShowContinueError("  Lower UA estimate = " + TrimSigDigits(UA0, 6) + " W/m2-K (1% of Design Coil Load)");
                            ShowContinueError("  Upper UA estimate = " + TrimSigDigits(UA1, 6) + " W/m2-K (100% of Design Coil Load)");
                            ShowContinueError("  AirloopHVAC \"" + FinalSysSizing(CurSysNum).AirPriLoopName +
                                              "\" coil sizing conditions (may be different than Sizing inputs):");
                            ShowContinueError("  Coil inlet air temperature     = " + TrimSigDigits(DataDesInletAirTemp, 3) + " C");
                            ShowContinueError("  Coil inlet air humidity ratio  = " + TrimSigDigits(DataDesInletAirHumRat, 3) + " kgWater/kgDryAir");
                            ShowContinueError("  Coil inlet air mass flow rate  = " + TrimSigDigits(DataFlowUsedForSizing, 6) + " kg/s");
                            ShowContinueError("  Design Coil Capacity           = " + TrimSigDigits(DataDesignCoilCapacity, 3) + " W");
                            ShowContinueError("  Design Coil Load               = " + TrimSigDigits(DataCapacityUsedForSizing, 3) + " W");
                            if (DataNomCapInpMeth) {
                                ShowContinueError("  Coil outlet air temperature    = " + TrimSigDigits(DataDesOutletAirTemp, 3) + " C");
                                ShowContinueError("  Coil outlet air humidity ratio = " + TrimSigDigits(DataDesOutletAirHumRat, 3) +
                                                  " kgWater/kgDryAir");
                            }
                            if (DataDesignCoilCapacity < DataCapacityUsedForSizing && !DataNomCapInpMeth) {
                                ShowContinueError("  Inadequate water side capacity: in Plant Sizing for this hot water loop");
                                ShowContinueError("  increase design loop exit temperature and/or decrease design loop delta T");
                                ShowContinueError("  Plant Sizing object = " + PlantSizData(DataPltSizHeatNum).PlantLoopName);
                                ShowContinueError(
                                    "  Plant design loop exit temperature = " + TrimSigDigits(PlantSizData(DataPltSizHeatNum).ExitTemp, 3) + " C");
                                ShowContinueError("  Plant design loop delta T          = " + TrimSigDigits(DataWaterCoilSizHeatDeltaT, 3) + " C");
                            }
                            DataErrorsFound = true;
                        }
                    } else {
                        AutosizeDes = 1.0;
                        if (DataWaterFlowUsedForSizing > 0.0 && DataCapacityUsedForSizing < SmallLoad) {
                            ShowWarningError("The design coil load used for UA sizing is too small for Coil:Heating:Water " + CompName);
                            ShowContinueError("An autosize value for UA cannot be calculated");
                            ShowContinueError("Input a value for UA, change the heating design day, or raise");
                            ShowContinueError("  the system heating design supply air temperature");
                            ShowContinueError("Water coil UA is set to 1 and the simulation continues.");
                        }
                    }
                } else if (SizingType == MaxHeaterOutletTempSizing) {
                    AutosizeDes = FinalSysSizing(CurSysNum).HeatSupTemp;
                } else if (SizingType == DesiccantDehumidifierBFPerfDataFaceVelocitySizing) {
                    AutosizeDes = 4.30551 + 0.01969 * DataAirFlowUsedForSizing;
                    AutosizeDes = min(6.0, AutosizeDes);
                } else if (SizingType == MinSATempCoolingSizing) {
                    if (DataCapacityUsedForSizing > 0.0 && DataFlowUsedForSizing > 0.0) {
                        if (CurOASysNum > 0) { // coil is in OA stream
                            CoilInTemp = FinalSysSizing(CurSysNum).OutTempAtCoolPeak;
                            CoilInHumRat = FinalSysSizing(CurSysNum).OutHumRatAtCoolPeak;
                        } else {                                                   // coil is in main air loop
                            if (PrimaryAirSystem(CurSysNum).NumOACoolCoils == 0) { // there is no precooling of the OA stream
                                CoilInTemp = FinalSysSizing(CurSysNum).MixTempAtCoolPeak;
                                CoilInHumRat = FinalSysSizing(CurSysNum).MixHumRatAtCoolPeak;
                            } else { // thereis precooling of the OA stream
                                if (DataFlowUsedForSizing > 0.0) {
                                    OutAirFrac = FinalSysSizing(CurSysNum).DesOutAirVolFlow / DataFlowUsedForSizing;
                                } else {
                                    OutAirFrac = 1.0;
                                }
                                OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                                CoilInTemp = OutAirFrac * FinalSysSizing(CurSysNum).PrecoolTemp +
                                             (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).RetTempAtCoolPeak;
                                CoilInHumRat = OutAirFrac * FinalSysSizing(CurSysNum).PrecoolHumRat +
                                               (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).RetHumRatAtCoolPeak;
                            }
                        }
                        AutosizeDes =
                            CoilInTemp - (DataCapacityUsedForSizing / (DataFlowUsedForSizing * StdRhoAir * PsyCpAirFnWTdb(CoilInHumRat, CoilInTemp)));
                    } else {
                        ShowSevereError(CallingRoutine + ' ' + CompType + ' ' + CompName + ", Developer Error: Component sizing incomplete.");
                        ShowContinueError("SizingString = " + SizingString +
                                          ", DataCapacityUsedForSizing = " + TrimSigDigits(DataCapacityUsedForSizing, 1));
                        ShowContinueError("SizingString = " + SizingString + ", DataFlowUsedForSizing = " + TrimSigDigits(DataFlowUsedForSizing, 1));
                    }
                } else if (SizingType == MaxSATempHeatingSizing) {
                    if (DataCapacityUsedForSizing > 0.0 && DataFlowUsedForSizing > 0.0) {
                        if (CurOASysNum > 0) { // coil is in OA stream
                            CoilInTemp = FinalSysSizing(CurSysNum).HeatOutTemp;
                            CoilInHumRat = FinalSysSizing(CurSysNum).HeatOutHumRat;
                        } else {                                                   // coil is in main air loop
                            if (PrimaryAirSystem(CurSysNum).NumOAHeatCoils == 0) { // there is no precooling of the OA stream
                                CoilInTemp = FinalSysSizing(CurSysNum).HeatMixTemp;
                                CoilInHumRat = FinalSysSizing(CurSysNum).HeatMixHumRat;
                            } else { // thereis precooling of the OA stream
                                if (DataFlowUsedForSizing > 0.0) {
                                    OutAirFrac = FinalSysSizing(CurSysNum).DesOutAirVolFlow / DataFlowUsedForSizing;
                                } else {
                                    OutAirFrac = 1.0;
                                }
                                OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                                CoilInTemp =
                                    OutAirFrac * FinalSysSizing(CurSysNum).PreheatTemp + (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).HeatRetTemp;
                                CoilInHumRat = OutAirFrac * FinalSysSizing(CurSysNum).PreheatHumRat +
                                               (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).HeatRetHumRat;
                            }
                        }
                        AutosizeDes =
                            CoilInTemp + (DataCapacityUsedForSizing / (DataFlowUsedForSizing * StdRhoAir * PsyCpAirFnWTdb(CoilInHumRat, CoilInTemp)));
                    } else {
                        ShowSevereError(CallingRoutine + ' ' + CompType + ' ' + CompName + ", Developer Error: Component sizing incomplete.");
                        ShowContinueError("SizingString = " + SizingString +
                                          ", DataCapacityUsedForSizing = " + TrimSigDigits(DataCapacityUsedForSizing, 1));
                        ShowContinueError("SizingString = " + SizingString + ", DataFlowUsedForSizing = " + TrimSigDigits(DataFlowUsedForSizing, 1));
                    }
                } else if (SizingType == ASHRAEMinSATCoolingSizing) {
                    if (DataCapacityUsedForSizing > 0.0 && DataFlowUsedForSizing > 0.0 && DataZoneUsedForSizing > 0) {
                        AutosizeDes = FinalZoneSizing(DataZoneUsedForSizing).ZoneTempAtCoolPeak -
                                      (DataCapacityUsedForSizing / (DataFlowUsedForSizing * StdRhoAir *
                                                                    PsyCpAirFnWTdb(FinalZoneSizing(DataZoneUsedForSizing).ZoneHumRatAtCoolPeak,
                                                                                   FinalZoneSizing(DataZoneUsedForSizing).ZoneTempAtCoolPeak)));
                    } else {
                        ShowSevereError(CallingRoutine + ' ' + CompType + ' ' + CompName + ", Developer Error: Component sizing incomplete.");
                        ShowContinueError("SizingString = " + SizingString +
                                          ", DataCapacityUsedForSizing = " + TrimSigDigits(DataCapacityUsedForSizing, 1));
                        ShowContinueError("SizingString = " + SizingString + ", DataFlowUsedForSizing = " + TrimSigDigits(DataFlowUsedForSizing, 1));
                        ShowContinueError("SizingString = " + SizingString +
                                          ", DataZoneUsedForSizing = " + TrimSigDigits(Real64(DataZoneUsedForSizing), 0));
                    }
                } else if (SizingType == ASHRAEMaxSATHeatingSizing) {
                    if (DataCapacityUsedForSizing > 0.0 && DataFlowUsedForSizing > 0.0 && DataZoneUsedForSizing > 0) {
                        AutosizeDes = FinalZoneSizing(DataZoneUsedForSizing).ZoneTempAtHeatPeak +
                                      (DataCapacityUsedForSizing / (DataFlowUsedForSizing * StdRhoAir *
                                                                    PsyCpAirFnWTdb(FinalZoneSizing(DataZoneUsedForSizing).ZoneHumRatAtHeatPeak,
                                                                                   FinalZoneSizing(DataZoneUsedForSizing).ZoneTempAtHeatPeak)));
                    } else {
                        ShowSevereError(CallingRoutine + ' ' + CompType + ' ' + CompName + ", Developer Error: Component sizing incomplete.");
                        ShowContinueError("SizingString = " + SizingString +
                                          ", DataCapacityUsedForSizing = " + TrimSigDigits(DataCapacityUsedForSizing, 1));
                        ShowContinueError("SizingString = " + SizingString + ", DataFlowUsedForSizing = " + TrimSigDigits(DataFlowUsedForSizing, 1));
                        ShowContinueError("SizingString = " + SizingString +
                                          ", DataZoneUsedForSizing = " + TrimSigDigits(Real64(DataZoneUsedForSizing), 0));
                    }
                }
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

        if (DataScalableSizingON) {
            if (SizingType == CoolingAirflowSizing || SizingType == HeatingAirflowSizing || SizingType == SystemAirflowSizing) {
                {
                    auto const SELECT_CASE_var(ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingType));
                    if (SELECT_CASE_var == SupplyAirFlowRate || SELECT_CASE_var == None) {
                        ScalableSM = "User-Specified (scaled by flow / zone) ";
                    } else if (SELECT_CASE_var == FlowPerFloorArea) {
                        ScalableSM = "User-Specified (scaled by flow / area) ";
                    } else if (SELECT_CASE_var == FractionOfAutosizedCoolingAirflow || SELECT_CASE_var == FractionOfAutosizedHeatingAirflow) {
                        ScalableSM = "User-Specified (scaled by fractional multiplier) ";
                    } else if (SELECT_CASE_var == FlowPerCoolingCapacity || SELECT_CASE_var == FlowPerHeatingCapacity) {
                        ScalableSM = "User-Specified (scaled by flow / capacity) ";
                    } else {
                        ScalableSM = "Design Size ";
                    }
                }
            }
        }
        if (DataScalableCapSizingON) {
            {
                auto const SELECT_CASE_var(ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingType));
                if (SELECT_CASE_var == HeatingDesignCapacity || SELECT_CASE_var == CoolingDesignCapacity) {
                    ScalableSM = "User-Specified ";
                    if (SizingResult == AutoSize) ScalableSM = "Design Size ";
                } else if (SELECT_CASE_var == CapacityPerFloorArea) {
                    ScalableSM = "User-Specified (scaled by capacity / area) ";
                } else if (SELECT_CASE_var == FractionOfAutosizedHeatingCapacity || SELECT_CASE_var == FractionOfAutosizedCoolingCapacity) {
                    ScalableSM = "User-Specified (scaled by fractional multiplier) ";
                } else {
                    ScalableSM = "Design Size ";
                }
            }
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

                if (DataIsDXCoil && FlagCheckVolFlowPerRatedTotCap && (SizingType == CoolingCapacitySizing || SizingType == HeatingCapacitySizing)) {
                    if (SizingResult > 0.0) {
                        RatedVolFlowPerRatedTotCap = DesVolFlow / SizingResult;
                    } else {
                        RatedVolFlowPerRatedTotCap = 0.0;
                    }
                    if (RatedVolFlowPerRatedTotCap < MinRatedVolFlowPerRatedTotCap(DXCT)) {
                        if (!DataEMSOverride && DisplayExtraWarnings && PrintWarningFlag) {
                            ShowWarningError(CallingRoutine + ' ' + CompType + ' ' + CompName);
                            ShowContinueError("..." + SizingString +
                                              " will be limited by the minimum rated volume flow per rated total capacity ratio.");
                            ShowContinueError("...DX coil volume flow rate (m3/s ) = " + TrimSigDigits(DesVolFlow, 6));
                            ShowContinueError("...Requested capacity (W ) = " + TrimSigDigits(SizingResult, 3));
                            ShowContinueError("...Requested flow/capacity ratio (m3/s/W ) = " + TrimSigDigits(RatedVolFlowPerRatedTotCap, 3));
                            ShowContinueError("...Minimum flow/capacity ratio (m3/s/W ) = " + TrimSigDigits(MinRatedVolFlowPerRatedTotCap(DXCT), 3));
                        }

                        DXFlowPerCapMinRatio = (DesVolFlow / MinRatedVolFlowPerRatedTotCap(DXCT)) /
                                               SizingResult; // set DX Coil Capacity Increase Ratio from Too Low Flow/Capacity Ratio
                        SizingResult = DesVolFlow / MinRatedVolFlowPerRatedTotCap(DXCT);

                        if (!DataEMSOverride && DisplayExtraWarnings && PrintWarningFlag) {
                            ShowContinueError("...Adjusted capacity ( W ) = " + TrimSigDigits(SizingResult, 3));
                        }
                    } else if (RatedVolFlowPerRatedTotCap > MaxRatedVolFlowPerRatedTotCap(DXCT)) {
                        if (!DataEMSOverride && DisplayExtraWarnings && PrintWarningFlag) {
                            ShowWarningError(CallingRoutine + ' ' + CompType + ' ' + CompName);
                            ShowContinueError("..." + SizingString +
                                              " will be limited by the maximum rated volume flow per rated total capacity ratio.");
                            ShowContinueError("...DX coil volume flow rate ( m3/s ) = " + TrimSigDigits(DesVolFlow, 6));
                            ShowContinueError("...Requested capacity ( W ) = " + TrimSigDigits(SizingResult, 3));
                            ShowContinueError("...Requested flow/capacity ratio ( m3/s/W ) = " + TrimSigDigits(RatedVolFlowPerRatedTotCap, 3));
                            ShowContinueError("...Maximum flow/capacity ratio ( m3/s/W ) = " + TrimSigDigits(MaxRatedVolFlowPerRatedTotCap(DXCT), 3));
                        }

                        DXFlowPerCapMaxRatio = DesVolFlow / MaxRatedVolFlowPerRatedTotCap(DXCT) /
                                               SizingResult; // set DX Coil Capacity Decrease Ratio from Too High Flow/Capacity Ratio
                        SizingResult = DesVolFlow / MaxRatedVolFlowPerRatedTotCap(DXCT);

                        if (!DataEMSOverride && DisplayExtraWarnings && PrintWarningFlag) {
                            ShowContinueError("...Adjusted capacity ( W ) = " + TrimSigDigits(SizingResult, 3));
                        }
                        AutosizeDes = SizingResult;
                    }
                }
                if (DataAutosizable && AutosizeUser > 0.0 && AutosizeDes > 0.0 && PrintWarningFlag) {
                    if (UtilityRoutines::SameString(CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE") &&
                        SizingType == CoolingAirflowSizing && DataIsDXCoil) {
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
        if (SizingType == CoolingAirflowSizing) {
            if (coilSelectionReportObj->isCompTypeCoil(CompType)) {
                // SizingResult is airflow in m3/s
                coilSelectionReportObj->setCoilAirFlow(CompName, CompType, SizingResult, IsAutoSize);
            }

            //  fill fan peak day and time here
            if (coilSelectionReportObj->isCompTypeFan(CompType)) {
                if (DataScalableSizingON) {
                    DDNameFanPeak = "Scaled size, not from any peak";
                    dateTimeFanPeak = "Scaled size, not from any peak";
                }
                OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchFanDesDay, CompName, DDNameFanPeak);
                OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchFanPkTime, CompName, dateTimeFanPeak);
            }
        } else if (SizingType == CoolingWaterflowSizing) { // used by watercoils.cc
            // SizingResult is water flow in m3/s
            coilSelectionReportObj->setCoilWaterFlowPltSizNum(CompName, CompType, SizingResult, IsAutoSize, DataPltSizCoolNum, DataWaterLoopNum);
            coilSelectionReportObj->setCoilWaterDeltaT(CompName, CompType, CoilDesWaterDeltaT);
            if (DataDesInletWaterTemp > 0.0) {
                coilSelectionReportObj->setCoilEntWaterTemp(CompName, CompType, DataDesInletWaterTemp);
                coilSelectionReportObj->setCoilLvgWaterTemp(CompName, CompType, DataDesInletWaterTemp + CoilDesWaterDeltaT);
            } else {
                coilSelectionReportObj->setCoilEntWaterTemp(CompName, CompType, DataGlobals::CWInitConvTemp);
                coilSelectionReportObj->setCoilLvgWaterTemp(CompName, CompType, DataGlobals::CWInitConvTemp + CoilDesWaterDeltaT);
            }

        } else if (SizingType == HeatingWaterflowSizing) {
            // SizingResult is water flow in m3/s
            coilSelectionReportObj->setCoilWaterFlowPltSizNum(CompName, CompType, SizingResult, IsAutoSize, DataPltSizHeatNum, DataWaterLoopNum);
            coilSelectionReportObj->setCoilWaterDeltaT(CompName, CompType, PlantSizData(DataPltSizHeatNum).DeltaT);
            coilSelectionReportObj->setCoilEntWaterTemp(CompName, CompType, DataGlobals::HWInitConvTemp);
            coilSelectionReportObj->setCoilLvgWaterTemp(CompName, CompType, DataGlobals::HWInitConvTemp - PlantSizData(DataPltSizHeatNum).DeltaT);
        } else if (CurSysNum <= NumPrimaryAirSys && SizingType == CoolingWaterDesAirInletTempSizing) {
            coilSelectionReportObj->setCoilEntAirTemp(CompName, CompType, SizingResult, CurSysNum, CurZoneEqNum);
        } else if (SizingType == CoolingWaterDesAirInletHumRatSizing) {
            coilSelectionReportObj->setCoilEntAirHumRat(CompName, CompType, SizingResult);
        } else if (SizingType == CoolingWaterDesWaterInletTempSizing) {
            coilSelectionReportObj->setCoilEntWaterTemp(CompName, CompType, SizingResult);
        } else if (SizingType == CoolingWaterDesAirOutletTempSizing) {
            coilSelectionReportObj->setCoilLvgAirTemp(CompName, CompType, SizingResult);
        } else if (SizingType == CoolingWaterDesAirOutletHumRatSizing) {
            coilSelectionReportObj->setCoilLvgAirHumRat(CompName, CompType, SizingResult);
        } else if (SizingType == CoolingWaterNumofTubesPerRowSizing) {
            // do nothing
        } else if (CurSysNum <= NumPrimaryAirSys && SizingType == HeatingWaterDesAirInletTempSizing) {
            coilSelectionReportObj->setCoilEntAirTemp(CompName, CompType, SizingResult, CurSysNum, CurZoneEqNum);
        } else if (SizingType == HeatingWaterDesAirInletHumRatSizing) {
            coilSelectionReportObj->setCoilEntAirHumRat(CompName, CompType, SizingResult);
        } else if (CurSysNum <= NumPrimaryAirSys && SizingType == HeatingWaterDesCoilLoadUsedForUASizing) {
            coilSelectionReportObj->setCoilHeatingCapacity(CompName,
                                                           CompType,
                                                           SizingResult,
                                                           IsAutoSize,
                                                           CurSysNum,
                                                           CurZoneEqNum,
                                                           CurOASysNum,
                                                           FanCoolLoad,
                                                           TotCapTempModFac,
                                                           DXFlowPerCapMinRatio,
                                                           DXFlowPerCapMaxRatio);
        } else if (SizingType == HeatingWaterDesCoilWaterVolFlowUsedForUASizing) {
            coilSelectionReportObj->setCoilWaterFlowPltSizNum(CompName, CompType, SizingResult, IsAutoSize, DataPltSizHeatNum, DataWaterLoopNum);
        } else if (SizingType == HeatingAirflowSizing) {
            if (coilSelectionReportObj->isCompTypeCoil(CompType)) {
                // SizingResult is airflow in m3/s
                coilSelectionReportObj->setCoilAirFlow(CompName, CompType, SizingResult, IsAutoSize);
            }
            //  fill fan peak day and time here
            if (coilSelectionReportObj->isCompTypeFan(CompType)) {
                if (DataScalableSizingON) {
                    DDNameFanPeak = "Scaled size, not from any peak";
                    dateTimeFanPeak = "Scaled size, not from any peak";
                }
                OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchFanDesDay, CompName, DDNameFanPeak);
                OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchFanPkTime, CompName, dateTimeFanPeak);
            }
        } else if (SizingType == HeatingAirflowUASizing) {

            // do nothing
        } else if (SizingType == SystemAirflowSizing) {
            //  fill fan peak day and time here
            if (coilSelectionReportObj->isCompTypeFan(CompType)) {
                if (DataScalableSizingON) {
                    DDNameFanPeak = "Scaled size, not from any peak";
                    dateTimeFanPeak = "Scaled size, not from any peak";
                }
                OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchFanDesDay, CompName, DDNameFanPeak);
                OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchFanPkTime, CompName, dateTimeFanPeak);
            }

        } else if (CurSysNum <= NumPrimaryAirSys && SizingType == CoolingCapacitySizing) {
            if (coilSelectionReportObj->isCompTypeCoil(CompType)) {
                if (CoilInTemp > -999.0) { // set inlet air properties used during capacity sizing if available, allow for negative winter temps
                    coilSelectionReportObj->setCoilEntAirTemp(CompName, CompType, CoilInTemp, CurSysNum, CurZoneEqNum);
                    coilSelectionReportObj->setCoilEntAirHumRat(CompName, CompType, CoilInHumRat);
                }
                if (CoilOutTemp > -999.0) { // set outlet air properties used during capacity sizing if available
                    coilSelectionReportObj->setCoilLvgAirTemp(CompName, CompType, CoilOutTemp);
                    coilSelectionReportObj->setCoilLvgAirHumRat(CompName, CompType, CoilOutHumRat);
                }
                coilSelectionReportObj->setCoilCoolingCapacity(CompName,
                                                               CompType,
                                                               SizingResult,
                                                               IsAutoSize,
                                                               CurSysNum,
                                                               CurZoneEqNum,
                                                               CurOASysNum,
                                                               FanCoolLoad,
                                                               TotCapTempModFac,
                                                               DXFlowPerCapMinRatio,
                                                               DXFlowPerCapMaxRatio);
            }
        } else if (CurSysNum <= NumPrimaryAirSys && SizingType == HeatingCapacitySizing) {
            if (coilSelectionReportObj->isCompTypeCoil(CompType)) {
                if (CoilInTemp > -999.0) { // set inlet air properties used during capacity sizing if available
                    coilSelectionReportObj->setCoilEntAirTemp(CompName, CompType, CoilInTemp, CurSysNum, CurZoneEqNum);
                    coilSelectionReportObj->setCoilEntAirHumRat(CompName, CompType, CoilInHumRat);
                }
                if (CoilOutTemp > -999.0) { // set outlet air properties used during capacity sizing if available
                    coilSelectionReportObj->setCoilLvgAirTemp(CompName, CompType, CoilOutTemp);
                    coilSelectionReportObj->setCoilLvgAirHumRat(CompName, CompType, CoilOutHumRat);
                }
                coilSelectionReportObj->setCoilHeatingCapacity(CompName,
                                                               CompType,
                                                               SizingResult,
                                                               IsAutoSize,
                                                               CurSysNum,
                                                               CurZoneEqNum,
                                                               CurOASysNum,
                                                               FanCoolLoad,
                                                               TotCapTempModFac,
                                                               DXFlowPerCapMinRatio,
                                                               DXFlowPerCapMaxRatio);
            }
        } else if (SizingType == WaterHeatingCapacitySizing) {
            if (coilSelectionReportObj->isCompTypeCoil(CompType)) {
                coilSelectionReportObj->setCoilWaterHeaterCapacityPltSizNum(
                    CompName, CompType, SizingResult, IsAutoSize, DataPltSizHeatNum, DataWaterLoopNum);
            }
        } else if (CurSysNum <= NumPrimaryAirSys && SizingType == WaterHeatingCoilUASizing) {
            coilSelectionReportObj->setCoilUA(CompName, CompType, SizingResult, DataCapacityUsedForSizing, IsAutoSize, CurSysNum, CurZoneEqNum);

        } else if (SizingType == CoolingSHRSizing) {

        } else if (SizingType == HeatingDefrostSizing) {

        } else if (SizingType == MaxHeaterOutletTempSizing) {

        } else if (SizingType == AutoCalculateSizing) {
            // do nothing
        } else if (SizingType == ZoneCoolingLoadSizing) {
            // do nothing
        } else if (SizingType == ZoneHeatingLoadSizing) {
            // do nothing
        } else if (SizingType == MinSATempCoolingSizing) {

        } else if (SizingType == MaxSATempHeatingSizing) {

        } else if (SizingType == HeatingCoilDesAirInletTempSizing) {
            coilSelectionReportObj->setCoilEntAirTemp(CompName, CompType, SizingResult, CurSysNum, CurZoneEqNum);
        } else if (SizingType == HeatingCoilDesAirOutletTempSizing) {
            coilSelectionReportObj->setCoilLvgAirTemp(CompName, CompType, SizingResult);
        } else if (SizingType == HeatingCoilDesAirInletHumRatSizing) {
            coilSelectionReportObj->setCoilEntAirHumRat(CompName, CompType, SizingResult);
        }
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
