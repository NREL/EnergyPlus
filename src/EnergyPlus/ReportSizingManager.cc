// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// C++ Headers
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <ReportSizingManager.hh>
#include <CurveManager.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DXCoils.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <Psychrometrics.hh>
#include <SQLiteProcedures.hh>
#include <UtilityRoutines.hh>
#include <FluidProperties.hh>
#include <DataPlant.hh>
#include <WaterCoils.hh>
#include <Fans.hh>
#include <DesiccantDehumidifiers.hh>

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

	void
	ReportSizingOutput(
		std::string const & CompType, // the type of the component
		std::string const & CompName, // the name of the component
		std::string const & VarDesc, // the description of the input variable
		Real64 const VarValue, // the value from the sizing calculation
		Optional_string_const UsrDesc, // the description of a user-specified variable
		Optional< Real64 const > UsrValue // the value from the user for the desc item
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
		static bool MyOneTimeFlag( true );

		// Formats
		static gio::Fmt Format_990( "('! <Component Sizing Information>, Component Type, Component Name, ','Input Field Description, Value')" );
		static gio::Fmt Format_991( "(' Component Sizing Information, ',A,', ',A,', ',A,', ',A)" );

		if ( MyOneTimeFlag ) {
			gio::write( OutputFileInits, Format_990 );
			MyOneTimeFlag = false;
		}

		gio::write( OutputFileInits, Format_991 ) << CompType << CompName << VarDesc << RoundSigDigits( VarValue, 5 );
		//add to tabular output reports
		AddCompSizeTableEntry( CompType, CompName, VarDesc, VarValue );

		if ( present( UsrDesc ) && present( UsrValue ) ) {
			gio::write( OutputFileInits, Format_991 ) << CompType << CompName << UsrDesc << RoundSigDigits( UsrValue, 5 );
			AddCompSizeTableEntry( CompType, CompName, UsrDesc, UsrValue );
		} else if ( present( UsrDesc ) || present( UsrValue ) ) {
			ShowFatalError( "ReportSizingOutput: (Developer Error) - called with user-specified description or value but not both." );
		}

		// add to SQL output
		if ( sqlite ) sqlite->addSQLiteComponentSizingRecord( CompType, CompName, VarDesc, VarValue );

	}

	void
	RequestSizing(
		std::string const & CompType, // type of component
		std::string const & CompName, // name of component
		int const SizingType, // integerized type of sizing requested (see DataHVACGlobals, e.g. CoolingCapacitySizing)
		std::string const & SizingString, // string containing info for eio report
		Real64 & SizingResult, // result of the sizing procedure
		bool const PrintWarningFlag, // TRUE when requesting output (eio) reporting
		std::string const & CallingRoutine // name of calling routine for warning messages
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
		// CoolingWaterflowSizing( 2 ); // request sizing for cooling water flow rate (DataPltSizCoolNum, DataWaterLoopNum, AIRLOOP COILS: DataCapacityUsedForSizing)
		// HeatingWaterflowSizing( 3 ); // request sizing for heating coil water flow rate (DataPltSizHeatNum, DataWaterLoopNum, AIRLOOP COILS: DataCapacityUsedForSizing)
		// CoolingWaterDesAirInletTempSizing( 4 ); // request sizing for cooling water coil inlet air temp
		// CoolingWaterDesAirInletHumRatSizing( 5 ); // request sizing for cooling water coil inlet air humidity ratio
		// CoolingWaterDesWaterInletTempSizing( 6 ); // request sizing for cooling water coil inlet water temp (DataPltSizCoolNum)
		// CoolingWaterDesAirOutletTempSizing( 7 ); // request sizing for cooling water coil outlet air temp (DataPltSizCoolNum, DataWaterLoopNum, DataDesInletAirTemp, DataDesInletAirHumRat, DataAirFlowUsedForSizing, DataWaterFlowUsedForSizing, AIRLOOP COILS: DataDesInletWaterTemp)
		// CoolingWaterDesAirOutletHumRatSizing( 8 ); // request sizing for cooling water coil outlet air humidity ratio (DataDesInletAirHumRat, DataDesInletWaterTemp, DataCapacityUsedForSizing, ZONE COILS: DataDesOutletAirTemp)
		// CoolingWaterNumofTubesPerRowSizing( 9 ); // request sizing for cooling water coil number of tubes per row (DataWaterFlowUsedForSizing)
		// HeatingWaterDesAirInletTempSizing( 10 ); // request sizing for heating water coil inlet air temp
		// HeatingWaterDesAirInletHumRatSizing( 11 ); // request sizing for heating water coil inlet air humidity ratio
		// HeatingWaterDesCoilLoadUsedForUASizing( 12 ); // request sizing for heating water coil capacity used for UA sizing (DataWaterLoopNum, DataPltSizHeatNum, AIRLOOP COILS: DataAirFlowUsedForSizing)
		// HeatingWaterDesCoilWaterVolFlowUsedForUASizing( 13 ); // request sizing for heating water coil volume flow rate used for UA sizing (DataWaterFlowUsedForSizing)
		// HeatingAirflowSizing( 14 ); // request sizing for heating air flow rate
		// HeatingAirflowUASizing( 15 ); // request sizing for heating air flow rate
		// SystemAirflowSizing( 16 ); // request sizing for system air flow rate
		// CoolingCapacitySizing( 17 ); // request sizing for cooling capacity (DataFlowUsedForSizing [m3/s], DataTotCapCurveIndex [DX coils only])
		// HeatingCapacitySizing( 18 ); // request sizing for heating capacity (DataCoolCoilCap, DataFlowUsedForSizing, AIRLOOP COILS: DataCoilIsSuppHeater, OPTIONAL: DataHeatSizeRatio)
		// WaterHeatingCapacitySizing( 19 ); // request sizing for water-side heating capacity (ZONE COILS ONLY: DataWaterLoopNum, DataPltSizHeatNum, OPTIONAL: DataHeatSizeRatio)
		// WaterHeatingCoilUASizing( 20 ); // request sizing for heating coil UA (DataCapacityUsedForSizing, DataCoilNum, DataFanOpMode, DataCapacityUsedForSizing, DataDesInletAirTemp, DataDesInletAirHumRat, DataFlowUsedForSizing, DataDesignCoilCapacity)
		// SystemCapacitySizing( 21 ); // request sizing for system capacity
		// CoolingSHRSizing( 22 ); // request sizing for cooling SHR (DataFlowUsedForSizing, DataCapacityUsedForSizing)
		// HeatingDefrostSizing( 23 ); // request sizing for heating defrost capacity
		// MaxHeaterOutletTempSizing( 24 ); // request sizing for heating coil maximum outlet temperature
		// AutoCalculateSizing ( 25 ); // identifies an autocalulate input
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

		// REFERENCES :
		// na

		// USE STATEMENTS :
		using CurveManager::CurveValue;
		using DataEnvironment::StdRhoAir;
		using DataEnvironment::StdBaroPress;
		using DataAirLoop::AirLoopControlInfo;
		using DataAirSystems::PrimaryAirSystem;
		using DataGlobals::DisplayExtraWarnings;
		using DataGlobals::InitConvTemp;;
		using namespace DataSizing;
		using namespace DataHVACGlobals;
		using DXCoils::ValidateADP;
		using General::RoundSigDigits;
		using General::TrimSigDigits;
		using General::SolveRegulaFalsi;
		using InputProcessor::SameString;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyWFnTdpPb;
		using Psychrometrics::PsyWFnTdbH;
		using Psychrometrics::PsyTdbFnHW;
		using Psychrometrics::PsyTdpFnWPb;
		using Psychrometrics::PsyWFnTdbRhPb;
		using Psychrometrics::PsyRhFnTdbWPb;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using DataPlant::PlantLoop;
		using WaterCoils::SimpleHeatingCoilUAResidual;
		using Fans::FanDesDT;
		using Fans::FanDesHeatGain;
		using DesiccantDehumidifiers::DesicDehum;

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const Acc( 0.0001 ); // Accuracy of result
		int const MaxIte( 500 ); // Maximum number of iterations

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DDNum; // design day number corresponding to TimeStepNumAtMax
		int SolFla; // Flag of solver
		int TimeStepNumAtMax; // time step number at max load
		bool IsAutoSize; // Indicator to autosize for reporting
		bool HardSizeNoDesRun; // Indicator to hardsize with no sizing runs for reporting
		bool SizingDesRunThisAirSys; // true if a particular air system had a Sizing : System object and system sizing done
		bool SizingDesRunThisZone; // true if a particular zone had a Sizing : Zone object and zone sizing was done
		bool SizingDesValueFromParent; // true if the parent set the design size (whether or not there is a sizing run)
		bool OASysFlag; // Logical flag determines if parent object set OA Sys coil property
		bool AirLoopSysFlag; // Logical flag determines if parent object set air loop coil property
		bool bCheckForZero; // logical to flag whether or not to check for very small autosized values
		Real64 AutosizeDes; // autosized value
		Real64 AutosizeUser; // user sized value
		Real64 OutTemp; // outdoor air temperature [C]
		Real64 CoilInTemp; // entering coil air temperature [C]
		Real64 CoilInHumRat; // entering coil air humidity ratio [kg/kg]
		Real64 CoilInWetBulb; // entering coil air wet-bulb temperature [C]
		Real64 TotCapTempModFac; // DX coil total capacity as a function of temperature curve pionter
		Real64 CoilInEnth; // entering coil air enthalpy [J/kg]
		Real64 CoilOutTemp; // coil outlet air temperature [C]
		Real64 CoilOutHumRat; // coil outlet air humidity ratio [kg/kg]
		Real64 CoilOutEnth; // coil outlet air enthalpy [J/kg]
		Real64 DesCoilLoad; // design coil load based on sizing inputs and entering air conditions [W]
		Real64 PeakCoilLoad; // adjusted coil size based on TotCapTempModFac [W]
		Real64 DesVolFlow; // coil design air volume flow rate [m3/s]
		Real64 DesMassFlow; // coil design mass flow rate [kg/s]
		Real64 CpAir; // specific heat of air [J/kg-K]
		Real64 rhoair; // density of air [kg/m3]
		Real64 OutAirFrac; // outdoor air fraction
		Real64 MinPriFlowFrac; // minimum primary air flow fraction for induction units
		Real64 CpAirStd; // specific heat of air at standard conditions [J/kg-K]
		Real64 NominalCapacityDes; // Autosized nominal capacity for reporting [W]
		Real64 RatedVolFlowPerRatedTotCap; // ratio of volume flow rate to capacity [m3/W]
		Real64 CoilDesWaterDeltaT; // water coil delta T used for sizing [C]
		Real64 Cp; // water loop fluid specific heat [J/kgK]
		Real64 rho; // water loop fluid density [kg/m3]
		Real64 DesSatEnthAtWaterInTemp; // temp variable used for warning messages
		Real64 DesHumRatAtWaterInTemp; // temp variable used for warning messages
		Real64 T1Out; // water coil air outlet temperature [C]
		Real64 T2Out; // water coil water outlet temperature [C]
		Real64 UA0; // lower bound of UA for autosizing
		Real64 UA1; // upper bound of UA for autosizing
		Real64 MinFlowFrac; // minimum flow fraction from terminal unit []
		Real64 TDpIn; // coil inlet air dew point temperature [C]
		int SupFanNum; // index to supply fan
		int RetFanNum; // index to return fan
		Real64 SupFanDT; // supply air fan delta temperature [C]
		Real64 RetFanDT; // return air fan delta temperature [C]
		Real64 FanCoolLoad; // load due to fan operation added to cooling load [W]
		Array1D< Real64 > Par( 4 ); // array passed to RegulaFalsi
		Real64 DesOAFlowFrac;   // design outdoor air flow volume fraction
		std::string ScalableSM; // scalable sizing methods label for reporting
		Real64 const RatedInletAirTemp( 26.6667 ); // 26.6667C or 80F
		Real64 const RatedInletAirHumRat( 0.01125 ); // Humidity ratio corresponding to 80F dry bulb/67F wet bulb

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
		FanCoolLoad = 0;
		SizingDesValueFromParent = false;

		if ( SysSizingRunDone || ZoneSizingRunDone ) {
			HardSizeNoDesRun = false;
		} else {
			HardSizeNoDesRun = true;
		}

		if ( CurSysNum > 0 ) {
			CheckThisAirSystemForSizing( CurSysNum, SizingDesRunThisAirSys );
			AirLoopSysFlag = UnitarySysEqSizing( CurSysNum ).CoolingCapacity || UnitarySysEqSizing( CurSysNum ).HeatingCapacity; // logicals used when parent sizes coil
			if ( CurOASysNum > 0 ) OASysFlag = OASysEqSizing( CurOASysNum ).CoolingCapacity || OASysEqSizing( CurOASysNum ).HeatingCapacity;
		} else {
			SizingDesRunThisAirSys = false;
		}

		if ( CurZoneEqNum > 0 ) {
			SizingDesValueFromParent = ZoneEqSizing( CurZoneEqNum ).DesignSizeFromParent;
			CheckThisZoneForSizing( CurZoneEqNum, SizingDesRunThisZone );
			HardSizeNoDesRun = false;
		} else {
			SizingDesRunThisZone = false;
		}

		if ( SizingResult == AutoSize ) {
			IsAutoSize = true;
			HardSizeNoDesRun = false;
			if ( !SizingDesRunThisAirSys && CurSysNum > 0 && SizingType != AutoCalculateSizing ) CheckSysSizing( CompType, CompName );
			if ( !SizingDesRunThisZone && CurZoneEqNum > 0 && !SizingDesValueFromParent && SizingType != AutoCalculateSizing ) CheckZoneSizing( CompType, CompName );
		}

		if ( SizingType == AutoCalculateSizing ) {
			if ( DataFractionUsedForSizing > 0.0 ) {
				AutosizeDes = DataConstantUsedForSizing * DataFractionUsedForSizing;
				HardSizeNoDesRun = false;
			} else {
				ShowSevereError( CallingRoutine + ' ' + CompType + ' ' + CompName );
				ShowContinueError( "... DataConstantUsedForSizing and DataFractionUsedForSizing used for autocalculating " + SizingString + " must both be greater than 0." );
				ShowFatalError( "Preceding conditions cause termination." );
			}
			bCheckForZero = false;
		} else if ( CurZoneEqNum > 0 ) {
			if ( !IsAutoSize && !SizingDesRunThisZone && !SizingDesValueFromParent ) {
				HardSizeNoDesRun = true;
				AutosizeUser = SizingResult;
				if ( PrintWarningFlag && SizingResult > 0.0 && !DataScalableCapSizingON ) {
					if ( SameString( CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE" ) && SizingType == CoolingAirflowSizing  && DataIsDXCoil ) {
						SizingResult /= ( 1 - DataBypassFrac ); // back out bypass fraction applied in GetInput
						ReportSizingOutput( CompType, CompName, "User-Specified " + SizingString, SizingResult );
						SizingResult *= ( 1 - DataBypassFrac ); // now reapply for second message and remianing simulation calcs
						ReportSizingOutput( CompType, CompName, "User-Specified " + SizingString + " (non-bypassed)", SizingResult );
					} else {
						ReportSizingOutput( CompType, CompName, "User-Specified " + SizingString, SizingResult );
					}
				}
			} else {
				if ( SizingType == SystemAirflowSizing ) {

					{ auto const SELECT_CASE_var( ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingType) );
					if ( ( SELECT_CASE_var == SupplyAirFlowRate ) || ( SELECT_CASE_var == None ) ) {

						if ( ZoneEqSizing( CurZoneEqNum ).SystemAirFlow ) {
							AutosizeDes = max( ZoneEqSizing( CurZoneEqNum ).AirVolFlow, FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
						} else {
							if ( ZoneCoolingOnlyFan ) {
								AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
							} else if ( ZoneHeatingOnlyFan ) {
								AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
							} else if ( ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow && !ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow ) {
								AutosizeDes = ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow;
							} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && !ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
								AutosizeDes = ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow;
							} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
								AutosizeDes = max( ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow, ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow );
							} else {
								AutosizeDes = max( FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
							}
						}
					} else if ( SELECT_CASE_var == FractionOfAutosizedCoolingAirflow ) {
						if ( ZoneCoolingOnlyFan ) {
							AutosizeDes = DataFracOfAutosizedCoolingAirflow * FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
						} else if ( ZoneHeatingOnlyFan ) {
							AutosizeDes = DataFracOfAutosizedHeatingAirflow * FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow && !ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow ) {
							AutosizeDes = DataFracOfAutosizedCoolingAirflow * ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && !ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = DataFracOfAutosizedHeatingAirflow * ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = max( DataFracOfAutosizedCoolingAirflow * ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow, DataFracOfAutosizedHeatingAirflow * ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow );
						} else {
							AutosizeDes = max( DataFracOfAutosizedCoolingAirflow * FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, DataFracOfAutosizedHeatingAirflow * FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
						}
					} else if ( SELECT_CASE_var == FractionOfAutosizedHeatingAirflow ) {
						if ( ZoneCoolingOnlyFan ) {
							AutosizeDes = DataFracOfAutosizedCoolingAirflow * FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
						} else if ( ZoneHeatingOnlyFan ) {
							AutosizeDes = DataFracOfAutosizedHeatingAirflow * FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow && !ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow ) {
							AutosizeDes = DataFracOfAutosizedCoolingAirflow * ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && !ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = DataFracOfAutosizedHeatingAirflow * ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = max( DataFracOfAutosizedCoolingAirflow * ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow, DataFracOfAutosizedHeatingAirflow * ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow );
						} else {
							AutosizeDes = max( DataFracOfAutosizedCoolingAirflow * FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, DataFracOfAutosizedHeatingAirflow * FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
						}
					} else if ( SELECT_CASE_var == FlowPerCoolingCapacity ) {
						if ( ZoneCoolingOnlyFan ) {
							AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
						} else if ( ZoneHeatingOnlyFan ) {
							AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
						} else if ( ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow && !ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow ) {
							AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && !ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = max( DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity, DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity );
						} else {
							AutosizeDes = max( DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity, DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity );
						}
					} else if ( SELECT_CASE_var == FlowPerHeatingCapacity ) {
						if ( ZoneCoolingOnlyFan ) {
							AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
						} else if ( ZoneHeatingOnlyFan ) {
							AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
						} else if ( ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow && !ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow ) {
							AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && !ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = max( DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity, DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity );
						} else {
							AutosizeDes = max( DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity, DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity );
						}
					} else {
						if ( ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow && !ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow ) {
							AutosizeDes = ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && !ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = max( ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow, ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow );
						} else {
							if ( ZoneEqSizing( CurZoneEqNum ).SystemAirFlow ) {
								AutosizeDes = max( ZoneEqSizing( CurZoneEqNum ).AirVolFlow, ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow, ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow );
							} else if ( ZoneCoolingOnlyFan ) {
								AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
							} else if ( ZoneHeatingOnlyFan ) {
								AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
							} else {
								AutosizeDes = max( FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
							}
						}
					}
					}
				} else if ( SizingType == CoolingAirflowSizing || SizingType == HeatingAirflowSizing ) {
					{ auto const SELECT_CASE_var( ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingType ) );
					if ( ( SELECT_CASE_var == SupplyAirFlowRate ) || ( SELECT_CASE_var == None ) || ( SELECT_CASE_var == FlowPerFloorArea ) ) {
						if ( ZoneEqSizing( CurZoneEqNum ).SystemAirFlow ) {
							if ( SizingType == CoolingAirflowSizing ) {
								AutosizeDes = max( ZoneEqSizing( CurZoneEqNum ).AirVolFlow, ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow );
							} else if ( SizingType == HeatingAirflowSizing ) {
								AutosizeDes = max( ZoneEqSizing( CurZoneEqNum ).AirVolFlow, ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow );
							} else {
								AutosizeDes = max( ZoneEqSizing( CurZoneEqNum ).AirVolFlow, ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow, ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow );
							}
						} else {
							if ( ZoneCoolingOnlyFan ) {
								AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
							} else if ( ZoneHeatingOnlyFan ) {
								AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
							} else if ( ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow && !ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow ) {
								AutosizeDes = ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow;
							} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && !ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
								AutosizeDes = ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow;
							} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
								AutosizeDes = max( ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow, ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow );
							} else {
								AutosizeDes = max( FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
							}
						}
					} else if ( SELECT_CASE_var == FractionOfAutosizedCoolingAirflow ) {
						if ( ZoneCoolingOnlyFan ) {
							AutosizeDes = DataFracOfAutosizedCoolingAirflow * FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
						} else if ( ZoneHeatingOnlyFan ) {
							AutosizeDes = DataFracOfAutosizedHeatingAirflow * FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow && !ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow ) {
							AutosizeDes = DataFracOfAutosizedCoolingAirflow * ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && !ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = DataFracOfAutosizedHeatingAirflow * ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = max( DataFracOfAutosizedCoolingAirflow * ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow, DataFracOfAutosizedHeatingAirflow * ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow );
						} else {
							AutosizeDes = max( DataFracOfAutosizedCoolingAirflow * FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, DataFracOfAutosizedHeatingAirflow * FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
						}
					} else if ( SELECT_CASE_var == FractionOfAutosizedHeatingAirflow ) {
						if ( ZoneCoolingOnlyFan ) {
							AutosizeDes = DataFracOfAutosizedCoolingAirflow * FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
						} else if ( ZoneHeatingOnlyFan ) {
							AutosizeDes = DataFracOfAutosizedHeatingAirflow * FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow && !ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow ) {
							AutosizeDes = DataFracOfAutosizedCoolingAirflow * ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && !ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = DataFracOfAutosizedHeatingAirflow * ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = max( DataFracOfAutosizedCoolingAirflow * ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow, DataFracOfAutosizedHeatingAirflow * ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow );
						} else {
							AutosizeDes = max( DataFracOfAutosizedCoolingAirflow * FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, DataFracOfAutosizedHeatingAirflow * FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
						}
					} else if ( SELECT_CASE_var == FlowPerCoolingCapacity ) {
						if ( ZoneCoolingOnlyFan ) {
							AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
						} else if ( ZoneHeatingOnlyFan ) {
							AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
						} else if ( ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow && !ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow ) {
							AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && !ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = max( DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity, DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity );
						} else {
							AutosizeDes = max( DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity, DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity );
						}
					} else if ( SELECT_CASE_var == FlowPerHeatingCapacity ) {
						if ( ZoneCoolingOnlyFan ) {
							AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
						} else if ( ZoneHeatingOnlyFan ) {
							AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
						} else if ( ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow && !ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow ) {
							AutosizeDes = DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && !ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = max( DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity, DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity );
						} else {
							AutosizeDes = max( DataFlowPerCoolingCapacity * DataAutosizedCoolingCapacity, DataFlowPerHeatingCapacity * DataAutosizedHeatingCapacity );
						}
					} else {
						if ( ZoneCoolingOnlyFan ) {
							AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
						} else if ( TermUnitIU ) {
							AutosizeDes = TermUnitSizing( CurZoneEqNum ).AirVolFlow;
						} else if ( ZoneEqFanCoil ) {
							AutosizeDes = ZoneEqSizing( CurZoneEqNum ).AirVolFlow;
						} else if ( ZoneHeatingOnlyFan ) {
							AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow && !ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow ) {
							AutosizeDes = ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && !ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow;
						} else if ( ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow && ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow ) {
							AutosizeDes = max( ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow, ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow );
						} else {
							AutosizeDes = max( FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
						}
					}}
				} else if ( SizingType == CoolingWaterflowSizing ) {
					CoilDesWaterDeltaT = PlantSizData( DataPltSizCoolNum ).DeltaT;
					Cp = GetSpecificHeatGlycol( PlantLoop( DataWaterLoopNum ).FluidName, 5.0, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
					rho = GetDensityGlycol( PlantLoop( DataWaterLoopNum ).FluidName, InitConvTemp, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
					if ( TermUnitIU ) {
						AutosizeDes = TermUnitSizing( CurZoneEqNum ).MaxCWVolFlow;
					} else if (ZoneEqFanCoil || ZoneEqUnitVent || ZoneEqVentedSlab ) {
						AutosizeDes = ZoneEqSizing( CurZoneEqNum ).MaxCWVolFlow;
					} else {
						CoilInTemp = FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp;
						CoilOutTemp = FinalZoneSizing( CurZoneEqNum ).CoolDesTemp;
						CoilOutHumRat = FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat;
						CoilInHumRat = FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRat;
						DesCoilLoad = FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow * ( PsyHFnTdbW( CoilInTemp, CoilInHumRat ) - PsyHFnTdbW( CoilOutTemp, CoilOutHumRat ) );
						if ( DesCoilLoad >= SmallLoad ) {
							AutosizeDes = DesCoilLoad / ( CoilDesWaterDeltaT * Cp * rho );
						} else {
							AutosizeDes = 0.0;
						}
					}
					bCheckForZero = false;
				} else if ( SizingType == HeatingWaterflowSizing ) {
					if ( TermUnitSingDuct || TermUnitPIU || TermUnitIU ) {
						AutosizeDes = TermUnitSizing( CurZoneEqNum ).MaxHWVolFlow;
						Cp = GetSpecificHeatGlycol( PlantLoop( DataWaterLoopNum ).FluidName, 60.0, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						rho = GetDensityGlycol( PlantLoop( DataWaterLoopNum ).FluidName, InitConvTemp, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						DesCoilLoad = AutosizeDes * PlantSizData( DataPltSizHeatNum ).DeltaT * Cp * rho;
					} else if ( ZoneEqFanCoil ) {
						AutosizeDes = ZoneEqSizing( CurZoneEqNum ).MaxHWVolFlow;
						Cp = GetSpecificHeatGlycol( PlantLoop( DataWaterLoopNum ).FluidName, 60.0, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						rho = GetDensityGlycol( PlantLoop( DataWaterLoopNum ).FluidName, InitConvTemp, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						DesCoilLoad = AutosizeDes * PlantSizData( DataPltSizHeatNum ).DeltaT * Cp * rho;
					} else if ( ZoneEqUnitHeater || ZoneEqVentedSlab ) {  // for unit ventilator the cp value is calculated at 5.05(InitConvTemp) for the child and 60.0C for the unit ventilator //|| ZoneEqUnitVent
						AutosizeDes = ZoneEqSizing( CurZoneEqNum ).MaxHWVolFlow;
					} else {
						CoilInTemp = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTemp;
						CoilOutTemp = FinalZoneSizing( CurZoneEqNum ).HeatDesTemp;
						CoilOutHumRat = FinalZoneSizing( CurZoneEqNum ).HeatDesHumRat;
						DesMassFlow = FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow;
						DesCoilLoad = PsyCpAirFnWTdb( CoilOutHumRat, 0.5 * ( CoilInTemp + CoilOutTemp ) ) * DesMassFlow * ( CoilOutTemp - CoilInTemp );
						if ( DesCoilLoad >= SmallLoad ) {
							Cp = GetSpecificHeatGlycol( PlantLoop( DataWaterLoopNum ).FluidName, 60.0, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
							rho = GetDensityGlycol( PlantLoop( DataWaterLoopNum ).FluidName, InitConvTemp, PlantLoop(DataWaterLoopNum ).FluidIndex, CallingRoutine );
							AutosizeDes = DesCoilLoad / ( PlantSizData( DataPltSizHeatNum ).DeltaT * Cp * rho );
						} else {
							AutosizeDes = 0.0;
						}
					}
					bCheckForZero = false;
				} else if ( SizingType == HeatingWaterDesAirInletTempSizing ) {
					if ( TermUnitPIU ) {
						MinFlowFrac = TermUnitSizing( CurZoneEqNum ).MinFlowFrac;
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU * MinFlowFrac + FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak * ( 1.0 - MinFlowFrac );
					} else if ( TermUnitIU ) {
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak;
					} else if ( TermUnitSingDuct ) {
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU;
					} else if ( ZoneEqFanCoil ) {
						if ( FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow > 0.0 ) {
							OutAirFrac = min( StdRhoAir * ZoneEqSizing( CurZoneEqNum ).OAVolFlow / FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow, 1.0 );
						} else {
							OutAirFrac = 0.0;
						}
						AutosizeDes = OutAirFrac * FinalZoneSizing( CurZoneEqNum ).OutTempAtHeatPeak + ( 1.0 - OutAirFrac ) * FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak;
					} else {
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTemp;
					}
					bCheckForZero = false;
				} else if ( SizingType == HeatingWaterDesAirInletHumRatSizing ) {
					if ( TermUnitPIU ) {
						MinFlowFrac = TermUnitSizing( CurZoneEqNum ).MinFlowFrac;
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInHumRatTU * MinFlowFrac + FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtHeatPeak * ( 1.0 - MinFlowFrac );
					} else if ( TermUnitIU ) {
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtHeatPeak;
					} else if ( TermUnitSingDuct ) {
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInHumRatTU;
					} else if ( ZoneEqFanCoil ) {
						if ( FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow > 0.0 ) {
							OutAirFrac = min( StdRhoAir * ZoneEqSizing( CurZoneEqNum ).OAVolFlow / FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow, 1.0 );
						} else {
							OutAirFrac = 0.0;
						}
						AutosizeDes = OutAirFrac * FinalZoneSizing( CurZoneEqNum ).OutHumRatAtHeatPeak + ( 1.0 - OutAirFrac ) * FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtHeatPeak;
					} else {
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInHumRat;
					}
					bCheckForZero = false;
				} else if ( SizingType == CoolingWaterDesAirInletTempSizing ) {
					if ( TermUnitIU ) {
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak;
					} else if ( ZoneEqFanCoil ) {
						if ( FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow > 0.0 ) {
							OutAirFrac = min( StdRhoAir * ZoneEqSizing( CurZoneEqNum ).OAVolFlow / FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow, 1.0 );
						} else {
							OutAirFrac = 0.0;
						}
						AutosizeDes = OutAirFrac * FinalZoneSizing( CurZoneEqNum ).OutTempAtCoolPeak + ( 1.0 - OutAirFrac ) * FinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak;
					} else {
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp;
					}
					bCheckForZero = false;
				} else if ( SizingType == CoolingWaterDesWaterInletTempSizing ) {
					AutosizeDes = PlantSizData( DataPltSizCoolNum ).ExitTemp;
					bCheckForZero = false;
				} else if ( SizingType == CoolingWaterNumofTubesPerRowSizing ) {
					// result will be integerized external to this routine , add 0.5 to existing calc to round the result
					AutosizeDes = int ( max ( 3.0, 13750.0 * DataWaterFlowUsedForSizing + 1.0 ) );
					bCheckForZero = false;
				} else if ( SizingType == CoolingWaterDesAirOutletTempSizing ) {
					if ( TermUnitIU ) {
						Cp = GetSpecificHeatGlycol( PlantLoop( DataWaterLoopNum ).FluidName, 5.0, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						rho = GetDensityGlycol( PlantLoop( DataWaterLoopNum ).FluidName, InitConvTemp, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						DesCoilLoad = DataWaterFlowUsedForSizing * PlantSizData( DataPltSizCoolNum ).DeltaT * Cp * rho;
						T1Out = DataDesInletAirTemp - DesCoilLoad / ( StdRhoAir * PsyCpAirFnWTdb( DataDesInletAirHumRat, DataDesInletAirTemp ) * DataAirFlowUsedForSizing );
						T2Out = PlantSizData( DataPltSizCoolNum ).ExitTemp + 2.0;
						AutosizeDes = max( T1Out, T2Out );
					} else {
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).CoolDesTemp;
					}

					if ( AutosizeDes < DataDesInletWaterTemp && DataWaterFlowUsedForSizing > 0.0 ) { // flow here is water vol flow rate
						ShowWarningError( CallingRoutine + ":" + " Coil=\"" + CompName + "\", Cooling Coil has leaving air temperature > entering water temperature." );
						ShowContinueError( "    Tair,out  =  " + RoundSigDigits( AutosizeDes, 3 ) );
						ShowContinueError( "    Twater,in = " + RoundSigDigits( DataDesInletWaterTemp, 3 ) );
						AutosizeDes = DataDesInletWaterTemp + 0.5;
						ShowContinueError( "....coil leaving air temperature will be reset to:" );
						ShowContinueError( "    Tair,out = " + RoundSigDigits( AutosizeDes, 3 ) );
					}
					bCheckForZero = false;
				} else if ( SizingType == CoolingWaterDesAirInletHumRatSizing ) {
					if ( TermUnitIU ) {
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtCoolPeak;
					} else if ( ZoneEqFanCoil ) {
						if ( FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow > 0.0 ) {
							OutAirFrac = min( StdRhoAir * ZoneEqSizing( CurZoneEqNum ).OAVolFlow / FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow, 1.0 );
						} else {
							OutAirFrac = 0.0;
						}
						AutosizeDes = OutAirFrac * FinalZoneSizing( CurZoneEqNum ).OutHumRatAtCoolPeak + ( 1.0 - OutAirFrac ) * FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtCoolPeak;
					} else {
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRat;
					}
					bCheckForZero = false;
				} else if ( SizingType == CoolingWaterDesAirOutletHumRatSizing ) {
					if ( TermUnitIU ) {
						TDpIn = PsyTdpFnWPb( DataDesInletAirHumRat, StdBaroPress );
						if ( TDpIn <= DataDesInletWaterTemp ) {
							AutosizeDes = DataDesInletAirHumRat;
						} else {
							AutosizeDes = min( PsyWFnTdbRhPb( DataDesOutletAirTemp, 0.9, StdBaroPress ), DataDesInletAirHumRat );
						}
					} else {
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat;
					}
					if ( AutosizeDes > DataDesInletAirHumRat && DataCapacityUsedForSizing > 0.0 && SameString( CompType, "COIL:COOLING:WATER" ) ) { // zone coil uses mDot>0, sys coil uses cap > 0
						ShowWarningError( CallingRoutine + ":" + " Coil=\"" + CompName + "\", Cooling Coil has leaving humidity ratio > entering humidity ratio." );
						ShowContinueError( "    Wair,in =  " + RoundSigDigits( DataDesInletAirHumRat, 6 ) );
						ShowContinueError( "    Wair,out = " + RoundSigDigits( AutosizeDes, 6 ) );
						if ( DataDesInletAirHumRat > 0.016 ) {
							AutosizeDes = 0.5 * DataDesInletAirHumRat;
						} else {
							AutosizeDes = DataDesInletAirHumRat;
						}
						ShowContinueError( "....coil leaving humidity ratio will be reset to:" );
						ShowContinueError( "    Wair,out = " + RoundSigDigits( AutosizeDes, 6 ) );
					}

					// check for dry coil and reset outlet humrat if needed
					DesSatEnthAtWaterInTemp = PsyHFnTdbW( DataDesInletWaterTemp, PsyWFnTdpPb( DataDesInletWaterTemp, StdBaroPress ) );
					DesHumRatAtWaterInTemp = PsyWFnTdbH( DataDesInletWaterTemp, DesSatEnthAtWaterInTemp, CallingRoutine );
					if ( AutosizeDes < DataDesInletAirHumRat && DesHumRatAtWaterInTemp > DataDesInletAirHumRat && DataCapacityUsedForSizing > 0.0 ) {
						if ( DataDesInletAirHumRat > AutosizeDes && SameString( CompType, "COIL:COOLING:WATER" ) ) {
							ShowWarningError( CallingRoutine + ":" + " Coil=\"" + CompName + "\", Cooling Coil is dry and has air leaving humidity ratio < entering humidity ratio." );
							ShowContinueError( "    Wair,in =  " + RoundSigDigits( DataDesInletAirHumRat, 6 ) );
							ShowContinueError( "    Wair,out = " + RoundSigDigits( AutosizeDes, 6 ) );
							AutosizeDes = DataDesInletAirHumRat;
							ShowContinueError( "....coil leaving humidity ratio will be reset to:" );
							ShowContinueError( "    Wair,out = " + RoundSigDigits( AutosizeDes, 6 ) );
						}
					}
					bCheckForZero = false;
				} else if ( SizingType == CoolingSHRSizing ) {
					if ( DataFlowUsedForSizing >= SmallAirVolFlow && DataCapacityUsedForSizing > 0.0 ) {
						// For autosizing the rated SHR, we set a minimum SHR of 0.676 and a maximum of 0.798. The min SHR occurs occurs at the
						// minimum flow / capacity ratio = MinRatedVolFlowPerRatedTotCap = 0.00004027 [m3/s / W] = 300 [cfm/ton].
						// The max SHR occurs at maximum flow / capacity ratio = MaxRatedVolFlowPerRatedTotCap = 0.00006041 [m3/s / W] = 450 [cfm/ton].
						// For flow / capacity ratios between the min and max we linearly interpolate between min and max SHR. Thus rated SHR is a
						// linear function of the rated flow / capacity ratio. This linear function (see below) is the result of a regression
						// of flow/capacity ratio vs SHR for several actual coils.
						RatedVolFlowPerRatedTotCap = DataFlowUsedForSizing / DataCapacityUsedForSizing;
						if ( DXCT == RegularDXCoil ) {
							if ( RatedVolFlowPerRatedTotCap > MaxRatedVolFlowPerRatedTotCap( DXCT ) ) {
								AutosizeDes = 0.431 + 6086.0 * MaxRatedVolFlowPerRatedTotCap ( DXCT );
							} else if ( RatedVolFlowPerRatedTotCap < MinRatedVolFlowPerRatedTotCap( DXCT ) ) {
								AutosizeDes = 0.431 + 6086.0*MinRatedVolFlowPerRatedTotCap ( DXCT );
							} else {
								AutosizeDes = 0.431 + 6086.0*RatedVolFlowPerRatedTotCap;
							}
						} else { // DOASDXCoil, or DXCT = 2
							if ( RatedVolFlowPerRatedTotCap > MaxRatedVolFlowPerRatedTotCap( DXCT ) ) {
								AutosizeDes = 0.389 + 7684.0*MaxRatedVolFlowPerRatedTotCap ( DXCT );
							} else if ( RatedVolFlowPerRatedTotCap < MinRatedVolFlowPerRatedTotCap( DXCT ) ) {
								AutosizeDes = 0.389 + 7684.0*MinRatedVolFlowPerRatedTotCap ( DXCT );
							} else {
								AutosizeDes = 0.389 + 7684.0*RatedVolFlowPerRatedTotCap;
							}
						}

						// check that the autosized SHR corresponds to a valid apperatus dew point (ADP) temperature
						DesMassFlow = DataFlowUsedForSizing * PsyRhoAirFnPbTdbW( StdBaroPress, RatedInletAirTemp, RatedInletAirHumRat, CallingRoutine );
						AutosizeDes = ValidateADP( CompType, CompName, RatedInletAirTemp, RatedInletAirHumRat, DataCapacityUsedForSizing, DesMassFlow, AutosizeDes, CallingRoutine );

					} else {
						AutosizeDes = 1.0;
					}
				} else if ( SizingType == CoolingCapacitySizing ) {
					if ( ZoneEqSizing( CurZoneEqNum ).CoolingCapacity ) { // Parent object calculated capacity
						AutosizeDes = ZoneEqSizing ( CurZoneEqNum ).DesCoolingLoad;
						DesVolFlow = DataFlowUsedForSizing;
					} else {
						if ( SameString( CompType, "COIL:COOLING:WATER" ) || SameString( CompType, "COIL:COOLING:WATER:DETAILEDGEOMETRY") || SameString( CompType, "ZONEHVAC:IDEALLOADSAIRSYSTEM" ) ) {
							if ( TermUnitIU ) {
								AutosizeDes = TermUnitSizing( CurZoneEqNum ).DesCoolingLoad;
							} else if ( ZoneEqFanCoil ) {
								AutosizeDes = ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad;
							} else {
								CoilInTemp = FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp;
								CoilInHumRat = FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRat;
								CoilOutTemp = min( CoilInTemp, FinalZoneSizing( CurZoneEqNum ).CoolDesTemp);
								CoilOutHumRat = min( CoilInHumRat, FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat);
								AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow * ( PsyHFnTdbW( CoilInTemp, CoilInHumRat ) - PsyHFnTdbW( CoilOutTemp, CoilOutHumRat ) );
								DesVolFlow = FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow / StdRhoAir;
							}
						} else {
							DesVolFlow = DataFlowUsedForSizing;
							if ( DesVolFlow >= SmallAirVolFlow ) {
								if ( ZoneEqDXCoil ) {
									if ( ZoneEqSizing( CurZoneEqNum ).OAVolFlow > 0.0) {
										CoilInTemp = FinalZoneSizing ( CurZoneEqNum ).DesCoolCoilInTemp;
										CoilInHumRat = FinalZoneSizing ( CurZoneEqNum ).DesCoolCoilInHumRat;
									} else {
										CoilInTemp = FinalZoneSizing ( CurZoneEqNum ).ZoneRetTempAtCoolPeak;
										CoilInHumRat = FinalZoneSizing ( CurZoneEqNum ).ZoneHumRatAtCoolPeak;
									}
								} else if ( ZoneEqFanCoil ) {
									DesOAFlowFrac = FinalZoneSizing(CurZoneEqNum).DesCoolOAFlowFrac;
									CoilInTemp = DesOAFlowFrac * FinalZoneSizing(CurZoneEqNum).OutTempAtCoolPeak + (1.0 - DesOAFlowFrac) * FinalZoneSizing(CurZoneEqNum).ZoneTempAtCoolPeak;
									CoilInHumRat = DesOAFlowFrac * FinalZoneSizing(CurZoneEqNum).OutHumRatAtCoolPeak + (1.0 - DesOAFlowFrac) * FinalZoneSizing(CurZoneEqNum).ZoneHumRatAtCoolPeak;
								} else {
									CoilInTemp = FinalZoneSizing ( CurZoneEqNum ).DesCoolCoilInTemp;
									CoilInHumRat = FinalZoneSizing ( CurZoneEqNum ).DesCoolCoilInHumRat;
								}
								CoilOutTemp = min( CoilInTemp, FinalZoneSizing( CurZoneEqNum ).CoolDesTemp );
								CoilOutHumRat = min( CoilInHumRat, FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat);
								TimeStepNumAtMax = FinalZoneSizing ( CurZoneEqNum ).TimeStepNumAtCoolMax;
								DDNum = FinalZoneSizing ( CurZoneEqNum ).CoolDDNum;
								if ( DDNum > 0 && TimeStepNumAtMax > 0 ) {
									OutTemp = DesDayWeath ( DDNum ).Temp ( TimeStepNumAtMax );
								} else {
									OutTemp = 0.0;
								}
								rhoair = PsyRhoAirFnPbTdbW ( StdBaroPress, CoilInTemp, CoilInHumRat, CallingRoutine );
								CoilInEnth = PsyHFnTdbW ( CoilInTemp, CoilInHumRat );
								CoilInWetBulb = PsyTwbFnTdbWPb ( CoilInTemp, CoilInHumRat, StdBaroPress, CallingRoutine );
								CoilOutEnth = PsyHFnTdbW ( CoilOutTemp, CoilOutHumRat );
								if ( DataTotCapCurveIndex > 0 ) {
									TotCapTempModFac = CurveValue ( DataTotCapCurveIndex, CoilInWetBulb, OutTemp );
								} else if ( DataTotCapCurveValue > 0 ) {
									TotCapTempModFac = DataTotCapCurveValue;
								} else {
									TotCapTempModFac = 1.0;
								}
								if (ZoneEqFanCoil) {
									PeakCoilLoad = max( 0.0, ( StdRhoAir * DesVolFlow * ( CoilInEnth - CoilOutEnth ) ) );
								} else if ( ZoneEqUnitVent ) {
									PeakCoilLoad = max( 0.0, ( StdRhoAir * DesVolFlow * ( CoilInEnth - CoilOutEnth ) ) );
								} else {
									PeakCoilLoad = max( 0.0, ( rhoair * DesVolFlow * ( CoilInEnth - CoilOutEnth ) ) );
								}
								if ( TotCapTempModFac > 0.0 ) {
									AutosizeDes = PeakCoilLoad / TotCapTempModFac;
								} else {
									AutosizeDes = PeakCoilLoad;
								}
							} else {
								AutosizeDes = 0.0;
								CoilOutTemp = -999.0;
							}
						}
					}
					AutosizeDes = AutosizeDes * DataFracOfAutosizedCoolingCapacity;
					if ( DisplayExtraWarnings && AutosizeDes <= 0.0 ) {
						ShowWarningMessage( CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName );
						ShowContinueError( "...Rated Total Cooling Capacity = " + TrimSigDigits( AutosizeDes, 2 ) + " [W]" );
						if ( ZoneEqSizing( CurZoneEqNum ).CoolingCapacity ) {
							ShowContinueError( "...Capacity passed by parent object to size child component = " + TrimSigDigits( AutosizeDes, 2 ) + " [W]" );
						} else {
							if ( SameString( CompType, "COIL:COOLING:WATER" ) || SameString( CompType, "COIL:COOLING:WATER:DETAILEDGEOMETRY" ) || SameString( CompType, "ZONEHVAC:IDEALLOADSAIRSYSTEM" ) ) {
								if ( TermUnitIU || ZoneEqFanCoil ) {
									ShowContinueError( "...Capacity passed by parent object to size child component = " + TrimSigDigits( AutosizeDes, 2 ) + " [W]" );
								} else {
									ShowContinueError( "...Air flow rate used for sizing = " + TrimSigDigits( DesVolFlow, 5 ) + " [m3/s]" );
									ShowContinueError( "...Coil inlet air temperature used for sizing = " + TrimSigDigits( CoilInTemp, 2 ) + " [C]" );
									ShowContinueError( "...Coil outlet air temperature used for sizing = " + TrimSigDigits( CoilOutTemp, 2 ) + " [C]" );
								}
							} else {
								if ( CoilOutTemp > -999.0 ) {
									ShowContinueError( "...Air flow rate used for sizing = " + TrimSigDigits( DesVolFlow, 5 ) + " [m3/s]" );
									ShowContinueError( "...Coil inlet air temperature used for sizing = " + TrimSigDigits( CoilInTemp, 2 ) + " [C]" );
									ShowContinueError( "...Coil outlet air temperature used for sizing = " + TrimSigDigits( CoilOutTemp, 2 ) + " [C]" );
								} else {
									ShowContinueError( "...Capacity used to size child component set to 0 [W]" );
								}
							}
						}
					}
				} else if ( SizingType == HeatingCapacitySizing ) {
					if ( ZoneEqSizing(CurZoneEqNum).HeatingCapacity ) {
						NominalCapacityDes = ZoneEqSizing(CurZoneEqNum).DesHeatingLoad;
						if ( DataFlowUsedForSizing > 0.0 ) {
							DesVolFlow = DataFlowUsedForSizing;
						}
					} else if ( DataCoolCoilCap > 0.0 && DataFlowUsedForSizing > 0.0 ) {
						NominalCapacityDes = DataCoolCoilCap;
						DesVolFlow = DataFlowUsedForSizing;
					} else if ( FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow >= SmallMassFlow ) {
						if ( DataFlowUsedForSizing > 0.0) {
							DesVolFlow = DataFlowUsedForSizing;
						}
						if ( TermUnitPIU ) {
							MinPriFlowFrac = TermUnitSizing( CurZoneEqNum ).MinFlowFrac;
							if ( TermUnitSizing( CurZoneEqNum ).InducesPlenumAir ) {
								CoilInTemp = ( FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU * MinPriFlowFrac ) + ( FinalZoneSizing( CurZoneEqNum ).ZoneRetTempAtHeatPeak * ( 1.0 - MinPriFlowFrac ) );
							} else {
								CoilInTemp = ( FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU * MinPriFlowFrac ) + ( FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak * ( 1.0 - MinPriFlowFrac ) );
							}
						} else if ( ZoneEqFanCoil ) {
								DesOAFlowFrac = FinalZoneSizing( CurZoneEqNum ).DesHeatOAFlowFrac;
								CoilInTemp = DesOAFlowFrac * FinalZoneSizing(CurZoneEqNum).OutTempAtHeatPeak + (1.0 - DesOAFlowFrac) * FinalZoneSizing(CurZoneEqNum).ZoneTempAtHeatPeak;
						} else if ( TermUnitIU ) {
							CoilInTemp = FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak;
						} else if ( TermUnitSingDuct ) {
							CoilInTemp = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU;
						} else {
							CoilInTemp = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTemp;
						}
						if ( TermUnitSingDuct || TermUnitPIU ) {
							CoilOutTemp = FinalZoneSizing( CurZoneEqNum ).HeatDesTemp;
							CoilOutHumRat = FinalZoneSizing( CurZoneEqNum ).HeatDesHumRat;
							CpAir = PsyCpAirFnWTdb( CoilOutHumRat, 0.5 * ( CoilInTemp + CoilOutTemp ) );
							DesCoilLoad = CpAir * StdRhoAir * TermUnitSizing( CurZoneEqNum ).AirVolFlow * ( CoilOutTemp - CoilInTemp );
							DesVolFlow = TermUnitSizing( CurZoneEqNum ).AirVolFlow;
						} else if ( TermUnitIU ) {
							if ( TermUnitSizing( CurZoneEqNum ).InducRat > 0.01 ) {
								DesVolFlow = TermUnitSizing( CurZoneEqNum ).AirVolFlow / TermUnitSizing( CurZoneEqNum ).InducRat;
								CpAir = PsyCpAirFnWTdb( FinalZoneSizing( CurZoneEqNum ).HeatDesHumRat, FinalZoneSizing( CurZoneEqNum ).HeatDesTemp );
								// the design heating coil load is the zone load minus whatever the central system does.Note that
								// DesHeatCoilInTempTU is really the primary air inlet temperature for the unit.
								DesCoilLoad = FinalZoneSizing( CurZoneEqNum ).DesHeatLoad - ( CpAir * StdRhoAir * DesVolFlow * ( FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU - FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak ) );
							} else {
								DesCoilLoad = 0.0;
							}
						} else {
							CoilOutTemp = FinalZoneSizing( CurZoneEqNum ).HeatDesTemp;
							CoilOutHumRat = FinalZoneSizing( CurZoneEqNum ).HeatDesHumRat;
							CpAir = PsyCpAirFnWTdb( CoilOutHumRat, 0.5 * ( CoilInTemp + CoilOutTemp ) );
							DesCoilLoad = CpAir * FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow * ( CoilOutTemp - CoilInTemp );
							DesVolFlow = FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow / StdRhoAir;
						}
						NominalCapacityDes = max( 0.0, DesCoilLoad );
					} else {
						NominalCapacityDes = 0.0;
						CoilOutTemp = -999.0;
					}
					if ( DataCoolCoilCap > 0.0 ) {
						AutosizeDes = NominalCapacityDes * DataHeatSizeRatio;
					} else {
						AutosizeDes = NominalCapacityDes * DataHeatSizeRatio * DataFracOfAutosizedHeatingCapacity;
					}
					if ( DisplayExtraWarnings && AutosizeDes <= 0.0 ) {
						ShowWarningMessage( CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName );
						ShowContinueError( "...Rated Total Heating Capacity = " + TrimSigDigits( AutosizeDes, 2 ) + " [W]" );
						if ( ZoneEqSizing( CurZoneEqNum ).HeatingCapacity || ( DataCoolCoilCap > 0.0 && DataFlowUsedForSizing > 0.0 ) ) {
							ShowContinueError( "...Capacity passed by parent object to size child component = " + TrimSigDigits( NominalCapacityDes, 2 ) + " [W]" );
						} else {
							if ( CoilOutTemp > -999.0 ) {
								ShowContinueError( "...Air flow rate used for sizing = " + TrimSigDigits( DesVolFlow, 5 ) + " [m3/s]" );
								ShowContinueError( "...Coil inlet air temperature used for sizing = " + TrimSigDigits( CoilInTemp, 2 ) + " [C]" );
								ShowContinueError( "...Coil outlet air temperature used for sizing = " + TrimSigDigits( CoilOutTemp, 2 ) + " [C]" );
							} else {
								ShowContinueError( "...Capacity used to size child component set to 0 [W]" );
							}
						}
					}
				} else if ( SizingType == WaterHeatingCapacitySizing ) {
					if ( TermUnitSingDuct || TermUnitPIU || TermUnitIU ) {
						DesMassFlow = TermUnitSizing( CurZoneEqNum ).MaxHWVolFlow;
						Cp = GetSpecificHeatGlycol( PlantLoop( DataWaterLoopNum ).FluidName, 60.0, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						rho = GetDensityGlycol( PlantLoop( DataWaterLoopNum ).FluidName, InitConvTemp, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						NominalCapacityDes = DesMassFlow * PlantSizData( DataPltSizHeatNum ).DeltaT * Cp * rho;
					} else if ( ZoneEqFanCoil || ZoneEqUnitHeater ) {
						DesMassFlow = ZoneEqSizing( CurZoneEqNum ).MaxHWVolFlow;
						Cp = GetSpecificHeatGlycol( PlantLoop( DataWaterLoopNum ).FluidName, 60.0, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						rho = GetDensityGlycol( PlantLoop( DataWaterLoopNum ).FluidName, InitConvTemp, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						NominalCapacityDes = DesMassFlow * PlantSizData( DataPltSizHeatNum ).DeltaT * Cp * rho;
						// if coil is part of a zonal unit, calc coil load to get hot water flow rate
					} else {
						CoilInTemp = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTemp;
						CoilOutTemp = FinalZoneSizing( CurZoneEqNum ).HeatDesTemp;
						CoilOutHumRat = FinalZoneSizing( CurZoneEqNum ).HeatDesHumRat;
						DesMassFlow = FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow;
						NominalCapacityDes = PsyCpAirFnWTdb( CoilOutHumRat, 0.5 * ( CoilInTemp + CoilOutTemp ) ) * DesMassFlow * ( CoilOutTemp - CoilInTemp );
					}
					AutosizeDes = NominalCapacityDes * DataHeatSizeRatio;
					if ( DisplayExtraWarnings && AutosizeDes <= 0.0 ) {
						ShowWarningMessage( CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName );
						ShowContinueError( "...Rated Total Heating Capacity = " + TrimSigDigits( AutosizeDes, 2 ) + " [W]" );
						ShowContinueError( "...Air flow rate used for sizing = " + TrimSigDigits( DesMassFlow / StdRhoAir, 5 ) + " [m3/s]" );
						if ( TermUnitSingDuct || TermUnitPIU || TermUnitIU || ZoneEqFanCoil ) {
							ShowContinueError( "...Plant loop temperature difference = " + TrimSigDigits( PlantSizData( DataPltSizHeatNum ).DeltaT, 2 ) + " [C]" );
						} else {
							ShowContinueError( "...Coil inlet air temperature used for sizing = " + TrimSigDigits( CoilInTemp, 2 ) + " [C]" );
							ShowContinueError( "...Coil outlet air temperature used for sizing = " + TrimSigDigits( CoilOutTemp, 2 ) + " [C]" );
							ShowContinueError( "...Coil outlet air humidity ratio used for sizing = " + TrimSigDigits( CoilOutHumRat, 2 ) + " [kgWater/kgDryAir]" );
						}
					}
				} else if ( SizingType == HeatingWaterDesCoilLoadUsedForUASizing ) {
					if ( TermUnitSingDuct || TermUnitPIU || TermUnitIU ) {
						DesMassFlow = StdRhoAir * TermUnitSizing( CurZoneEqNum ).AirVolFlow * TermUnitSizing( CurZoneEqNum ).ReheatAirFlowMult;
						Cp = GetSpecificHeatGlycol( PlantLoop( DataWaterLoopNum ).FluidName, 60.0, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						rho = GetDensityGlycol( PlantLoop( DataWaterLoopNum ).FluidName, InitConvTemp, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						AutosizeDes = DataWaterFlowUsedForSizing * PlantSizData( DataPltSizHeatNum ).DeltaT * Cp * rho * TermUnitSizing( CurZoneEqNum ).ReheatLoadMult;
					} else if ( ZoneEqFanCoil || ZoneEqUnitHeater ) {
						DesMassFlow = StdRhoAir * FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
						Cp = GetSpecificHeatGlycol( PlantLoop( DataWaterLoopNum ).FluidName, 60.0, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						rho = GetDensityGlycol( PlantLoop( DataWaterLoopNum ).FluidName, InitConvTemp, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						AutosizeDes = DataWaterFlowUsedForSizing * PlantSizData( DataPltSizHeatNum ).DeltaT * Cp * rho;
					} else {
						DesMassFlow = FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow;
						CoilInTemp = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTemp;
						CoilInHumRat = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInHumRat;
						CoilOutTemp = FinalZoneSizing( CurZoneEqNum ).HeatDesTemp;
						CoilOutHumRat = FinalZoneSizing( CurZoneEqNum ).HeatDesHumRat;
						AutosizeDes = PsyCpAirFnWTdb( CoilOutHumRat, 0.5 * ( CoilInTemp + CoilOutTemp ) ) * DesMassFlow * ( CoilOutTemp - CoilInTemp );
					}
				} else if ( SizingType == HeatingWaterDesCoilWaterVolFlowUsedForUASizing ) {
					if ( TermUnitSingDuct || TermUnitPIU || TermUnitIU ) {
						AutosizeDes = DataWaterFlowUsedForSizing * TermUnitSizing( CurZoneEqNum ).ReheatLoadMult;
					} else if ( ZoneEqFanCoil ) {
						AutosizeDes = DataWaterFlowUsedForSizing;
					} else {
						AutosizeDes = DataWaterFlowUsedForSizing;
					}
					bCheckForZero = false;
				} else if ( SizingType == HeatingAirflowUASizing ) {
					if ( TermUnitSingDuct || TermUnitPIU || TermUnitIU ) {
						AutosizeDes = StdRhoAir * TermUnitSizing( CurZoneEqNum ).AirVolFlow * TermUnitSizing( CurZoneEqNum ).ReheatAirFlowMult;
					} else if ( ZoneEqFanCoil ) {
						AutosizeDes = StdRhoAir * FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
					} else {
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow;
					}
				} else if (SizingType == WaterHeatingCoilUASizing) {
					if ( DataCapacityUsedForSizing > 0.0 ) {
						Par( 1 ) = DataCapacityUsedForSizing;
						Par( 2 ) = double( DataCoilNum );
						Par( 3 ) = double( DataFanOpMode ); //fan operating mode
						Par( 4 ) = 1.0; // part-load ratio
						UA0 = 0.001 * DataCapacityUsedForSizing;
						UA1 = DataCapacityUsedForSizing;
						// Invert the simple heating coil model: given the design inlet conditions and the design load,
						// find the design UA.
						SolveRegulaFalsi( Acc, MaxIte, SolFla, AutosizeDes, SimpleHeatingCoilUAResidual, UA0, UA1, Par );
						if ( SolFla == -1 ) {
							ShowSevereError( "Autosizing of heating coil UA failed for Coil:Heating:Water \"" + CompName + "\"" );
							ShowContinueError( "  Iteration limit exceeded in calculating coil UA" );
							ShowContinueError( "  Lower UA estimate = " + TrimSigDigits( UA0, 6 ) + " W/m2-K (0.1% of Design Coil Load)" );
							ShowContinueError( "  Upper UA estimate = " + TrimSigDigits( UA1, 6 ) + " W/m2-K (100% of Design Coil Load)" );
							ShowContinueError( "  Final UA estimate when iterations exceeded limit = " + TrimSigDigits( AutosizeDes, 6 ) + " W/m2-K" );
							ShowContinueError( "  Zone \"" + FinalZoneSizing( CurZoneEqNum ).ZoneName + "\" coil sizing conditions (may be different than Sizing inputs):" );
							ShowContinueError( "  Coil inlet air temperature     = " + TrimSigDigits( DataDesInletAirTemp, 3 ) + " C" );
							ShowContinueError( "  Coil inlet air humidity ratio  = " + TrimSigDigits( DataDesInletAirHumRat, 3 ) + " kgWater/kgDryAir" );
							ShowContinueError( "  Coil inlet air mass flow rate  = " + TrimSigDigits( DataFlowUsedForSizing, 6 ) + " kg/s" );
							// TotWaterHeatingCoilRate is set in CALL to CalcSimpleHeatingCoil
							ShowContinueError( "  Design Coil Capacity           = " + TrimSigDigits( DataDesignCoilCapacity, 3 ) + " W" );
							if ( TermUnitSingDuct || TermUnitPIU || TermUnitIU || ZoneEqFanCoil ) {
								ShowContinueError( "  Design Coil Load               = " + TrimSigDigits( DataCapacityUsedForSizing, 3 ) + " W" );
							} else {
								ShowContinueError( "  Design Coil Load               = " + TrimSigDigits( DataCapacityUsedForSizing, 3 ) + " W" );
								ShowContinueError( "  Coil outlet air temperature    = " + TrimSigDigits( FinalZoneSizing( CurZoneEqNum ).HeatDesTemp, 3 ) + " C" );
								ShowContinueError( "  Coil outlet air humidity ratio = " + TrimSigDigits( FinalZoneSizing( CurZoneEqNum ).HeatDesHumRat, 3 ) + " kgWater/kgDryAir" );
							}
							DataErrorsFound = true;
						} else if ( SolFla == -2 ) {
							ShowSevereError( "Autosizing of heating coil UA failed for Coil:Heating:Water \"" + CompName + "\"" );
							ShowContinueError( "  Bad starting values for UA" );
							ShowContinueError( "  Lower UA estimate = " + TrimSigDigits( UA0, 6 ) + " W/m2-K (0.1% of Design Coil Load)" );
							ShowContinueError( "  Upper UA estimate = " + TrimSigDigits( UA1, 6 ) + " W/m2-K (100% of Design Coil Load)" );
							ShowContinueError( "  Zone \"" + FinalZoneSizing( CurZoneEqNum ).ZoneName + "\" coil sizing conditions (may be different than Sizing inputs):" );
							ShowContinueError( "  Coil inlet air temperature     = " + TrimSigDigits( DataDesInletAirTemp, 3 ) + " C" );
							ShowContinueError( "  Coil inlet air humidity ratio  = " + TrimSigDigits( DataDesInletAirHumRat, 3 ) + " kgWater/kgDryAir" );
							ShowContinueError( "  Coil inlet air mass flow rate  = " + TrimSigDigits( DataFlowUsedForSizing, 6 ) + " kg/s" );
							ShowContinueError( "  Design Coil Capacity           = " + TrimSigDigits( DataDesignCoilCapacity, 3 ) + " W" );
							if ( TermUnitSingDuct || TermUnitPIU || TermUnitIU || ZoneEqFanCoil ) {
								ShowContinueError( "  Design Coil Load               = " + TrimSigDigits( DataCapacityUsedForSizing, 3 ) + " W" );
							} else {
								ShowContinueError( "  Design Coil Load               = " + TrimSigDigits( DataCapacityUsedForSizing, 3 ) + " W" );
								ShowContinueError( "  Coil outlet air temperature    = " + TrimSigDigits( FinalZoneSizing( CurZoneEqNum ).HeatDesTemp, 3 ) + " C" );
								ShowContinueError( "  Coil outlet air humidity ratio = " + TrimSigDigits( FinalZoneSizing( CurZoneEqNum ).HeatDesHumRat, 3 ) + " kgWater/kgDryAir" );
							}
							// TotWaterHeatingCoilRate is set in CALL to CalcSimpleHeatingCoil
							if ( DataDesignCoilCapacity < DataCapacityUsedForSizing ) {
								ShowContinueError( "  Inadequate water side capacity: in Plant Sizing for this hot water loop" );
								ShowContinueError( "  increase design loop exit temperature and/or decrease design loop delta T" );
								ShowContinueError( "  Plant Sizing object = " + PlantSizData( DataPltSizHeatNum ).PlantLoopName );
								ShowContinueError( "  Plant design loop exit temperature = " + TrimSigDigits( PlantSizData( DataPltSizHeatNum ).ExitTemp, 3 ) + " C" );
								ShowContinueError( "  Plant design loop delta T          = " + TrimSigDigits( PlantSizData( DataPltSizHeatNum ).DeltaT, 3 ) + " C" );
							}
							DataErrorsFound = true;
						}
					} else {
						AutosizeDes = 1.0;
						if ( DataWaterFlowUsedForSizing > 0.0 ) {
							DataErrorsFound = true;
							ShowSevereError( "The design coil load is zero for Coil:Heating:Water " + CompName );
							ShowContinueError( "An autosize value for UA cannot be calculated" );
							ShowContinueError( "Input a value for UA, change the heating design day, or raise" );
							ShowContinueError( "  the zone heating design supply air temperature" );
						}
					}
				} else if (SizingType == MaxHeaterOutletTempSizing) {
					AutosizeDes = FinalZoneSizing( CurZoneEqNum ).HeatDesTemp;
				} else if( SizingType == ZoneCoolingLoadSizing ) {
					AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesCoolLoad;
				} else if( SizingType == ZoneHeatingLoadSizing ) {
					AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesHeatLoad;
				} else if( SizingType == MinSATempCoolingSizing ) {
					if ( DataCapacityUsedForSizing > 0.0 && DataFlowUsedForSizing > 0.0 ) {
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp - ( DataCapacityUsedForSizing / ( DataFlowUsedForSizing * StdRhoAir * PsyCpAirFnWTdb( FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRat, FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp ) ) );
					} else{
						ShowSevereError( CallingRoutine + ' ' + CompType + ' ' + CompName + ", Developer Error: Component sizing incomplete." );
						ShowContinueError( "SizingString = " + SizingString + ", DataCapacityUsedForSizing = " + TrimSigDigits( DataCapacityUsedForSizing, 1 ) );
						ShowContinueError( "SizingString = " + SizingString + ", DataFlowUsedForSizing = " + TrimSigDigits( DataFlowUsedForSizing, 1 ) );
					}
				} else if( SizingType == MaxSATempHeatingSizing ) {
					if ( DataCapacityUsedForSizing > 0.0 && DataFlowUsedForSizing > 0.0 ) {
						AutosizeDes = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTemp + ( DataCapacityUsedForSizing / ( DataFlowUsedForSizing * StdRhoAir * PsyCpAirFnWTdb( FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInHumRat, FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTemp ) ) );
					} else{
						ShowSevereError( CallingRoutine + ' ' + CompType + ' ' + CompName + ", Developer Error: Component sizing incomplete." );
						ShowContinueError( "SizingString = " + SizingString + ", DataCapacityUsedForSizing = " + TrimSigDigits( DataCapacityUsedForSizing, 1 ) );
						ShowContinueError( "SizingString = " + SizingString + ", DataFlowUsedForSizing = " + TrimSigDigits( DataFlowUsedForSizing, 1 ) );
					}
				} else {
					// should never happen
				}

			}
		} else if ( CurSysNum > 0 ) {
			if ( !IsAutoSize && !SizingDesRunThisAirSys ) {
				HardSizeNoDesRun = true;
				AutosizeUser = SizingResult;
				if ( PrintWarningFlag && SizingResult > 0.0 ) {
					if ( SameString( CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE" ) && SizingType == CoolingAirflowSizing && DataIsDXCoil ) {
						SizingResult /= ( 1 - DataBypassFrac ); // back out bypass fraction applied in GetInput
						ReportSizingOutput( CompType, CompName, "User-Specified " + SizingString, SizingResult );
						SizingResult *= ( 1 - DataBypassFrac ); // now reapply for second message and remianing simulation calcs
						ReportSizingOutput( CompType, CompName, "User-Specified " + SizingString + " (non-bypassed)", SizingResult );
					} else {
						ReportSizingOutput( CompType, CompName, "User-Specified " + SizingString, SizingResult );
					}
				}
			} else {
				if ( SizingType == CoolingAirflowSizing ) {
					if ( CurOASysNum > 0 ) {
						if ( OASysEqSizing( CurOASysNum ).AirFlow ) {
							// Parent object sets flow rate
							AutosizeDes = OASysEqSizing ( CurOASysNum ).AirVolFlow;
						} else if ( OASysEqSizing( CurOASysNum ).CoolingAirFlow ) {
							// Parent object sets flow rate
							AutosizeDes = OASysEqSizing ( CurOASysNum ).CoolingAirVolFlow;
						} else {
							AutosizeDes = FinalSysSizing ( CurSysNum ).DesOutAirVolFlow;
						}
					} else if ( DataAirFlowUsedForSizing > 0.0 ) {
						AutosizeDes = DataAirFlowUsedForSizing;
					} else {
						if ( UnitarySysEqSizing(CurSysNum).AirFlow ) {
							AutosizeDes = UnitarySysEqSizing(CurSysNum).AirVolFlow;
						} else if ( UnitarySysEqSizing(CurSysNum).CoolingAirFlow ) {
							AutosizeDes = UnitarySysEqSizing(CurSysNum).CoolingAirVolFlow;
						} else {
							if ( CurDuctType == Main ) {
								AutosizeDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
							} else if ( CurDuctType == Cooling ) {
								AutosizeDes = FinalSysSizing( CurSysNum ).DesCoolVolFlow;
							} else if ( CurDuctType == Heating ) {
								AutosizeDes = FinalSysSizing( CurSysNum ).DesHeatVolFlow;
							} else if ( CurDuctType == Other ) {
								AutosizeDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
							} else {
								AutosizeDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
							}
						}
					}

				} else if ( SizingType == HeatingAirflowSizing ) {
					if ( CurOASysNum > 0 ) {
						if ( OASysEqSizing( CurOASysNum ).AirFlow ) {
							// Parent object sets system flow rate
							AutosizeDes = OASysEqSizing ( CurOASysNum ).AirVolFlow;
						} else if ( OASysEqSizing( CurOASysNum ).HeatingAirFlow ) {
							// Parent object sets heating flow rate
							AutosizeDes = OASysEqSizing ( CurOASysNum ).HeatingAirVolFlow;
						} else {
							AutosizeDes = FinalSysSizing( CurSysNum ).DesOutAirVolFlow;
						}
					} else {
						if ( UnitarySysEqSizing(CurSysNum).AirFlow ) {
							AutosizeDes = UnitarySysEqSizing(CurSysNum).AirVolFlow;
						} else if ( UnitarySysEqSizing(CurSysNum).HeatingAirFlow ) {
							AutosizeDes = UnitarySysEqSizing(CurSysNum).HeatingAirVolFlow;
						} else {
							if ( CurDuctType == Main ) {
								if ( SameString ( CompType, "COIL:HEATING:WATER" ) ) {
									if ( FinalSysSizing( CurSysNum ).SysAirMinFlowRat > 0.0 && ! DataDesicRegCoil ) {
										AutosizeDes = FinalSysSizing( CurSysNum ).SysAirMinFlowRat * FinalSysSizing( CurSysNum ).DesMainVolFlow;
									} else {
										AutosizeDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
									}
								} else {
									AutosizeDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
								}
							} else if ( CurDuctType == Cooling ) {
								if ( SameString ( CompType, "COIL:HEATING:WATER" ) ) {
									if ( FinalSysSizing( CurSysNum ).SysAirMinFlowRat > 0.0 && !DataDesicRegCoil ) {
										AutosizeDes = FinalSysSizing( CurSysNum ).SysAirMinFlowRat * FinalSysSizing( CurSysNum ).DesCoolVolFlow;
									} else {
										AutosizeDes = FinalSysSizing( CurSysNum ).DesCoolVolFlow;
									}
								} else {
									AutosizeDes = FinalSysSizing( CurSysNum ).DesCoolVolFlow;
								}
							} else if ( CurDuctType == Heating ) {
								AutosizeDes = FinalSysSizing( CurSysNum ).DesHeatVolFlow;
							} else if ( CurDuctType == Other ) {
								AutosizeDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
							} else {
								AutosizeDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
							}
						}
					}
				} else if ( SizingType == SystemAirflowSizing ) {
					if ( AirLoopSysFlag ) {
						if ( UnitarySysEqSizing ( CurSysNum ).CoolingAirFlow && UnitarySysEqSizing ( CurSysNum ).HeatingAirFlow ) {
							AutosizeDes = std::max ( UnitarySysEqSizing ( CurSysNum ).CoolingAirVolFlow, UnitarySysEqSizing ( CurSysNum ).HeatingAirVolFlow );
						} else if ( UnitarySysEqSizing ( CurSysNum ).CoolingAirFlow ) {
							AutosizeDes = UnitarySysEqSizing ( CurSysNum ).CoolingAirVolFlow;
						} else if ( UnitarySysEqSizing ( CurSysNum ).HeatingAirFlow ) {
							AutosizeDes = UnitarySysEqSizing ( CurSysNum ).HeatingAirVolFlow;
						} else {
							AutosizeDes = FinalSysSizing ( CurSysNum ).DesMainVolFlow;
						}
					} else {
						AutosizeDes = FinalSysSizing ( CurSysNum ).DesMainVolFlow;
					}
				} else if ( SizingType == CoolingWaterflowSizing ) {
					if ( CurOASysNum > 0 ) {
						CoilDesWaterDeltaT = 0.5 * PlantSizData( DataPltSizCoolNum ).DeltaT;
					} else {
						CoilDesWaterDeltaT = PlantSizData( DataPltSizCoolNum ).DeltaT;
					}
					if ( DataCapacityUsedForSizing >= SmallLoad ) {
						Cp = GetSpecificHeatGlycol( PlantLoop( DataWaterLoopNum ).FluidName, 5.0, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						rho = GetDensityGlycol( PlantLoop( DataWaterLoopNum ).FluidName, InitConvTemp, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						AutosizeDes = DataCapacityUsedForSizing / ( CoilDesWaterDeltaT * Cp * rho );
					} else {
						AutosizeDes = 0.0;
						// Warning about zero design coil load is issued elsewhere.
					}
					bCheckForZero = false;
				} else if ( SizingType == HeatingWaterflowSizing ) {
					if ( DataCapacityUsedForSizing >= SmallLoad ) {
						Cp = GetSpecificHeatGlycol( PlantLoop( DataWaterLoopNum ).FluidName, 60.0, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						rho = GetDensityGlycol( PlantLoop( DataWaterLoopNum ).FluidName, InitConvTemp, PlantLoop( DataWaterLoopNum ).FluidIndex, CallingRoutine );
						AutosizeDes = DataCapacityUsedForSizing / ( PlantSizData( DataPltSizHeatNum ).DeltaT * Cp * rho );
					} else {
						AutosizeDes = 0.0;
						// Warning about zero design coil load is issued elsewhere.
					}
					bCheckForZero = false;
				} else if ( SizingType == HeatingWaterDesAirInletTempSizing ) {
					if ( CurOASysNum > 0 ) {
						OutAirFrac = 1.0;
					} else if ( FinalSysSizing( CurSysNum ).HeatOAOption == MinOA ) {
						if ( DataFlowUsedForSizing > 0.0 ) {
							OutAirFrac = FinalSysSizing( CurSysNum ).DesOutAirVolFlow / DataFlowUsedForSizing;
						} else {
							OutAirFrac = 1.0;
						}
						OutAirFrac = min( 1.0, max( 0.0, OutAirFrac ) );
					} else {
						OutAirFrac = 1.0;
					}
					// coil inlet temperature
					if ( CurOASysNum == 0 && PrimaryAirSystem( CurSysNum ).NumOAHeatCoils > 0 ) {
						AutosizeDes = OutAirFrac * FinalSysSizing( CurSysNum ).PreheatTemp + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).HeatRetTemp;
					} else {
						AutosizeDes = OutAirFrac * FinalSysSizing( CurSysNum ).HeatOutTemp + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).HeatRetTemp;
					}
					bCheckForZero = false;
				} else if ( SizingType == HeatingWaterDesAirInletHumRatSizing ) {
					if ( CurOASysNum > 0 ) {
						OutAirFrac = 1.0;
					} else if ( FinalSysSizing( CurSysNum ).HeatOAOption == MinOA ) {
						if ( DataFlowUsedForSizing > 0.0 ) {
							OutAirFrac = FinalSysSizing( CurSysNum ).DesOutAirVolFlow / DataFlowUsedForSizing;
						} else {
							OutAirFrac = 1.0;
						}
						OutAirFrac = min( 1.0, max( 0.0, OutAirFrac ) );
					} else {
						OutAirFrac = 1.0;
					}
					// coil inlet temperature
					if ( CurOASysNum == 0 && PrimaryAirSystem( CurSysNum ).NumOAHeatCoils > 0 ) {
						AutosizeDes = OutAirFrac * FinalSysSizing( CurSysNum ).PreheatHumRat + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).HeatRetHumRat;
					} else {
						AutosizeDes = OutAirFrac * FinalSysSizing( CurSysNum ).HeatOutHumRat + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).HeatRetHumRat;
					}
					bCheckForZero = false;
				} else if ( SizingType == CoolingWaterDesAirInletTempSizing ) {
					if ( CurOASysNum > 0 ) { // coil is in OA stream
						AutosizeDes = FinalSysSizing( CurSysNum ).OutTempAtCoolPeak;
					} else { // coil is in main air loop
						if ( PrimaryAirSystem( CurSysNum ).NumOACoolCoils == 0 ) { // there is no precooling of the OA stream
							AutosizeDes = FinalSysSizing( CurSysNum ).MixTempAtCoolPeak;
						} else { // thereis precooling of the OA stream
							if ( DataFlowUsedForSizing > 0.0 ) {
								OutAirFrac = FinalSysSizing( CurSysNum ).DesOutAirVolFlow / DataFlowUsedForSizing;
							} else {
								OutAirFrac = 1.0;
							}
							OutAirFrac = min( 1.0, max( 0.0, OutAirFrac ) );
							AutosizeDes = OutAirFrac * FinalSysSizing( CurSysNum ).PrecoolTemp + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).RetTempAtCoolPeak;
						}
					}
					bCheckForZero = false;
				} else if ( SizingType == CoolingWaterDesWaterInletTempSizing ) {
					AutosizeDes = PlantSizData( DataPltSizCoolNum ).ExitTemp;
					bCheckForZero = false;
				} else if ( SizingType == CoolingWaterNumofTubesPerRowSizing ) {
					AutosizeDes = int ( max ( 3.0, 13750.0 * DataWaterFlowUsedForSizing + 1.0 ) );
					bCheckForZero = false;
				} else if ( SizingType == CoolingWaterDesAirOutletTempSizing ) {
					if ( CurOASysNum > 0 ) {
						AutosizeDes = FinalSysSizing( CurSysNum ).PrecoolTemp;
					} else if ( DataDesOutletAirTemp > 0.0 ) {
						AutosizeDes = DataDesOutletAirTemp;
					} else {
						AutosizeDes = FinalSysSizing( CurSysNum ).CoolSupTemp;
					}
					if ( AutosizeDes < DataDesInletWaterTemp && DataWaterFlowUsedForSizing > 0.0 ) {
						ShowWarningError( CallingRoutine + ":" + " Coil=\"" + CompName + "\", Cooling Coil has leaving air temperature > entering water temperature." );
						ShowContinueError( "    Tair,out  =  " + RoundSigDigits( AutosizeDes, 3 ) );
						ShowContinueError( "    Twater,in = " + RoundSigDigits( DataDesInletWaterTemp, 3 ) );
						AutosizeDes = DataDesInletWaterTemp + 0.5;
						ShowContinueError( "....coil leaving air temperature will be reset to:" );
						ShowContinueError( "    Tair,out = " + RoundSigDigits( AutosizeDes, 3 ) );
					}
					bCheckForZero = false;
				} else if ( SizingType == CoolingWaterDesAirInletHumRatSizing ) {
					if ( CurOASysNum > 0 ) { // coil is in OA stream
						AutosizeDes = FinalSysSizing( CurSysNum ).OutHumRatAtCoolPeak;
					} else { // coil is in main air loop
						if ( PrimaryAirSystem( CurSysNum ).NumOACoolCoils == 0 ) { // there is no precooling of the OA stream
							AutosizeDes = FinalSysSizing( CurSysNum ).MixHumRatAtCoolPeak;
						} else { // there is precooling of the OA stream
							if ( DataFlowUsedForSizing > 0.0 ) {
								OutAirFrac = FinalSysSizing( CurSysNum ).DesOutAirVolFlow / DataFlowUsedForSizing;
							} else {
								OutAirFrac = 1.0;
							}
							OutAirFrac = min( 1.0, max( 0.0, OutAirFrac ) );
							AutosizeDes = OutAirFrac * FinalSysSizing( CurSysNum ).PrecoolHumRat + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).RetHumRatAtCoolPeak;
						}
					}
					bCheckForZero = false;
				} else if ( SizingType == CoolingWaterDesAirOutletHumRatSizing ) {
					if ( CurOASysNum > 0 ) {
						AutosizeDes = FinalSysSizing( CurSysNum ).PrecoolHumRat;
					} else if ( DataDesOutletAirHumRat > 0.0 ) {
						AutosizeDes = DataDesOutletAirHumRat;
					} else {
						AutosizeDes = FinalSysSizing( CurSysNum ).CoolSupHumRat;
					}
					if ( AutosizeDes > DataDesInletAirHumRat && DataCapacityUsedForSizing > 0.0 && SameString( CompType, "COIL:COOLING:WATER" ) ) { // flow here is water vol flow rate
						ShowWarningError( CallingRoutine + ":" + " Coil=\"" + CompName + "\", Cooling Coil has leaving humidity ratio > entering humidity ratio." );
						ShowContinueError( "    Wair,in =  " + RoundSigDigits( DataDesInletAirHumRat, 6 ) );
						ShowContinueError( "    Wair,out = " + RoundSigDigits( AutosizeDes, 6 ) );
						if ( DataDesInletAirHumRat > 0.016 ) {
							AutosizeDes = 0.5 * DataDesInletAirHumRat;
						} else {
							AutosizeDes = DataDesInletAirHumRat;
						}
						ShowContinueError( "....coil leaving humidity ratio will be reset to:" );
						ShowContinueError( "    Wair,out = " + RoundSigDigits( AutosizeDes, 6 ) );
					}

					// check for dry coil and reset outlet humrat if needed
					DesSatEnthAtWaterInTemp = PsyHFnTdbW( DataDesInletWaterTemp, PsyWFnTdpPb( DataDesInletWaterTemp, StdBaroPress ) );
					DesHumRatAtWaterInTemp = PsyWFnTdbH( DataDesInletWaterTemp, DesSatEnthAtWaterInTemp, CallingRoutine );
					if ( AutosizeDes < DataDesInletAirHumRat && DesHumRatAtWaterInTemp > DataDesInletAirHumRat && DataCapacityUsedForSizing > 0.0 ) { // flow here is water vol flow rate
						if ( DataDesInletAirHumRat > AutosizeDes && SameString( CompType, "COIL:COOLING:WATER" ) ) {
							ShowWarningError( CallingRoutine + ":" + " Coil=\"" + CompName + "\", Cooling Coil is dry and has air leaving humidity ratio < entering humidity ratio." );
							ShowContinueError( "    Wair,in =  " + RoundSigDigits( DataDesInletAirHumRat, 6 ) );
							ShowContinueError( "    Wair,out = " + RoundSigDigits( AutosizeDes, 6 ) );
							AutosizeDes = DataDesInletAirHumRat;
							ShowContinueError( "....coil leaving humidity ratio will be reset to:" );
							ShowContinueError( "    Wair,out = " + RoundSigDigits( AutosizeDes, 6 ) );
						}
					}
					bCheckForZero = false;
				} else if ( SizingType == HeatingCoilDesAirInletTempSizing ) {
					if ( DataDesicRegCoil ) {
						if ( DesicDehum( DataDesicDehumNum ).RegenInletIsOutsideAirNode ) {
							AutosizeDes = FinalSysSizing( CurSysNum ).HeatOutTemp;
						} else {
							AutosizeDes = FinalSysSizing( CurSysNum ).HeatRetTemp;
						}
					}
					bCheckForZero = false;
				} else if ( SizingType == HeatingCoilDesAirOutletTempSizing ) {
					if ( DataDesicRegCoil ) {
						AutosizeDes = DesicDehum( DataDesicDehumNum ).RegenSetPointTemp;
					}

					bCheckForZero = false;
				} else if ( SizingType == HeatingCoilDesAirInletHumRatSizing ) {
					if ( DataDesicRegCoil ) {
						if ( DesicDehum( DataDesicDehumNum ).RegenInletIsOutsideAirNode ) {
							AutosizeDes = FinalSysSizing( CurSysNum ).HeatOutHumRat;
						} else {
							AutosizeDes = FinalSysSizing( CurSysNum ).HeatRetHumRat;
						}
					}

					bCheckForZero = false;
				} else if ( SizingType == CoolingSHRSizing ) {
					if ( DataFlowUsedForSizing >= SmallAirVolFlow && DataCapacityUsedForSizing > 0.0 ) {
						// For autosizing the rated SHR, we set a minimum SHR of 0.676 and a maximum of 0.798. The min SHR occurs occurs at the
						// minimum flow / capacity ratio = MinRatedVolFlowPerRatedTotCap = 0.00004027 [m3/s / W] = 300 [cfm/ton].
						// The max SHR occurs at maximum flow / capacity ratio = MaxRatedVolFlowPerRatedTotCap = 0.00006041 [m3/s / W] = 450 [cfm/ton].
						// For flow / capacity ratios between the min and max we linearly interpolate between min and max SHR. Thus rated SHR is a
						// linear function of the rated flow / capacity ratio. This linear function (see below) is the result of a regression
						// of flow/capacity ratio vs SHR for several actual coils.
						RatedVolFlowPerRatedTotCap = DataFlowUsedForSizing / DataCapacityUsedForSizing;
						if ( DXCT == RegularDXCoil ) {
							if ( RatedVolFlowPerRatedTotCap > MaxRatedVolFlowPerRatedTotCap( DXCT ) ) {
								AutosizeDes = 0.431 + 6086.0 * MaxRatedVolFlowPerRatedTotCap ( DXCT );
							} else if ( RatedVolFlowPerRatedTotCap < MinRatedVolFlowPerRatedTotCap( DXCT ) ) {
								AutosizeDes = 0.431 + 6086.0*MinRatedVolFlowPerRatedTotCap ( DXCT );
							} else {
								AutosizeDes = 0.431 + 6086.0*RatedVolFlowPerRatedTotCap;
							}
						} else { // DOASDXCoil, or DXCT = 2
							if ( RatedVolFlowPerRatedTotCap > MaxRatedVolFlowPerRatedTotCap( DXCT ) ) {
								AutosizeDes = 0.389 + 7684.0*MaxRatedVolFlowPerRatedTotCap ( DXCT );
							} else if ( RatedVolFlowPerRatedTotCap < MinRatedVolFlowPerRatedTotCap( DXCT ) ) {
								AutosizeDes = 0.389 + 7684.0*MinRatedVolFlowPerRatedTotCap ( DXCT );
							} else {
								AutosizeDes = 0.389 + 7684.0*RatedVolFlowPerRatedTotCap;
							}
						}

						// check that the autosized SHR corresponds to a valid apperatus dew point (ADP) temperature
						DesMassFlow = DataFlowUsedForSizing * PsyRhoAirFnPbTdbW( StdBaroPress, RatedInletAirTemp, RatedInletAirHumRat, CallingRoutine );
						AutosizeDes = ValidateADP( CompType, CompName, RatedInletAirTemp, RatedInletAirHumRat, DataCapacityUsedForSizing, DesMassFlow, AutosizeDes, CallingRoutine );

					} else {
						ShowSevereError( CallingRoutine + ' ' + CompType + ' ' + CompName );
						ShowContinueError( "... DataFlowUsedForSizing and DataCapacityUsedForSizing " + SizingString + " must both be greater than 0." );
						ShowFatalError( "Preceding conditions cause termination." );
					}
				} else if ( SizingType == CoolingCapacitySizing ) {
					DataFracOfAutosizedCoolingCapacity = 1.0;
					if ( OASysFlag ) {
						AutosizeDes = OASysEqSizing ( CurOASysNum ).DesCoolingLoad;
						DesVolFlow = DataFlowUsedForSizing;
					} else if ( AirLoopSysFlag ) {
						AutosizeDes = UnitarySysEqSizing ( CurSysNum ).DesCoolingLoad;
						DesVolFlow = DataFlowUsedForSizing;
					} else {
						CheckSysSizing ( CompType, CompName );
						DesVolFlow = DataFlowUsedForSizing;
						if ( FinalSysSizing( CurSysNum ).CoolingCapMethod == FractionOfAutosizedCoolingCapacity ) {
							DataFracOfAutosizedCoolingCapacity = FinalSysSizing( CurSysNum ).FractionOfAutosizedCoolingCapacity;
						}
						if (FinalSysSizing( CurSysNum ).CoolingCapMethod == CapacityPerFloorArea) {
							NominalCapacityDes = FinalSysSizing( CurSysNum ).CoolingTotalCapacity;
							AutosizeDes = NominalCapacityDes;
						} else if (FinalSysSizing( CurSysNum ).CoolingCapMethod == CoolingDesignCapacity && FinalSysSizing( CurSysNum ).CoolingTotalCapacity > 0.0 ) {
							NominalCapacityDes = FinalSysSizing( CurSysNum ).CoolingTotalCapacity;
							AutosizeDes = NominalCapacityDes;
						} else if ( DesVolFlow >= SmallAirVolFlow ) {
							OutAirFrac = 0.0;
							if ( DesVolFlow > 0.0 ) {
								OutAirFrac = FinalSysSizing( CurSysNum ).DesOutAirVolFlow / DesVolFlow;
							} else {
								OutAirFrac = 1.0;
							}
							OutAirFrac = min( 1.0, max( 0.0, OutAirFrac ) );
							if ( CurOASysNum > 0 ) { // coil is in the OA stream
								CoilInTemp = FinalSysSizing ( CurSysNum ).OutTempAtCoolPeak;
								CoilInHumRat = FinalSysSizing ( CurSysNum ).OutHumRatAtCoolPeak;
								CoilOutTemp = FinalSysSizing ( CurSysNum ).PrecoolTemp;
								CoilOutHumRat = FinalSysSizing ( CurSysNum ).PrecoolHumRat;
							} else { // coil is on the main air loop
								if ( DataAirFlowUsedForSizing > 0.0 ) {
									DesVolFlow = DataAirFlowUsedForSizing;
								}
								if ( DataDesOutletAirTemp > 0.0 ) {
									CoilOutTemp = DataDesOutletAirTemp;
								} else {
								CoilOutTemp = FinalSysSizing ( CurSysNum ).CoolSupTemp;
								}
								CoilOutHumRat = FinalSysSizing ( CurSysNum ).CoolSupHumRat;
								if ( PrimaryAirSystem( CurSysNum ).NumOACoolCoils == 0 ) { // there is no precooling of the OA stream
									CoilInTemp = FinalSysSizing ( CurSysNum ).MixTempAtCoolPeak;
									CoilInHumRat = FinalSysSizing ( CurSysNum ).MixHumRatAtCoolPeak;
								} else { // there is precooling of OA stream
									if ( DesVolFlow > 0.0 ) {
										OutAirFrac = FinalSysSizing( CurSysNum ).DesOutAirVolFlow / DesVolFlow;
									} else {
										OutAirFrac = 1.0;
									}
									OutAirFrac = min( 1.0, max( 0.0, OutAirFrac ) );
									CoilInTemp = OutAirFrac * FinalSysSizing ( CurSysNum ).PrecoolTemp + ( 1.0 - OutAirFrac ) * FinalSysSizing ( CurSysNum ).RetTempAtCoolPeak;
									CoilInHumRat = OutAirFrac*FinalSysSizing ( CurSysNum ).PrecoolHumRat + ( 1.0 - OutAirFrac )*FinalSysSizing ( CurSysNum ).RetHumRatAtCoolPeak;
								}
							}
							OutTemp = FinalSysSizing ( CurSysNum ).OutTempAtCoolPeak;
							if ( SameString( CompType, "COIL:COOLING:WATER" ) || SameString( CompType, "COIL:COOLING:WATER:DETAILEDGEOMETRY" ) ) {
								rhoair = StdRhoAir;
							} else {
								rhoair = PsyRhoAirFnPbTdbW ( StdBaroPress, CoilInTemp, CoilInHumRat, CallingRoutine );
							}
							CoilOutTemp = min( CoilInTemp, CoilOutTemp );
							CoilOutHumRat = min( CoilInHumRat, CoilOutHumRat );
							CoilInEnth = PsyHFnTdbW ( CoilInTemp, CoilInHumRat );
							CoilInWetBulb = PsyTwbFnTdbWPb ( CoilInTemp, CoilInHumRat, StdBaroPress, CallingRoutine );
							CoilOutEnth = PsyHFnTdbW ( CoilOutTemp, CoilOutHumRat );
							if ( DataTotCapCurveIndex > 0 ) {
								TotCapTempModFac = CurveValue ( DataTotCapCurveIndex, CoilInWetBulb, OutTemp );
							} else {
								TotCapTempModFac = 1.0;
							}
							SupFanNum = PrimaryAirSystem( CurSysNum ).SupFanNum;
							RetFanNum = PrimaryAirSystem( CurSysNum ).RetFanNum;
							PsyCpAirFnWTdb( CoilOutHumRat, 0.5 * ( CoilInTemp + CoilOutTemp ) );
							FanCoolLoad = FanDesHeatGain( SupFanNum, DesVolFlow ) + ( 1.0 - OutAirFrac ) * FanDesHeatGain( RetFanNum, DesVolFlow );
							PrimaryAirSystem( CurSysNum ).FanDesCoolLoad = FanCoolLoad;
							PeakCoilLoad = max ( 0.0, ( rhoair * DesVolFlow * ( CoilInEnth - CoilOutEnth ) + FanCoolLoad ) );
							if ( TotCapTempModFac > 0.0 ) {
								NominalCapacityDes = PeakCoilLoad / TotCapTempModFac;
							} else {
								NominalCapacityDes = PeakCoilLoad;
							}
						} else {
							NominalCapacityDes = 0.0;
						}
						AutosizeDes = NominalCapacityDes * DataFracOfAutosizedCoolingCapacity; //Fixed Moved up 1 line inside block per Richard Raustad
					} // IF(OASysFlag) THEN or ELSE IF(AirLoopSysFlag) THEN
					if ( DisplayExtraWarnings && AutosizeDes <= 0.0 ) {
						ShowWarningMessage( CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName );
						ShowContinueError( "...Rated Total Cooling Capacity = " + TrimSigDigits( AutosizeDes, 2 ) + " [W]" );
						if ( OASysFlag || AirLoopSysFlag || FinalSysSizing( CurSysNum ).CoolingCapMethod == CapacityPerFloorArea || ( FinalSysSizing( CurSysNum ).CoolingCapMethod == CoolingDesignCapacity && FinalSysSizing( CurSysNum ).CoolingTotalCapacity ) ) {
							ShowContinueError( "...Capacity passed by parent object to size child component = " + TrimSigDigits( AutosizeDes, 2 ) + " [W]" );
						} else {
							ShowContinueError( "...Air flow rate used for sizing = " + TrimSigDigits( DesVolFlow, 5 ) + " [m3/s]" );
							ShowContinueError( "...Outdoor air fraction used for sizing = " + TrimSigDigits( OutAirFrac, 2 ) );
							ShowContinueError( "...Coil inlet air temperature used for sizing = " + TrimSigDigits( CoilInTemp, 2 ) + " [C]" );
							ShowContinueError( "...Coil outlet air temperature used for sizing = " + TrimSigDigits( CoilOutTemp, 2 ) + " [C]" );
						}
					}
				} else if ( SizingType == HeatingCapacitySizing ) {
					DataFracOfAutosizedHeatingCapacity = 1.0;
					if (CurOASysNum > 0) {
						if ( OASysEqSizing( CurOASysNum ).AirFlow ) {
							DesVolFlow = OASysEqSizing(CurOASysNum).AirVolFlow;
						} else if (OASysEqSizing(CurOASysNum).HeatingAirFlow) {
							DesVolFlow = OASysEqSizing(CurOASysNum).HeatingAirVolFlow;
						} else {
							DesVolFlow = FinalSysSizing( CurSysNum ).DesOutAirVolFlow;
						}
					} else {
						if (FinalSysSizing( CurSysNum ).HeatingCapMethod == FractionOfAutosizedHeatingCapacity) {
							DataFracOfAutosizedHeatingCapacity = FinalSysSizing(CurSysNum).FractionOfAutosizedHeatingCapacity;
						}
						if ( DataFlowUsedForSizing > 0.0 ) {
							DesVolFlow = DataFlowUsedForSizing;
						} else if ( UnitarySysEqSizing( CurSysNum ).AirFlow ) {
							DesVolFlow = UnitarySysEqSizing( CurSysNum ).AirVolFlow;
						} else if ( UnitarySysEqSizing( CurSysNum ).HeatingAirFlow ) {
							DesVolFlow = UnitarySysEqSizing( CurSysNum ).HeatingAirVolFlow;
						} else {
							if ( CurDuctType == Main ) {
								if ( FinalSysSizing( CurSysNum ).SysAirMinFlowRat > 0.0 && !DataDesicRegCoil ) {
									DesVolFlow = FinalSysSizing( CurSysNum ).SysAirMinFlowRat*FinalSysSizing( CurSysNum ).DesMainVolFlow;
								} else {
									DesVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
								}
							} else if ( CurDuctType == Cooling ) {
								if ( FinalSysSizing( CurSysNum ).SysAirMinFlowRat > 0.0 && !DataDesicRegCoil ) {
									DesVolFlow = FinalSysSizing( CurSysNum ).SysAirMinFlowRat*FinalSysSizing( CurSysNum ).DesCoolVolFlow;
								} else {
									DesVolFlow = FinalSysSizing( CurSysNum ).DesCoolVolFlow;
								}
							} else if ( CurDuctType == Heating ) {
								DesVolFlow = FinalSysSizing( CurSysNum ).DesHeatVolFlow;
							} else if ( CurDuctType == Other ) {
								DesVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
							} else {
								DesVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
							}
						}
					}
					DesMassFlow = StdRhoAir * DesVolFlow;
					// get the outside air fraction
					if ( CurOASysNum > 0 ) {
						OutAirFrac = 1.0;
					} else if ( FinalSysSizing( CurSysNum ).HeatOAOption == MinOA ) {
						if ( DesVolFlow > 0.0 ) {
							OutAirFrac = FinalSysSizing( CurSysNum ).DesOutAirVolFlow / DesVolFlow;
						} else {
							OutAirFrac = 1.0;
						}
						OutAirFrac = std::min( 1.0, std::max( 0.0, OutAirFrac ) );
					} else {
						OutAirFrac = 1.0;
					}
					// coil inlet temperature
					if ( CurOASysNum == 0 && PrimaryAirSystem( CurSysNum ).NumOAHeatCoils > 0 ) {
						CoilInTemp = OutAirFrac * FinalSysSizing( CurSysNum ).PreheatTemp + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).HeatRetTemp;
					} else {
						CoilInTemp = OutAirFrac * FinalSysSizing( CurSysNum ).HeatOutTemp + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).HeatRetTemp;
					}
					// coil load
					if ( CurOASysNum > 0 ) {
						if ( OASysEqSizing( CurOASysNum ).HeatingCapacity ) {
							DesCoilLoad = OASysEqSizing( CurOASysNum ).DesHeatingLoad;
							CoilOutTemp = -999.0;
						} else if ( DataDesicRegCoil ) {
							DesCoilLoad = CpAirStd * DesMassFlow * ( DataDesOutletAirTemp - DataDesInletAirTemp );
							CoilOutTemp = DataDesOutletAirTemp;
						} else {
							DesCoilLoad = CpAirStd * DesMassFlow * ( FinalSysSizing( CurSysNum ).PreheatTemp - CoilInTemp );
							CoilOutTemp = FinalSysSizing( CurSysNum ).PreheatTemp;
						}
					} else {
						if ( UnitarySysEqSizing( CurSysNum ).HeatingCapacity ) {
							DesCoilLoad = UnitarySysEqSizing( CurSysNum ).DesHeatingLoad;
							CoilOutTemp = -999.0;
						} else if ( DataDesicRegCoil ) {
							DesCoilLoad = CpAirStd * DesMassFlow * ( DataDesOutletAirTemp - DataDesInletAirTemp );
							CoilOutTemp = DataDesOutletAirTemp;
						} else {
							DesCoilLoad = CpAirStd * DesMassFlow * ( FinalSysSizing( CurSysNum ).HeatSupTemp - CoilInTemp );
							CoilOutTemp = FinalSysSizing( CurSysNum ).HeatSupTemp;
						}
					}
					if ( AirLoopControlInfo( CurSysNum ).UnitarySys ) {
						if ( DataCoilIsSuppHeater ) {
							NominalCapacityDes = SuppHeatCap;
						} else if ( DataCoolCoilCap > 0.0 ) {
							NominalCapacityDes = DataCoolCoilCap;
						} else {
							// TRUE for all air loop parent equipment except UnitarySystem where flag is reset to FALSE after simulating
							// This method allows downstream heating coils to size individually.Probably should do this for all air loop equipment
							// ChangoverBypass model always sets AirLoopControlInfo%UnitarySys to FALSE so heating coil can individually size
							if ( AirLoopControlInfo( CurSysNum ).UnitarySysSimulating && ! SameString( CompType, "COIL:HEATING:WATER" ) ) {
								NominalCapacityDes = UnitaryHeatCap;
							} else {
								if ( DesCoilLoad >= SmallLoad ) {
									NominalCapacityDes = DesCoilLoad;
								} else {
									NominalCapacityDes = 0.0;
								}
							}
						}
						DesCoilLoad = NominalCapacityDes;
						CoilOutTemp = -999.0;
					} else if ( FinalSysSizing( CurSysNum ).HeatingCapMethod == CapacityPerFloorArea ) {
						NominalCapacityDes = FinalSysSizing( CurSysNum ).HeatingTotalCapacity;
						CoilOutTemp = -999.0;
					} else if ( FinalSysSizing( CurSysNum ).HeatingCapMethod == HeatingDesignCapacity && FinalSysSizing( CurSysNum ).HeatingTotalCapacity > 0.0 ) {
						NominalCapacityDes = FinalSysSizing( CurSysNum ).HeatingTotalCapacity;
						CoilOutTemp = -999.0;
					} else {
						if ( DataCoolCoilCap > 0.0 ) {
							NominalCapacityDes = DataCoolCoilCap;
						} else if ( DesCoilLoad >= SmallLoad ) {
							NominalCapacityDes = DesCoilLoad;
						} else {
							NominalCapacityDes = 0.0;
						}
						CoilOutTemp = -999.0;
					}
					AutosizeDes = NominalCapacityDes * DataHeatSizeRatio * DataFracOfAutosizedHeatingCapacity;
					if ( DisplayExtraWarnings && AutosizeDes <= 0.0 ) {
						ShowWarningMessage( CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName );
						ShowContinueError( "...Rated Total Heating Capacity = " + TrimSigDigits( AutosizeDes, 2 ) + " [W]" );
						if ( CoilOutTemp > -999.0 ) {
							ShowContinueError( "...Air flow rate used for sizing = " + TrimSigDigits( DesVolFlow, 5 ) + " [m3/s]" );
							ShowContinueError( "...Outdoor air fraction used for sizing = " + TrimSigDigits( OutAirFrac, 2 ) );
							ShowContinueError( "...Coil inlet air temperature used for sizing = " + TrimSigDigits( CoilInTemp, 2 ) + " [C]" );
							ShowContinueError( "...Coil outlet air temperature used for sizing = " + TrimSigDigits( CoilOutTemp, 2 ) + " [C]");
						} else {
							ShowContinueError( "...Capacity passed by parent object to size child component = " + TrimSigDigits( DesCoilLoad, 2 ) + " [W]" );
						}
					}
				} else if ( SizingType == HeatingWaterDesCoilLoadUsedForUASizing ) {
					if ( CurOASysNum > 0 ) {
						OutAirFrac = 1.0;
					} else if ( FinalSysSizing( CurSysNum ).HeatOAOption == MinOA ) {
						if ( DataAirFlowUsedForSizing > 0.0 ) {
							OutAirFrac = FinalSysSizing( CurSysNum ).DesOutAirVolFlow / DataAirFlowUsedForSizing;
						} else {
							OutAirFrac = 1.0;
						}
						OutAirFrac = min( 1.0, max( 0.0, OutAirFrac ) );
					} else {
						OutAirFrac = 1.0;
					}
					if ( CurOASysNum == 0 && PrimaryAirSystem( CurSysNum ).NumOAHeatCoils > 0 ) {
						CoilInTemp = OutAirFrac * FinalSysSizing( CurSysNum ).PreheatTemp + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).HeatRetTemp;
					} else {
						CoilInTemp = OutAirFrac * FinalSysSizing( CurSysNum ).HeatOutTemp + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).HeatRetTemp;
					}
					// coil load
					CpAirStd = PsyCpAirFnWTdb( 0.0, 20.0 );
					if ( CurOASysNum > 0 ) {
						if ( DataDesicRegCoil ) {
							AutosizeDes = CpAirStd * StdRhoAir * DataAirFlowUsedForSizing * ( DataDesOutletAirTemp - DataDesInletAirTemp );
						} else {
							AutosizeDes = CpAirStd * StdRhoAir * DataAirFlowUsedForSizing * ( FinalSysSizing( CurSysNum ).PreheatTemp - CoilInTemp );
						}
					} else {
						if ( DataDesicRegCoil ) {
							AutosizeDes = CpAirStd * StdRhoAir * DataAirFlowUsedForSizing * ( DataDesOutletAirTemp - DataDesInletAirTemp );
						} else {
							AutosizeDes = CpAirStd * StdRhoAir * DataAirFlowUsedForSizing * ( FinalSysSizing( CurSysNum ).HeatSupTemp - CoilInTemp );
						}
					}
				} else if ( SizingType == HeatingWaterDesCoilWaterVolFlowUsedForUASizing ) {
					AutosizeDes = DataWaterFlowUsedForSizing;
					bCheckForZero = false;
				} else if ( SizingType == HeatingAirflowUASizing ) {
					if ( CurOASysNum > 0 ) {
						AutosizeDes = FinalSysSizing( CurSysNum ).DesOutAirVolFlow;
					} else {
						if ( CurDuctType == Main ) {
							if (FinalSysSizing( CurSysNum ).SysAirMinFlowRat > 0.0) {
								AutosizeDes = FinalSysSizing( CurSysNum ).SysAirMinFlowRat * FinalSysSizing( CurSysNum ).DesMainVolFlow;
							} else {
								AutosizeDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
							}
						} else if ( CurDuctType == Cooling ) {
							if ( FinalSysSizing( CurSysNum ).SysAirMinFlowRat > 0.0 ) {
								AutosizeDes = FinalSysSizing( CurSysNum ).SysAirMinFlowRat * FinalSysSizing( CurSysNum ).DesCoolVolFlow;
							} else {
								AutosizeDes = FinalSysSizing( CurSysNum ).DesCoolVolFlow;
							}
						} else if ( CurDuctType == Heating ) {
							AutosizeDes = FinalSysSizing( CurSysNum ).DesHeatVolFlow;
						} else if ( CurDuctType == Other ) {
							AutosizeDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
						} else {
							AutosizeDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
						}
					}
					AutosizeDes *= StdRhoAir;
				} else if (SizingType == WaterHeatingCoilUASizing) {
					if ( DataCapacityUsedForSizing >= SmallLoad ) {
						Par( 1 ) = DataCapacityUsedForSizing;
						Par( 2 ) = double( DataCoilNum );
						Par( 3 ) = double( DataFanOpMode ); //fan operating mode
						Par( 4 ) = 1.0; // part-load ratio
						UA0 = 0.001 * DataCapacityUsedForSizing;
						UA1 = DataCapacityUsedForSizing;
						// Invert the simple heating coil model: given the design inlet conditions and the design load,
						// find the design UA.
						SolveRegulaFalsi( Acc, MaxIte, SolFla, AutosizeDes, SimpleHeatingCoilUAResidual, UA0, UA1, Par );
						if ( SolFla == -1 ) {
							ShowSevereError( "Autosizing of heating coil UA failed for Coil:Heating:Water \"" + CompName + "\"" );
							ShowContinueError( "  Iteration limit exceeded in calculating coil UA" );
							ShowContinueError( "  Lower UA estimate = " + TrimSigDigits( UA0, 6 ) + " W/m2-K (1% of Design Coil Load)" );
							ShowContinueError( "  Upper UA estimate = " + TrimSigDigits( UA1, 6 ) + " W/m2-K (100% of Design Coil Load)" );
							ShowContinueError( "  Final UA estimate when iterations exceeded limit = " + TrimSigDigits( AutosizeDes, 6 ) + " W/m2-K" );
							ShowContinueError( "  AirloopHVAC \"" + FinalSysSizing( CurSysNum ).AirPriLoopName + "\" coil sizing conditions (may be different than Sizing inputs):" );
							ShowContinueError( "  Coil inlet air temperature     = " + TrimSigDigits( DataDesInletAirTemp, 3 ) + " C" );
							ShowContinueError( "  Coil inlet air humidity ratio  = " + TrimSigDigits( DataDesInletAirHumRat, 3 ) + " kgWater/kgDryAir" );
							ShowContinueError( "  Coil inlet air mass flow rate  = " + TrimSigDigits( DataFlowUsedForSizing, 6 ) + " kg/s" );
							ShowContinueError( "  Design Coil Capacity           = " + TrimSigDigits( DataDesignCoilCapacity, 3 ) + " W" );
							ShowContinueError( "  Design Coil Load               = " + TrimSigDigits( DataCapacityUsedForSizing, 3 ) + " W" );
							DataErrorsFound = true;
						} else if ( SolFla == -2 ) {
							ShowSevereError( "Autosizing of heating coil UA failed for Coil:Heating:Water \"" + CompName + "\"" );
							ShowContinueError( "  Bad starting values for UA" );
							ShowContinueError( "  Lower UA estimate = " + TrimSigDigits( UA0, 6 ) + " W/m2-K (1% of Design Coil Load)" );
							ShowContinueError( "  Upper UA estimate = " + TrimSigDigits( UA1, 6 ) + " W/m2-K (100% of Design Coil Load)" );
							ShowContinueError( "  AirloopHVAC \"" + FinalSysSizing( CurSysNum ).AirPriLoopName + "\" coil sizing conditions (may be different than Sizing inputs):" );
							ShowContinueError( "  Coil inlet air temperature     = " + TrimSigDigits( DataDesInletAirTemp, 3 ) + " C" );
							ShowContinueError( "  Coil inlet air humidity ratio  = " + TrimSigDigits( DataDesInletAirHumRat, 3 ) + " kgWater/kgDryAir" );
							ShowContinueError( "  Coil inlet air mass flow rate  = " + TrimSigDigits( DataFlowUsedForSizing, 6 ) + " kg/s" );
							ShowContinueError( "  Design Coil Capacity           = " + TrimSigDigits( DataDesignCoilCapacity, 3 ) + " W" );
							ShowContinueError( "  Design Coil Load               = " + TrimSigDigits( DataCapacityUsedForSizing, 3 ) + " W" );
							if ( DataDesignCoilCapacity < DataCapacityUsedForSizing ) {
								ShowContinueError( "  Inadequate water side capacity: in Plant Sizing for this hot water loop" );
								ShowContinueError( "  increase design loop exit temperature and/or decrease design loop delta T" );
								ShowContinueError( "  Plant Sizing object = " + PlantSizData( DataPltSizHeatNum ).PlantLoopName );
								ShowContinueError( "  Plant design loop exit temperature = " + TrimSigDigits( PlantSizData( DataPltSizHeatNum ).ExitTemp, 3 ) + " C" );
								ShowContinueError( "  Plant design loop delta T          = " + TrimSigDigits( PlantSizData( DataPltSizHeatNum ).DeltaT, 3 ) + " C" );
							}
							DataErrorsFound = true;
						}
					} else {
						AutosizeDes = 1.0;
						if ( DataWaterFlowUsedForSizing > 0.0 ) {
							DataErrorsFound = true;
							ShowSevereError( "The design coil load is zero for Coil:Heating:Water " + CompName );
							ShowContinueError( "An autosize value for UA cannot be calculated" );
							ShowContinueError( "Input a value for UA, change the heating design day, or raise" );
							ShowContinueError( "  the system heating design supply air temperature" );
						}
					}
				} else if (SizingType == MaxHeaterOutletTempSizing) {
					AutosizeDes = FinalSysSizing( CurSysNum ).HeatSupTemp;
				}
			}
		} else {
			// some components don't set CurZoneEqNum or CurSysNum (e.g., Plant HPWH fans)
			HardSizeNoDesRun = true;
			AutosizeDes = DataNonZoneNonAirloopValue;
			if ( DataNonZoneNonAirloopValue > 0.0 && IsAutoSize ) {
				SizingResult = DataNonZoneNonAirloopValue;
			}
			if ( PrintWarningFlag ) {
				if ( IsAutoSize && SizingResult > 0.0 ) {
					ReportSizingOutput( CompType, CompName, "Design Size " + SizingString, SizingResult );
				} else if ( SizingResult > 0.0 ) {
					AutosizeUser = SizingResult;
					if ( ( std::abs( AutosizeDes - AutosizeUser ) / AutosizeUser ) > AutoVsHardSizingThreshold ) {
						ReportSizingOutput( CompType, CompName, "Design Size " + SizingString, AutosizeDes, "User-Specified " + SizingString, AutosizeUser );
					} else {
						ReportSizingOutput( CompType, CompName, "User-Specified " + SizingString, AutosizeUser );
					}
					if ( DisplayExtraWarnings ) {
						if ( ( std::abs( AutosizeDes - AutosizeUser ) / AutosizeUser ) > AutoVsHardSizingThreshold ) {
							ShowMessage( CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName );
							ShowContinueError( "User-Specified " + SizingString + " = " + RoundSigDigits( AutosizeUser, 5 ) );
							ShowContinueError( "differs from Design Size " + SizingString + " = " + RoundSigDigits( AutosizeDes, 5 ) );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				} else {
					ShowSevereError( CallingRoutine + ' ' + CompType + ' ' + CompName + ", Developer Error: Component sizing incomplete." );
					ShowContinueError( "SizingString = " + SizingString + ", SizingResult = " + TrimSigDigits( SizingResult, 1 ) );
					// ShowFatalError( " Previous errors cause program termination" );
				}
			}
		}


		if ( IsAutoSize || SizingDesRunThisAirSys || SizingDesRunThisZone ) {
			if ( AutosizeDes < SmallAirVolFlow && bCheckForZero) {
				AutosizeDes = 0.0;
			}
		}

		if ( SizingResult == AutoSize ) {
			if ( DataEMSOverrideON ) {
				SizingResult = DataEMSOverride;
			} else {
				SizingResult = AutosizeDes;
			}
		} else if ( DataScalableSizingON || DataScalableCapSizingON ) {
			if ( DataEMSOverrideON ) {
				SizingResult = DataEMSOverride;
			} else {
				AutosizeUser = SizingResult;
			}
		} else {
			AutosizeUser = SizingResult;
		}
		if ( DataScalableSizingON ) {
			if ( SizingType == CoolingAirflowSizing || SizingType == HeatingAirflowSizing || SizingType == SystemAirflowSizing ) {
				{ auto const SELECT_CASE_var( ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingType ) );
				if ( SELECT_CASE_var == SupplyAirFlowRate || SELECT_CASE_var == None ) {
					ScalableSM = "User-Specified (scaled by flow / zone) ";
				} else if ( SELECT_CASE_var == FlowPerFloorArea ) {
					ScalableSM = "User-Specified (scaled by flow / area) ";
				} else if ( SELECT_CASE_var == FractionOfAutosizedCoolingAirflow || SELECT_CASE_var == FractionOfAutosizedHeatingAirflow ) {
					ScalableSM = "User-Specified (scaled by fractional multiplier) ";
				} else if ( SELECT_CASE_var == FlowPerCoolingCapacity || SELECT_CASE_var == FlowPerHeatingCapacity ) {
					ScalableSM = "User-Specified (scaled by flow / capacity) ";
				} else {
					ScalableSM = "Design Size ";
				}}
			}
		}

		if ( DataScalableCapSizingON ) {
			{ auto const SELECT_CASE_var( ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingType ) );
			if ( SELECT_CASE_var == HeatingDesignCapacity || SELECT_CASE_var == CoolingDesignCapacity ) {
				ScalableSM = "User-Specified ";
				if ( SizingResult == AutoSize ) ScalableSM = "Design Size ";
			} else if ( SELECT_CASE_var == CapacityPerFloorArea ) {
				ScalableSM = "User-Specified (scaled by capacity / area) ";
			} else if ( SELECT_CASE_var == FractionOfAutosizedHeatingCapacity || SELECT_CASE_var == FractionOfAutosizedCoolingCapacity ) {
				ScalableSM = "User-Specified (scaled by fractional multiplier) ";
			} else {
				ScalableSM = "Design Size ";
			}
			}
		}
//		if ( PrintWarningFlag ) { // these special if tests inside here are only executed when PrintWarningFlag is true, but we might want to get the size without printing
			if ( !HardSizeNoDesRun || DataScalableSizingON || DataScalableCapSizingON ) {
				if ( IsAutoSize ) { // Design Size values are available for both autosized and hard - sized
					// check capacity to make sure design volume flow per total capacity is within range
					if ( DataIsDXCoil && ( SizingType == CoolingCapacitySizing || SizingType == HeatingCapacitySizing ) ) {
						if ( SizingResult > 0.0 ) {
							RatedVolFlowPerRatedTotCap = DesVolFlow / SizingResult;
						} else {
							RatedVolFlowPerRatedTotCap = 0.0;
						}
						if ( RatedVolFlowPerRatedTotCap < MinRatedVolFlowPerRatedTotCap( DXCT ) ) {
							if ( !DataEMSOverride && DisplayExtraWarnings && PrintWarningFlag ) {
								ShowWarningError( CallingRoutine + ' ' + CompType + ' ' + CompName );
								ShowContinueError( "..." + SizingString + " will be limited by the minimum rated volume flow per rated total capacity ratio." );
								ShowContinueError( "...DX coil volume flow rate (m3/s ) = " + TrimSigDigits( DesVolFlow, 6 ) );
								ShowContinueError( "...Requested capacity (W ) = " + TrimSigDigits( SizingResult, 3 ) );
								ShowContinueError( "...Requested flow/capacity ratio (m3/s/W ) = " + TrimSigDigits( RatedVolFlowPerRatedTotCap, 3 ) );
								ShowContinueError( "...Minimum flow/capacity ratio (m3/s/W ) = " + TrimSigDigits( MinRatedVolFlowPerRatedTotCap( DXCT ), 3 ) );
							}
							SizingResult = DesVolFlow / MinRatedVolFlowPerRatedTotCap( DXCT );
							if ( !DataEMSOverride && DisplayExtraWarnings && PrintWarningFlag ) {
								ShowContinueError( "...Adjusted capacity ( W ) = " + TrimSigDigits( SizingResult, 3 ) );
							}
						} else if ( RatedVolFlowPerRatedTotCap > MaxRatedVolFlowPerRatedTotCap( DXCT ) ) {
							if ( !DataEMSOverride && DisplayExtraWarnings && PrintWarningFlag ) {
								ShowWarningError( CallingRoutine + ' ' + CompType + ' ' + CompName );
								ShowContinueError( "..." + SizingString + " will be limited by the maximum rated volume flow per rated total capacity ratio." );
								ShowContinueError( "...DX coil volume flow rate ( m3/s ) = " + TrimSigDigits( DesVolFlow, 6 ) );
								ShowContinueError( "...Requested capacity ( W ) = " + TrimSigDigits( SizingResult, 3 ) );
								ShowContinueError( "...Requested flow/capacity ratio ( m3/s/W ) = " + TrimSigDigits( RatedVolFlowPerRatedTotCap, 3 ) );
								ShowContinueError( "...Maximum flow/capacity ratio ( m3/s/W ) = " + TrimSigDigits( MaxRatedVolFlowPerRatedTotCap( DXCT ), 3 ) );
							}
							SizingResult = DesVolFlow / MaxRatedVolFlowPerRatedTotCap( DXCT );
							if ( !DataEMSOverride && DisplayExtraWarnings && PrintWarningFlag ) {
								ShowContinueError( "...Adjusted capacity ( W ) = " + TrimSigDigits( SizingResult, 3 ) );
							}
							AutosizeDes = SizingResult;
						}
					}
					if ( DataAutosizable && AutosizeUser > 0.0 && AutosizeDes > 0.0 && PrintWarningFlag ) {
						if ( SameString( CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE" ) && SizingType == CoolingAirflowSizing  && DataIsDXCoil ) {
							ReportSizingOutput( CompType, CompName, "Design Size " + SizingString, AutosizeDes, "User-Specified " + SizingString, AutosizeUser );
							SizingResult *= ( 1 - DataBypassFrac ); // now apply bypass fraction for second message and remaining simulation calcs
							AutosizeUser *= ( 1 - DataBypassFrac ); // now apply bypass fraction for second message and remaining simulation calcs
							ReportSizingOutput( CompType, CompName, "Design Size " + SizingString + " ( non-bypassed )", AutosizeDes, "User-Specified " + SizingString + " ( non-bypassed )", AutosizeUser );
						} else {
							ReportSizingOutput( CompType, CompName, "Design Size " + SizingString, AutosizeDes, "User-Specified " + SizingString, AutosizeUser );
						}
						if ( DisplayExtraWarnings && PrintWarningFlag ) {
							if ( ( std::abs( AutosizeDes - AutosizeUser ) / AutosizeUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName );
								ShowContinueError( "User-Specified " + SizingString + " = " + RoundSigDigits( AutosizeUser, 5 ) );
								ShowContinueError( "differs from Design Size " + SizingString + " = " + RoundSigDigits( AutosizeDes, 5 ) );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					} else if ( ( DataScalableSizingON || DataScalableCapSizingON ) && AutosizeDes > 0.0 && PrintWarningFlag ) {
						if (SameString(CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE") && SizingType == CoolingAirflowSizing  && DataIsDXCoil) {
							if ( DataAutosizable ) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString, SizingResult);
							SizingResult *= (1 - DataBypassFrac); // now apply bypass fraction for second message and remaining simulation calcs
							if ( DataAutosizable ) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString + " ( non-bypassed )", SizingResult);
						} else {
							if ( DataAutosizable ) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString, SizingResult);
						}
					} else if (PrintWarningFlag ) {
						if ( SameString( CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE" ) && SizingType == CoolingAirflowSizing  && DataIsDXCoil ) {
							if ( DataAutosizable ) ReportSizingOutput( CompType, CompName, "Design Size " + SizingString, SizingResult );
							SizingResult *= ( 1 - DataBypassFrac ); // now apply bypass fraction for second message and remaining simulation calcs
							if ( DataAutosizable ) ReportSizingOutput( CompType, CompName, "Design Size " + SizingString + " ( non-bypassed )", SizingResult );
						} else {
							if ( DataAutosizable && PrintWarningFlag ) ReportSizingOutput( CompType, CompName, "Design Size " + SizingString, SizingResult );
						}
					}
				} else {
					if ( DataAutosizable && AutosizeUser > 0.0 && AutosizeDes > 0.0 && PrintWarningFlag && !( DataScalableSizingON || DataScalableCapSizingON ) ) {
						ReportSizingOutput( CompType, CompName, "Design Size " + SizingString, AutosizeDes, "User-Specified " + SizingString, AutosizeUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( AutosizeDes - AutosizeUser ) / AutosizeUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName );
								ShowContinueError( "User-Specified " + SizingString + " = " + RoundSigDigits( AutosizeUser, 5 ) );
								ShowContinueError( "differs from Design Size " + SizingString + " = " + RoundSigDigits( AutosizeDes, 5 ) );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					} else if ( DataAutosizable && AutosizeUser > 0.0 && AutosizeDes > 0.0 && PrintWarningFlag && ( DataScalableSizingON || DataScalableCapSizingON ) ) {
						ReportSizingOutput( CompType, CompName, "Design Size " + SizingString, AutosizeDes, ScalableSM + SizingString, AutosizeUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( AutosizeDes - AutosizeUser ) / AutosizeUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName );
								ShowContinueError( ScalableSM + SizingString + " = " + RoundSigDigits( AutosizeUser, 5 ) );
								ShowContinueError( "differs from Design Size " + SizingString + " = " + RoundSigDigits( AutosizeDes, 5 ) );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					} else if ( ( DataScalableSizingON || DataScalableCapSizingON ) && AutosizeDes > 0.0 && PrintWarningFlag ) {
						if (SameString(CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE") && SizingType == CoolingAirflowSizing  && DataIsDXCoil) {
							if (DataAutosizable) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString, SizingResult);
							SizingResult *= (1 - DataBypassFrac); // now apply bypass fraction for second message and remaining simulation calcs
							if (DataAutosizable) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString + " ( non-bypassed )", SizingResult);
						} else {
							if (DataAutosizable) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString, SizingResult);
						}
					} else if ( ( DataScalableSizingON || DataScalableCapSizingON ) && AutosizeUser > 0.0 && PrintWarningFlag ) {
						if (SameString(CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE") && SizingType == CoolingAirflowSizing  && DataIsDXCoil) {
							if (DataAutosizable) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString, SizingResult);
							SizingResult *= (1 - DataBypassFrac); // now apply bypass fraction for second message and remaining simulation calcs
							if (DataAutosizable) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString + " ( non-bypassed )", SizingResult);
						} else {
							if (DataAutosizable) ReportSizingOutput(CompType, CompName, ScalableSM + SizingString, SizingResult);
						}
					} else if ( PrintWarningFlag ) {
						if ( SameString( CompType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE" ) && SizingType == CoolingAirflowSizing  && DataIsDXCoil ) {
							if ( DataAutosizable ) ReportSizingOutput( CompType, CompName, "User-Specified " + SizingString, SizingResult );
							SizingResult *= ( 1 - DataBypassFrac ); // now apply bypass fraction for second message and remaining simulation calcs
							if ( DataAutosizable ) ReportSizingOutput( CompType, CompName, "User-Specified " + SizingString + " (non-bypassed)", SizingResult );
						} else {
							if ( DataAutosizable && PrintWarningFlag ) ReportSizingOutput( CompType, CompName, "User-Specified " + SizingString, SizingResult );
						}
					}
				}
//			} else {
//				eventually move hardsize reporting here? [up in calcs, 3 places at e.g., if ( !IsAutoSize && !SizingDesRunThisAirSys )]
			}
//		}
	}

	void
	GetCoilDesFlowT(
		int SysNum, // central air system index
		Real64 CpAir, // specific heat to be used in calculations [J/kgC]
		Real64 & DesFlow, // returned design mass flow [kg/s]
		Real64 & DesExitTemp // returned design coil exit temperature [kg/s]
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
		int DDAtSensPeak;
		int TimeStepAtSensPeak;
		int DDAtFlowPeak;
		int TimeStepAtFlowPeak;
		int CoolCapCtrl; // type of coil capacity control
		int PeakLoadType;
		int DDAtTotPeak;
		int TimeStepAtTotPeak;
		int TimeStepAtPeak;
		Real64 ZoneCoolLoadSum( 0 ); // sum of zone cooling loads at the peak [W]
		Real64 AvgZoneTemp( 0 ); // average zone temperature [C]
		Real64 AvgSupTemp; // average supply temperature for bypass control [C]
		Real64 TotFlow; // total flow for bypass control [m3/s]
		Real64 MixTemp; // mixed air temperature at the peak [C]

		CoolCapCtrl = SysSizInput( SysNum ).CoolCapControl;
		PeakLoadType = SysSizInput( SysNum ).CoolingPeakLoadType;
		DDAtSensPeak = SysSizPeakDDNum( SysNum ).SensCoolPeakDD;
		TimeStepAtSensPeak = SysSizPeakDDNum( SysNum ).TimeStepAtSensCoolPk( DDAtSensPeak );
		DDAtFlowPeak = SysSizPeakDDNum( SysNum ).CoolFlowPeakDD;
		TimeStepAtFlowPeak = SysSizPeakDDNum( SysNum ).TimeStepAtCoolFlowPk( DDAtFlowPeak );
		DDAtTotPeak = SysSizPeakDDNum( SysNum ).TotCoolPeakDD;
		TimeStepAtTotPeak = SysSizPeakDDNum( SysNum ).TimeStepAtTotCoolPk( DDAtTotPeak );

		if ( PeakLoadType == TotalCoolingLoad ) {
			TimeStepAtPeak = TimeStepAtTotPeak;
		} else {
			TimeStepAtPeak = TimeStepAtSensPeak;
		}
		if ( CoolCapCtrl == VAV ) {
			DesExitTemp = FinalSysSizing( SysNum ).CoolSupTemp;
			DesFlow = FinalSysSizing( SysNum ).MassFlowAtCoolPeak / StdRhoAir;
		} else if ( CoolCapCtrl == OnOff ) {
			DesExitTemp = FinalSysSizing( SysNum ).CoolSupTemp;
			DesFlow = DataAirFlowUsedForSizing;
		} else if ( CoolCapCtrl == VT ) {
			if ( FinalSysSizing( SysNum ).CoolingPeakLoadType == SensibleCoolingLoad ) {
				ZoneCoolLoadSum = CalcSysSizing( SysNum ).SumZoneCoolLoadSeq( TimeStepAtPeak );
				AvgZoneTemp = CalcSysSizing( SysNum ).CoolZoneAvgTempSeq( TimeStepAtPeak );
			} else if ( FinalSysSizing( SysNum ).CoolingPeakLoadType == TotalCoolingLoad ) {
				ZoneCoolLoadSum = CalcSysSizing( SysNum ).SumZoneCoolLoadSeq( TimeStepAtPeak );
				AvgZoneTemp = CalcSysSizing( SysNum ).CoolZoneAvgTempSeq( TimeStepAtPeak );
			}
			DesExitTemp = max( FinalSysSizing( SysNum ).CoolSupTemp, AvgZoneTemp - ZoneCoolLoadSum / ( StdRhoAir * CpAir * FinalSysSizing( SysNum ).DesCoolVolFlow ) );
			DesFlow = FinalSysSizing( SysNum ).DesCoolVolFlow;
		} else if ( CoolCapCtrl == Bypass ) {
			if ( FinalSysSizing( SysNum ).CoolingPeakLoadType == SensibleCoolingLoad ) {
				ZoneCoolLoadSum = CalcSysSizing( SysNum ).SumZoneCoolLoadSeq( TimeStepAtPeak );
				AvgZoneTemp = CalcSysSizing( SysNum ).CoolZoneAvgTempSeq( TimeStepAtPeak );
			} else if ( FinalSysSizing( SysNum ).CoolingPeakLoadType == TotalCoolingLoad ) {
				ZoneCoolLoadSum = CalcSysSizing( SysNum ).SumZoneCoolLoadSeq( TimeStepAtPeak );
				AvgZoneTemp = CalcSysSizing( SysNum ).CoolZoneAvgTempSeq( TimeStepAtPeak );
			}
			AvgSupTemp = AvgZoneTemp - ZoneCoolLoadSum / ( StdRhoAir * CpAir * FinalSysSizing( SysNum ).DesCoolVolFlow );
			TotFlow = FinalSysSizing( SysNum ).DesCoolVolFlow;
			MixTemp = CalcSysSizing( SysNum ).MixTempAtCoolPeak;
			DesExitTemp = FinalSysSizing( SysNum ).CoolSupTemp;
			if ( MixTemp > DesExitTemp ) {
				DesFlow = TotFlow * max( 0.0, min( 1.0, ( ( MixTemp - AvgSupTemp ) / ( MixTemp - DesExitTemp ) ) ) );
			} else {
				DesFlow = TotFlow;
			}
		}
	}

} // ReportSizingManager

} // EnergyPlus
