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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/numeric.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EconomicTariff.hh>
#include <DataCostEstimate.hh>
#include <DataEnvironment.hh>
#include <DataGlobalConstants.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DisplayRoutines.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <OutputReportTabular.hh>
#include <ScheduleManager.hh>
#include <SQLiteProcedures.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace EconomicTariff {

	// MODULE INFORMATION:
	//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
	//    DATE WRITTEN   May 2004
	//    MODIFIED       na
	//    RE-ENGINEERED  na
	// PURPOSE OF THIS MODULE:
	//    Compute utility bills for a building based on energy
	//    use estimate.
	// METHODOLOGY EMPLOYED:
	// REFERENCES:
	//    None.
	// OTHER NOTES:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace InputProcessor;
	using ScheduleManager::GetScheduleIndex;

	// Data
	//ECONOMCIS:TARIFF enumerated lists

	int const kindUnknown( 0 );
	int const kindTariff( 1 );
	int const kindQualify( 2 );
	int const kindChargeSimple( 3 );
	int const kindChargeBlock( 4 );
	int const kindRatchet( 5 );
	int const kindVariable( 6 );
	int const kindComputation( 7 );
	int const kindCategory( 8 );
	int const kindNative( 9 );
	int const kindAssignCompute( 10 );

	int const conversionUSERDEF( 0 );
	int const conversionKWH( 1 );
	int const conversionTHERM( 2 );
	int const conversionMMBTU( 3 ); // million btu
	int const conversionMJ( 4 );
	int const conversionKBTU( 5 );
	int const conversionMCF( 6 ); // thousand cubic feet
	int const conversionCCF( 7 ); // hundred cubic feet

	Array1D_string const convEneStrings( {0,7}, { "", "kWh", "Therm", "MMBtu", "MJ", "kBtu", "MCF", "CCF" } );
	Array1D_string const convDemStrings( {0,7}, { "", "kW", "Therm", "MMBtu", "MJ", "kBtu", "MCF", "CCF" } );

	int const demandWindowQuarter( 1 );
	int const demandWindowHalf( 2 );
	int const demandWindowHour( 3 );
	int const demandWindowDay( 4 );
	int const demandWindowWeek( 5 );

	Array1D_string const demWindowStrings( {0,5}, { "", "/Hr", "/Hr", "/Hr", "/Day", "/Wk" } );

	int const buyFromUtility( 1 );
	int const sellToUtility( 2 );
	int const netMetering( 3 );

	//For several different objects that reference seasons
	int const seasonWinter( 1 );
	int const seasonSpring( 2 );
	int const seasonSummer( 3 );
	int const seasonFall( 4 );
	int const seasonAnnual( 5 );
	int const seasonMonthly( 6 );

	//For AssignVariablePt
	int const varIsArgument( 1 ); // if used as a value or on right side of expression
	int const varIsAssigned( 2 ); // if variable is assigned to or on left side of expression

	//For ComputeSteps
	// All are negative because all variables are positive
	int const opSUM( -1 );
	int const opMULTIPLY( -2 );
	int const opSUBTRACT( -3 );
	int const opDIVIDE( -4 );
	int const opABSOLUTE( -5 );
	int const opINTEGER( -6 );
	int const opSIGN( -7 );
	int const opROUND( -8 );
	int const opMAXIMUM( -9 );
	int const opMINIMUM( -10 );
	int const opEXCEEDS( -11 );
	int const opANNUALMINIMUM( -12 );
	int const opANNUALMAXIMUM( -13 );
	int const opANNUALSUM( -14 );
	int const opANNUALAVERAGE( -15 );
	int const opANNUALOR( -16 );
	int const opANNUALAND( -17 );
	int const opANNUALMAXIMUMZERO( -18 );
	int const opANNUALMINIMUMZERO( -19 );
	int const opIF( -20 );
	int const opGREATERTHAN( -21 );
	int const opGREATEREQUAL( -22 );
	int const opLESSTHAN( -23 );
	int const opLESSEQUAL( -24 );
	int const opEQUAL( -25 );
	int const opNOTEQUAL( -26 );
	int const opAND( -27 );
	int const opOR( -28 );
	int const opNOT( -29 );
	int const opADD( -30 );
	int const opNOOP( -31 ); // no operation - just list the operand variables - shown as FROM

	//not predefined variable (user defined name - many variables and all objects)
	// used in econvar%specific
	int const varUserDefined( 1 );
	int const varNotYetDefined( 2 );

	//category variables (used in econvar%specific)
	int const catEnergyCharges( 11 );
	int const catDemandCharges( 12 );
	int const catServiceCharges( 13 );
	int const catBasis( 14 );
	int const catAdjustment( 15 );
	int const catSurcharge( 16 );
	int const catSubtotal( 17 );
	int const catTaxes( 18 );
	int const catTotal( 19 );
	int const catNotIncluded( 20 );

	//native variables (based on energy and demands from the simulation) used in econvar%specific
	int const nativeTotalEnergy( 101 );
	int const nativeTotalDemand( 102 );
	int const nativePeakEnergy( 103 );
	int const nativePeakDemand( 104 );
	int const nativeShoulderEnergy( 105 );
	int const nativeShoulderDemand( 106 );
	int const nativeOffPeakEnergy( 107 );
	int const nativeOffPeakDemand( 108 );
	int const nativeMidPeakEnergy( 109 );
	int const nativeMidPeakDemand( 110 );
	int const nativePeakExceedsOffPeak( 111 );
	int const nativeOffPeakExceedsPeak( 112 );
	int const nativePeakExceedsMidPeak( 113 );
	int const nativeMidPeakExceedsPeak( 114 );
	int const nativePeakExceedsShoulder( 115 );
	int const nativeShoulderExceedsPeak( 116 );
	int const nativeIsWinter( 117 );
	int const nativeIsNotWinter( 118 );
	int const nativeIsSpring( 119 );
	int const nativeIsNotSpring( 120 );
	int const nativeIsSummer( 121 );
	int const nativeIsNotSummer( 122 );
	int const nativeIsAutumn( 123 );
	int const nativeIsNotAutumn( 124 );

	int const nativePeakAndShoulderEnergy( 125 );
	int const nativePeakAndShoulderDemand( 126 );
	int const nativePeakAndMidPeakEnergy( 127 );
	int const nativePeakAndMidPeakDemand( 128 );
	int const nativeShoulderAndOffPeakEnergy( 129 );
	int const nativeShoulderAndOffPeakDemand( 130 );
	int const nativePeakAndOffPeakEnergy( 131 );
	int const nativePeakAndOffPeakDemand( 132 );

	int const nativeRealTimePriceCosts( 133 );
	int const nativeAboveCustomerBaseCosts( 134 );
	int const nativeBelowCustomerBaseCosts( 135 );
	int const nativeAboveCustomerBaseEnergy( 136 );
	int const nativeBelowCustomerBaseEnergy( 137 );

	int const countPeriod( 4 );
	int const MaxNumMonths( 12 );
	int const maxNumBlk( 15 );

	int const periodPeak( 1 );
	int const periodShoulder( 2 );
	int const periodOffPeak( 3 );
	int const periodMidPeak( 4 );

	int const kindMeterNotElectric( 0 ); // must be zero because testing of >0 done later.
	int const kindMeterElecSimple( 1 );
	int const kindMeterElecProduced( 2 );
	int const kindMeterElecPurchased( 3 );
	int const kindMeterElecSurplusSold( 4 );
	int const kindMeterElecNet( 5 );

	int const varUnitTypeEnergy( 1 );
	int const varUnitTypeDemand( 2 );
	int const varUnitTypeDimensionless( 3 );
	int const varUnitTypeCurrency( 4 );

	//MODULE PARAMETER DEFINITIONS:

	int numEconVar( 0 );
	int sizeEconVar( 0 );

	// holds the outbound connections for each variable
	Array1D_int operand; // sized to sizeOperand
	int numOperand( 0 );
	int sizeOperand( 0 );

	int numTariff( 0 );

	int numQualify( 0 );

	int numChargeSimple( 0 );

	int numChargeBlock( 0 );

	int numRatchet( 0 );

	int numComputation( 0 );

	//list of pointers to variable, 0 end of line, negative indicate operations
	Array1D_int steps;
	Array1D_int stepsCopy;
	int numSteps( 0 );
	int sizeSteps( 0 );

	int topOfStack( 0 );
	int sizeStack( 0 );

	//MODULE VARIABLE DECLARATIONS:

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Object Data
	Array1D< EconVarType > econVar;
	Array1D< TariffType > tariff;
	Array1D< QualifyType > qualify;
	Array1D< ChargeSimpleType > chargeSimple;
	Array1D< ChargeBlockType > chargeBlock;
	Array1D< RatchetType > ratchet;
	Array1D< ComputationType > computation;
	Array1D< StackType > stack;

	//======================================================================================================================
	//======================================================================================================================
	//    MAIN ROUTINE CALLED EACH TIMESTEP
	//======================================================================================================================
	//======================================================================================================================

	// Functions

	void
	UpdateUtilityBills()
	{

		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   September 2003
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Single routine used to call all get input
		//    routines for economics.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using DataGlobals::DoOutputReporting;
		using DataGlobals::KindOfSim;
		using DataGlobals::ksRunPeriodWeather;
		using OutputReportTabular::AddTOCEntry;

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Locals
		static bool GetInput( true );
		static bool ErrorsFound( false );

		if ( GetInput ) {
			GetInputEconomicsTariff( ErrorsFound );
			// do rest of GetInput only if at least one tariff is defined.
			GetInputEconomicsCurrencyType( ErrorsFound );
			if ( numTariff >= 1 ) {
				if ( ! ErrorsFound ) AddTOCEntry( "Economics Results Summary Report", "Entire Facility" );
				CreateCategoryNativeVariables();
				GetInputEconomicsQualify( ErrorsFound );
				GetInputEconomicsChargeSimple( ErrorsFound );
				GetInputEconomicsChargeBlock( ErrorsFound );
				GetInputEconomicsRatchet( ErrorsFound );
				GetInputEconomicsVariable( ErrorsFound );
				GetInputEconomicsComputation( ErrorsFound );
				CreateDefaultComputation();
			}
			GetInput = false;
			if ( ErrorsFound ) ShowFatalError( "UpdateUtilityBills: Preceding errors cause termination." );
		}
		if ( DoOutputReporting && ( KindOfSim == ksRunPeriodWeather ) ) {
			GatherForEconomics();
		}
	}

	//======================================================================================================================
	//======================================================================================================================

	//    GET INPUT ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	GetInputEconomicsTariff( bool & ErrorsFound ) // true if errors found during getting input objects.
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Read the input file for "Economics:Tariff" objects.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::NumOfTimeStepInHour;
		using OutputReportTabular::AddTOCEntry;
		using OutputProcessor::EnergyMeters;
		using DataGlobalConstants::AssignResourceTypeNum;
		using namespace DataIPShortCuts;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetInputEconomicsTariff: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int iInObj; // loop index variable for reading in objects
		int jObj; // loop index for objects
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		//CHARACTER(len=MaxNameLength),DIMENSION(100)  :: AlphaArray !character string data
		//REAL(r64),                        DIMENSION(100)  :: NumArray  !numeric data
		int IOStat; // IO Status when calling get input subroutine
		int found;
		bool isNotNumeric;
		// variables for getting report variable/meter index
		int KeyCount;
		int TypeVar;
		int AvgSumVar;
		int StepTypeVar;
		std::string UnitsVar; // Units sting, may be blank
		Array1D_string NamesOfKeys; // Specific key name
		Array1D_int IndexesForKeyVar; // Array index
		int jFld;
		std::string CurrentModuleObject; // for ease in renaming.

		CurrentModuleObject = "UtilityCost:Tariff";
		numTariff = GetNumObjectsFound( CurrentModuleObject );
		tariff.allocate( numTariff );
		for ( iInObj = 1; iInObj <= numTariff; ++iInObj ) {
			GetObjectItem( CurrentModuleObject, iInObj, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			//check to make sure none of the values are another economic object
			for ( jFld = 1; jFld <= NumAlphas; ++jFld ) {
				//  args are always turned to upper case but this is okay...
				if ( hasi( cAlphaArgs( jFld ), "UtilityCost:" ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"." );
					ShowContinueError( "... a field was found containing UtilityCost: which may indicate a missing comma." );
				}
			}
			//name of the tariff
			tariff( iInObj ).tariffName = cAlphaArgs( 1 );
			//check if tariff name is unique
			found = 0;
			for ( jObj = 1; jObj <= iInObj - 1; ++jObj ) {
				if ( tariff( iInObj ).tariffName == tariff( jObj ).tariffName ) {
					found = jObj;
					break;
				}
			}
			if ( found > 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data" );
				ShowContinueError( "...Duplicate name. Name has already been used." );
				ErrorsFound = true;
			}
			//name of the report meter
			tariff( iInObj ).reportMeter = cAlphaArgs( 2 );
			// call the key count function but only need count during this pass
			GetVariableKeyCountandType( tariff( iInObj ).reportMeter, KeyCount, TypeVar, AvgSumVar, StepTypeVar, UnitsVar );
			// if no meters found for that name
			if ( KeyCount == 0 ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" missing meter" );
				ShowContinueError( "Meter referenced is not present due to a lack of equipment that uses that energy source/meter:\"" + tariff( iInObj ).reportMeter + "\"." );
				tariff( iInObj ).reportMeterIndx = 0;
			} else {
				NamesOfKeys.allocate( KeyCount );
				IndexesForKeyVar.allocate( KeyCount );
				GetVariableKeys( tariff( iInObj ).reportMeter, TypeVar, NamesOfKeys, IndexesForKeyVar );
				//although this retrieves all keys for a variable, we only need one so the first one is chosen
				if ( KeyCount > 1 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" multiple keys" );
					ShowContinueError( "... Multiple keys for variable select. First key will be used." );
				}
				//assign the index
				tariff( iInObj ).reportMeterIndx = IndexesForKeyVar( 1 );
				//get rid of the arrays used to get the variable number
				NamesOfKeys.deallocate();
				IndexesForKeyVar.deallocate();
			}
			//conversion factor
			if ( SameString( cAlphaArgs( 3 ), "USERDEFINED" ) ) {
				tariff( iInObj ).convChoice = conversionUSERDEF;
				tariff( iInObj ).energyConv = rNumericArgs( 1 ); //energy conversion factor
				tariff( iInObj ).demandConv = rNumericArgs( 2 ); //demand conversion factor
			} else if ( SameString( cAlphaArgs( 3 ), "KWH" ) ) {
				tariff( iInObj ).convChoice = conversionKWH;
				tariff( iInObj ).energyConv = 0.0000002778;
				tariff( iInObj ).demandConv = 0.001;
			} else if ( SameString( cAlphaArgs( 3 ), "THERM" ) ) {
				tariff( iInObj ).convChoice = conversionTHERM;
				tariff( iInObj ).energyConv = 9.4781712e-9;
				tariff( iInObj ).demandConv = 0.00003412;
			} else if ( SameString( cAlphaArgs( 3 ), "MMBTU" ) ) {
				tariff( iInObj ).convChoice = conversionMMBTU;
				tariff( iInObj ).energyConv = 9.4781712e-10;
				tariff( iInObj ).demandConv = 0.000003412;
			} else if ( SameString( cAlphaArgs( 3 ), "MJ" ) ) {
				tariff( iInObj ).convChoice = conversionMJ;
				tariff( iInObj ).energyConv = 0.000001;
				tariff( iInObj ).demandConv = 0.0036;
			} else if ( SameString( cAlphaArgs( 3 ), "KBTU" ) ) {
				tariff( iInObj ).convChoice = conversionKBTU;
				tariff( iInObj ).energyConv = 9.4781712e-7;
				tariff( iInObj ).demandConv = 0.003412;
			} else if ( SameString( cAlphaArgs( 3 ), "MCF" ) ) {
				tariff( iInObj ).convChoice = conversionMCF;
				tariff( iInObj ).energyConv = 9.4781712e-10;
				tariff( iInObj ).demandConv = 0.000003412;
			} else if ( SameString( cAlphaArgs( 3 ), "CCF" ) ) {
				tariff( iInObj ).convChoice = conversionCCF;
				tariff( iInObj ).energyConv = 9.4781712e-9;
				tariff( iInObj ).demandConv = 0.00003412;
			} else {
				tariff( iInObj ).convChoice = conversionKWH;
				tariff( iInObj ).energyConv = 0.0000002778;
				tariff( iInObj ).demandConv = 0.001;
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data" );
				ShowContinueError( cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\", Defaulting to KWH." );
			}
			//schedules
			// period schedule
			if ( len( cAlphaArgs( 4 ) ) > 0 ) {
				tariff( iInObj ).periodSchedule = cAlphaArgs( 4 ); //name of the period schedule (time of day)
				tariff( iInObj ).periodSchIndex = GetScheduleIndex( cAlphaArgs( 4 ) ); //index to the period schedule
				if ( tariff( iInObj ).periodSchIndex == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data" );
					ShowContinueError( " not found " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
					ErrorsFound = true;
				}
			} else {
				tariff( iInObj ).periodSchIndex = 0; //flag value for no schedule used
			}
			// season schedule
			if ( len( cAlphaArgs( 5 ) ) > 0 ) {
				tariff( iInObj ).seasonSchedule = cAlphaArgs( 5 ); //name of the season schedule (winter/summer)
				tariff( iInObj ).seasonSchIndex = GetScheduleIndex( cAlphaArgs( 5 ) ); //index to the season schedule
				if ( tariff( iInObj ).seasonSchIndex == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data" );
					ShowContinueError( " not found " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
					ErrorsFound = true;
				}
			} else {
				tariff( iInObj ).seasonSchIndex = 0; //flag value for no schedule used
			}
			// month schedule
			if ( len( cAlphaArgs( 6 ) ) > 0 ) {
				tariff( iInObj ).monthSchedule = cAlphaArgs( 6 ); //name of month schedule (when months end)
				tariff( iInObj ).monthSchIndex = GetScheduleIndex( cAlphaArgs( 6 ) ); //index to the month schedule
				if ( tariff( iInObj ).monthSchIndex == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data" );
					ShowContinueError( " not found " + cAlphaFieldNames( 6 ) + "=\"" + cAlphaArgs( 6 ) + "\"." );
					ErrorsFound = true;
				}
			} else {
				tariff( iInObj ).monthSchIndex = 0; //flag value for no schedule used
			}
			//type of demand window
			if ( SameString( cAlphaArgs( 7 ), "QuarterHour" ) ) {
				// check to make sure that the demand window and the TIMESTEP IN HOUR are consistant.
				{ auto const SELECT_CASE_var( NumOfTimeStepInHour );
				if ( ( SELECT_CASE_var == 1 ) || ( SELECT_CASE_var == 3 ) || ( SELECT_CASE_var == 5 ) || ( SELECT_CASE_var == 15 ) ) {
					tariff( iInObj ).demandWindow = demandWindowHour;
					tariff( iInObj ).demWinTime = 1.00;
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data" );
					ShowContinueError( "Demand window of QuarterHour is not consistent with number of timesteps per hour [" + RoundSigDigits( NumOfTimeStepInHour ) + "]." );
					ShowContinueError( "Demand window will be set to FullHour, and the simulation continues." );
				} else if ( ( SELECT_CASE_var == 2 ) || ( SELECT_CASE_var == 6 ) || ( SELECT_CASE_var == 10 ) || ( SELECT_CASE_var == 30 ) ) {
					tariff( iInObj ).demandWindow = demandWindowHalf;
					tariff( iInObj ).demWinTime = 0.50;
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data" );
					ShowContinueError( "Demand window of QuarterHour is not consistent with number of timesteps per hour [" + RoundSigDigits( NumOfTimeStepInHour ) + "]." );
					ShowContinueError( "Demand window will be set to HalfHour, and the simulation continues." );
				} else if ( ( SELECT_CASE_var == 4 ) || ( SELECT_CASE_var == 12 ) || ( SELECT_CASE_var == 20 ) || ( SELECT_CASE_var == 60 ) ) {
					tariff( iInObj ).demandWindow = demandWindowQuarter;
					tariff( iInObj ).demWinTime = 0.25;
				}}
			} else if ( SameString( cAlphaArgs( 7 ), "HalfHour" ) ) {
				{ auto const SELECT_CASE_var( NumOfTimeStepInHour );
				if ( ( SELECT_CASE_var == 1 ) || ( SELECT_CASE_var == 3 ) || ( SELECT_CASE_var == 5 ) || ( SELECT_CASE_var == 15 ) ) {
					tariff( iInObj ).demandWindow = demandWindowHour;
					tariff( iInObj ).demWinTime = 1.00;
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data" );
					ShowContinueError( "Demand window of HalfHour is not consistent with number of timesteps per hour [" + RoundSigDigits( NumOfTimeStepInHour ) + "]." );
					ShowContinueError( "Demand window will be set to FullHour, and the simulation continues." );
				} else if ( ( SELECT_CASE_var == 2 ) || ( SELECT_CASE_var == 4 ) || ( SELECT_CASE_var == 6 ) || ( SELECT_CASE_var == 10 ) || ( SELECT_CASE_var == 12 ) || ( SELECT_CASE_var == 20 ) || ( SELECT_CASE_var == 30 ) || ( SELECT_CASE_var == 60 ) ) {
					tariff( iInObj ).demandWindow = demandWindowHalf;
					tariff( iInObj ).demWinTime = 0.50;
				}}
			} else if ( SameString( cAlphaArgs( 7 ), "FullHour" ) ) {
				tariff( iInObj ).demandWindow = demandWindowHour;
				tariff( iInObj ).demWinTime = 1.00;
			} else if ( SameString( cAlphaArgs( 7 ), "Day" ) ) {
				tariff( iInObj ).demandWindow = demandWindowDay;
				tariff( iInObj ).demWinTime = 24.00;
			} else if ( SameString( cAlphaArgs( 7 ), "Week" ) ) {
				tariff( iInObj ).demandWindow = demandWindowWeek;
				tariff( iInObj ).demWinTime = 24.0 * 7.0;
			} else {
				// if not entered default to the same logic as quarter of an hour
				{ auto const SELECT_CASE_var( NumOfTimeStepInHour );
				if ( ( SELECT_CASE_var == 1 ) || ( SELECT_CASE_var == 3 ) || ( SELECT_CASE_var == 5 ) || ( SELECT_CASE_var == 15 ) ) {
					tariff( iInObj ).demandWindow = demandWindowHour;
					tariff( iInObj ).demWinTime = 1.00;
				} else if ( ( SELECT_CASE_var == 2 ) || ( SELECT_CASE_var == 6 ) || ( SELECT_CASE_var == 10 ) || ( SELECT_CASE_var == 30 ) ) {
					tariff( iInObj ).demandWindow = demandWindowHalf;
					tariff( iInObj ).demWinTime = 0.50;
				} else if ( ( SELECT_CASE_var == 4 ) || ( SELECT_CASE_var == 12 ) || ( SELECT_CASE_var == 20 ) || ( SELECT_CASE_var == 60 ) ) {
					tariff( iInObj ).demandWindow = demandWindowQuarter;
					tariff( iInObj ).demWinTime = 0.25;
				}}
			}
			//monthly charge
			tariff( iInObj ).monthChgVal = ProcessNumber( cAlphaArgs( 8 ), isNotNumeric );
			tariff( iInObj ).monthChgPt = AssignVariablePt( cAlphaArgs( 8 ), isNotNumeric, varIsArgument, varNotYetDefined, kindUnknown, 0, iInObj );
			//minimum monthly charge
			if ( len( cAlphaArgs( 9 ) ) > 0 ) {
				tariff( iInObj ).minMonthChgVal = ProcessNumber( cAlphaArgs( 9 ), isNotNumeric );
			} else {
				tariff( iInObj ).minMonthChgVal = -huge( -1.0 ); //set to a very negative value
			}
			tariff( iInObj ).minMonthChgPt = AssignVariablePt( cAlphaArgs( 9 ), isNotNumeric, varIsArgument, varNotYetDefined, kindUnknown, 0, iInObj );
			//real time pricing
			tariff( iInObj ).chargeSchedule = cAlphaArgs( 10 );
			tariff( iInObj ).chargeSchIndex = GetScheduleIndex( cAlphaArgs( 10 ) );
			tariff( iInObj ).baseUseSchedule = cAlphaArgs( 11 );
			tariff( iInObj ).baseUseSchIndex = GetScheduleIndex( cAlphaArgs( 11 ) );
			//group name for separate distribution and transmission rates
			tariff( iInObj ).groupName = cAlphaArgs( 12 );
			//buy or sell option
			if ( SameString( cAlphaArgs( 13 ), "BuyFromUtility" ) ) {
				tariff( iInObj ).buyOrSell = buyFromUtility;
			} else if ( SameString( cAlphaArgs( 13 ), "SellToUtility" ) ) {
				tariff( iInObj ).buyOrSell = sellToUtility;
			} else if ( SameString( cAlphaArgs( 13 ), "NetMetering" ) ) {
				tariff( iInObj ).buyOrSell = netMetering;
			} else {
				tariff( iInObj ).buyOrSell = buyFromUtility;
			}
			// check if meter is consistent with buy or sell option
			if ( ( tariff( iInObj ).buyOrSell == sellToUtility ) && ( ! SameString( tariff( iInObj ).reportMeter, "ELECTRICITYSURPLUSSOLD:FACILITY" ) ) ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" atypical meter" );
				ShowContinueError( "The meter chosen \"" + tariff( iInObj ).reportMeter + "\" is not typically used with the sellToUtility option." );
				ShowContinueError( "Usually the ElectricitySurplusSold:Facility meter is selected when the sellToUtility option is used." );
			}
			if ( ( tariff( iInObj ).buyOrSell == netMetering ) && ( ! SameString( tariff( iInObj ).reportMeter, "ELECTRICITYNET:FACILITY" ) ) ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" atypical meter" );
				ShowContinueError( "The meter chosen \"" + tariff( iInObj ).reportMeter + " is not typically used with the netMetering option." );
				ShowContinueError( "Usually the ElectricityNet:Facility meter is selected when the netMetering option is used." );
			}
			//also test the buy option for electricity
			if ( tariff( iInObj ).buyOrSell == buyFromUtility ) {
				if ( hasi( tariff( iInObj ).reportMeter, "Elec" ) ) { //test if electric meter
					if ( ! ( SameString( tariff( iInObj ).reportMeter, "Electricity:Facility" ) || SameString( tariff( iInObj ).reportMeter, "ElectricityPurchased:Facility" ) ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" atypical meter" );
						ShowContinueError( "The meter chosen \"" + tariff( iInObj ).reportMeter + " is not typically used with the buyFromUtility option." );
						ShowContinueError( "Usually the Electricity:Facility meter or the ElectricityPurchased:Facility is selected when the buyFromUtility option is used." );
					}
				}
			}
			// initialize gathering arrays
			tariff( iInObj ).seasonForMonth = 0;
			tariff( iInObj ).gatherEnergy = 0.0;
			tariff( iInObj ).gatherDemand = 0.0;
			//assume that the tariff is qualified
			tariff( iInObj ).isQualified = true;
			tariff( iInObj ).ptDisqualifier = 0;
			//assume that the tariff is not selected
			tariff( iInObj ).isSelected = false;
			tariff( iInObj ).totalAnnualCost = 0.0;
			//now create the Table Of Contents entries for an HTML file
			AddTOCEntry( "Tariff Report", tariff( iInObj ).tariffName );
			//associate the resource number with each tariff
			if ( tariff( iInObj ).reportMeterIndx >= 1 ) {
				tariff( iInObj ).resourceNum = AssignResourceTypeNum( EnergyMeters( tariff( iInObj ).reportMeterIndx ).ResourceType );
			}
		}
	}

	void
	GetInputEconomicsQualify( bool & ErrorsFound ) // true if errors found during getting input objects.
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Read the input file for "Economics:Qualify" objects.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetInputEconomicsQualify: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iInObj; // loop index variable for reading in objects
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		//CHARACTER(len=MaxNameLength),DIMENSION(100)  :: cAlphaArgs !character string data
		//REAL(r64),                        DIMENSION(100)  :: rNumericArgs  !numeric data
		int IOStat; // IO Status when calling get input subroutine
		bool isNotNumeric;
		int jFld;
		std::string CurrentModuleObject; // for ease in renaming.

		CurrentModuleObject = "UtilityCost:Qualify";
		numQualify = GetNumObjectsFound( CurrentModuleObject );
		qualify.allocate( numQualify );
		for ( iInObj = 1; iInObj <= numQualify; ++iInObj ) {
			GetObjectItem( CurrentModuleObject, iInObj, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			//check to make sure none of the values are another economic object
			for ( jFld = 1; jFld <= NumAlphas; ++jFld ) {
				if ( hasi( cAlphaArgs( jFld ), "UtilityCost:" ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"." );
					ShowContinueError( "... a field was found containing UtilityCost: which may indicate a missing comma." );
				}
			}
			//index of the tariff name in the tariff array
			qualify( iInObj ).tariffIndx = FindTariffIndex( cAlphaArgs( 2 ), cAlphaArgs( 1 ), ErrorsFound, CurrentModuleObject );
			warnIfNativeVarname( cAlphaArgs( 1 ), qualify( iInObj ).tariffIndx, ErrorsFound, CurrentModuleObject );
			qualify( iInObj ).namePt = AssignVariablePt( cAlphaArgs( 1 ), true, varIsAssigned, varNotYetDefined, kindQualify, iInObj, qualify( iInObj ).tariffIndx );
			//index of the variable in the variable array
			qualify( iInObj ).sourcePt = AssignVariablePt( cAlphaArgs( 3 ), true, varIsArgument, varNotYetDefined, kindUnknown, 0, qualify( iInObj ).tariffIndx );
			//indicator if maximum test otherwise minimum
			if ( SameString( cAlphaArgs( 4 ), "Minimum" ) ) {
				qualify( iInObj ).isMaximum = false;
			} else if ( SameString( cAlphaArgs( 4 ), "Maximum" ) ) {
				qualify( iInObj ).isMaximum = true;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data" );
				ShowContinueError( cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
				ErrorsFound = true;
				qualify( iInObj ).isMaximum = true;
			}
			//value of the threshold
			qualify( iInObj ).thresholdVal = ProcessNumber( cAlphaArgs( 5 ), isNotNumeric );
			qualify( iInObj ).thresholdPt = AssignVariablePt( cAlphaArgs( 5 ), isNotNumeric, varIsArgument, varNotYetDefined, kindUnknown, 0, qualify( iInObj ).tariffIndx );
			//enumerated list of the kind of season
			qualify( iInObj ).season = LookUpSeason( cAlphaArgs( 6 ), cAlphaArgs( 1 ) );
			//indicator if consecutive months otherwise count
			if ( SameString( cAlphaArgs( 7 ), "Count" ) ) {
				qualify( iInObj ).isConsecutive = false;
			} else if ( SameString( cAlphaArgs( 7 ), "Consecutive" ) ) {
				qualify( iInObj ).isConsecutive = true;
			} else {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data" );
				ShowContinueError( cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
				ErrorsFound = true;
				qualify( iInObj ).isConsecutive = true;
			}
			//number of months the test must be good for
			qualify( iInObj ).numberOfMonths = rNumericArgs( 1 );
		}
	}

	void
	GetInputEconomicsChargeSimple( bool & ErrorsFound ) // true if errors found during getting input objects.
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Read the input file for "Economics:Charge:Simple" objects.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetInputEconomicsChargeSimple: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int iInObj; // loop index variable for reading in objects
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		//CHARACTER(len=MaxNameLength),DIMENSION(100)  :: cAlphaArgs !character string data
		//REAL(r64),                        DIMENSION(100)  :: rNumericArgs  !numeric data
		int IOStat; // IO Status when calling get input subroutine
		bool isNotNumeric;
		int jFld;
		std::string CurrentModuleObject; // for ease in renaming.

		CurrentModuleObject = "UtilityCost:Charge:Simple";
		numChargeSimple = GetNumObjectsFound( CurrentModuleObject );
		chargeSimple.allocate( numChargeSimple );
		for ( iInObj = 1; iInObj <= numChargeSimple; ++iInObj ) {
			GetObjectItem( CurrentModuleObject, iInObj, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			//check to make sure none of the values are another economic object
			for ( jFld = 1; jFld <= NumAlphas; ++jFld ) {
				if ( hasi( cAlphaArgs( jFld ), "UtilityCost:" ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"." );
					ShowContinueError( "... a field was found containing UtilityCost: which may indicate a missing comma." );
				}
			}
			//index of the tariff name in the tariff array
			chargeSimple( iInObj ).tariffIndx = FindTariffIndex( cAlphaArgs( 2 ), cAlphaArgs( 1 ), ErrorsFound, CurrentModuleObject );
			warnIfNativeVarname( cAlphaArgs( 1 ), chargeSimple( iInObj ).tariffIndx, ErrorsFound, CurrentModuleObject );
			chargeSimple( iInObj ).namePt = AssignVariablePt( cAlphaArgs( 1 ), true, varIsAssigned, varNotYetDefined, kindChargeSimple, iInObj, chargeSimple( iInObj ).tariffIndx );
			//index of the variable in the variable array
			chargeSimple( iInObj ).sourcePt = AssignVariablePt( cAlphaArgs( 3 ), true, varIsArgument, varNotYetDefined, kindUnknown, 0, chargeSimple( iInObj ).tariffIndx );
			//enumerated list of the kind of season
			chargeSimple( iInObj ).season = LookUpSeason( cAlphaArgs( 4 ), cAlphaArgs( 1 ) );
			//check to make sure a seasonal schedule is specified if the season is not annual
			if ( chargeSimple( iInObj ).season != seasonAnnual ) {
				if ( chargeSimple( iInObj ).tariffIndx != 0 ) {
					if ( tariff( chargeSimple( iInObj ).tariffIndx ).seasonSchIndex == 0 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data" );
						ShowContinueError( cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
						ShowContinueError( " a Season other than Annual is used but no Season Schedule Name is specified in the UtilityCost:Tariff." );
					}
				}
			}
			//index of the category in the variable array
			chargeSimple( iInObj ).categoryPt = AssignVariablePt( cAlphaArgs( 5 ), true, varIsAssigned, varNotYetDefined, kindCategory, iInObj, chargeSimple( iInObj ).tariffIndx );
			//cost per unit value or variable
			chargeSimple( iInObj ).costPerVal = ProcessNumber( cAlphaArgs( 6 ), isNotNumeric );
			chargeSimple( iInObj ).costPerPt = AssignVariablePt( cAlphaArgs( 6 ), isNotNumeric, varIsArgument, varNotYetDefined, kindUnknown, 0, chargeSimple( iInObj ).tariffIndx );
		}
	}

	void
	GetInputEconomicsChargeBlock( bool & ErrorsFound ) // true if errors found during getting input objects.
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Read the input file for "Economics:Charge:Block" objects.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetInputEconomicsChargeBlock: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int iInObj; // loop index variable for reading in objects
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		//CHARACTER(len=MaxNameLength),DIMENSION(100)  :: cAlphaArgs !character string data
		//REAL(r64),                        DIMENSION(100)  :: rNumericArgs  !numeric data
		int IOStat; // IO Status when calling get input subroutine
		bool isNotNumeric;
		int jBlk; // loop index for blocks
		int alphaOffset; // offset used in blocks for alpha array
		Real64 hugeNumber( 0.0 ); //Autodesk Value not used but suppresses warning about huge() call
		int jFld;
		std::string CurrentModuleObject; // for ease in renaming.

		CurrentModuleObject = "UtilityCost:Charge:Block";
		hugeNumber = huge( hugeNumber );
		numChargeBlock = GetNumObjectsFound( CurrentModuleObject );
		chargeBlock.allocate( numChargeBlock );
		for ( iInObj = 1; iInObj <= numChargeBlock; ++iInObj ) {
			GetObjectItem( CurrentModuleObject, iInObj, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			//check to make sure none of the values are another economic object
			for ( jFld = 1; jFld <= NumAlphas; ++jFld ) {
				if ( hasi( cAlphaArgs( jFld ), "UtilityCost:" ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"." );
					ShowContinueError( "... a field was found containing UtilityCost: which may indicate a missing comma." );
				}
			}
			//index of the tariff name in the tariff array
			chargeBlock( iInObj ).tariffIndx = FindTariffIndex( cAlphaArgs( 2 ), cAlphaArgs( 1 ), ErrorsFound, CurrentModuleObject );
			warnIfNativeVarname( cAlphaArgs( 1 ), chargeBlock( iInObj ).tariffIndx, ErrorsFound, CurrentModuleObject );
			chargeBlock( iInObj ).namePt = AssignVariablePt( cAlphaArgs( 1 ), true, varIsAssigned, varNotYetDefined, kindChargeBlock, iInObj, chargeBlock( iInObj ).tariffIndx );
			//index of the variable in the variable array
			chargeBlock( iInObj ).sourcePt = AssignVariablePt( cAlphaArgs( 3 ), true, varIsArgument, varNotYetDefined, kindUnknown, 0, chargeBlock( iInObj ).tariffIndx );
			//enumerated list of the kind of season
			chargeBlock( iInObj ).season = LookUpSeason( cAlphaArgs( 4 ), cAlphaArgs( 1 ) );
			//check to make sure a seasonal schedule is specified if the season is not annual
			if ( chargeBlock( iInObj ).season != seasonAnnual ) {
				if ( chargeBlock( iInObj ).tariffIndx != 0 ) {
					if ( tariff( chargeBlock( iInObj ).tariffIndx ).seasonSchIndex == 0 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data" );
						ShowContinueError( cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
						ShowContinueError( " a Season other than Annual is used but no Season Schedule Name is specified in the UtilityCost:Tariff." );
					}
				}
			}
			//index of the category in the variable array
			chargeBlock( iInObj ).categoryPt = AssignVariablePt( cAlphaArgs( 5 ), true, varIsAssigned, varNotYetDefined, kindCategory, iInObj, chargeBlock( iInObj ).tariffIndx );
			//index of the remaining into variable in the variable array
			chargeBlock( iInObj ).remainingPt = AssignVariablePt( cAlphaArgs( 6 ), true, varIsAssigned, varNotYetDefined, kindCategory, iInObj, chargeBlock( iInObj ).tariffIndx );
			//block size multiplier
			if ( len( cAlphaArgs( 7 ) ) == 0 ) { //if blank
				chargeBlock( iInObj ).blkSzMultVal = 1.0; //default is 1 if left blank
				chargeBlock( iInObj ).blkSzMultPt = 0;
			} else {
				chargeBlock( iInObj ).blkSzMultVal = ProcessNumber( cAlphaArgs( 7 ), isNotNumeric );
				chargeBlock( iInObj ).blkSzMultPt = AssignVariablePt( cAlphaArgs( 7 ), isNotNumeric, varIsArgument, varNotYetDefined, kindUnknown, 0, chargeBlock( iInObj ).tariffIndx );
			}
			//number of blocks used
			chargeBlock( iInObj ).numBlk = ( NumAlphas - 7 ) / 2;
			for ( jBlk = 1; jBlk <= chargeBlock( iInObj ).numBlk; ++jBlk ) {
				alphaOffset = 7 + ( jBlk - 1 ) * 2;
				//catch the "remaining" code word for the block size
				if ( SameString( cAlphaArgs( alphaOffset + 1 ), "REMAINING" ) ) {
					chargeBlock( iInObj ).blkSzVal( jBlk ) = hugeNumber / 1000000; //using small portion of largest possible value to prevent overflow
					chargeBlock( iInObj ).blkSzPt( jBlk ) = 0;
				} else {
					//array of block size
					chargeBlock( iInObj ).blkSzVal( jBlk ) = ProcessNumber( cAlphaArgs( alphaOffset + 1 ), isNotNumeric );

					chargeBlock( iInObj ).blkSzPt( jBlk ) = AssignVariablePt( cAlphaArgs( alphaOffset + 1 ), isNotNumeric, varIsArgument, varNotYetDefined, kindUnknown, 0, chargeBlock( iInObj ).tariffIndx );
				}
				//array of block cost
				chargeBlock( iInObj ).blkCostVal( jBlk ) = ProcessNumber( cAlphaArgs( alphaOffset + 2 ), isNotNumeric );
				chargeBlock( iInObj ).blkCostPt( jBlk ) = AssignVariablePt( cAlphaArgs( alphaOffset + 2 ), isNotNumeric, varIsArgument, varNotYetDefined, kindUnknown, 0, chargeBlock( iInObj ).tariffIndx );
			}
		}
	}

	void
	GetInputEconomicsRatchet( bool & ErrorsFound ) // true if errors found during getting input objects.
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Read the input file for "Economics:Ratchet" objects.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetInputEconomicsRatchet: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int iInObj; // loop index variable for reading in objects
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		//CHARACTER(len=MaxNameLength),DIMENSION(100)  :: cAlphaArgs !character string data
		//REAL(r64),                        DIMENSION(100)  :: rNumericArgs  !numeric data
		int IOStat; // IO Status when calling get input subroutine
		bool isNotNumeric;
		int jFld;
		std::string CurrentModuleObject; // for ease in renaming.

		CurrentModuleObject = "UtilityCost:Ratchet";
		numRatchet = GetNumObjectsFound( CurrentModuleObject );
		ratchet.allocate( numRatchet );
		for ( iInObj = 1; iInObj <= numRatchet; ++iInObj ) {
			GetObjectItem( CurrentModuleObject, iInObj, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			//check to make sure none of the values are another economic object
			for ( jFld = 1; jFld <= NumAlphas; ++jFld ) {
				if ( hasi( cAlphaArgs( jFld ), "UtilityCost:" ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"." );
					ShowContinueError( "... a field was found containing UtilityCost: which may indicate a missing comma." );
				}
			}
			//index of the tariff name in the tariff array
			ratchet( iInObj ).tariffIndx = FindTariffIndex( cAlphaArgs( 2 ), cAlphaArgs( 1 ), ErrorsFound, CurrentModuleObject );
			warnIfNativeVarname( cAlphaArgs( 1 ), ratchet( iInObj ).tariffIndx, ErrorsFound, CurrentModuleObject );
			ratchet( iInObj ).namePt = AssignVariablePt( cAlphaArgs( 1 ), true, varIsAssigned, varNotYetDefined, kindRatchet, iInObj, ratchet( iInObj ).tariffIndx );
			//index of the variable in the variable array
			ratchet( iInObj ).baselinePt = AssignVariablePt( cAlphaArgs( 3 ), true, varIsArgument, varNotYetDefined, kindRatchet, iInObj, ratchet( iInObj ).tariffIndx );
			//index of the variable in the variable array
			ratchet( iInObj ).adjustmentPt = AssignVariablePt( cAlphaArgs( 4 ), true, varIsArgument, varNotYetDefined, kindRatchet, iInObj, ratchet( iInObj ).tariffIndx );
			// seasons to and from
			ratchet( iInObj ).seasonFrom = LookUpSeason( cAlphaArgs( 5 ), cAlphaArgs( 1 ) );
			ratchet( iInObj ).seasonTo = LookUpSeason( cAlphaArgs( 6 ), cAlphaArgs( 1 ) );
			//ratchet multiplier
			ratchet( iInObj ).multiplierVal = ProcessNumber( cAlphaArgs( 7 ), isNotNumeric );
			ratchet( iInObj ).multiplierPt = AssignVariablePt( cAlphaArgs( 7 ), isNotNumeric, varIsArgument, varNotYetDefined, kindUnknown, 0, ratchet( iInObj ).tariffIndx );
			//ratchet offset
			ratchet( iInObj ).offsetVal = ProcessNumber( cAlphaArgs( 8 ), isNotNumeric );
			ratchet( iInObj ).offsetPt = AssignVariablePt( cAlphaArgs( 8 ), isNotNumeric, varIsArgument, varNotYetDefined, kindUnknown, 0, ratchet( iInObj ).tariffIndx );
		}
	}

	void
	GetInputEconomicsVariable( bool & ErrorsFound ) // true if errors found during getting input objects.
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Read the input file for "Economics:Variable" objects.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetInputEconomicsVariable: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int numEconVarObj;
		int tariffPt;
		int iInObj; // loop index variable for reading in objects
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		//CHARACTER(len=MaxNameLength),DIMENSION(100)  :: cAlphaArgs !character string data
		//REAL(r64),                        DIMENSION(100)  :: rNumericArgs  !numeric data
		int IOStat; // IO Status when calling get input subroutine
		int jVal;
		int variablePt;
		int jFld;
		std::string CurrentModuleObject; // for ease in renaming.

		CurrentModuleObject = "UtilityCost:Variable";
		numEconVarObj = GetNumObjectsFound( CurrentModuleObject );
		for ( iInObj = 1; iInObj <= numEconVarObj; ++iInObj ) {
			GetObjectItem( CurrentModuleObject, iInObj, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			//check to make sure none of the values are another economic object
			for ( jFld = 1; jFld <= NumAlphas; ++jFld ) {
				if ( hasi( cAlphaArgs( jFld ), "UtilityCost:" ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"." );
					ShowContinueError( "... a field was found containing UtilityCost: which may indicate a missing comma." );
				}
			}
			tariffPt = FindTariffIndex( cAlphaArgs( 2 ), cAlphaArgs( 1 ), ErrorsFound, CurrentModuleObject );
			variablePt = AssignVariablePt( cAlphaArgs( 1 ), true, varIsArgument, varUserDefined, kindVariable, iInObj, tariffPt );
			warnIfNativeVarname( cAlphaArgs( 1 ), tariffPt, ErrorsFound, CurrentModuleObject );
			//validate the kind of variable - not used internally except for validation
			if ( SameString( cAlphaArgs( 3 ), "ENERGY" ) ) {
				econVar( variablePt ).varUnitType = varUnitTypeEnergy;
			} else if ( SameString( cAlphaArgs( 3 ), "DEMAND" ) ) {
				econVar( variablePt ).varUnitType = varUnitTypeDemand;
			} else if ( SameString( cAlphaArgs( 3 ), "DIMENSIONLESS" ) ) {
				econVar( variablePt ).varUnitType = varUnitTypeDimensionless;
			} else if ( SameString( cAlphaArgs( 3 ), "CURRENCY" ) ) {
				econVar( variablePt ).varUnitType = varUnitTypeCurrency;
			} else {
				econVar( variablePt ).varUnitType = varUnitTypeDimensionless;
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data" );
				ShowContinueError( "invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
				ErrorsFound = true;
			}
			//move number inputs into econVar
			for ( jVal = 1; jVal <= NumNums; ++jVal ) {
				econVar( variablePt ).values( jVal ) = rNumericArgs( jVal );
			}
			// fill the rest of the array with the last value entered
			if ( NumNums < MaxNumMonths ) {
				for ( jVal = NumNums + 1; jVal <= MaxNumMonths; ++jVal ) {
					econVar( variablePt ).values( jVal ) = rNumericArgs( NumNums );
				}
			}
		}
	}

	void
	GetInputEconomicsComputation( bool & ErrorsFound ) // true if errors found during getting input objects.
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Read the input file for "Economics:Computation" objects.
		//    This object is only used for very complex rates.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetInputEconomicsComputation: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int tariffPt;
		int iInObj; // loop index variable for reading in objects
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		//CHARACTER(len=MaxNameLength),DIMENSION(100)  :: cAlphaArgs !character string data
		//REAL(r64),                        DIMENSION(100)  :: rNumericArgs  !numeric data
		int IOStat; // IO Status when calling get input subroutine
		int jLine;
		int jFld;
		std::string CurrentModuleObject; // for ease in renaming.

		CurrentModuleObject = "UtilityCost:Computation";
		numComputation = GetNumObjectsFound( CurrentModuleObject );
		computation.allocate( numTariff ); //not the number of Computations but the number of tariffs
		//set default values for computation
		for ( auto & e : computation ) {
			e.computeName.clear();
			e.firstStep = 0;
			e.lastStep = -1;
			e.isUserDef = false;
		}
		for ( iInObj = 1; iInObj <= numComputation; ++iInObj ) {
			GetObjectItem( CurrentModuleObject, iInObj, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			//check to make sure none of the values are another economic object
			for ( jFld = 1; jFld <= NumAlphas; ++jFld ) {
				if ( hasi( cAlphaArgs( jFld ), "UtilityCost:" ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"." );
					ShowContinueError( "... a field was found containing UtilityCost: which may indicate a missing comma." );
				}
			}
			tariffPt = FindTariffIndex( cAlphaArgs( 2 ), cAlphaArgs( 1 ), ErrorsFound, CurrentModuleObject );
			warnIfNativeVarname( cAlphaArgs( 1 ), tariffPt, ErrorsFound, CurrentModuleObject );
			//tariff and computation share the same index, the tariff index
			//so all references are to the tariffPt
			if ( isWithinRange( tariffPt, 1, numTariff ) ) {
				computation( tariffPt ).computeName = cAlphaArgs( 1 );
				computation( tariffPt ).firstStep = numSteps + 1;
				for ( jLine = 3; jLine <= NumAlphas; ++jLine ) {
					parseComputeLine( cAlphaArgs( jLine ), tariffPt );
				}
				computation( tariffPt ).lastStep = numSteps;
				// check to make sure that some steps were defined
				if ( computation( tariffPt ).firstStep >= computation( tariffPt ).lastStep ) {
					computation( tariffPt ).firstStep = 0;
					computation( tariffPt ).lastStep = -1;
					computation( tariffPt ).isUserDef = false;
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data." );
					ShowContinueError( "... No lines in the computation can be interpreted " );
					ErrorsFound = true;
				} else {
					computation( tariffPt ).isUserDef = true;
				}
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data." );
				ShowContinueError( "... not found " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ErrorsFound = true;
			}
		}
	}

	void
	GetInputEconomicsCurrencyType( bool & ErrorsFound ) // true if errors found during getting input objects.
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Sets the type of currency (U.S. Dollar, Euro, Yen, etc.. )
		//   This is a "unique" object.

		// METHODOLOGY EMPLOYED:
		//   Uses get input structure similar to other objects

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataCostEstimate;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurrentModuleObject( "CurrencyType" );
		static std::string const RoutineName( "GetInputEconomicsCurrencyType: " );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumCurrencyType;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		//CHARACTER(len=MaxNameLength),DIMENSION(5) :: cAlphaArgs !character string data - should be 1
		//REAL(r64),                   DIMENSION(5) :: rNumericArgs  !numeric data          - should be 0
		int IOStat; // IO Status when calling get input subroutine
		int i;

		initializeMonetaryUnit();
		NumCurrencyType = GetNumObjectsFound( CurrentModuleObject );
		selectedMonetaryUnit = 0; // invalid
		if ( NumCurrencyType == 0 ) {
			selectedMonetaryUnit = 1; //USD - U.S. Dollar
		} else if ( NumCurrencyType == 1 ) {
			GetObjectItem( CurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			// Monetary Unit
			for ( i = 1; i <= numMonetaryUnit; ++i ) {
				if ( SameString( cAlphaArgs( 1 ), monetaryUnit( i ).code ) ) {
					selectedMonetaryUnit = i;
					break;
				}
			}
			if ( selectedMonetaryUnit == 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data." );
				ShowContinueError( "... invalid " + cAlphaFieldNames( 1 ) + '.' );
				ErrorsFound = true;
			}
		} else if ( NumCurrencyType > 1 ) {
			ShowWarningError( RoutineName + CurrentModuleObject + " Only one instance of this object is allowed. USD will be used." );
			selectedMonetaryUnit = 1; //USD - U.S. Dollar
		}
	}

	void
	parseComputeLine(
		std::string const & lineOfCompute,
		int const fromTariff
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   June 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Converts a single line in the ECONOMICS:COMPUTE
		//   command into tokens for computation

		// METHODOLOGY EMPLOYED:
		//   Scan the line from the end of the line to the front of the
		//   line and search for operators and variables. All items
		//   are put into the step array.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string word;
		std::string::size_type endOfWord;
		int token;

		endOfWord = len( lineOfCompute ) - 1;
		while ( endOfWord != std::string::npos ) {
			// get a single word (text string delimited by spaces)
			GetLastWord( lineOfCompute, endOfWord, word );
			// first see if word is an operator
			token = lookupOperator( word );
			// if not an operator then look for
			if ( token == 0 ) {
				// see if argument or assignment (assignment will be first string on line)
				if ( endOfWord != std::string::npos ) {
					token = AssignVariablePt( word, true, varIsArgument, varNotYetDefined, kindUnknown, 0, fromTariff );
				} else {
					token = AssignVariablePt( word, true, varIsAssigned, varNotYetDefined, kindAssignCompute, 0, fromTariff );
				}
			}
			// if a token is found then put it into step array
			if ( token == 0 ) {
				ShowWarningError( "In UtilityCost:Computation line: " + lineOfCompute );
				ShowContinueError( "  Do not recognize: " + word + " Will skip." );
			} else {
				incrementSteps();
				steps( numSteps ) = token;
			}
		}
		incrementSteps();
		steps( numSteps ) = 0; //at the end of the line show a zero to clear the stack
	}

	void
	GetLastWord(
		std::string const & lineOfText,
		std::string::size_type & endOfScan,
		std::string & aWord
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   June 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Returns the last substring of the line of text to the
		//   left of the endOfSubStrg pointer. A substring is
		//   delimitted by spaces.  Quotes are not significant
		//   (they are treated just like any other non-space character)

		// METHODOLOGY EMPLOYED:
		//   Scan the string from the end.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool isInWord;
		bool isSpace;
		std::string::size_type iString;
		std::string::size_type curEndOfScan;
		std::string::size_type beginOfWord;
		std::string::size_type endOfWord;

		curEndOfScan = endOfScan;
		if ( curEndOfScan != std::string::npos ) {
			if ( curEndOfScan >= len( lineOfText ) ) {
				curEndOfScan = len( lineOfText ) - 1;
			}
			//check if currently on a space or not
			if ( lineOfText[ curEndOfScan ] == ' ' ) {
				isInWord = false;
				beginOfWord = 0;
				endOfWord = 0;
			} else {
				isInWord = true;
				beginOfWord = curEndOfScan;
				endOfWord = curEndOfScan;
			}
			//scan backwards from
			for ( iString = curEndOfScan; iString <= curEndOfScan; --iString ) { // Unsigned will wrap to npos after 0
				if ( lineOfText[ iString ] == ' ' ) {
					isSpace = true;
				} else {
					isSpace = false;
				}
				// all logical conditions of isSpace and isInWord
				if ( isSpace ) {
					if ( isInWord ) {
						//found the space in front of the word
						break;
					} else {
						//still have not found the back of the word
						// do nothing
					}
				} else {
					if ( isInWord ) {
						//still have not found the space in front of the word
						beginOfWord = iString;
					} else {
						//found the last character of the word
						endOfWord = iString;
						beginOfWord = iString;
						isInWord = true;
					}
				}
			}
			aWord = lineOfText.substr( beginOfWord, endOfWord - beginOfWord + 1 );
			endOfScan = beginOfWord - 1;
			if ( endOfScan == std::string::npos ) {
				endOfScan = std::string::npos;
			}
		} else {
			endOfScan = std::string::npos;
			aWord = "";
		}
	}

	void
	initializeMonetaryUnit()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Sets the type of monetary unit array.

		// METHODOLOGY EMPLOYED:
		//   Uses get input structure similar to other objects
		//   The monetaryUnitSymbols.xls spreadsheet helps create the code for this routine

		// REFERENCES:
		//   www.xe.com/symbols.php

		// Using/Aliasing
		using namespace DataCostEstimate;

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		numMonetaryUnit = 111;
		monetaryUnit.allocate( numMonetaryUnit );
		monetaryUnit( 1 ).code = "USD";
		monetaryUnit( 2 ).code = "AFN";
		monetaryUnit( 3 ).code = "ALL";
		monetaryUnit( 4 ).code = "ANG";
		monetaryUnit( 5 ).code = "ARS";
		monetaryUnit( 6 ).code = "AUD";
		monetaryUnit( 7 ).code = "AWG";
		monetaryUnit( 8 ).code = "AZN";
		monetaryUnit( 9 ).code = "BAM";
		monetaryUnit( 10 ).code = "BBD";
		monetaryUnit( 11 ).code = "BGN";
		monetaryUnit( 12 ).code = "BMD";
		monetaryUnit( 13 ).code = "BND";
		monetaryUnit( 14 ).code = "BOB";
		monetaryUnit( 15 ).code = "BRL";
		monetaryUnit( 16 ).code = "BSD";
		monetaryUnit( 17 ).code = "BWP";
		monetaryUnit( 18 ).code = "BYR";
		monetaryUnit( 19 ).code = "BZD";
		monetaryUnit( 20 ).code = "CAD";
		monetaryUnit( 21 ).code = "CHF";
		monetaryUnit( 22 ).code = "CLP";
		monetaryUnit( 23 ).code = "CNY";
		monetaryUnit( 24 ).code = "COP";
		monetaryUnit( 25 ).code = "CRC";
		monetaryUnit( 26 ).code = "CUP";
		monetaryUnit( 27 ).code = "CZK";
		monetaryUnit( 28 ).code = "DKK";
		monetaryUnit( 29 ).code = "DOP";
		monetaryUnit( 30 ).code = "EEK";
		monetaryUnit( 31 ).code = "EGP";
		monetaryUnit( 32 ).code = "EUR";
		monetaryUnit( 33 ).code = "FJD";
		monetaryUnit( 34 ).code = "GBP";
		monetaryUnit( 35 ).code = "GHC";
		monetaryUnit( 36 ).code = "GIP";
		monetaryUnit( 37 ).code = "GTQ";
		monetaryUnit( 38 ).code = "GYD";
		monetaryUnit( 39 ).code = "HKD";
		monetaryUnit( 40 ).code = "HNL";
		monetaryUnit( 41 ).code = "HRK";
		monetaryUnit( 42 ).code = "HUF";
		monetaryUnit( 43 ).code = "IDR";
		monetaryUnit( 44 ).code = "ILS";
		monetaryUnit( 45 ).code = "IMP";
		monetaryUnit( 46 ).code = "INR";
		monetaryUnit( 47 ).code = "IRR";
		monetaryUnit( 48 ).code = "ISK";
		monetaryUnit( 49 ).code = "JEP";
		monetaryUnit( 50 ).code = "JMD";
		monetaryUnit( 51 ).code = "JPY";
		monetaryUnit( 52 ).code = "KGS";
		monetaryUnit( 53 ).code = "KHR";
		monetaryUnit( 54 ).code = "KPW";
		monetaryUnit( 55 ).code = "KRW";
		monetaryUnit( 56 ).code = "KYD";
		monetaryUnit( 57 ).code = "KZT";
		monetaryUnit( 58 ).code = "LAK";
		monetaryUnit( 59 ).code = "LBP";
		monetaryUnit( 60 ).code = "LKR";
		monetaryUnit( 61 ).code = "LRD";
		monetaryUnit( 62 ).code = "LTL";
		monetaryUnit( 63 ).code = "LVL";
		monetaryUnit( 64 ).code = "MKD";
		monetaryUnit( 65 ).code = "MNT";
		monetaryUnit( 66 ).code = "MUR";
		monetaryUnit( 67 ).code = "MXN";
		monetaryUnit( 68 ).code = "MYR";
		monetaryUnit( 69 ).code = "MZN";
		monetaryUnit( 70 ).code = "NAD";
		monetaryUnit( 71 ).code = "NGN";
		monetaryUnit( 72 ).code = "NIO";
		monetaryUnit( 73 ).code = "NOK";
		monetaryUnit( 74 ).code = "NPR";
		monetaryUnit( 75 ).code = "NZD";
		monetaryUnit( 76 ).code = "OMR";
		monetaryUnit( 77 ).code = "PAB";
		monetaryUnit( 78 ).code = "PEN";
		monetaryUnit( 79 ).code = "PHP";
		monetaryUnit( 80 ).code = "PKR";
		monetaryUnit( 81 ).code = "PLN";
		monetaryUnit( 82 ).code = "PYG";
		monetaryUnit( 83 ).code = "QAR";
		monetaryUnit( 84 ).code = "RON";
		monetaryUnit( 85 ).code = "RSD";
		monetaryUnit( 86 ).code = "RUB";
		monetaryUnit( 87 ).code = "SAR";
		monetaryUnit( 88 ).code = "SBD";
		monetaryUnit( 89 ).code = "SCR";
		monetaryUnit( 90 ).code = "SEK";
		monetaryUnit( 91 ).code = "SGD";
		monetaryUnit( 92 ).code = "SHP";
		monetaryUnit( 93 ).code = "SOS";
		monetaryUnit( 94 ).code = "SRD";
		monetaryUnit( 95 ).code = "SVC";
		monetaryUnit( 96 ).code = "SYP";
		monetaryUnit( 97 ).code = "THB";
		monetaryUnit( 98 ).code = "TRL";
		monetaryUnit( 99 ).code = "TRY";
		monetaryUnit( 100 ).code = "TTD";
		monetaryUnit( 101 ).code = "TVD";
		monetaryUnit( 102 ).code = "TWD";
		monetaryUnit( 103 ).code = "UAH";
		monetaryUnit( 104 ).code = "UYU";
		monetaryUnit( 105 ).code = "UZS";
		monetaryUnit( 106 ).code = "VEF";
		monetaryUnit( 107 ).code = "VND";
		monetaryUnit( 108 ).code = "XCD";
		monetaryUnit( 109 ).code = "YER";
		monetaryUnit( 110 ).code = "ZAR";
		monetaryUnit( 111 ).code = "ZWD";

		monetaryUnit( 1 ).txt = "$";
		monetaryUnit( 2 ).txt = "AFN";
		monetaryUnit( 3 ).txt = "Lek";
		monetaryUnit( 4 ).txt = "ANG";
		monetaryUnit( 5 ).txt = "$";
		monetaryUnit( 6 ).txt = "$";
		monetaryUnit( 7 ).txt = "AWG";
		monetaryUnit( 8 ).txt = "AZN";
		monetaryUnit( 9 ).txt = "KM";
		monetaryUnit( 10 ).txt = "$";
		monetaryUnit( 11 ).txt = "BGN";
		monetaryUnit( 12 ).txt = "$";
		monetaryUnit( 13 ).txt = "$";
		monetaryUnit( 14 ).txt = "$b";
		monetaryUnit( 15 ).txt = "R$";
		monetaryUnit( 16 ).txt = "$";
		monetaryUnit( 17 ).txt = "P";
		monetaryUnit( 18 ).txt = "p.";
		monetaryUnit( 19 ).txt = "BZ$";
		monetaryUnit( 20 ).txt = "$";
		monetaryUnit( 21 ).txt = "CHF";
		monetaryUnit( 22 ).txt = "$";
		monetaryUnit( 23 ).txt = "CNY";
		monetaryUnit( 24 ).txt = "$";
		monetaryUnit( 25 ).txt = "CRC";
		monetaryUnit( 26 ).txt = "CUP";
		monetaryUnit( 27 ).txt = "CZK";
		monetaryUnit( 28 ).txt = "kr";
		monetaryUnit( 29 ).txt = "RD$";
		monetaryUnit( 30 ).txt = "kr";
		monetaryUnit( 31 ).txt = "£";
		monetaryUnit( 32 ).txt = "EUR";
		monetaryUnit( 33 ).txt = "$";
		monetaryUnit( 34 ).txt = "£";
		monetaryUnit( 35 ).txt = "¢";
		monetaryUnit( 36 ).txt = "£";
		monetaryUnit( 37 ).txt = "Q";
		monetaryUnit( 38 ).txt = "$";
		monetaryUnit( 39 ).txt = "HK$";
		monetaryUnit( 40 ).txt = "L";
		monetaryUnit( 41 ).txt = "kn";
		monetaryUnit( 42 ).txt = "Ft";
		monetaryUnit( 43 ).txt = "Rp";
		monetaryUnit( 44 ).txt = "ILS";
		monetaryUnit( 45 ).txt = "£";
		monetaryUnit( 46 ).txt = "INR";
		monetaryUnit( 47 ).txt = "IRR";
		monetaryUnit( 48 ).txt = "kr";
		monetaryUnit( 49 ).txt = "£";
		monetaryUnit( 50 ).txt = "J$";
		monetaryUnit( 51 ).txt = "¥";
		monetaryUnit( 52 ).txt = "KGS";
		monetaryUnit( 53 ).txt = "KHR";
		monetaryUnit( 54 ).txt = "KPW";
		monetaryUnit( 55 ).txt = "KRW";
		monetaryUnit( 56 ).txt = "$";
		monetaryUnit( 57 ).txt = "KZT";
		monetaryUnit( 58 ).txt = "LAK";
		monetaryUnit( 59 ).txt = "£";
		monetaryUnit( 60 ).txt = "LKR";
		monetaryUnit( 61 ).txt = "$";
		monetaryUnit( 62 ).txt = "Lt";
		monetaryUnit( 63 ).txt = "Ls";
		monetaryUnit( 64 ).txt = "MKD";
		monetaryUnit( 65 ).txt = "MNT";
		monetaryUnit( 66 ).txt = "MUR";
		monetaryUnit( 67 ).txt = "$";
		monetaryUnit( 68 ).txt = "RM";
		monetaryUnit( 69 ).txt = "MT";
		monetaryUnit( 70 ).txt = "$";
		monetaryUnit( 71 ).txt = "NGN";
		monetaryUnit( 72 ).txt = "C$";
		monetaryUnit( 73 ).txt = "kr";
		monetaryUnit( 74 ).txt = "NPR";
		monetaryUnit( 75 ).txt = "$";
		monetaryUnit( 76 ).txt = "OMR";
		monetaryUnit( 77 ).txt = "B/.";
		monetaryUnit( 78 ).txt = "S/.";
		monetaryUnit( 79 ).txt = "Php";
		monetaryUnit( 80 ).txt = "PKR";
		monetaryUnit( 81 ).txt = "PLN";
		monetaryUnit( 82 ).txt = "Gs";
		monetaryUnit( 83 ).txt = "QAR";
		monetaryUnit( 84 ).txt = "lei";
		monetaryUnit( 85 ).txt = "RSD";
		monetaryUnit( 86 ).txt = "RUB";
		monetaryUnit( 87 ).txt = "SAR";
		monetaryUnit( 88 ).txt = "$";
		monetaryUnit( 89 ).txt = "SCR";
		monetaryUnit( 90 ).txt = "kr";
		monetaryUnit( 91 ).txt = "$";
		monetaryUnit( 92 ).txt = "£";
		monetaryUnit( 93 ).txt = "S";
		monetaryUnit( 94 ).txt = "$";
		monetaryUnit( 95 ).txt = "$";
		monetaryUnit( 96 ).txt = "£";
		monetaryUnit( 97 ).txt = "THB";
		monetaryUnit( 98 ).txt = "TRL";
		monetaryUnit( 99 ).txt = "YTL";
		monetaryUnit( 100 ).txt = "TT$";
		monetaryUnit( 101 ).txt = "$";
		monetaryUnit( 102 ).txt = "NT$";
		monetaryUnit( 103 ).txt = "UAH";
		monetaryUnit( 104 ).txt = "$U";
		monetaryUnit( 105 ).txt = "UZS";
		monetaryUnit( 106 ).txt = "Bs";
		monetaryUnit( 107 ).txt = "VND";
		monetaryUnit( 108 ).txt = "$";
		monetaryUnit( 109 ).txt = "YER";
		monetaryUnit( 110 ).txt = "R";
		monetaryUnit( 111 ).txt = "Z$";

		monetaryUnit( 1 ).html = "$";
		monetaryUnit( 2 ).html = "&#x060b;";
		monetaryUnit( 3 ).html = "Lek";
		monetaryUnit( 4 ).html = "&#x0192;";
		monetaryUnit( 5 ).html = "$";
		monetaryUnit( 6 ).html = "$";
		monetaryUnit( 7 ).html = "&#x0192;";
		monetaryUnit( 8 ).html = "&#x043c;&#x0430;&#x043d;";
		monetaryUnit( 9 ).html = "KM";
		monetaryUnit( 10 ).html = "$";
		monetaryUnit( 11 ).html = "&#x043b;&#x0432;";
		monetaryUnit( 12 ).html = "$";
		monetaryUnit( 13 ).html = "$";
		monetaryUnit( 14 ).html = "$b";
		monetaryUnit( 15 ).html = "R$";
		monetaryUnit( 16 ).html = "$";
		monetaryUnit( 17 ).html = "P";
		monetaryUnit( 18 ).html = "p.";
		monetaryUnit( 19 ).html = "BZ$";
		monetaryUnit( 20 ).html = "$";
		monetaryUnit( 21 ).html = "CHF";
		monetaryUnit( 22 ).html = "$";
		monetaryUnit( 23 ).html = "&#x5143;";
		monetaryUnit( 24 ).html = "$";
		monetaryUnit( 25 ).html = "&#x20a1;";
		monetaryUnit( 26 ).html = "&#x20b1;";
		monetaryUnit( 27 ).html = "&#x004b;&#x010d;";
		monetaryUnit( 28 ).html = "kr";
		monetaryUnit( 29 ).html = "RD$";
		monetaryUnit( 30 ).html = "kr";
		monetaryUnit( 31 ).html = "£";
		monetaryUnit( 32 ).html = "&#x20ac;";
		monetaryUnit( 33 ).html = "$";
		monetaryUnit( 34 ).html = "£";
		monetaryUnit( 35 ).html = "¢";
		monetaryUnit( 36 ).html = "£";
		monetaryUnit( 37 ).html = "Q";
		monetaryUnit( 38 ).html = "$";
		monetaryUnit( 39 ).html = "HK$";
		monetaryUnit( 40 ).html = "L";
		monetaryUnit( 41 ).html = "kn";
		monetaryUnit( 42 ).html = "Ft";
		monetaryUnit( 43 ).html = "Rp";
		monetaryUnit( 44 ).html = "&#x20aa;";
		monetaryUnit( 45 ).html = "£";
		monetaryUnit( 46 ).html = "&#x20a8;";
		monetaryUnit( 47 ).html = "&#xfdfc;";
		monetaryUnit( 48 ).html = "kr";
		monetaryUnit( 49 ).html = "£";
		monetaryUnit( 50 ).html = "J$";
		monetaryUnit( 51 ).html = "¥";
		monetaryUnit( 52 ).html = "&#x043b;&#x0432;";
		monetaryUnit( 53 ).html = "&#x17db;";
		monetaryUnit( 54 ).html = "&#x20a9;";
		monetaryUnit( 55 ).html = "&#x20a9;";
		monetaryUnit( 56 ).html = "$";
		monetaryUnit( 57 ).html = "&#x043b;&#x0432;";
		monetaryUnit( 58 ).html = "&#x20ad;";
		monetaryUnit( 59 ).html = "£";
		monetaryUnit( 60 ).html = "&#x20a8;";
		monetaryUnit( 61 ).html = "$";
		monetaryUnit( 62 ).html = "Lt";
		monetaryUnit( 63 ).html = "Ls";
		monetaryUnit( 64 ).html = "&#x0434;&#x0435;&#x043d;";
		monetaryUnit( 65 ).html = "&#x20ae;";
		monetaryUnit( 66 ).html = "&#x20a8;";
		monetaryUnit( 67 ).html = "$";
		monetaryUnit( 68 ).html = "RM";
		monetaryUnit( 69 ).html = "MT";
		monetaryUnit( 70 ).html = "$";
		monetaryUnit( 71 ).html = "&#x20a6;";
		monetaryUnit( 72 ).html = "C$";
		monetaryUnit( 73 ).html = "kr";
		monetaryUnit( 74 ).html = "&#x20a8;";
		monetaryUnit( 75 ).html = "$";
		monetaryUnit( 76 ).html = "&#xfdfc;";
		monetaryUnit( 77 ).html = "B/.";
		monetaryUnit( 78 ).html = "S/.";
		monetaryUnit( 79 ).html = "Php";
		monetaryUnit( 80 ).html = "&#x20a8;";
		monetaryUnit( 81 ).html = "&#x007a;&#x0142;";
		monetaryUnit( 82 ).html = "Gs";
		monetaryUnit( 83 ).html = "&#xfdfc;";
		monetaryUnit( 84 ).html = "lei";
		monetaryUnit( 85 ).html = "&#x0414;&#x0438;&#x043d;&#x002e;";
		monetaryUnit( 86 ).html = "&#x0440;&#x0443;&#x0431;";
		monetaryUnit( 87 ).html = "&#xfdfc;";
		monetaryUnit( 88 ).html = "$";
		monetaryUnit( 89 ).html = "&#x20a8;";
		monetaryUnit( 90 ).html = "kr";
		monetaryUnit( 91 ).html = "$";
		monetaryUnit( 92 ).html = "£";
		monetaryUnit( 93 ).html = "S";
		monetaryUnit( 94 ).html = "$";
		monetaryUnit( 95 ).html = "$";
		monetaryUnit( 96 ).html = "£";
		monetaryUnit( 97 ).html = "&#x0e3f;";
		monetaryUnit( 98 ).html = "&#x20a4;";
		monetaryUnit( 99 ).html = "YTL";
		monetaryUnit( 100 ).html = "TT$";
		monetaryUnit( 101 ).html = "$";
		monetaryUnit( 102 ).html = "NT$";
		monetaryUnit( 103 ).html = "&#x20b4;";
		monetaryUnit( 104 ).html = "$U";
		monetaryUnit( 105 ).html = "&#x043b;&#x0432;";
		monetaryUnit( 106 ).html = "Bs";
		monetaryUnit( 107 ).html = "&#x20ab;";
		monetaryUnit( 108 ).html = "$";
		monetaryUnit( 109 ).html = "&#xfdfc;";
		monetaryUnit( 110 ).html = "R";
		monetaryUnit( 111 ).html = "Z$";
	}

	int
	LookUpSeason(
		std::string const & nameOfSeason,
		std::string const & nameOfReferingObj
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Find the index for the season string provided or else
		//    raise a warning.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		int LookUpSeason;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		if ( SameString( nameOfSeason, "Summer" ) ) {
			LookUpSeason = seasonSummer;
		} else if ( SameString( nameOfSeason, "Winter" ) ) {
			LookUpSeason = seasonWinter;
		} else if ( SameString( nameOfSeason, "Spring" ) ) {
			LookUpSeason = seasonSpring;
		} else if ( SameString( nameOfSeason, "Fall" ) ) {
			LookUpSeason = seasonFall;
		} else if ( SameString( nameOfSeason, "Annual" ) ) {
			LookUpSeason = seasonAnnual;
		} else {
			ShowWarningError( "UtilityCost: Invalid season name " + nameOfSeason + " in: " + nameOfReferingObj );
			ShowContinueError( "  Defaulting to Annual" );
			LookUpSeason = seasonAnnual;
		}
		return LookUpSeason;
	}

	int
	FindTariffIndex(
		std::string const & nameOfTariff,
		std::string const & nameOfReferingObj,
		bool & ErrorsFound,
		std::string const & nameOfCurObj
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Find the index for the tariff string provided or else
		//    raise a warning.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		int FindTariffIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iTariff;
		int found;

		found = 0;
		for ( iTariff = 1; iTariff <= numTariff; ++iTariff ) {
			if ( SameString( nameOfTariff, tariff( iTariff ).tariffName ) ) {
				found = iTariff;
				break;
			}
		}
		if ( found > 0 ) {
			FindTariffIndex = found;
		} else {
			ShowSevereError( nameOfCurObj + "=\"" + nameOfReferingObj + "\" invalid tariff referenced" );
			ShowContinueError( "not found UtilityCost:Tariff=\"" + nameOfTariff + "\"." );
			ErrorsFound = true;
			FindTariffIndex = 0;
		}
		return FindTariffIndex;
	}

	void
	warnIfNativeVarname(
		std::string const & objName,
		int const curTariffIndex,
		bool & ErrorsFound,
		std::string const & curobjName
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   March 2007
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Issue a warning if the variable name (usually the object name) is
		//   one of the names of native variables

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool throwError;

		throwError = false;
		if ( SameString( objName, "TotalEnergy" ) ) throwError = true;
		if ( SameString( objName, "TotalDemand" ) ) throwError = true;
		if ( SameString( objName, "PeakEnergy" ) ) throwError = true;
		if ( SameString( objName, "PeakDemand" ) ) throwError = true;
		if ( SameString( objName, "ShoulderEnergy" ) ) throwError = true;
		if ( SameString( objName, "ShoulderDemand" ) ) throwError = true;
		if ( SameString( objName, "OffPeakEnergy" ) ) throwError = true;
		if ( SameString( objName, "OffPeakDemand" ) ) throwError = true;
		if ( SameString( objName, "MidPeakEnergy" ) ) throwError = true;
		if ( SameString( objName, "MidPeakDemand" ) ) throwError = true;
		if ( SameString( objName, "PeakExceedsOffPeak" ) ) throwError = true;
		if ( SameString( objName, "OffPeakExceedsPeak" ) ) throwError = true;
		if ( SameString( objName, "PeakExceedsMidPeak" ) ) throwError = true;
		if ( SameString( objName, "MidPeakExceedsPeak" ) ) throwError = true;
		if ( SameString( objName, "PeakExceedsShoulder" ) ) throwError = true;
		if ( SameString( objName, "ShoulderExceedsPeak" ) ) throwError = true;
		if ( SameString( objName, "IsWinter" ) ) throwError = true;
		if ( SameString( objName, "IsNotWinter" ) ) throwError = true;
		if ( SameString( objName, "IsSpring" ) ) throwError = true;
		if ( SameString( objName, "IsNotSpring" ) ) throwError = true;
		if ( SameString( objName, "IsSummer" ) ) throwError = true;
		if ( SameString( objName, "IsNotSummer" ) ) throwError = true;
		if ( SameString( objName, "IsAutumn" ) ) throwError = true;
		if ( SameString( objName, "IsNotAutumn" ) ) throwError = true;
		if ( SameString( objName, "PeakAndShoulderEnergy" ) ) throwError = true;
		if ( SameString( objName, "PeakAndShoulderDemand" ) ) throwError = true;
		if ( SameString( objName, "PeakAndMidPeakEnergy" ) ) throwError = true;
		if ( SameString( objName, "PeakAndMidPeakDemand" ) ) throwError = true;
		if ( SameString( objName, "ShoulderAndOffPeakEnergy" ) ) throwError = true;
		if ( SameString( objName, "ShoulderAndOffPeakDemand" ) ) throwError = true;
		if ( SameString( objName, "PeakAndOffPeakEnergy" ) ) throwError = true;
		if ( SameString( objName, "PeakAndOffPeakDemand" ) ) throwError = true;
		if ( SameString( objName, "RealTimePriceCosts" ) ) throwError = true;
		if ( SameString( objName, "AboveCustomerBaseCosts" ) ) throwError = true;
		if ( SameString( objName, "BelowCustomerBaseCosts" ) ) throwError = true;
		if ( SameString( objName, "AboveCustomerBaseEnergy" ) ) throwError = true;
		if ( SameString( objName, "BelowCustomerBaseEnergy" ) ) throwError = true;
		if ( SameString( objName, "EnergyCharges" ) ) throwError = true;
		if ( SameString( objName, "DemandCharges" ) ) throwError = true;
		if ( SameString( objName, "ServiceCharges" ) ) throwError = true;
		if ( SameString( objName, "Basis" ) ) throwError = true;
		if ( SameString( objName, "Surcharges" ) ) throwError = true;
		if ( SameString( objName, "Adjustments" ) ) throwError = true;
		if ( SameString( objName, "Subtotal" ) ) throwError = true;
		if ( SameString( objName, "Taxes" ) ) throwError = true;
		if ( SameString( objName, "Total" ) ) throwError = true;
		if ( throwError ) {
			ErrorsFound = true;
			if ( curTariffIndex >= 1 && curTariffIndex <= numTariff ) {
				ShowSevereError( "UtilityCost:Tariff=\"" + tariff( curTariffIndex ).tariffName + "\" invalid referenced name" );
				ShowContinueError( curobjName + "=\"" + objName + "\" You cannot name an object using the same name as a native variable." );
			} else {
				ShowSevereError( curobjName + "=\"" + objName + "\" You cannot name an object using the same name as a native variable." );
			}
		}
	}

	int
	AssignVariablePt(
		std::string const & stringIn,
		bool const flagIfNotNumeric,
		int const useOfVar,
		int const varSpecific,
		int const econObjKind,
		int const objIndex,
		int const tariffPt
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   If the string is not numeric, check if it is a valid string to use as
		//   a variable name. Check if name has been used before and if not create
		//   the variable using the string as its name.
		//   Return the index of the variable.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		int AssignVariablePt;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string inNoSpaces;
		int found;
		int iVar;

		if ( flagIfNotNumeric && ( len( stringIn ) >= 1 ) ) {
			inNoSpaces = RemoveSpaces( stringIn );
			found = 0;
			if ( allocated( econVar ) ) {
				for ( iVar = 1; iVar <= numEconVar; ++iVar ) {
					if ( econVar( iVar ).tariffIndx == tariffPt ) {
						if ( SameString( econVar( iVar ).name, inNoSpaces ) ) {
							found = iVar;
							break;
						}
					}
				}
			}
			if ( found > 0 ) {
				AssignVariablePt = found;
				if ( econVar( found ).kindOfObj == 0 ) {
					econVar( found ).kindOfObj = econObjKind;
					if ( econVar( found ).index == 0 ) econVar( found ).index = objIndex;
				}
			} else {
				incrementEconVar();
				econVar( numEconVar ).name = inNoSpaces;
				econVar( numEconVar ).kindOfObj = econObjKind;
				econVar( numEconVar ).index = objIndex;
				AssignVariablePt = numEconVar;
			}
			// now set the flag for the type of usage the variable has
			if ( useOfVar == varIsArgument ) {
				econVar( AssignVariablePt ).isArgument = true;
			} else if ( useOfVar == varIsAssigned ) {
				econVar( AssignVariablePt ).isAssigned = true;
			}
			econVar( AssignVariablePt ).tariffIndx = tariffPt;
			// if the user defines the UtilityCost:Computation then this is called when reading the
			// UtilityCost:Tariff with varNotYetDefined but they are already defined because
			// the subroutine CreateCategoryNativeVariables has already been called.
			if ( ! ( ( varSpecific == varNotYetDefined ) && ( econVar( AssignVariablePt ).specific >= catEnergyCharges ) ) ) {
				econVar( AssignVariablePt ).specific = varSpecific;
			}
		} else { //if the string was numeric return a zero
			AssignVariablePt = 0;
		}
		return AssignVariablePt;
	}

	void
	incrementEconVar()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Increment the Increase the size of the

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static int sizeIncrement( 100 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		if ( ! allocated( econVar ) ) {
			econVar.allocate( sizeIncrement );
			sizeEconVar = sizeIncrement;
			numEconVar = 1;
		} else {
			++numEconVar;
			// if larger than current size grow the array
			if ( numEconVar > sizeEconVar ) {
				econVar.redimension( sizeEconVar += sizeIncrement );
			}
		}
		// initialize new record) //Autodesk Most of these match default initialization so not needed
		econVar( numEconVar ).name = "";
		econVar( numEconVar ).tariffIndx = 0;
		econVar( numEconVar ).kindOfObj = 0;
		econVar( numEconVar ).index = 0;
		econVar( numEconVar ).values = 0.0;
		econVar( numEconVar ).isArgument = false;
		econVar( numEconVar ).isAssigned = false;
		econVar( numEconVar ).specific = varNotYetDefined;
//		econVar( numEconVar ).values = 0.0; //Autodesk Already initialized above
		//Autodesk Don't initialize cntMeDependOn
		econVar( numEconVar ).Operator = 0;
		econVar( numEconVar ).firstOperand = 1; //Autodesk Default initialization sets this to 0
		econVar( numEconVar ).lastOperand = 0;
		econVar( numEconVar ).activeNow = false;
		econVar( numEconVar ).isEvaluated = false;
		//Autodesk Don't initialize isReported
		//Autodesk Don't initialize varUnitType
	}

	void
	incrementSteps()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   June 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Increment the step array counter and if
		//   necessary increase the size of the array.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static int sizeIncrement( 100 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		if ( ! allocated( steps ) ) {
			steps.allocate( sizeIncrement );
			sizeSteps = sizeIncrement;
			numSteps = 1;
		} else {
			++numSteps;
			// if larger than current size grow the array
			if ( numSteps > sizeSteps ) {
				steps.redimension( sizeSteps += sizeIncrement );
			}
		}
		// initialize new record
		steps( numSteps ) = 0;
	}

	std::string
	RemoveSpaces( std::string const & StringIn )
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Return the string with all spaces removed.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		std::string StringOut;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		bool foundSpaces = false;
		for ( std::string::size_type iString = 0; iString < len( StringIn ); ++iString ) {
			if ( StringIn[ iString ] != ' ' ) {
				StringOut += StringIn[ iString ];
			} else {
				foundSpaces = true;
			}
		}
		if ( foundSpaces ) {
			ShowWarningError( "UtilityCost: Spaces were removed from the variable=\"" + StringIn + "\"." );
			ShowContinueError( "...Resultant variable=\"" + StringOut + "\"." );
		}
		return StringOut;
	}

	void
	CreateCategoryNativeVariables()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    For each tariff create variables that are used for the
		//    categories (i.e., EnergyCharges).

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iTariff;

		for ( iTariff = 1; iTariff <= numTariff; ++iTariff ) {
			// category variables first
			tariff( iTariff ).ptEnergyCharges = AssignVariablePt( "EnergyCharges", true, varIsAssigned, catEnergyCharges, kindCategory, 0, iTariff );
			tariff( iTariff ).firstCategory = numEconVar;
			tariff( iTariff ).ptDemandCharges = AssignVariablePt( "DemandCharges", true, varIsAssigned, catDemandCharges, kindCategory, 0, iTariff );
			tariff( iTariff ).ptServiceCharges = AssignVariablePt( "ServiceCharges", true, varIsAssigned, catServiceCharges, kindCategory, 0, iTariff );
			tariff( iTariff ).ptBasis = AssignVariablePt( "Basis", true, varIsAssigned, catBasis, kindCategory, 0, iTariff );
			tariff( iTariff ).ptAdjustment = AssignVariablePt( "Adjustment", true, varIsAssigned, catAdjustment, kindCategory, 0, iTariff );
			tariff( iTariff ).ptSurcharge = AssignVariablePt( "Surcharge", true, varIsAssigned, catSurcharge, kindCategory, 0, iTariff );
			tariff( iTariff ).ptSubtotal = AssignVariablePt( "Subtotal", true, varIsAssigned, catSubtotal, kindCategory, 0, iTariff );
			tariff( iTariff ).ptTaxes = AssignVariablePt( "Taxes", true, varIsAssigned, catTaxes, kindCategory, 0, iTariff );
			tariff( iTariff ).ptTotal = AssignVariablePt( "Total", true, varIsAssigned, catTotal, kindCategory, 0, iTariff );
			tariff( iTariff ).ptNotIncluded = AssignVariablePt( "NotIncluded", true, varIsAssigned, catNotIncluded, kindCategory, 0, iTariff );
			tariff( iTariff ).lastCategory = numEconVar;
			// category variables first
			tariff( iTariff ).nativeTotalEnergy = AssignVariablePt( "TotalEnergy", true, varIsArgument, nativeTotalEnergy, kindNative, 0, iTariff );
			tariff( iTariff ).firstNative = numEconVar;
			tariff( iTariff ).nativeTotalDemand = AssignVariablePt( "TotalDemand", true, varIsArgument, nativeTotalDemand, kindNative, 0, iTariff );
			tariff( iTariff ).nativePeakEnergy = AssignVariablePt( "PeakEnergy", true, varIsArgument, nativePeakEnergy, kindNative, 0, iTariff );
			tariff( iTariff ).nativePeakDemand = AssignVariablePt( "PeakDemand", true, varIsArgument, nativePeakDemand, kindNative, 0, iTariff );
			tariff( iTariff ).nativeShoulderEnergy = AssignVariablePt( "ShoulderEnergy", true, varIsArgument, nativeShoulderEnergy, kindNative, 0, iTariff );
			tariff( iTariff ).nativeShoulderDemand = AssignVariablePt( "ShoulderDemand", true, varIsArgument, nativeShoulderDemand, kindNative, 0, iTariff );
			tariff( iTariff ).nativeOffPeakEnergy = AssignVariablePt( "OffPeakEnergy", true, varIsArgument, nativeOffPeakEnergy, kindNative, 0, iTariff );
			tariff( iTariff ).nativeOffPeakDemand = AssignVariablePt( "OffPeakDemand", true, varIsArgument, nativeOffPeakDemand, kindNative, 0, iTariff );
			tariff( iTariff ).nativeMidPeakEnergy = AssignVariablePt( "MidPeakEnergy", true, varIsArgument, nativeMidPeakEnergy, kindNative, 0, iTariff );
			tariff( iTariff ).nativeMidPeakDemand = AssignVariablePt( "MidPeakDemand", true, varIsArgument, nativeMidPeakDemand, kindNative, 0, iTariff );
			tariff( iTariff ).nativePeakExceedsOffPeak = AssignVariablePt( "PeakExceedsOffPeak", true, varIsArgument, nativePeakExceedsOffPeak, kindNative, 0, iTariff );
			tariff( iTariff ).nativeOffPeakExceedsPeak = AssignVariablePt( "OffPeakExceedsPeak", true, varIsArgument, nativeOffPeakExceedsPeak, kindNative, 0, iTariff );
			tariff( iTariff ).nativePeakExceedsMidPeak = AssignVariablePt( "PeakExceedsMidPeak", true, varIsArgument, nativePeakExceedsMidPeak, kindNative, 0, iTariff );
			tariff( iTariff ).nativeMidPeakExceedsPeak = AssignVariablePt( "MidPeakExceedsPeak", true, varIsArgument, nativeMidPeakExceedsPeak, kindNative, 0, iTariff );
			tariff( iTariff ).nativePeakExceedsShoulder = AssignVariablePt( "PeakExceedsShoulder", true, varIsArgument, nativePeakExceedsShoulder, kindNative, 0, iTariff );
			tariff( iTariff ).nativeShoulderExceedsPeak = AssignVariablePt( "ShoulderExceedsPeak", true, varIsArgument, nativeShoulderExceedsPeak, kindNative, 0, iTariff );
			tariff( iTariff ).nativeIsWinter = AssignVariablePt( "IsWinter", true, varIsArgument, nativeIsWinter, kindNative, 0, iTariff );
			tariff( iTariff ).nativeIsNotWinter = AssignVariablePt( "IsNotWinter", true, varIsArgument, nativeIsNotWinter, kindNative, 0, iTariff );
			tariff( iTariff ).nativeIsSpring = AssignVariablePt( "IsSpring", true, varIsArgument, nativeIsSpring, kindNative, 0, iTariff );
			tariff( iTariff ).nativeIsNotSpring = AssignVariablePt( "IsNotSpring", true, varIsArgument, nativeIsNotSpring, kindNative, 0, iTariff );
			tariff( iTariff ).nativeIsSummer = AssignVariablePt( "IsSummer", true, varIsArgument, nativeIsSummer, kindNative, 0, iTariff );
			tariff( iTariff ).nativeIsNotSummer = AssignVariablePt( "IsNotSummer", true, varIsArgument, nativeIsNotSummer, kindNative, 0, iTariff );
			tariff( iTariff ).nativeIsAutumn = AssignVariablePt( "IsAutumn", true, varIsArgument, nativeIsAutumn, kindNative, 0, iTariff );
			tariff( iTariff ).nativeIsNotAutumn = AssignVariablePt( "IsNotAutumn", true, varIsArgument, nativeIsNotAutumn, kindNative, 0, iTariff );

			tariff( iTariff ).nativePeakAndShoulderEnergy = AssignVariablePt( "PeakAndShoulderEnergy", true, varIsArgument, nativePeakAndShoulderEnergy, kindNative, 0, iTariff );
			tariff( iTariff ).nativePeakAndShoulderDemand = AssignVariablePt( "PeakAndShoulderDemand", true, varIsArgument, nativePeakAndShoulderDemand, kindNative, 0, iTariff );
			tariff( iTariff ).nativePeakAndMidPeakEnergy = AssignVariablePt( "PeakAndMidPeakEnergy", true, varIsArgument, nativePeakAndMidPeakEnergy, kindNative, 0, iTariff );
			tariff( iTariff ).nativePeakAndMidPeakDemand = AssignVariablePt( "PeakAndMidPeakDemand", true, varIsArgument, nativePeakAndMidPeakDemand, kindNative, 0, iTariff );
			tariff( iTariff ).nativeShoulderAndOffPeakEnergy = AssignVariablePt( "ShoulderAndOffPeakEnergy", true, varIsArgument, nativeShoulderAndOffPeakEnergy, kindNative, 0, iTariff );
			tariff( iTariff ).nativeShoulderAndOffPeakDemand = AssignVariablePt( "ShoulderAndOffPeakDemand", true, varIsArgument, nativeShoulderAndOffPeakDemand, kindNative, 0, iTariff );
			tariff( iTariff ).nativePeakAndOffPeakEnergy = AssignVariablePt( "PeakAndOffPeakEnergy", true, varIsArgument, nativePeakAndOffPeakEnergy, kindNative, 0, iTariff );
			tariff( iTariff ).nativePeakAndOffPeakDemand = AssignVariablePt( "PeakAndOffPeakDemand", true, varIsArgument, nativePeakAndOffPeakDemand, kindNative, 0, iTariff );
			tariff( iTariff ).nativeRealTimePriceCosts = AssignVariablePt( "RealTimePriceCosts", true, varIsArgument, nativeRealTimePriceCosts, kindNative, 0, iTariff );
			tariff( iTariff ).nativeAboveCustomerBaseCosts = AssignVariablePt( "AboveCustomerBaseCosts", true, varIsArgument, nativeAboveCustomerBaseCosts, kindNative, 0, iTariff );
			tariff( iTariff ).nativeBelowCustomerBaseCosts = AssignVariablePt( "BelowCustomerBaseCosts", true, varIsArgument, nativeBelowCustomerBaseCosts, kindNative, 0, iTariff );
			tariff( iTariff ).nativeAboveCustomerBaseEnergy = AssignVariablePt( "AboveCustomerBaseEnergy", true, varIsArgument, nativeAboveCustomerBaseEnergy, kindNative, 0, iTariff );
			tariff( iTariff ).nativeBelowCustomerBaseEnergy = AssignVariablePt( "BelowCustomerBaseEnergy", true, varIsArgument, nativeBelowCustomerBaseEnergy, kindNative, 0, iTariff );
			tariff( iTariff ).lastNative = numEconVar;
		}
	}

	int
	lookupOperator( std::string const & opString )
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		int lookupOperator;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		if ( SameString( opString, "Sum" ) ) {
			lookupOperator = opSUM;
		} else if ( SameString( opString, "MULTIPLY" ) ) {
			lookupOperator = opMULTIPLY;
		} else if ( SameString( opString, "MULT" ) ) {
			lookupOperator = opMULTIPLY;
		} else if ( SameString( opString, "SUBTRACT" ) ) {
			lookupOperator = opSUBTRACT;
		} else if ( SameString( opString, "SUBT" ) ) {
			lookupOperator = opSUBTRACT;
		} else if ( SameString( opString, "DIVIDE" ) ) {
			lookupOperator = opDIVIDE;
		} else if ( SameString( opString, "DIV" ) ) {
			lookupOperator = opDIVIDE;
		} else if ( SameString( opString, "ABSOLUTE" ) ) {
			lookupOperator = opABSOLUTE;
		} else if ( SameString( opString, "ABS" ) ) {
			lookupOperator = opABSOLUTE;
		} else if ( SameString( opString, "INTEGER" ) ) {
			lookupOperator = opINTEGER;
		} else if ( SameString( opString, "INT" ) ) {
			lookupOperator = opINTEGER;
		} else if ( SameString( opString, "SIGN" ) ) {
			lookupOperator = opSIGN;
		} else if ( SameString( opString, "ROUND" ) ) {
			lookupOperator = opROUND;
		} else if ( SameString( opString, "Maximum" ) ) {
			lookupOperator = opMAXIMUM;
		} else if ( SameString( opString, "MAX" ) ) {
			lookupOperator = opMAXIMUM;
		} else if ( SameString( opString, "MINIMUM" ) ) {
			lookupOperator = opMINIMUM;
		} else if ( SameString( opString, "MIN" ) ) {
			lookupOperator = opMINIMUM;
		} else if ( SameString( opString, "EXCEEDS" ) ) {
			lookupOperator = opEXCEEDS;
		} else if ( SameString( opString, "ANNUALMINIMUM" ) ) {
			lookupOperator = opANNUALMINIMUM;
		} else if ( SameString( opString, "ANMIN" ) ) {
			lookupOperator = opANNUALMINIMUM;
		} else if ( SameString( opString, "ANNUALMAXIMUM" ) ) {
			lookupOperator = opANNUALMAXIMUM;
		} else if ( SameString( opString, "ANMAX" ) ) {
			lookupOperator = opANNUALMAXIMUM;
		} else if ( SameString( opString, "ANNUALSUM" ) ) {
			lookupOperator = opANNUALSUM;
		} else if ( SameString( opString, "ANSUM" ) ) {
			lookupOperator = opANNUALSUM;
		} else if ( SameString( opString, "ANNUALAVERAGE" ) ) {
			lookupOperator = opANNUALAVERAGE;
		} else if ( SameString( opString, "ANAVG" ) ) {
			lookupOperator = opANNUALAVERAGE;
		} else if ( SameString( opString, "ANNUALOR" ) ) {
			lookupOperator = opANNUALOR;
		} else if ( SameString( opString, "ANOR" ) ) {
			lookupOperator = opANNUALOR;
		} else if ( SameString( opString, "ANNUALAND" ) ) {
			lookupOperator = opANNUALAND;
		} else if ( SameString( opString, "ANAND" ) ) {
			lookupOperator = opANNUALAND;
		} else if ( SameString( opString, "ANNUALMAXIMUMZERO" ) ) {
			lookupOperator = opANNUALMAXIMUMZERO;
		} else if ( SameString( opString, "ANMAXZ" ) ) {
			lookupOperator = opANNUALMAXIMUMZERO;
		} else if ( SameString( opString, "ANNUALMINIMUMZERO" ) ) {
			lookupOperator = opANNUALMINIMUMZERO;
		} else if ( SameString( opString, "ANMINZ" ) ) {
			lookupOperator = opANNUALMINIMUMZERO;
		} else if ( SameString( opString, "IF" ) ) {
			lookupOperator = opIF;
		} else if ( SameString( opString, "GREATERTHAN" ) ) {
			lookupOperator = opGREATERTHAN;
		} else if ( SameString( opString, "GT" ) ) {
			lookupOperator = opGREATERTHAN;
		} else if ( SameString( opString, "GREATEREQUAL" ) ) {
			lookupOperator = opGREATEREQUAL;
		} else if ( SameString( opString, "GE" ) ) {
			lookupOperator = opGREATEREQUAL;
		} else if ( SameString( opString, "LESSTHAN" ) ) {
			lookupOperator = opLESSTHAN;
		} else if ( SameString( opString, "LT" ) ) {
			lookupOperator = opLESSTHAN;
		} else if ( SameString( opString, "LESSEQUAL" ) ) {
			lookupOperator = opLESSEQUAL;
		} else if ( SameString( opString, "LE" ) ) {
			lookupOperator = opLESSEQUAL;
		} else if ( SameString( opString, "EQUAL" ) ) {
			lookupOperator = opEQUAL;
		} else if ( SameString( opString, "EQ" ) ) {
			lookupOperator = opEQUAL;
		} else if ( SameString( opString, "NOTEQUAL" ) ) {
			lookupOperator = opNOTEQUAL;
		} else if ( SameString( opString, "NE" ) ) {
			lookupOperator = opNOTEQUAL;
		} else if ( SameString( opString, "AND" ) ) {
			lookupOperator = opAND;
		} else if ( SameString( opString, "OR" ) ) {
			lookupOperator = opOR;
		} else if ( SameString( opString, "NOT" ) ) {
			lookupOperator = opNOT;
		} else if ( SameString( opString, "FROM" ) ) {
			lookupOperator = opNOOP;
		} else if ( SameString( opString, "ADD" ) ) {
			lookupOperator = opADD;
		} else {
			lookupOperator = 0;
		}
		return lookupOperator;
	}

	//======================================================================================================================
	//======================================================================================================================

	//    DEFAULT COMPUTATION RELATED ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	CreateDefaultComputation()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   June 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		//    For most tariffs defined in EnergyPlus no specific
		//    ECONOMICS:COMPUTATION will be entered. In that case,
		//    a default sequence of computation steps needs to be
		//    created.  This routine creates the default
		//    computation steps.
		//    Object           Fields         Depend On Fields
		//    Qualify          namePt         sourcePt
		//                                    thresholdPt
		//    Charge:Simple    namePt         sourcePt
		//                     categoryPt     costPerPt
		//    Charge:Block     namePt         sourcePt
		//                     categoryPt     blkSzMultPt
		//                     remainingPt    blkSzPt
		//                                    blkCostPt
		//    Ratchet          namePt         baselinePt
		//                                    adjustmentPt
		//                                    multiplierPt
		//                                    offsetPt
		//    These will be formed into expressions that look like
		//      namePt NOOP sourcePt thresholdPt
		//    The different Charges are combined using the SUM operation
		//    into categories.
		//      category SUM chg1Name chg2Name chg3Name
		//    Since the dependency array has one target and multiple
		//    parameters, remainingPt is shown as a seperate equation that
		//    depends on namePt for Charge:Block. The equation will not be
		//    displayed or processed except in the sort.
		//      remainingPt NOOP namePt
		//    Many lines of the computation will include just the name of
		//    a single variable which triggers the calculation for that
		//    charge, ratchet or qualify.
		//      chg1Name
		//    It is also possible that two variables referenced within one
		//    object could include a dependancy relationship also. For
		//    example, the blkSzPt could be calculated using the same sourePt
		//    in Charge:Block.

		// METHODOLOGY EMPLOYED:
		//    Since some ECONOMCIS:* objects depend on other variables
		//    first must create the order of when to perform the
		//    computations. First a dependancy table is created that
		//    indicates what variables are dependant on other variables.
		//    A directed acyclic graph (DAG) describes the general
		//    problem which is usually solved using a topological
		//    sorting algorithm.
		//    Each line/step is generated and put into the depend
		//    array. Also in the array are counts of how many items it
		//    depends on and a list of entries that are dependant on that
		//    line.

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputReportTabular::IntToStr;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int iTariff;
		int iVar;
		int jVar;
		int kObj;
		int mBlock;
		int kOperand;
		int curBasis;
		int curSubtotal;
		int curTotal;
		int curObject;
		int numNoDepend;
		int referVar;
		int loopCount;
		bool remainingVarFlag;
		int remainPt;

		// for each tariff that does not have a UtilityCost:Computation object go through the variables
		for ( iTariff = 1; iTariff <= numTariff; ++iTariff ) {
			if ( ! computation( iTariff ).isUserDef ) {
				// clear all variables so that they are not active
				for ( jVar = 1; jVar <= numEconVar; ++jVar ) {
					econVar( jVar ).activeNow = false;
				}
				//make all native variables active
				for ( jVar = tariff( iTariff ).firstNative; jVar <= tariff( iTariff ).lastNative; ++jVar ) {
					econVar( jVar ).activeNow = true;
				}
				//"clear" the dependOn array
				numOperand = 0;
				//Define the preset equations (category sumation)
				curTotal = tariff( iTariff ).ptTotal;
				curSubtotal = tariff( iTariff ).ptSubtotal;
				curBasis = tariff( iTariff ).ptBasis;
				// total SUM subtotal taxes
				econVar( curTotal ).Operator = opSUM;
				econVar( curTotal ).activeNow = true;
				addOperand( curTotal, curSubtotal );
				addOperand( curTotal, tariff( iTariff ).ptTaxes );
				// subtotal SUM basis adjustments surcharges
				econVar( curSubtotal ).Operator = opSUM;
				econVar( curSubtotal ).activeNow = true;
				addOperand( curSubtotal, curBasis );
				addOperand( curSubtotal, tariff( iTariff ).ptAdjustment );
				addOperand( curSubtotal, tariff( iTariff ).ptSurcharge );
				// basis SUM EnergyCharges DemandCharges ServiceCharges
				econVar( curBasis ).Operator = opSUM;
				econVar( curBasis ).activeNow = true;
				addOperand( curBasis, tariff( iTariff ).ptEnergyCharges );
				addOperand( curBasis, tariff( iTariff ).ptDemandCharges );
				addOperand( curBasis, tariff( iTariff ).ptServiceCharges );
				//set up the equations for other objects
				addChargesToOperand( iTariff, tariff( iTariff ).ptEnergyCharges );
				addChargesToOperand( iTariff, tariff( iTariff ).ptDemandCharges );
				addChargesToOperand( iTariff, tariff( iTariff ).ptServiceCharges );
				addChargesToOperand( iTariff, tariff( iTariff ).ptAdjustment );
				addChargesToOperand( iTariff, tariff( iTariff ).ptSurcharge );
				addChargesToOperand( iTariff, tariff( iTariff ).ptTaxes );
				//add the real time pricing to the energy charges
				if ( tariff( iTariff ).chargeSchIndex != 0 ) {
					addOperand( tariff( iTariff ).ptEnergyCharges, tariff( iTariff ).nativeRealTimePriceCosts );
				}
				//now add equations with NOOP to represent each object with its
				//dependancies
				// Qualify
				for ( kObj = 1; kObj <= numQualify; ++kObj ) {
					if ( qualify( kObj ).tariffIndx == iTariff ) {
						curObject = qualify( kObj ).namePt;
						econVar( curObject ).Operator = opNOOP;
						econVar( curObject ).activeNow = true;
						addOperand( curObject, qualify( kObj ).sourcePt );
						addOperand( curObject, qualify( kObj ).thresholdPt );
					}
				}
				// Ratchet
				for ( kObj = 1; kObj <= numRatchet; ++kObj ) {
					if ( ratchet( kObj ).tariffIndx == iTariff ) {
						curObject = ratchet( kObj ).namePt;
						econVar( curObject ).Operator = opNOOP;
						econVar( curObject ).activeNow = true;
						addOperand( curObject, ratchet( kObj ).baselinePt );
						addOperand( curObject, ratchet( kObj ).adjustmentPt );
						addOperand( curObject, ratchet( kObj ).multiplierPt );
						addOperand( curObject, ratchet( kObj ).offsetPt );
					}
				}
				// ChargeSimple
				for ( kObj = 1; kObj <= numChargeSimple; ++kObj ) {
					if ( chargeSimple( kObj ).tariffIndx == iTariff ) {
						curObject = chargeSimple( kObj ).namePt;
						econVar( curObject ).Operator = opNOOP;
						econVar( curObject ).activeNow = true;
						addOperand( curObject, chargeSimple( kObj ).sourcePt );
						addOperand( curObject, chargeSimple( kObj ).costPerPt );
					}
				}
				// ChargeBlock
				for ( kObj = 1; kObj <= numChargeBlock; ++kObj ) {
					if ( chargeBlock( kObj ).tariffIndx == iTariff ) {
						curObject = chargeBlock( kObj ).namePt;
						econVar( curObject ).Operator = opNOOP;
						econVar( curObject ).activeNow = true;
						addOperand( curObject, chargeBlock( kObj ).sourcePt );
						addOperand( curObject, chargeBlock( kObj ).blkSzMultPt );
						for ( mBlock = 1; mBlock <= chargeBlock( kObj ).numBlk; ++mBlock ) {
							addOperand( curObject, chargeBlock( kObj ).blkSzPt( mBlock ) );
							addOperand( curObject, chargeBlock( kObj ).blkCostPt( mBlock ) );
						}
						// now add a new "equation" for dependency of remainingPt on namePt
						remainPt = chargeBlock( kObj ).remainingPt;
						if ( remainPt > 0 ) {
							econVar( remainPt ).Operator = opNOOP;
							econVar( remainPt ).activeNow = true;
							addOperand( remainPt, curObject );
						}
					}
				}
				// Economic:Variable
				//make all of the user defined variables as active
				for ( iVar = 1; iVar <= numEconVar; ++iVar ) {
					if ( econVar( iVar ).tariffIndx == iTariff ) {
						if ( econVar( iVar ).kindOfObj == kindVariable ) {
							econVar( iVar ).activeNow = true;
						}
					}
				}
				// make sure no compuation is already user defined
				if ( computation( iTariff ).firstStep != 0 ) {
					ShowWarningError( "In UtilityCost:Tariff: Overwriting user defined tariff " + tariff( iTariff ).tariffName );
				}
				//initialize the computation
				computation( iTariff ).computeName = "Autogenerated - " + tariff( iTariff ).tariffName;
				computation( iTariff ).firstStep = numSteps + 1;
				computation( iTariff ).lastStep = -1; //this will be incremented by addStep
				computation( iTariff ).isUserDef = false;
				// now all "equations" are defined, treat the variables with the list
				// of dependancies as a directed acyclic graph and use "count down" algorithm
				// to do a topological sort of the variables into the order for computation
				// First, clear the counters
				for ( jVar = 1; jVar <= numEconVar; ++jVar ) {
					econVar( jVar ).cntMeDependOn = 0;
				}
				// Second, add up the number of dependancies on each variable
				for ( iVar = 1; iVar <= numEconVar; ++iVar ) {
					if ( econVar( iVar ).activeNow ) {
						if ( econVar( iVar ).lastOperand >= econVar( iVar ).firstOperand ) {
							econVar( iVar ).cntMeDependOn = 1 + econVar( iVar ).lastOperand - econVar( iVar ).firstOperand;
						}
					}
				}
				// Third, start removing items with zero connections and decrease each
				//   counter.
				numNoDepend = -1;
				loopCount = 0;
				while ( ( numNoDepend != 0 ) || ( loopCount > 100000 ) ) {
					numNoDepend = 0;
					for ( iVar = 1; iVar <= numEconVar; ++iVar ) {
						if ( econVar( iVar ).activeNow ) {
							//find a variable that has no more dangling dependancies
							if ( econVar( iVar ).cntMeDependOn == 0 ) {
								// If the variable is a native variable then
								//IF (econVar(iVar)%kindOfObj .NE. kindNative) THEN
								if ( ( econVar( iVar ).kindOfObj != kindNative ) && ( econVar( iVar ).kindOfObj != kindVariable ) ) {
									if ( econVar( iVar ).lastOperand >= econVar( iVar ).firstOperand ) {
										//transfer variables and operator to the computation and list of steps
										// go through the operands backwards (end of line is evaluated first)
										for ( kOperand = econVar( iVar ).lastOperand; kOperand >= econVar( iVar ).firstOperand; --kOperand ) {
											incrementSteps();
											steps( numSteps ) = operand( kOperand );
										}
										// append the operator (either SUM or NOOP)
										incrementSteps();
										steps( numSteps ) = econVar( iVar ).Operator;
										// append the variable itself
										incrementSteps();
										steps( numSteps ) = iVar;
										//at the end of the line show a zero to clear the stack
										incrementSteps();
										steps( numSteps ) = 0;
									}
								}
								// go through each other variable looking for places where this variable is used
								// and decrement their counters.
								for ( jVar = 1; jVar <= numEconVar; ++jVar ) {
									if ( econVar( jVar ).activeNow ) {
										for ( kOperand = econVar( jVar ).firstOperand; kOperand <= econVar( jVar ).lastOperand; ++kOperand ) {
											referVar = operand( kOperand );
											if ( iVar == referVar ) {
												--econVar( jVar ).cntMeDependOn;
												// for each variable that has been decremented to zero increment the counter
												if ( econVar( jVar ).cntMeDependOn <= 0 ) {
													++numNoDepend;
												}
											}
										}
									}
								}
								//make the variable inactive
								econVar( iVar ).activeNow = false;
							}
						}
					}
					++loopCount;
				}
				if ( loopCount > 100000 ) {
					ShowWarningError( "UtilityCost:Tariff: Loop count exceeded when counting dependancies in tariff: " + tariff( iTariff ).tariffName );
				}
				//make sure that all variables associated with the tariff are included
				remainingVarFlag = false;
				for ( iVar = 1; iVar <= numEconVar; ++iVar ) {
					if ( econVar( iVar ).activeNow ) {
						remainingVarFlag = true;
					}
				}
				if ( remainingVarFlag ) {
					ShowWarningError( "CreateDefaultComputation: In UtilityCost:Computation: Circular or invalid dependencies found in tariff: " + tariff( iTariff ).tariffName );
					ShowContinueError( "  UtilityCost variables that may have invalid dependencies and the variables they are dependant on." );
					for ( iVar = 1; iVar <= numEconVar; ++iVar ) {
						if ( econVar( iVar ).tariffIndx == iTariff ) {
							if ( econVar( iVar ).activeNow ) {
								ShowContinueError( "     " + econVar( iVar ).name );
								for ( kOperand = econVar( iVar ).firstOperand; kOperand <= econVar( iVar ).lastOperand; ++kOperand ) {
									ShowContinueError( "        ->  " + econVar( operand( kOperand ) ).name );
								}
							}
						}
					}
				}
				//set the end of the computations
				computation( iTariff ).lastStep = numSteps;
				if ( computation( iTariff ).firstStep >= computation( iTariff ).lastStep ) {
					computation( iTariff ).firstStep = 0;
					computation( iTariff ).lastStep = -1;
					ShowWarningError( "CreateDefaultComputation: In UtilityCost:Computation: No lines in the auto generated computation can be interpreted in tariff: " + tariff( iTariff ).tariffName );
				}
			}
		}
	}

	void
	addOperand(
		int const varMe,
		int const varOperand
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Used by CreateDefaultComputation to create the dependancy
		//   relationship in the EconVar array

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int sizeIncrement( 100 );
		static int prevVarMe( 0 );

		if ( varOperand != 0 ) {
			//increment the numOperand and allocate/reallocate the array
			//if necessary
			if ( ! allocated( operand ) ) {
				operand.allocate( sizeIncrement );
				sizeOperand = sizeIncrement;
				numOperand = 1;
			} else {
				++numOperand;
				// if larger than current size grow the array
				if ( numOperand > sizeOperand ) {
					operand.redimension( sizeOperand += sizeIncrement );
				}
			}
			//now add the dependancy relationship
			operand( numOperand ) = varOperand;
			econVar( varMe ).lastOperand = numOperand;
			//if it is the first time addOperand was called with the varMe value
			//then set the first pointer as well
			if ( varMe != prevVarMe ) {
				econVar( varMe ).firstOperand = numOperand;
				prevVarMe = varMe;
			}
		}
	}

	void
	addChargesToOperand(
		int const curTariff,
		int const curPointer
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Used by CreateDefaultComputation to create the "equation"
		//   for the categories that are summations of ECONOMICS:CHARGES:BLOCK
		//   and ECONOMICS:CHARGES:SIMPLE

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int kObj;

		econVar( curPointer ).Operator = opSUM;
		econVar( curPointer ).activeNow = true;
		for ( kObj = 1; kObj <= numChargeSimple; ++kObj ) {
			if ( chargeSimple( kObj ).tariffIndx == curTariff ) {
				if ( chargeSimple( kObj ).categoryPt == curPointer ) {
					addOperand( curPointer, chargeSimple( kObj ).namePt );
				}
			}
		}
		for ( kObj = 1; kObj <= numChargeBlock; ++kObj ) {
			if ( chargeBlock( kObj ).tariffIndx == curTariff ) {
				if ( chargeBlock( kObj ).categoryPt == curPointer ) {
					addOperand( curPointer, chargeBlock( kObj ).namePt );
				}
			}
		}
	}

	//======================================================================================================================
	//======================================================================================================================

	//    GATHER TIMESTEP VALUES ROUTINE

	//======================================================================================================================
	//======================================================================================================================

	void
	GatherForEconomics()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   June 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Gathers the data each timestep and updates the arrays
		//   holding the data that will be used by the tariff
		//   calculation.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataGlobals::TimeStepZoneSec;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataEnvironment::Month;

		// Locals

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int iTariff;
		Real64 curInstantValue;
		Real64 curDemand;
		Real64 curEnergy;
		bool isGood;
		int curSeason;
		int curMonth;
		int curPeriod;
		Real64 curRTPprice; // real time price
		Real64 curRTPbaseline; // real time price customer baseline load
		Real64 curRTPenergy; // energy applied to real time price
		Real64 curRTPcost; // cost for energy for current time

		if ( numTariff >= 1 ) {
			for ( iTariff = 1; iTariff <= numTariff; ++iTariff ) {
				isGood = false;
				//if the meter is defined get the value
				if ( tariff( iTariff ).reportMeterIndx != 0 ) {
					curInstantValue = GetCurrentMeterValue( tariff( iTariff ).reportMeterIndx );
				} else {
					curInstantValue = 0.0;
				}
				// remember the demand is still energy over a period of time divided by the
				// length of time. This gathers the energy also.
				tariff( iTariff ).collectEnergy += curInstantValue;
				tariff( iTariff ).collectTime += TimeStepZoneSec;
				//added *SecInHour when adding RTP support August 2008
				if ( tariff( iTariff ).collectTime >= tariff( iTariff ).demWinTime * SecInHour ) {
					//get current value that has been converted into desired units
					curDemand = tariff( iTariff ).demandConv * tariff( iTariff ).collectEnergy / tariff( iTariff ).collectTime;
					curEnergy = tariff( iTariff ).energyConv * tariff( iTariff ).collectEnergy;
					// get the schedule values
					// remember no confirmation of schedule values occurs prior to now
					if ( tariff( iTariff ).seasonSchIndex != 0 ) {
						curSeason = GetCurrentScheduleValue( tariff( iTariff ).seasonSchIndex );
					} else {
						curSeason = 1;
					}
					if ( tariff( iTariff ).periodSchIndex != 0 ) {
						curPeriod = GetCurrentScheduleValue( tariff( iTariff ).periodSchIndex );
					} else {
						curPeriod = 1;
					}
					if ( tariff( iTariff ).monthSchIndex != 0 ) {
						curMonth = GetCurrentScheduleValue( tariff( iTariff ).monthSchIndex );
					} else {
						curMonth = Month; //from DataEnvironment
					}
					if ( isWithinRange( curSeason, 1, 5 ) ) {
						if ( isWithinRange( curPeriod, 1, 4 ) ) {
							if ( isWithinRange( curMonth, 1, 12 ) ) {
								isGood = true;
							}
						}
					}
					if ( isGood ) {
						tariff( iTariff ).seasonForMonth( curMonth ) = curSeason;
						tariff( iTariff ).gatherEnergy( curMonth, curPeriod ) += curEnergy;
						if ( tariff( iTariff ).gatherDemand( curMonth, curPeriod ) < curDemand ) {
							tariff( iTariff ).gatherDemand( curMonth, curPeriod ) = curDemand;
						}
					} else {
						ShowWarningError( "UtilityCost:Tariff: While gathering for: " + tariff( iTariff ).tariffName );
						ShowContinueError( "Invalid schedule values - outside of range" );
					}
					// Real Time Pricing
					if ( tariff( iTariff ).chargeSchIndex != 0 ) {
						curRTPprice = GetCurrentScheduleValue( tariff( iTariff ).chargeSchIndex );
						// if customer baseline load schedule is used, subtract that off of the
						// current energy
						if ( tariff( iTariff ).baseUseSchIndex != 0 ) {
							curRTPbaseline = GetCurrentScheduleValue( tariff( iTariff ).baseUseSchIndex );
							curRTPenergy = curEnergy - curRTPbaseline;
						} else {
							curRTPenergy = curEnergy;
						}
						// calculate the real time cost for current times energy
						curRTPcost = curRTPenergy * curRTPprice;
						tariff( iTariff ).RTPcost( curMonth ) += curRTPcost;
						if ( curRTPcost > 0 ) {
							tariff( iTariff ).RTPaboveBaseCost( curMonth ) += curRTPcost;
						} else {
							tariff( iTariff ).RTPbelowBaseCost( curMonth ) += curRTPcost;
						}
						if ( curRTPenergy > 0 ) {
							tariff( iTariff ).RTPaboveBaseEnergy( curMonth ) += curRTPenergy;
						} else {
							tariff( iTariff ).RTPbelowBaseEnergy( curMonth ) += curRTPenergy;
						}
					}
					// reset the counters
					tariff( iTariff ).collectEnergy = 0.0;
					tariff( iTariff ).collectTime = 0.0;
				}
			}
		}
	}

	bool
	isWithinRange(
		int const testVal,
		int const minThreshold,
		int const maxThreshold
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Simple function to check if an integer is equal to or between
		//   two other values.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		bool isWithinRange;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		if ( maxThreshold < minThreshold ) {
			ShowWarningError( "UtilityCost: Invalid thresholds in IsWithinRange routine." );
		}
		if ( ( testVal <= maxThreshold ) && ( testVal >= minThreshold ) ) {
			isWithinRange = true;
		} else {
			isWithinRange = false;
		}
		return isWithinRange;
	}

	//======================================================================================================================
	//======================================================================================================================

	//    COMPUTE THE UTILITY BILLS AND CREATE REPORTS

	//======================================================================================================================
	//======================================================================================================================

	void
	ComputeTariff()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Perform the calculation steps to compute the monthly
		//    utility bills for the user entered tariffs.
		//    The list of steps for the tariff computation are in order
		//    for stack based computation (reverse polish notation)

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using OutputReportTabular::WriteTabularFiles;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS

		// values used in specific operations
		Array1D< Real64 > a( MaxNumMonths );
		int aPt;
		Array1D< Real64 > b( MaxNumMonths );
		int bPt;
		Array1D< Real64 > c( MaxNumMonths );
		int cPt;
		Array1D< Real64 > d( MaxNumMonths );

		int iTariff;
		int jStep;
		int lMonth;
		int nVar;
		int curStep;
		int const noVar( 0 );

		Real64 hugeValue;
		Real64 annualAggregate;
		int annualCnt;

		hugeValue = huge( Real64() );
		//  Clear the isEvaluated flags for all economics variables.
		for ( nVar = 1; nVar <= numEconVar; ++nVar ) {
			econVar( nVar ).isEvaluated = false;
		}
		if ( numTariff >= 1 ) {
			WriteTabularFiles = true;
			setNativeVariables();
			for ( iTariff = 1; iTariff <= numTariff; ++iTariff ) {
				for ( jStep = computation( iTariff ).firstStep; jStep <= computation( iTariff ).lastStep; ++jStep ) {
					curStep = steps( jStep );
					{ auto const SELECT_CASE_var( curStep );
					if ( SELECT_CASE_var == 0 ) { //end of line - assign variable and clear stack
						// if the stack still has two items on it then assign the values to the
						// pointer otherwise if it follows a NOOP line it will only have one item
						// that has already been assigned and no further action is required.
						if ( topOfStack >= 2 ) {
							popStack( b, bPt ); //pop the variable pointer
							popStack( a, aPt ); //pop the values
							if ( isWithinRange( bPt, 1, numEconVar ) ) {
								econVar( bPt ).values = a;
							}
						}
						topOfStack = 0;
					} else if ( ( SELECT_CASE_var >= 1 ) ) { //all positive values are a reference to an econVar
						pushStack( econVar( curStep ).values, curStep );
					} else if ( SELECT_CASE_var == opSUM ) {
						a = 0.0;
						for ( int kStack = 1, kStack_end = topOfStack; kStack <= kStack_end; ++kStack ) { // popStack modifies topOfStack
							popStack( b, bPt );
							a += b;
						}
						pushStack( a, noVar );
					} else if ( SELECT_CASE_var == opMULTIPLY ) {
						popStack( b, bPt );
						popStack( a, aPt );
						pushStack( a * b, noVar );
					} else if ( SELECT_CASE_var == opSUBTRACT ) {
						popStack( b, bPt );
						popStack( a, aPt );
						pushStack( b - a, noVar );
					} else if ( SELECT_CASE_var == opDIVIDE ) {
						popStack( a, aPt );
						popStack( b, bPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( b( lMonth ) != 0 ) {
								c( lMonth ) = a( lMonth ) / b( lMonth );
							} else {
								c( lMonth ) = 0.0;
							}
						}
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opABSOLUTE ) {
						popStack( a, aPt );
						pushStack( abs( a ), noVar );
					} else if ( SELECT_CASE_var == opINTEGER ) {
						popStack( a, aPt );
						pushStack( Array1D_double( Array1D_int( a ) ), noVar );
					} else if ( SELECT_CASE_var == opSIGN ) {
						popStack( a, aPt );
						pushStack( sign( 1.0, a ), noVar );
						//        CASE (opROUND)
						//          CALL popStack(b,bPt)
						//          CALL popStack(a,aPt)
						//          DO lMonth = 1,MaxNumMonths
						//            IF ((b(lMonth) .LE. 5) .AND. (b(lMonth) .GE. -5)) THEN
						//              c(lMonth) = FLOAT(INT(a(lMonth) / (10 ** b(lMonth))) * (10 ** b(lMonth)))
						//            END IF
						//          END DO
						//          CALL pushStack(c,noVar)
					} else if ( SELECT_CASE_var == opMAXIMUM ) {
						a = -hugeValue;
						for ( int kStack = 1, kStack_end = topOfStack; kStack <= kStack_end; ++kStack ) { // popStack modifies topOfStack
							popStack( b, bPt );
							for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
								if ( b( lMonth ) > a( lMonth ) ) {
									a( lMonth ) = b( lMonth );
								}
							}
						}
						pushStack( a, noVar );
					} else if ( SELECT_CASE_var == opMINIMUM ) {
						a = hugeValue;
						for ( int kStack = 1, kStack_end = topOfStack; kStack <= kStack_end; ++kStack ) { // popStack modifies topOfStack
							popStack( b, bPt );
							for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
								if ( b( lMonth ) < a( lMonth ) ) {
									a( lMonth ) = b( lMonth );
								}
							}
						}
						pushStack( a, noVar );
					} else if ( SELECT_CASE_var == opEXCEEDS ) {
						popStack( b, bPt );
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) > b( lMonth ) ) {
								c( lMonth ) = a( lMonth ) - b( lMonth );
							} else {
								c( lMonth ) = 0.0;
							}
						}
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opANNUALMINIMUM ) {
						//takes the minimum but ignores zeros
						annualAggregate = hugeValue;
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) != 0 ) {
								if ( a( lMonth ) < annualAggregate ) {
									annualAggregate = a( lMonth );
								}
							}
						}
						// if all months are zero then hugeValue still in annual but should be zero
						if ( annualAggregate == hugeValue ) {
							annualAggregate = 0.0;
						}
						c = annualAggregate;
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opANNUALMAXIMUM ) {
						//takes the maximum but ignores zeros
						annualAggregate = -hugeValue;
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) != 0 ) {
								if ( a( lMonth ) > annualAggregate ) {
									annualAggregate = a( lMonth );
								}
							}
						}
						// if all months are zero then hugeValue still in annual but should be zero
						if ( annualAggregate == -hugeValue ) {
							annualAggregate = 0.0;
						}
						c = annualAggregate;
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opANNUALSUM ) {
						//takes the maximum but ignores zeros
						annualAggregate = 0.0;
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							annualAggregate += a( lMonth );
						}
						c = annualAggregate;
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opANNUALAVERAGE ) {
						//takes the annual sum but ignores zeros
						annualAggregate = 0.0;
						annualCnt = 0;
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) != 0 ) {
								annualAggregate += a( lMonth );
								++annualCnt;
							}
						}
						// if all months are zero then return zero
						if ( annualCnt != 0 ) {
							c = annualAggregate / annualCnt;
						} else {
							c = 0.0;
						}
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opANNUALOR ) {
						annualCnt = 0;
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) != 0 ) {
								++annualCnt;
							}
						}
						// if any months is not zero then "true"
						if ( annualCnt >= 1 ) {
							c = 1.0;
						} else {
							c = 0.0;
						}
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opANNUALAND ) {
						annualCnt = 0;
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) != 0 ) {
								++annualCnt;
							}
						}
						// if all months are not zero then "true"
						if ( annualCnt == MaxNumMonths ) {
							c = 1.0;
						} else {
							c = 0.0;
						}
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opANNUALMAXIMUMZERO ) {
						//takes the maximum including zeros
						annualAggregate = -hugeValue;
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) > annualAggregate ) {
								annualAggregate = a( lMonth );
							}
						}
						c = annualAggregate;
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opANNUALMINIMUMZERO ) {
						//takes the maximum including zeros
						annualAggregate = hugeValue;
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) < annualAggregate ) {
								annualAggregate = a( lMonth );
							}
						}
						c = annualAggregate;
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opIF ) {
						popStack( c, cPt );
						popStack( b, bPt );
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) != 0 ) {
								d( lMonth ) = b( lMonth );
							} else {
								d( lMonth ) = c( lMonth );
							}
						}
						pushStack( d, noVar );
					} else if ( SELECT_CASE_var == opGREATERTHAN ) {
						popStack( b, bPt );
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) > b( lMonth ) ) {
								c( lMonth ) = 1.0;
							} else {
								c( lMonth ) = 0.0;
							}
						}
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opGREATEREQUAL ) {
						popStack( b, bPt );
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) >= b( lMonth ) ) {
								c( lMonth ) = 1.0;
							} else {
								c( lMonth ) = 0.0;
							}
						}
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opLESSTHAN ) {
						popStack( b, bPt );
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) < b( lMonth ) ) {
								c( lMonth ) = 1.0;
							} else {
								c( lMonth ) = 0.0;
							}
						}
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opLESSEQUAL ) {
						popStack( b, bPt );
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) <= b( lMonth ) ) {
								c( lMonth ) = 1.0;
							} else {
								c( lMonth ) = 0.0;
							}
						}
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opEQUAL ) {
						popStack( b, bPt );
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) == b( lMonth ) ) {
								c( lMonth ) = 1.0;
							} else {
								c( lMonth ) = 0.0;
							}
						}
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opNOTEQUAL ) {
						popStack( b, bPt );
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) != b( lMonth ) ) {
								c( lMonth ) = 1.0;
							} else {
								c( lMonth ) = 0.0;
							}
						}
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opAND ) {
						popStack( b, bPt );
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( ( a( lMonth ) != 0 ) && ( b( lMonth ) != 0 ) ) {
								c( lMonth ) = 1.0;
							} else {
								c( lMonth ) = 0.0;
							}
						}
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opOR ) {
						popStack( b, bPt );
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( ( a( lMonth ) != 0 ) || ( b( lMonth ) != 0 ) ) {
								c( lMonth ) = 1.0;
							} else {
								c( lMonth ) = 0.0;
							}
						}
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opNOT ) {
						popStack( a, aPt );
						for ( lMonth = 1; lMonth <= MaxNumMonths; ++lMonth ) {
							if ( a( lMonth ) == 0 ) {
								c( lMonth ) = 1.0;
							} else {
								c( lMonth ) = 0.0;
							}
						}
						pushStack( c, noVar );
					} else if ( SELECT_CASE_var == opADD ) {
						popStack( b, bPt );
						popStack( a, aPt );
						pushStack( a + b, noVar );
					} else if ( SELECT_CASE_var == opNOOP ) {
						//do nothing but clear the stack
						topOfStack = 0;
						// No longer pushing a zero to fix bug
						//and push zero
						//a = 0
						//CALL pushStack(a,noVar)
					}}
				}
				checkMinimumMonthlyCharge( iTariff );
			}
			selectTariff();
			LEEDtariffReporting();
		}
	}

	void
	pushStack(
		Array1A< Real64 > const monthlyArray,
		int const variablePointer
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    A stack is used in the evaluation of the tariff since
		//    the variables and operators are in a reverse polish
		//    notation order. The stack operates on a last-in
		//    first out basis. The stack consists of both a pointer
		//    to the variable and the twelve monthly values.
		//    This routine puts an item on the top of the stack.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputReportTabular::IntToStr;

		// Argument array dimensioning
		monthlyArray.dim( MaxNumMonths );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D< Real64 > curMonthlyArray( MaxNumMonths );
		static int sizeIncrement( 50 );

		curMonthlyArray = monthlyArray;
		if ( ! allocated( stack ) ) {
			stack.allocate( sizeIncrement );
			sizeStack = sizeIncrement;
			topOfStack = 1;
		} else {
			++topOfStack;
			// if larger than current size grow the array
			if ( topOfStack > sizeStack ) {
				stack.redimension( sizeStack += sizeIncrement );
			}
		}
		//now push the values on to the stack
		stack( topOfStack ).varPt = variablePointer;
		//check if variable has been evaluated if it is CHARGE:SIMPLE, CHARGE:BLOCK, RATCHET, or QUALIFY
		//if it has not overwrite the values for monthlyArray with the evaluated values
		if ( variablePointer != 0 ) {
			if ( ! econVar( variablePointer ).isEvaluated ) {
				{ auto const SELECT_CASE_var( econVar( variablePointer ).kindOfObj );
				if ( SELECT_CASE_var == kindChargeSimple ) {
					evaluateChargeSimple( variablePointer );
				} else if ( SELECT_CASE_var == kindChargeBlock ) {
					evaluateChargeBlock( variablePointer );
				} else if ( SELECT_CASE_var == kindRatchet ) {
					evaluateRatchet( variablePointer );
				} else if ( SELECT_CASE_var == kindQualify ) {
					evaluateQualify( variablePointer );
				} else if ( SELECT_CASE_var == kindUnknown ) {
					ShowWarningError( "UtilityCost variable not defined: " + econVar( variablePointer ).name );
					ShowContinueError( "   In tariff: " + tariff( econVar( variablePointer ).tariffIndx ).tariffName );
					ShowContinueError( "   This may be the result of a mispelled variable name in the UtilityCost:Computation object." );
					ShowContinueError( "   All zero values will be assumed for this variable." );
				} else if ( ( SELECT_CASE_var == kindVariable ) || ( SELECT_CASE_var == kindCategory ) || ( SELECT_CASE_var == kindNative ) || ( SELECT_CASE_var == kindAssignCompute ) || ( SELECT_CASE_var == kindTariff ) || ( SELECT_CASE_var == kindComputation ) ) {
					// do nothing
				} else {
					ShowWarningError( "UtilityCost Debugging issue. Invalid kind of variable used (pushStack). " + IntToStr( econVar( variablePointer ).kindOfObj ) + " in tariff: " + tariff( econVar( variablePointer ).tariffIndx ).tariffName );
				}}
				// if the serviceCharges are being evaluated add in the monthly charges
				if ( econVar( variablePointer ).specific == catServiceCharges ) addMonthlyCharge( variablePointer );
				//get the results of performing the evaulation - should have been
				//put into the econVar values
				curMonthlyArray = econVar( variablePointer ).values;
			}
		}
		//now assign
		stack( topOfStack ).values = curMonthlyArray;
	}

	void
	popStack(
		Array1A< Real64 > monthlyArray,
		int & variablePointer
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    A stack is used in the evaluation of the tariff since
		//    the variables and operators are in a reverse polish
		//    notation order. The stack operates on a last-in
		//    first out basis. The stack consists of both a pointer
		//    to the variable and the twelve monthly values.
		//    This routine returns the item on the top of the stack
		//    and removes it from the stack.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Argument array dimensioning
		monthlyArray.dim( MaxNumMonths );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		if ( topOfStack >= 1 ) {
			variablePointer = stack( topOfStack ).varPt;
			monthlyArray = stack( topOfStack ).values;
		} else {
			ShowWarningError( "UtilityCost:Tariff: stack underflow in calculation of utility bills. On variable: " + econVar( variablePointer ).name );
			variablePointer = 0;
			monthlyArray = 0.0;
			topOfStack = 0;
		}
		--topOfStack;
	}

	void
	evaluateChargeSimple( int const usingVariable )
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int curTariff;
		int indexInChg;
		Array1D< Real64 > sourceVals( MaxNumMonths );
		Array1D< Real64 > costPer( MaxNumMonths );
		Array1D< Real64 > resultChg( MaxNumMonths );
		Array1D< Real64 > seasonMask( MaxNumMonths );

		curTariff = econVar( usingVariable ).tariffIndx;
		indexInChg = econVar( usingVariable ).index;

		//check the tariff - make sure they match
		if ( chargeSimple( indexInChg ).namePt != usingVariable ) {
			ShowWarningError( "UtilityCost:Tariff Debugging issue. ChargeSimple index does not match variable pointer." );
			ShowContinueError( "   Between: " + econVar( usingVariable ).name );
			ShowContinueError( "       And: " + econVar( chargeSimple( indexInChg ).namePt ).name );
		}
		if ( chargeSimple( indexInChg ).tariffIndx != curTariff ) {
			ShowWarningError( "UtilityCost:Tariff Debugging issue. ChargeSimple index does not match tariff index." );
			ShowContinueError( "   Between: " + tariff( curTariff ).tariffName );
			ShowContinueError( "       And: " + tariff( chargeSimple( indexInChg ).tariffIndx ).tariffName );
		}
		// data from the Charge:Simple
		sourceVals = econVar( chargeSimple( indexInChg ).sourcePt ).values;
		// determine if costPer should be based on variable or value
		if ( chargeSimple( indexInChg ).costPerPt != 0 ) {
			costPer = econVar( chargeSimple( indexInChg ).costPerPt ).values;
		} else {
			costPer = chargeSimple( indexInChg ).costPerVal;
		}
		// find proper season mask
		{ auto const SELECT_CASE_var( chargeSimple( indexInChg ).season );
		if ( SELECT_CASE_var == seasonSummer ) {
			seasonMask = econVar( tariff( curTariff ).nativeIsSummer ).values;
		} else if ( SELECT_CASE_var == seasonWinter ) {
			seasonMask = econVar( tariff( curTariff ).nativeIsWinter ).values;
		} else if ( SELECT_CASE_var == seasonSpring ) {
			seasonMask = econVar( tariff( curTariff ).nativeIsSpring ).values;
		} else if ( SELECT_CASE_var == seasonFall ) {
			seasonMask = econVar( tariff( curTariff ).nativeIsAutumn ).values;
		} else if ( SELECT_CASE_var == seasonAnnual ) {
			seasonMask = 1.0; //all months are 1
		}}
		// finally perform calculations
		resultChg = sourceVals * costPer * seasonMask;
		//store the cost in the name of the variable
		econVar( usingVariable ).values = resultChg;
		//set the flag that it has been evaluated so it won't be evaluated multiple times
		econVar( usingVariable ).isEvaluated = true;
	}

	void
	evaluateChargeBlock( int const usingVariable )
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int curTariff;
		int indexInChg;
		int iBlk;
		int jMonth;
		Array1D< Real64 > sourceVals( MaxNumMonths );
		Array1D< Real64 > blkSzMult( MaxNumMonths );
		Array1D< Real64 > remainVals( MaxNumMonths );
		Array1D< Real64 > resultChg( MaxNumMonths );
		Array1D< Real64 > amountForBlk( MaxNumMonths );
		Array1D< Real64 > curBlkSz( MaxNumMonths );
		Array1D< Real64 > curBlkCost( MaxNumMonths );
		Array1D< Real64 > seasonMask( MaxNumMonths );
		bool flagAllZero;

		curTariff = econVar( usingVariable ).tariffIndx;
		indexInChg = econVar( usingVariable ).index;

		//check the tariff - make sure they match
		if ( chargeBlock( indexInChg ).namePt != usingVariable ) {
			ShowWarningError( "UtilityCost:Tariff Debugging issue. chargeBlock index does not match variable pointer." );
			ShowContinueError( "   Between: " + econVar( usingVariable ).name );
			ShowContinueError( "       And: " + econVar( chargeBlock( indexInChg ).namePt ).name );
		}
		if ( chargeBlock( indexInChg ).tariffIndx != curTariff ) {
			ShowWarningError( "UtilityCost:Tariff Debugging issue. chargeBlock index does not match tariff index." );
			ShowContinueError( "   Between: " + tariff( curTariff ).tariffName );
			ShowContinueError( "       And: " + tariff( chargeBlock( indexInChg ).tariffIndx ).tariffName );
		}
		// data from the chargeBlock
		sourceVals = econVar( chargeBlock( indexInChg ).sourcePt ).values;
		// find proper season mask
		{ auto const SELECT_CASE_var( chargeBlock( indexInChg ).season );
		if ( SELECT_CASE_var == seasonSummer ) {
			seasonMask = econVar( tariff( curTariff ).nativeIsSummer ).values;
		} else if ( SELECT_CASE_var == seasonWinter ) {
			seasonMask = econVar( tariff( curTariff ).nativeIsWinter ).values;
		} else if ( SELECT_CASE_var == seasonSpring ) {
			seasonMask = econVar( tariff( curTariff ).nativeIsSpring ).values;
		} else if ( SELECT_CASE_var == seasonFall ) {
			seasonMask = econVar( tariff( curTariff ).nativeIsAutumn ).values;
		} else if ( SELECT_CASE_var == seasonAnnual ) {
			seasonMask = 1.0; //all months are 1
		}}
		// get block size multiplier
		if ( chargeBlock( indexInChg ).blkSzMultPt != 0 ) {
			blkSzMult = econVar( chargeBlock( indexInChg ).blkSzMultPt ).values;
		} else {
			blkSzMult = chargeBlock( indexInChg ).blkSzMultVal;
		}
		//initially set the remaing energy or demand to the source
		remainVals = sourceVals;
		//initially set the result (cost) to zero
		resultChg = 0.0;
		//loop through the blocks performing calculations
		for ( iBlk = 1; iBlk <= chargeBlock( indexInChg ).numBlk; ++iBlk ) {
			if ( chargeBlock( indexInChg ).blkSzPt( iBlk ) != 0 ) {
				curBlkSz = econVar( chargeBlock( indexInChg ).blkSzPt( iBlk ) ).values;
			} else {
				curBlkSz = chargeBlock( indexInChg ).blkSzVal( iBlk );
			}
			if ( chargeBlock( indexInChg ).blkCostPt( iBlk ) != 0 ) {
				curBlkCost = econVar( chargeBlock( indexInChg ).blkCostPt( iBlk ) ).values;
			} else {
				curBlkCost = chargeBlock( indexInChg ).blkCostVal( iBlk );
			}
			//loop through the months
			for ( jMonth = 1; jMonth <= MaxNumMonths; ++jMonth ) {
				if ( seasonMask( jMonth ) == 1 ) {
					// IF ((curBlkSz(jMonth) * blkSzMult(jMonth)) .GT. remainVals(jMonth)) THEN - CR 6547
					if ( blkSzMult( jMonth ) != 0 ) {
						if ( curBlkSz( jMonth ) > ( remainVals( jMonth ) / blkSzMult( jMonth ) ) ) {
							amountForBlk( jMonth ) = remainVals( jMonth );
						} else {
							amountForBlk( jMonth ) = curBlkSz( jMonth ) * blkSzMult( jMonth );
						}
					} else {
						amountForBlk( jMonth ) = 0.0;
					}
					resultChg( jMonth ) += amountForBlk( jMonth ) * curBlkCost( jMonth );
					remainVals( jMonth ) -= amountForBlk( jMonth );
				}
			}
		}
		// store the amount remaining if a variable is specified
		if ( chargeBlock( indexInChg ).remainingPt != 0 ) {
			econVar( chargeBlock( indexInChg ).remainingPt ).values = remainVals;
		} else {
			flagAllZero = true;
			for ( jMonth = 1; jMonth <= MaxNumMonths; ++jMonth ) {
				if ( seasonMask( jMonth ) == 1 ) {
					if ( remainVals( jMonth ) != 0 ) {
						flagAllZero = false;
					}
				}
			}
			if ( ! flagAllZero ) {
				ShowWarningError( "UtilityCost:Tariff Not all energy or demand was assigned in the block charge: " + econVar( usingVariable ).name );
			}
		}
		//store the cost in the name of the variable
		econVar( usingVariable ).values = resultChg;
		//set the flag that it has been evaluated so it won't be evaluated multiple times
		econVar( usingVariable ).isEvaluated = true;
	}

	void
	evaluateRatchet( int const usingVariable )
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int curTariff;
		int indexInChg;
		Array1D< Real64 > baselineVals( MaxNumMonths );
		Array1D< Real64 > adjustmentVals( MaxNumMonths );
		Array1D< Real64 > multiplierVals( MaxNumMonths );
		Array1D< Real64 > offsetVals( MaxNumMonths );
		Array1D< Real64 > seasonFromMask( MaxNumMonths );
		Array1D< Real64 > seasonToMask( MaxNumMonths );
		bool isMonthly( false );
		Array1D< Real64 > adjSeasonal( MaxNumMonths );
		Array1D< Real64 > adjPeak( MaxNumMonths );
		Array1D< Real64 > maxAdjBase( MaxNumMonths );
		Real64 maximumVal;
		int iMonth;
		Array1D< Real64 > finalResult( MaxNumMonths );

		curTariff = econVar( usingVariable ).tariffIndx;
		indexInChg = econVar( usingVariable ).index;

		//check the tariff - make sure they match
		if ( ratchet( indexInChg ).namePt != usingVariable ) {
			ShowWarningError( "UtilityCost:Tariff Debugging issue. Ratchet index does not match variable pointer." );
			ShowContinueError( "   Between: " + econVar( usingVariable ).name );
			ShowContinueError( "       And: " + econVar( ratchet( indexInChg ).namePt ).name );
		}
		if ( ratchet( indexInChg ).tariffIndx != curTariff ) {
			ShowWarningError( "UtilityCost:Tariff Debugging issue. Ratchet index does not match tariff index." );
			ShowContinueError( "   Between: " + tariff( curTariff ).tariffName );
			ShowContinueError( "       And: " + tariff( ratchet( indexInChg ).tariffIndx ).tariffName );
		}
		// data from the Ratchet
		baselineVals = econVar( ratchet( indexInChg ).baselinePt ).values;
		adjustmentVals = econVar( ratchet( indexInChg ).adjustmentPt ).values;
		// determine if multiplier should be based on variable or value
		if ( ratchet( indexInChg ).multiplierPt != 0 ) {
			multiplierVals = econVar( ratchet( indexInChg ).multiplierPt ).values;
		} else {
			multiplierVals = ratchet( indexInChg ).multiplierVal;
		}
		// determine if offset should be based on variable or value
		if ( ratchet( indexInChg ).offsetPt != 0 ) {
			offsetVals = econVar( ratchet( indexInChg ).offsetPt ).values;
		} else {
			offsetVals = ratchet( indexInChg ).offsetVal;
		}
		// find proper season from mask
		{ auto const SELECT_CASE_var( ratchet( indexInChg ).seasonFrom );
		if ( SELECT_CASE_var == seasonSummer ) {
			seasonFromMask = econVar( tariff( curTariff ).nativeIsSummer ).values;
			isMonthly = false;
		} else if ( SELECT_CASE_var == seasonWinter ) {
			seasonFromMask = econVar( tariff( curTariff ).nativeIsWinter ).values;
			isMonthly = false;
		} else if ( SELECT_CASE_var == seasonSpring ) {
			seasonFromMask = econVar( tariff( curTariff ).nativeIsSpring ).values;
			isMonthly = false;
		} else if ( SELECT_CASE_var == seasonFall ) {
			seasonFromMask = econVar( tariff( curTariff ).nativeIsAutumn ).values;
			isMonthly = false;
		} else if ( SELECT_CASE_var == seasonAnnual ) {
			seasonFromMask = 1.0; //all months are 1
			isMonthly = false;
		} else if ( SELECT_CASE_var == seasonMonthly ) {
			seasonFromMask = 1.0; //all months are 1
			isMonthly = true;
		} else {
			assert( false );
		}}
		// find proper season to mask
		{ auto const SELECT_CASE_var( ratchet( indexInChg ).seasonTo );
		if ( SELECT_CASE_var == seasonSummer ) {
			seasonToMask = econVar( tariff( curTariff ).nativeIsSummer ).values;
		} else if ( SELECT_CASE_var == seasonWinter ) {
			seasonToMask = econVar( tariff( curTariff ).nativeIsWinter ).values;
		} else if ( SELECT_CASE_var == seasonSpring ) {
			seasonToMask = econVar( tariff( curTariff ).nativeIsSpring ).values;
		} else if ( SELECT_CASE_var == seasonFall ) {
			seasonToMask = econVar( tariff( curTariff ).nativeIsAutumn ).values;
		} else if ( SELECT_CASE_var == seasonAnnual ) {
			seasonToMask = 1.0; //all months are 1
		}}
		// finally perform calculations
		if ( isMonthly ) {
			adjSeasonal = adjustmentVals;
		} else {
			maximumVal = -huge( Real64() );
			for ( iMonth = 1; iMonth <= MaxNumMonths; ++iMonth ) {
				if ( seasonFromMask( iMonth ) == 1 ) {
					if ( adjustmentVals( iMonth ) > maximumVal ) {
						maximumVal = adjustmentVals( iMonth );
					}
				}
			}
			adjSeasonal = maximumVal;
		}
		for ( iMonth = 1; iMonth <= MaxNumMonths; ++iMonth ) {
			//calculate adjusted peak value after offset and multiplier
			adjPeak( iMonth ) = ( adjSeasonal( iMonth ) + offsetVals( iMonth ) ) * multiplierVals( iMonth );
			//the maximum of the adjustment and the baseline
			if ( adjPeak( iMonth ) > baselineVals( iMonth ) ) {
				maxAdjBase( iMonth ) = adjPeak( iMonth );
			} else {
				maxAdjBase( iMonth ) = baselineVals( iMonth );
			}
		}
		for ( iMonth = 1; iMonth <= MaxNumMonths; ++iMonth ) {
			if ( seasonToMask( iMonth ) == 1 ) {
				finalResult( iMonth ) = maxAdjBase( iMonth );
			} else {
				finalResult( iMonth ) = baselineVals( iMonth );
			}
		}
		//store the cost in the name of the variable
		econVar( usingVariable ).values = finalResult;
		//set the flag that it has been evaluated so it won't be evaluated multiple times
		econVar( usingVariable ).isEvaluated = true;
	}

	void
	evaluateQualify( int const usingVariable )
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int curTariff;
		int indexInQual;
		Array1D< Real64 > sourceVals( MaxNumMonths );
		Array1D< Real64 > thresholdVals( MaxNumMonths );
		Array1D_int monthsQualify( MaxNumMonths );
		Array1D< Real64 > seasonMask( MaxNumMonths );
		bool curIsMaximum;
		bool curIsConsecutive;
		int curNumberOfMonths;
		int adjNumberOfMonths;
		int iMonth;
		bool isQualified;
		int monthsInSeason;
		int cntAllQualMonths;
		int cntConsecQualMonths;
		int maxConsecQualMonths;

		curTariff = econVar( usingVariable ).tariffIndx;
		indexInQual = econVar( usingVariable ).index;
		//check the tariff - make sure they match
		if ( qualify( indexInQual ).namePt != usingVariable ) {
			ShowWarningError( "UtilityCost:Tariff Debugging issue. Qualify index does not match variable pointer." );
			ShowContinueError( "   Between: " + econVar( usingVariable ).name );
			ShowContinueError( "       And: " + econVar( qualify( indexInQual ).namePt ).name );
		}
		if ( qualify( indexInQual ).tariffIndx != curTariff ) {
			ShowWarningError( "UtilityCost:Tariff Debugging issue. Qualify index does not match tariff index." );
			ShowContinueError( "   Between: " + tariff( curTariff ).tariffName );
			ShowContinueError( "       And: " + tariff( qualify( indexInQual ).tariffIndx ).tariffName );
		}
		// data from the Qualify
		sourceVals = econVar( qualify( indexInQual ).sourcePt ).values;
		curIsMaximum = qualify( indexInQual ).isMaximum;
		curIsConsecutive = qualify( indexInQual ).isConsecutive;
		curNumberOfMonths = qualify( indexInQual ).numberOfMonths;
		// determine if threshold should be based on variable or value
		if ( qualify( indexInQual ).thresholdPt != 0 ) {
			thresholdVals = econVar( qualify( indexInQual ).thresholdPt ).values;
		} else {
			thresholdVals = qualify( indexInQual ).thresholdVal;
		}
		// find proper season mask
		{ auto const SELECT_CASE_var( qualify( indexInQual ).season );
		if ( SELECT_CASE_var == seasonSummer ) {
			seasonMask = econVar( tariff( curTariff ).nativeIsSummer ).values;
		} else if ( SELECT_CASE_var == seasonWinter ) {
			seasonMask = econVar( tariff( curTariff ).nativeIsWinter ).values;
		} else if ( SELECT_CASE_var == seasonSpring ) {
			seasonMask = econVar( tariff( curTariff ).nativeIsSpring ).values;
		} else if ( SELECT_CASE_var == seasonFall ) {
			seasonMask = econVar( tariff( curTariff ).nativeIsAutumn ).values;
		} else if ( SELECT_CASE_var == seasonAnnual ) {
			seasonMask = 1.0; //all months are 1
		}}
		//any months with no energy use are excluded from the qualification process
		for ( iMonth = 1; iMonth <= MaxNumMonths; ++iMonth ) {
			if ( econVar( tariff( curTariff ).nativeTotalEnergy ).values( iMonth ) == 0 ) {
				seasonMask( iMonth ) = 0.0;
			}
		}
		// finally perform calculations
		//loop through the months
		monthsInSeason = 0;
		for ( iMonth = 1; iMonth <= MaxNumMonths; ++iMonth ) {
			if ( seasonMask( iMonth ) == 1 ) {
				++monthsInSeason;
				//use threshold as maximum or minimum
				if ( curIsMaximum ) {
					if ( sourceVals( iMonth ) > thresholdVals( iMonth ) ) {
						monthsQualify( iMonth ) = 0; //greater than maximum threshold so it is not qualified
					} else {
						monthsQualify( iMonth ) = 1; //less than maximum threshold so it is qualified
					}
				} else {
					if ( sourceVals( iMonth ) < thresholdVals( iMonth ) ) {
						monthsQualify( iMonth ) = 0; //less than minimum threshold so it is not qualified
					} else {
						monthsQualify( iMonth ) = 1; //greater than minimum threshold so it is qualified
					}
				}
			} else {
				monthsQualify( iMonth ) = -1; //flag that indicates not part of the season
			}
		}
		//see if the number of months is longer then the number of months and adjust
		if ( curNumberOfMonths > monthsInSeason ) {
			adjNumberOfMonths = monthsInSeason;
		} else {
			adjNumberOfMonths = curNumberOfMonths;
		}
		//now that each month is qualified or not, depending on the type of test see if the entire qualify passe or not
		cntAllQualMonths = 0;
		cntConsecQualMonths = 0;
		maxConsecQualMonths = 0;
		for ( iMonth = 1; iMonth <= MaxNumMonths; ++iMonth ) {
			{ auto const SELECT_CASE_var( monthsQualify( iMonth ) );
			if ( SELECT_CASE_var == 1 ) { //qualified
				++cntAllQualMonths;
				++cntConsecQualMonths;
				//see if the count is greater then the previous count and if it is make it the new count
				if ( cntConsecQualMonths > maxConsecQualMonths ) {
					maxConsecQualMonths = cntConsecQualMonths;
				}
			} else if ( SELECT_CASE_var == 0 ) { //not qualified
				//reset the counter on consecutive months
				cntConsecQualMonths = 0;
			}}
		}
		//if test is for consecutive months
		if ( curIsConsecutive ) {
			if ( maxConsecQualMonths >= adjNumberOfMonths ) {
				isQualified = true;
			} else {
				isQualified = false;
			}
		} else { //count not consecutive
			if ( cntAllQualMonths >= adjNumberOfMonths ) {
				isQualified = true;
			} else {
				isQualified = false;
			}
		}
		//now update the tariff level qualifier - only update if the tariff is still qualified
		//and the current qualifer fails.
		if ( tariff( curTariff ).isQualified ) {
			if ( ! isQualified ) {
				tariff( curTariff ).isQualified = false;
				tariff( curTariff ).ptDisqualifier = usingVariable;
			}
		}
		//store the cost in the name of the variable
		econVar( usingVariable ).values = monthsQualify;
		//set the flag that it has been evaluated so it won't be evaluated multiple times
		econVar( usingVariable ).isEvaluated = true;
	}

	void
	addMonthlyCharge( int const usingVariable )
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Include the monthly charges in the calculations

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int curTariff;
		//INTEGER :: iMonth
		//INTEGER :: curTotalEnergy

		curTariff = econVar( usingVariable ).tariffIndx;
		//check the tariff - make sure they match
		if ( tariff( curTariff ).ptServiceCharges != usingVariable ) {
			ShowWarningError( "UtilityCost:Tariff Debugging issue. Tariff index for service charge does not match variable pointer." );
			ShowContinueError( "   Between: " + tariff( curTariff ).tariffName );
			ShowContinueError( "       And: " + tariff( tariff( curTariff ).ptServiceCharges ).tariffName );
		}
		if ( tariff( curTariff ).monthChgPt != 0 ) {
			econVar( usingVariable ).values += econVar( tariff( curTariff ).monthChgPt ).values;
		} else {
			econVar( usingVariable ).values += tariff( curTariff ).monthChgVal;
		}
		//zero out months with no energy consumption
		//curTotalEnergy = tariff(curTariff)%nativeTotalEnergy
		//DO iMonth = 1, MaxNumMonths
		//  IF (econVar(curTotalEnergy)%values(iMonth) .EQ. 0) THEN
		//    econVar(usingVariable)%values(iMonth) = 0
		//  END IF
		//END DO
	}

	void
	checkMinimumMonthlyCharge( int const curTariff )
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   August 2008
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Check if the total is as big as the minimum monthly charge

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iMonth;
		int totalVar;
		int minMonVar;

		totalVar = tariff( curTariff ).ptTotal;
		minMonVar = tariff( curTariff ).minMonthChgPt;
		// if a variable is defined use that
		if ( minMonVar != 0 ) {
			for ( iMonth = 1; iMonth <= MaxNumMonths; ++iMonth ) {
				if ( econVar( totalVar ).values( iMonth ) < econVar( minMonVar ).values( iMonth ) ) {
					econVar( totalVar ).values( iMonth ) = econVar( minMonVar ).values( iMonth );
				}
			}
		} else { //use the constant value
			for ( iMonth = 1; iMonth <= MaxNumMonths; ++iMonth ) {
				if ( econVar( totalVar ).values( iMonth ) < tariff( curTariff ).minMonthChgVal ) {
					econVar( totalVar ).values( iMonth ) = tariff( curTariff ).minMonthChgVal;
				}
			}
		}
	}

	void
	setNativeVariables()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Set up the "built in" i.e. native variables that hold
		//    the energy and demand from the simulation.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iTariff;
		int jPeriod;
		int kMonth;
		Array1D< Real64 > monthVal( MaxNumMonths );
		Real64 bigNumber( 0.0 ); //Autodesk Value not used but suppresses warning about huge() call

		bigNumber = huge( bigNumber );
		for ( iTariff = 1; iTariff <= numTariff; ++iTariff ) {
			//nativeTotalEnergy
			monthVal = 0.0;
			for ( jPeriod = 1; jPeriod <= countPeriod; ++jPeriod ) {
				for ( kMonth = 1; kMonth <= MaxNumMonths; ++kMonth ) {
					monthVal( kMonth ) += tariff( iTariff ).gatherEnergy( kMonth, jPeriod );
				}
			}
			econVar( tariff( iTariff ).nativeTotalEnergy ).values = monthVal;
			//nativeTotalDemand
			monthVal = -bigNumber;
			for ( jPeriod = 1; jPeriod <= countPeriod; ++jPeriod ) {
				for ( kMonth = 1; kMonth <= MaxNumMonths; ++kMonth ) {
					if ( tariff( iTariff ).gatherDemand( kMonth, jPeriod ) > monthVal( kMonth ) ) {
						monthVal( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, jPeriod );
					}
				}
			}
			//if no maximum was set just set to zero
			for ( kMonth = 1; kMonth <= MaxNumMonths; ++kMonth ) {
				if ( monthVal( kMonth ) == -bigNumber ) {
					monthVal( kMonth ) = 0.0;
				}
			}
			econVar( tariff( iTariff ).nativeTotalDemand ).values = monthVal;
			for ( kMonth = 1; kMonth <= MaxNumMonths; ++kMonth ) {
				//nativePeakEnergy
				econVar( tariff( iTariff ).nativePeakEnergy ).values( kMonth ) = tariff( iTariff ).gatherEnergy( kMonth, periodPeak );
				//nativePeakDemand
				econVar( tariff( iTariff ).nativePeakDemand ).values( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodPeak );
				//nativeShoulderEnergy
				econVar( tariff( iTariff ).nativeShoulderEnergy ).values( kMonth ) = tariff( iTariff ).gatherEnergy( kMonth, periodShoulder );
				//nativeShoulderDemand
				econVar( tariff( iTariff ).nativeShoulderDemand ).values( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodShoulder );
				//nativeOffPeakEnergy
				econVar( tariff( iTariff ).nativeOffPeakEnergy ).values( kMonth ) = tariff( iTariff ).gatherEnergy( kMonth, periodOffPeak );
				//nativeOffPeakDemand
				econVar( tariff( iTariff ).nativeOffPeakDemand ).values( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodOffPeak );
				//nativeMidPeakEnergy
				econVar( tariff( iTariff ).nativeMidPeakEnergy ).values( kMonth ) = tariff( iTariff ).gatherEnergy( kMonth, periodMidPeak );
				//nativeMidPeakDemand
				econVar( tariff( iTariff ).nativeMidPeakDemand ).values( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodMidPeak );
				//nativePeakExceedsOffPeak
				monthVal( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodPeak ) - tariff( iTariff ).gatherDemand( kMonth, periodOffPeak );
				if ( monthVal( kMonth ) > 0 ) {
					econVar( tariff( iTariff ).nativePeakExceedsOffPeak ).values( kMonth ) = monthVal( kMonth );
				} else {
					econVar( tariff( iTariff ).nativePeakExceedsOffPeak ).values( kMonth ) = 0.0;
				}
				//nativeOffPeakExceedsPeak
				monthVal( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodOffPeak ) - tariff( iTariff ).gatherDemand( kMonth, periodPeak );
				if ( monthVal( kMonth ) > 0 ) {
					econVar( tariff( iTariff ).nativeOffPeakExceedsPeak ).values( kMonth ) = monthVal( kMonth );
				} else {
					econVar( tariff( iTariff ).nativeOffPeakExceedsPeak ).values( kMonth ) = 0.0;
				}
				//nativePeakExceedsMidPeak
				monthVal( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodPeak ) - tariff( iTariff ).gatherDemand( kMonth, periodMidPeak );
				if ( monthVal( kMonth ) > 0 ) {
					econVar( tariff( iTariff ).nativePeakExceedsMidPeak ).values( kMonth ) = monthVal( kMonth );
				} else {
					econVar( tariff( iTariff ).nativePeakExceedsOffPeak ).values( kMonth ) = 0.0;
				}
				//nativeMidPeakExceedsPeak
				monthVal( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodMidPeak ) - tariff( iTariff ).gatherDemand( kMonth, periodPeak );
				if ( monthVal( kMonth ) > 0 ) {
					econVar( tariff( iTariff ).nativeMidPeakExceedsPeak ).values( kMonth ) = monthVal( kMonth );
				} else {
					econVar( tariff( iTariff ).nativeMidPeakExceedsPeak ).values( kMonth ) = 0.0;
				}
				//nativePeakExceedsShoulder
				monthVal( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodPeak ) - tariff( iTariff ).gatherDemand( kMonth, periodShoulder );
				if ( monthVal( kMonth ) > 0 ) {
					econVar( tariff( iTariff ).nativePeakExceedsShoulder ).values( kMonth ) = monthVal( kMonth );
				} else {
					econVar( tariff( iTariff ).nativePeakExceedsShoulder ).values( kMonth ) = 0.0;
				}
				//nativeShoulderExceedsPeak
				monthVal( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodShoulder ) - tariff( iTariff ).gatherDemand( kMonth, periodPeak );
				if ( monthVal( kMonth ) > 0 ) {
					econVar( tariff( iTariff ).nativeShoulderExceedsPeak ).values( kMonth ) = monthVal( kMonth );
				} else {
					econVar( tariff( iTariff ).nativeShoulderExceedsPeak ).values( kMonth ) = 0.0;
				}
				//nativeIsWinter
				//nativeIsNotWinter
				if ( tariff( iTariff ).seasonForMonth( kMonth ) == seasonWinter ) {
					econVar( tariff( iTariff ).nativeIsWinter ).values( kMonth ) = 1.0;
					econVar( tariff( iTariff ).nativeIsNotWinter ).values( kMonth ) = 0.0;
				} else {
					econVar( tariff( iTariff ).nativeIsWinter ).values( kMonth ) = 0.0;
					econVar( tariff( iTariff ).nativeIsNotWinter ).values( kMonth ) = 1.0;
				}
				//nativeIsSpring
				//nativeIsNotSpring
				if ( tariff( iTariff ).seasonForMonth( kMonth ) == seasonSpring ) {
					econVar( tariff( iTariff ).nativeIsSpring ).values( kMonth ) = 1.0;
					econVar( tariff( iTariff ).nativeIsNotSpring ).values( kMonth ) = 0.0;
				} else {
					econVar( tariff( iTariff ).nativeIsSpring ).values( kMonth ) = 0.0;
					econVar( tariff( iTariff ).nativeIsNotSpring ).values( kMonth ) = 1.0;
				}
				//nativeIsSummer
				//nativeIsNotSummer
				if ( tariff( iTariff ).seasonForMonth( kMonth ) == seasonSummer ) {
					econVar( tariff( iTariff ).nativeIsSummer ).values( kMonth ) = 1.0;
					econVar( tariff( iTariff ).nativeIsNotSummer ).values( kMonth ) = 0.0;
				} else {
					econVar( tariff( iTariff ).nativeIsSummer ).values( kMonth ) = 0.0;
					econVar( tariff( iTariff ).nativeIsNotSummer ).values( kMonth ) = 1.0;
				}
				//nativeIsAutumn
				//nativeIsNotAutumn
				if ( tariff( iTariff ).seasonForMonth( kMonth ) == seasonFall ) {
					econVar( tariff( iTariff ).nativeIsAutumn ).values( kMonth ) = 1.0;
					econVar( tariff( iTariff ).nativeIsNotAutumn ).values( kMonth ) = 0.0;
				} else {
					econVar( tariff( iTariff ).nativeIsAutumn ).values( kMonth ) = 0.0;
					econVar( tariff( iTariff ).nativeIsNotAutumn ).values( kMonth ) = 1.0;
				}
				//nativePeakAndShoulderEnergy
				econVar( tariff( iTariff ).nativePeakAndShoulderEnergy ).values( kMonth ) = tariff( iTariff ).gatherEnergy( kMonth, periodPeak ) + tariff( iTariff ).gatherEnergy( kMonth, periodShoulder );
				//nativePeakAndShoulderDemand
				if ( tariff( iTariff ).gatherDemand( kMonth, periodPeak ) > tariff( iTariff ).gatherDemand( kMonth, periodShoulder ) ) {
					econVar( tariff( iTariff ).nativePeakAndShoulderDemand ).values( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodPeak );
				} else {
					econVar( tariff( iTariff ).nativePeakAndShoulderDemand ).values( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodShoulder );
				}
				//nativePeakAndMidPeakEnergy
				econVar( tariff( iTariff ).nativePeakAndMidPeakEnergy ).values( kMonth ) = tariff( iTariff ).gatherEnergy( kMonth, periodPeak ) + tariff( iTariff ).gatherEnergy( kMonth, periodMidPeak );
				//nativePeakAndMidPeakDemand
				if ( tariff( iTariff ).gatherDemand( kMonth, periodPeak ) > tariff( iTariff ).gatherDemand( kMonth, periodMidPeak ) ) {
					econVar( tariff( iTariff ).nativePeakAndMidPeakDemand ).values( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodPeak );
				} else {
					econVar( tariff( iTariff ).nativePeakAndMidPeakDemand ).values( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodMidPeak );
				}
				//nativeShoulderAndOffPeakEnergy
				econVar( tariff( iTariff ).nativeShoulderAndOffPeakEnergy ).values( kMonth ) = tariff( iTariff ).gatherEnergy( kMonth, periodShoulder ) + tariff( iTariff ).gatherEnergy( kMonth, periodOffPeak );
				//nativeShoulderAndOffPeakDemand
				if ( tariff( iTariff ).gatherDemand( kMonth, periodShoulder ) > tariff( iTariff ).gatherDemand( kMonth, periodOffPeak ) ) {
					econVar( tariff( iTariff ).nativeShoulderAndOffPeakDemand ).values( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodShoulder );
				} else {
					econVar( tariff( iTariff ).nativeShoulderAndOffPeakDemand ).values( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodOffPeak );
				}
				//nativePeakAndOffPeakEnergy
				econVar( tariff( iTariff ).nativePeakAndOffPeakEnergy ).values( kMonth ) = tariff( iTariff ).gatherEnergy( kMonth, periodPeak ) + tariff( iTariff ).gatherEnergy( kMonth, periodOffPeak );
				//nativePeakAndOffPeakDemand
				if ( tariff( iTariff ).gatherDemand( kMonth, periodPeak ) > tariff( iTariff ).gatherDemand( kMonth, periodOffPeak ) ) {
					econVar( tariff( iTariff ).nativePeakAndOffPeakDemand ).values( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodPeak );
				} else {
					econVar( tariff( iTariff ).nativePeakAndOffPeakDemand ).values( kMonth ) = tariff( iTariff ).gatherDemand( kMonth, periodOffPeak );
				}
				//nativeRealTimePriceCosts
				econVar( tariff( iTariff ).nativeRealTimePriceCosts ).values( kMonth ) = tariff( iTariff ).RTPcost( kMonth );
				//nativeAboveCustomerBaseCosts
				econVar( tariff( iTariff ).nativeAboveCustomerBaseCosts ).values( kMonth ) = tariff( iTariff ).RTPaboveBaseCost( kMonth );
				//nativeBelowCustomerBaseCosts
				econVar( tariff( iTariff ).nativeBelowCustomerBaseCosts ).values( kMonth ) = tariff( iTariff ).RTPbelowBaseCost( kMonth );
				//nativeAboveCustomerBaseEnergy
				econVar( tariff( iTariff ).nativeAboveCustomerBaseEnergy ).values( kMonth ) = tariff( iTariff ).RTPaboveBaseEnergy( kMonth );
				//nativeBelowCustomerBaseEnergy
				econVar( tariff( iTariff ).nativeBelowCustomerBaseEnergy ).values( kMonth ) = tariff( iTariff ).RTPbelowBaseEnergy( kMonth );
			}
		}
	}

	void
	LEEDtariffReporting()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   October 2012
		//    MODIFIED
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Write the economic results for LEED reporting

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace OutputReportPredefined;

		// Locals

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int elecFacilMeter;
		int gasFacilMeter;
		Real64 elecTotalEne;
		Real64 gasTotalEne;
		Real64 otherTotalEne;
		Real64 elecTotalCost;
		Real64 gasTotalCost;
		Real64 otherTotalCost;
		Real64 allTotalCost;
		std::string elecTariffNames;
		std::string gasTariffNames;
		std::string othrTariffNames;
		int elecUnits;
		int gasUnits;
		int othrUnits;
		int gasDemWindowUnits;
		int othrDemWindowUnits;
		int iTariff;

		if ( numTariff > 0 ) {
			elecFacilMeter = GetMeterIndex( "ELECTRICITY:FACILITY" );
			gasFacilMeter = GetMeterIndex( "GAS:FACILITY" );
			elecTotalEne = 0.0;
			gasTotalEne = 0.0;
			otherTotalEne = 0.0;
			elecTotalCost = 0.0;
			gasTotalCost = 0.0;
			otherTotalCost = 0.0;
			allTotalCost = 0.0;
			elecUnits = 0;
			gasUnits = 0;
			othrUnits = 0;
			gasDemWindowUnits = 0;
			othrDemWindowUnits = 0;
			elecTariffNames = "";
			gasTariffNames = "";
			othrTariffNames = "";
			for ( iTariff = 1; iTariff <= numTariff; ++iTariff ) {
				if ( tariff( iTariff ).isSelected ) {
					allTotalCost += tariff( iTariff ).totalAnnualCost;
					if ( tariff( iTariff ).kindElectricMtr >= kindMeterElecSimple ) {
						if ( tariff( iTariff ).totalAnnualEnergy > elecTotalEne ) elecTotalEne = tariff( iTariff ).totalAnnualEnergy;
						elecTotalCost += tariff( iTariff ).totalAnnualCost;
						elecTariffNames += ' ' + tariff( iTariff ).tariffName;
						elecUnits = tariff( iTariff ).convChoice;
					} else if ( tariff( iTariff ).reportMeterIndx == gasFacilMeter ) {
						if ( tariff( iTariff ).totalAnnualEnergy > gasTotalEne ) gasTotalEne = tariff( iTariff ).totalAnnualEnergy;
						gasTotalCost += tariff( iTariff ).totalAnnualCost;
						gasTariffNames += ' ' + tariff( iTariff ).tariffName;
						gasUnits = tariff( iTariff ).convChoice;
						gasDemWindowUnits = tariff( iTariff ).demandWindow;
					} else {
						if ( tariff( iTariff ).totalAnnualEnergy > otherTotalEne ) otherTotalEne = tariff( iTariff ).totalAnnualEnergy;
						otherTotalCost += tariff( iTariff ).totalAnnualCost;
						othrTariffNames += ' ' + tariff( iTariff ).tariffName;
						othrUnits = tariff( iTariff ).convChoice;
						othrDemWindowUnits = tariff( iTariff ).demandWindow;
					}
				}
			}
			//names of the rates
			PreDefTableEntry( pdchLeedEtsRtNm, "Electricity", elecTariffNames );
			PreDefTableEntry( pdchLeedEtsRtNm, "Natural Gas", gasTariffNames );
			PreDefTableEntry( pdchLeedEtsRtNm, "Other", othrTariffNames );
			//virtual rate
			if ( elecTotalEne != 0 ) PreDefTableEntry( pdchLeedEtsVirt, "Electricity", elecTotalCost / elecTotalEne, 3 );
			if ( gasTotalEne != 0 ) PreDefTableEntry( pdchLeedEtsVirt, "Natural Gas", gasTotalCost / gasTotalEne, 3 );
			if ( otherTotalEne != 0 ) PreDefTableEntry( pdchLeedEtsVirt, "Other", otherTotalCost / otherTotalEne, 3 );
			//units
			PreDefTableEntry( pdchLeedEtsEneUnt, "Electricity", convEneStrings( elecUnits ) );
			PreDefTableEntry( pdchLeedEtsEneUnt, "Natural Gas", convEneStrings( gasUnits ) );
			PreDefTableEntry( pdchLeedEtsEneUnt, "Other", convEneStrings( othrUnits ) );
			PreDefTableEntry( pdchLeedEtsDemUnt, "Electricity", convDemStrings( elecUnits ) );
			PreDefTableEntry( pdchLeedEtsDemUnt, "Natural Gas", convDemStrings( gasUnits ) + demWindowStrings( gasDemWindowUnits ) );
			PreDefTableEntry( pdchLeedEtsDemUnt, "Other", convDemStrings( othrUnits ) + demWindowStrings( othrDemWindowUnits ) );
			// total cost
			PreDefTableEntry( pdchLeedEcsTotal, "Electricity", elecTotalCost, 2 );
			PreDefTableEntry( pdchLeedEcsTotal, "Natural Gas", gasTotalCost, 2 );
			PreDefTableEntry( pdchLeedEcsTotal, "Other", otherTotalCost, 2 );
			// save the total costs for later to compute process fraction
			LEEDelecCostTotal = elecTotalCost;
			LEEDgasCostTotal = gasTotalCost;
			LEEDothrCostTotal = otherTotalCost;
			PreDefTableEntry( pdchLeedEcsTotal, "Total", elecTotalCost + gasTotalCost + otherTotalCost, 2 );
		}
	}

	void
	WriteTabularTariffReports()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       January 2010, Kyle Benne
		//                   Added SQLite output
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputReportTabular::WriteReportHeaders;
		using OutputReportTabular::WriteSubtitle;
		using OutputReportTabular::WriteTable;
		using OutputReportTabular::RealToStr;
		using OutputReportTabular::WriteTextLine;
		using OutputReportTabular::buildingGrossFloorArea;
		using OutputReportTabular::buildingConditionedFloorArea;
		using OutputReportTabular::DetermineBuildingFloorArea;
		using OutputReportTabular::LookupSItoIP;
		using OutputReportTabular::ConvertIP;
		using OutputReportTabular::unitsStyle;
		using OutputReportTabular::unitsStyleInchPound;

		// Locals

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// all arrays are in the format: (row, column)
		Array1D_string columnHead;
		Array1D_int columnWidth;
		Array1D_string rowHead;
		Array2D_string tableBody;
		//other local variables
		int elecFacilMeter;
		int gasFacilMeter;
		Real64 elecTotalCost;
		Real64 gasTotalCost;
		Real64 otherTotalCost;
		Real64 allTotalCost;
		std::string outString; // an arbitarilty long string
		int curStep;
		int indexInChg;
		int iTariff;
		int kVar;
		int lStep;
		static std::string SIunit;
		static int unitConvIndex( 0 );
		static Real64 perAreaUnitConv( 0.0 );
		static std::string perAreaUnitName;

		// compute floor area if no ABUPS
		if ( buildingConditionedFloorArea == 0.0 ) {
			DetermineBuildingFloorArea();
		}

		// do unit conversions if necessary
		if ( unitsStyle == unitsStyleInchPound ) {
			SIunit = "[~~$~~/m2]";
			LookupSItoIP( SIunit, unitConvIndex, perAreaUnitName );
			perAreaUnitConv = ConvertIP( unitConvIndex, 1.0 );
		} else {
			perAreaUnitName = "[~~$~~/m2]";
			perAreaUnitConv = 1.0;
		}

		if ( numTariff > 0 ) {
			DisplayString( "Writing Tariff Reports" );
			for ( auto & e : econVar ) e.isReported = false;
			//CALL selectTariff moved to the end of computeTariff.
			showWarningsBasedOnTotal();
			//---------------------------------
			// Economics Results Summary Report
			//---------------------------------
			WriteReportHeaders( "Economics Results Summary Report", "Entire Facility", 1 );
			elecFacilMeter = GetMeterIndex( "ELECTRICITY:FACILITY" );
			gasFacilMeter = GetMeterIndex( "GAS:FACILITY" );
			//---- Annual Summary
			rowHead.allocate( 3 );
			columnHead.allocate( 4 );
			columnWidth.allocate( 4 );
			tableBody.allocate( 4, 3 );
			tableBody = "";
			columnHead( 1 ) = "Electric";
			columnHead( 2 ) = "Gas";
			columnHead( 3 ) = "Other";
			columnHead( 4 ) = "Total";
			rowHead( 1 ) = "Cost [~~$~~]";
			rowHead( 2 ) = "Cost per Total Building Area " + perAreaUnitName;
			rowHead( 3 ) = "Cost per Net Conditioned Building Area " + perAreaUnitName;
			elecTotalCost = 0.0;
			gasTotalCost = 0.0;
			otherTotalCost = 0.0;
			allTotalCost = 0.0;
			for ( iTariff = 1; iTariff <= numTariff; ++iTariff ) {
				if ( tariff( iTariff ).isSelected ) {
					allTotalCost += tariff( iTariff ).totalAnnualCost;
					if ( tariff( iTariff ).kindElectricMtr >= kindMeterElecSimple ) {
						elecTotalCost += tariff( iTariff ).totalAnnualCost;
					} else if ( tariff( iTariff ).reportMeterIndx == gasFacilMeter ) {
						gasTotalCost += tariff( iTariff ).totalAnnualCost;
					} else {
						otherTotalCost += tariff( iTariff ).totalAnnualCost;
						// removed because this was confusing        columnHead(3) = tariff(iTariff)%reportMeter
					}
				}
			}
			tableBody( 1, 1 ) = RealToStr( elecTotalCost, 2 );
			tableBody( 2, 1 ) = RealToStr( gasTotalCost, 2 );
			tableBody( 3, 1 ) = RealToStr( otherTotalCost, 2 );
			tableBody( 4, 1 ) = RealToStr( allTotalCost, 2 );
			if ( buildingGrossFloorArea > 0.0 ) {
				tableBody( 1, 2 ) = RealToStr( ( elecTotalCost / buildingGrossFloorArea ) * perAreaUnitConv, 2 );
				tableBody( 2, 2 ) = RealToStr( ( gasTotalCost / buildingGrossFloorArea ) * perAreaUnitConv, 2 );
				tableBody( 3, 2 ) = RealToStr( ( otherTotalCost / buildingGrossFloorArea ) * perAreaUnitConv, 2 );
				tableBody( 4, 2 ) = RealToStr( ( allTotalCost / buildingGrossFloorArea ) * perAreaUnitConv, 2 );
			}
			if ( buildingConditionedFloorArea > 0.0 ) {
				tableBody( 1, 3 ) = RealToStr( ( elecTotalCost / buildingConditionedFloorArea ) * perAreaUnitConv, 2 );
				tableBody( 2, 3 ) = RealToStr( ( gasTotalCost / buildingConditionedFloorArea ) * perAreaUnitConv, 2 );
				tableBody( 3, 3 ) = RealToStr( ( otherTotalCost / buildingConditionedFloorArea ) * perAreaUnitConv, 2 );
				tableBody( 4, 3 ) = RealToStr( ( allTotalCost / buildingConditionedFloorArea ) * perAreaUnitConv, 2 );
			}
			columnWidth = 14; //array assignment - same for all columns
			WriteSubtitle( "Annual Cost" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Economics Results Summary Report", "Entire Facility", "Annual Cost" );
			}
			columnHead.deallocate();
			rowHead.deallocate();
			columnWidth.deallocate();
			tableBody.deallocate();
			//---- Tariff Summary
			rowHead.allocate( numTariff );
			columnHead.allocate( 6 );
			columnWidth.allocate( 6 );
			tableBody.allocate( 6, numTariff );
			tableBody = "";
			columnHead( 1 ) = "Selected";
			columnHead( 2 ) = "Qualified";
			columnHead( 3 ) = "Meter";
			columnHead( 4 ) = "Buy or Sell";
			columnHead( 5 ) = "Group";
			columnHead( 6 ) = "Annual Cost (~~$~~)";
			for ( iTariff = 1; iTariff <= numTariff; ++iTariff ) {
				rowHead( iTariff ) = tariff( iTariff ).tariffName;
				if ( tariff( iTariff ).isSelected ) {
					tableBody( 1, iTariff ) = "Yes";
				} else {
					tableBody( 1, iTariff ) = "No";
				}
				if ( tariff( iTariff ).isQualified ) {
					tableBody( 2, iTariff ) = "Yes";
				} else {
					tableBody( 2, iTariff ) = "No";
				}
				tableBody( 3, iTariff ) = tariff( iTariff ).reportMeter;
				{ auto const SELECT_CASE_var( tariff( iTariff ).buyOrSell );
				if ( SELECT_CASE_var == buyFromUtility ) {
					tableBody( 4, iTariff ) = "Buy";
				} else if ( SELECT_CASE_var == sellToUtility ) {
					tableBody( 4, iTariff ) = "Sell";
				} else if ( SELECT_CASE_var == netMetering ) {
					tableBody( 4, iTariff ) = "Net";
				}}
				if ( tariff( iTariff ).groupName == "" ) {
					tableBody( 5, iTariff ) = "(none)";
				} else {
					tableBody( 5, iTariff ) = tariff( iTariff ).groupName;
				}
				tableBody( 6, iTariff ) = RealToStr( tariff( iTariff ).totalAnnualCost, 2 );
			}
			columnWidth = 14; //array assignment - same for all columns
			WriteSubtitle( "Tariff Summary" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Economics Results Summary Report", "Entire Facility", "Tariff Summary" );
			}
			columnHead.deallocate();
			rowHead.deallocate();
			columnWidth.deallocate();
			tableBody.deallocate();
			//---------------------------------
			// Tariff Report
			//---------------------------------
			for ( iTariff = 1; iTariff <= numTariff; ++iTariff ) {
				WriteReportHeaders( "Tariff Report", tariff( iTariff ).tariffName, 1 );
				rowHead.allocate( 7 );
				columnHead.allocate( 1 );
				columnWidth.allocate( 1 );
				tableBody.allocate( 1, 7 );
				tableBody = "";
				columnHead( 1 ) = "Parameter";
				rowHead( 1 ) = "Meter";
				rowHead( 2 ) = "Selected";
				rowHead( 3 ) = "Group";
				rowHead( 4 ) = "Qualified";
				rowHead( 5 ) = "Disqualifier";
				rowHead( 6 ) = "Computation";
				rowHead( 7 ) = "Units";
				tableBody( 1, 1 ) = tariff( iTariff ).reportMeter;
				if ( tariff( iTariff ).isSelected ) {
					tableBody( 1, 2 ) = "Yes";
				} else {
					tableBody( 1, 2 ) = "No";
				}
				if ( tariff( iTariff ).groupName == "" ) {
					tableBody( 1, 3 ) = "(none)";
				} else {
					tableBody( 1, 3 ) = tariff( iTariff ).groupName;
				}
				if ( tariff( iTariff ).isQualified ) {
					tableBody( 1, 4 ) = "Yes";
				} else {
					tableBody( 1, 4 ) = "No";
				}
				if ( tariff( iTariff ).isQualified ) {
					tableBody( 1, 5 ) = "n/a";
				} else {
					tableBody( 1, 5 ) = econVar( tariff( iTariff ).ptDisqualifier ).name;
				}
				if ( computation( iTariff ).isUserDef ) {
					tableBody( 1, 6 ) = computation( iTariff ).computeName;
				} else {
					tableBody( 1, 6 ) = "automatic";
				}
				{ auto const SELECT_CASE_var( tariff( iTariff ).convChoice );
				if ( SELECT_CASE_var == conversionUSERDEF ) {
					tableBody( 1, 7 ) = "User Defined";
				} else if ( SELECT_CASE_var == conversionKWH ) {
					tableBody( 1, 7 ) = "kWh";
				} else if ( SELECT_CASE_var == conversionTHERM ) {
					tableBody( 1, 7 ) = "Therm";
				} else if ( SELECT_CASE_var == conversionMMBTU ) {
					tableBody( 1, 7 ) = "MMBtu";
				} else if ( SELECT_CASE_var == conversionMJ ) {
					tableBody( 1, 7 ) = "MJ";
				} else if ( SELECT_CASE_var == conversionKBTU ) {
					tableBody( 1, 7 ) = "kBtu";
				} else if ( SELECT_CASE_var == conversionMCF ) {
					tableBody( 1, 7 ) = "MCF";
				} else if ( SELECT_CASE_var == conversionCCF ) {
					tableBody( 1, 7 ) = "CCF";
				}}
				columnWidth = 14; //array assignment - same for all columns
				WriteSubtitle( "General" );
				WriteTable( tableBody, rowHead, columnHead, columnWidth );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Tariff Report", tariff( iTariff ).tariffName, "General" );
				}
				columnHead.deallocate();
				rowHead.deallocate();
				columnWidth.deallocate();
				tableBody.deallocate();
				//---- Categories
				for ( auto & e : econVar ) e.activeNow = false;
				econVar( tariff( iTariff ).ptEnergyCharges ).activeNow = true;
				econVar( tariff( iTariff ).ptDemandCharges ).activeNow = true;
				econVar( tariff( iTariff ).ptServiceCharges ).activeNow = true;
				econVar( tariff( iTariff ).ptBasis ).activeNow = true;
				econVar( tariff( iTariff ).ptAdjustment ).activeNow = true;
				econVar( tariff( iTariff ).ptSurcharge ).activeNow = true;
				econVar( tariff( iTariff ).ptSubtotal ).activeNow = true;
				econVar( tariff( iTariff ).ptTaxes ).activeNow = true;
				econVar( tariff( iTariff ).ptTotal ).activeNow = true;
				ReportEconomicVariable( "Categories", false, true, tariff( iTariff ).tariffName );
				//---- Charges
				for ( auto & e : econVar ) e.activeNow = false;
				for ( kVar = 1; kVar <= numEconVar; ++kVar ) {
					if ( econVar( kVar ).tariffIndx == iTariff ) {
						if ( ( econVar( kVar ).kindOfObj == kindChargeSimple ) || ( econVar( kVar ).kindOfObj == kindChargeBlock ) ) {
							econVar( kVar ).activeNow = true;
						}
					}
				}
				ReportEconomicVariable( "Charges", true, true, tariff( iTariff ).tariffName );
				//---- Sources for Charges
				for ( auto & e : econVar ) e.activeNow = false;
				for ( kVar = 1; kVar <= numEconVar; ++kVar ) {
					if ( econVar( kVar ).tariffIndx == iTariff ) {
						indexInChg = econVar( kVar ).index;
						if ( econVar( kVar ).kindOfObj == kindChargeSimple ) {
							if ( chargeSimple( indexInChg ).sourcePt > 0 ) {
								econVar( chargeSimple( indexInChg ).sourcePt ).activeNow = true;
							}
						} else if ( econVar( kVar ).kindOfObj == kindChargeBlock ) {
							if ( chargeBlock( indexInChg ).sourcePt > 0 ) {
								econVar( chargeBlock( indexInChg ).sourcePt ).activeNow = true;
							}
						}
					}
				}
				ReportEconomicVariable( "Corresponding Sources for Charges", false, false, tariff( iTariff ).tariffName );
				//---- Rachets
				for ( auto & e : econVar ) e.activeNow = false;
				for ( kVar = 1; kVar <= numEconVar; ++kVar ) {
					if ( econVar( kVar ).tariffIndx == iTariff ) {
						if ( econVar( kVar ).kindOfObj == kindRatchet ) {
							econVar( kVar ).activeNow = true;
						}
					}
				}
				ReportEconomicVariable( "Ratchets", false, false, tariff( iTariff ).tariffName );
				//---- Qualifies
				for ( auto & e : econVar ) e.activeNow = false;
				for ( kVar = 1; kVar <= numEconVar; ++kVar ) {
					if ( econVar( kVar ).tariffIndx == iTariff ) {
						if ( econVar( kVar ).kindOfObj == kindQualify ) {
							econVar( kVar ).activeNow = true;
						}
					}
				}
				ReportEconomicVariable( "Qualifies", false, false, tariff( iTariff ).tariffName );
				//---- Native Variables
				for ( auto & e : econVar ) e.activeNow = false;
				for ( kVar = tariff( iTariff ).firstNative; kVar <= tariff( iTariff ).lastNative; ++kVar ) {
					econVar( kVar ).activeNow = true;
				}
				ReportEconomicVariable( "Native Variables", false, false, tariff( iTariff ).tariffName );
				//---- Other Variables
				for ( auto & e : econVar ) e.activeNow = false;
				for ( kVar = 1; kVar <= numEconVar; ++kVar ) {
					if ( econVar( kVar ).tariffIndx == iTariff ) {
						if ( ! econVar( kVar ).isReported ) {
							econVar( kVar ).activeNow = true;
						}
					}
				}
				ReportEconomicVariable( "Other Variables", false, false, tariff( iTariff ).tariffName );
				//---- Computation
				if ( computation( iTariff ).isUserDef ) {
					WriteTextLine( "Computation -  User Defined", true );
				} else {
					WriteTextLine( "Computation -  Automatic", true );
				}
				outString = "";
				for ( lStep = computation( iTariff ).firstStep; lStep <= computation( iTariff ).lastStep; ++lStep ) {
					curStep = steps( lStep );
					{ auto const SELECT_CASE_var( curStep );
					if ( SELECT_CASE_var == 0 ) { //end of line
						WriteTextLine( rstrip( outString ) );
						outString = "";
					} else if ( ( SELECT_CASE_var >= 1 ) ) { //all positive values are a reference to an econVar
						outString = econVar( curStep ).name + ' ' + outString;
					} else if ( SELECT_CASE_var == opSUM ) {
						outString = "SUM " + outString;
					} else if ( SELECT_CASE_var == opMULTIPLY ) {
						outString = "MULTIPLY " + outString;
					} else if ( SELECT_CASE_var == opSUBTRACT ) {
						outString = "SUBTRACT " + outString;
					} else if ( SELECT_CASE_var == opDIVIDE ) {
						outString = "DIVIDE " + outString;
					} else if ( SELECT_CASE_var == opABSOLUTE ) {
						outString = "ABSOLUTE " + outString;
					} else if ( SELECT_CASE_var == opINTEGER ) {
						outString = "INTEGER " + outString;
					} else if ( SELECT_CASE_var == opSIGN ) {
						outString = "SIGN " + outString;
					} else if ( SELECT_CASE_var == opROUND ) {
						outString = "ROUND " + outString;
					} else if ( SELECT_CASE_var == opMAXIMUM ) {
						outString = "MAXIMUM " + outString;
					} else if ( SELECT_CASE_var == opMINIMUM ) {
						outString = "MINIMUM " + outString;
					} else if ( SELECT_CASE_var == opEXCEEDS ) {
						outString = "EXCEEDS " + outString;
					} else if ( SELECT_CASE_var == opANNUALMINIMUM ) {
						outString = "ANNUALMINIMUM " + outString;
					} else if ( SELECT_CASE_var == opANNUALMAXIMUM ) {
						outString = "ANNUALMAXIMUM " + outString;
					} else if ( SELECT_CASE_var == opANNUALSUM ) {
						outString = "ANNUALSUM " + outString;
					} else if ( SELECT_CASE_var == opANNUALAVERAGE ) {
						outString = "ANNUALAVERAGE " + outString;
					} else if ( SELECT_CASE_var == opANNUALOR ) {
						outString = "ANNUALOR " + outString;
					} else if ( SELECT_CASE_var == opANNUALAND ) {
						outString = "ANNUALAND " + outString;
					} else if ( SELECT_CASE_var == opANNUALMAXIMUMZERO ) {
						outString = "ANNUALMAXIMUMZERO " + outString;
					} else if ( SELECT_CASE_var == opANNUALMINIMUMZERO ) {
						outString = "ANNUALMINIMUMZERO " + outString;
					} else if ( SELECT_CASE_var == opIF ) {
						outString = "IF " + outString;
					} else if ( SELECT_CASE_var == opGREATERTHAN ) {
						outString = "GREATERTHAN " + outString;
					} else if ( SELECT_CASE_var == opGREATEREQUAL ) {
						outString = "GREATEREQUAL " + outString;
					} else if ( SELECT_CASE_var == opLESSTHAN ) {
						outString = "LESSTHAN " + outString;
					} else if ( SELECT_CASE_var == opLESSEQUAL ) {
						outString = "LESSEQUAL " + outString;
					} else if ( SELECT_CASE_var == opEQUAL ) {
						outString = "EQUAL " + outString;
					} else if ( SELECT_CASE_var == opNOTEQUAL ) {
						outString = "NOTEQUAL " + outString;
					} else if ( SELECT_CASE_var == opAND ) {
						outString = "AND " + outString;
					} else if ( SELECT_CASE_var == opOR ) {
						outString = "OR " + outString;
					} else if ( SELECT_CASE_var == opNOT ) {
						outString = "NOT " + outString;
					} else if ( SELECT_CASE_var == opADD ) {
						outString = "ADD " + outString;
					} else if ( SELECT_CASE_var == opNOOP ) { //should clear the outString when done debugging
						//outString = ''
						outString = "FROM " + outString;
					}}
				}
			}
		}
	}

	void
	showWarningsBasedOnTotal()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Get the annual maximum and sum for the econVariable.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iTariff;

		if ( numTariff > 0 ) {
			for ( iTariff = 1; iTariff <= numTariff; ++iTariff ) {
				{ auto const SELECT_CASE_var( tariff( iTariff ).buyOrSell );
				if ( SELECT_CASE_var == buyFromUtility ) {
					if ( tariff( iTariff ).totalAnnualCost < 0 ) {
						ShowWarningError( "UtilityCost:Tariff: A negative annual total cost when buying electricity from a utility is unusual. " );
						ShowContinueError( "  In UtilityCost:Tariff named " + tariff( iTariff ).tariffName );
					}
				} else if ( SELECT_CASE_var == sellToUtility ) {
					if ( tariff( iTariff ).totalAnnualCost > 0 ) {
						ShowWarningError( "UtilityCost:Tariff: A positive annual total cost when selling electricity to a utility is unusual. " );
						ShowContinueError( "  In UtilityCost:Tariff named " + tariff( iTariff ).tariffName );
					}
				}}
			}
		}
	}

	void
	getMaxAndSum(
		int const varPointer,
		Real64 & sumResult,
		Real64 & maxResult
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Get the annual maximum and sum for the econVariable.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 sumVal;
		Real64 maximumVal( 0.0 ); //Autodesk Value not used but suppresses warning about huge() call
		Real64 curVal;
		int jMonth;

		sumVal = 0.0;
		maximumVal = -huge( maximumVal );
		for ( jMonth = 1; jMonth <= 12; ++jMonth ) { //note not all months get printed out if more than 12 are used.- need to fix this later
			curVal = econVar( varPointer ).values( jMonth );
			sumVal += curVal;
			if ( curVal > maximumVal ) {
				maximumVal = curVal;
			}
		}
		sumResult = sumVal;
		maxResult = maximumVal;
	}

	void
	ReportEconomicVariable(
		std::string const & titleString,
		bool const includeCategory,
		bool const showCurrencySymbol,
		std::string const & forString
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       January 2010, Kyle Benne
		//                   Added sqlite output
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Report all econVar that show as activeNow

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputReportTabular::WriteReportHeaders;
		using OutputReportTabular::WriteSubtitle;
		using OutputReportTabular::WriteTable;
		using OutputReportTabular::RealToStr;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// The majority of the input is the econVar array

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// all arrays are in the format: (row, column)
		Array1D_string columnHead;
		Array1D_int columnWidth;
		Array1D_string rowHead;
		Array2D_string tableBody;
		Real64 sumVal;
		Real64 maximumVal;
		Real64 curVal;
		int curIndex;
		int curCatPt;
		int curCategory;

		int iVar;
		int jMonth;
		int cntOfVar;
		int nCntOfVar;

		cntOfVar = 0;
		for ( iVar = 1; iVar <= numEconVar; ++iVar ) {
			if ( econVar( iVar ).activeNow ) {
				++cntOfVar;
			}
		}
		if ( includeCategory ) {
			rowHead.allocate( cntOfVar );
			columnHead.allocate( 15 );
			columnWidth.allocate( 15 );
			tableBody.allocate( 15, cntOfVar );
		} else {
			rowHead.allocate( cntOfVar );
			columnHead.allocate( 14 );
			columnWidth.allocate( 14 );
			tableBody.allocate( 14, cntOfVar );
		}
		// column names
		columnHead( 1 ) = "Jan";
		columnHead( 2 ) = "Feb";
		columnHead( 3 ) = "Mar";
		columnHead( 4 ) = "Apr";
		columnHead( 5 ) = "May";
		columnHead( 6 ) = "Jun";
		columnHead( 7 ) = "Jul";
		columnHead( 8 ) = "Aug";
		columnHead( 9 ) = "Sep";
		columnHead( 10 ) = "Oct";
		columnHead( 11 ) = "Nov";
		columnHead( 12 ) = "Dec";
		columnHead( 13 ) = "Sum";
		columnHead( 14 ) = "Max";
		if ( includeCategory ) {
			columnHead( 15 ) = "Category";
		}
		nCntOfVar = 0;
		//row names
		for ( iVar = 1; iVar <= numEconVar; ++iVar ) {
			if ( econVar( iVar ).activeNow ) {
				++nCntOfVar;
				if ( showCurrencySymbol ) {
					rowHead( nCntOfVar ) = econVar( iVar ).name + " (~~$~~)";
				} else {
					rowHead( nCntOfVar ) = econVar( iVar ).name;
				}
			}
		}
		// fill the body
		nCntOfVar = 0;
		for ( iVar = 1; iVar <= numEconVar; ++iVar ) {
			if ( econVar( iVar ).activeNow ) {
				++nCntOfVar;
				for ( jMonth = 1; jMonth <= 12; ++jMonth ) { //note not all months get printed out if more than 12 are used.- need to fix this later
					curVal = econVar( iVar ).values( jMonth );
					if ( ( curVal > 0 ) && ( curVal < 1 ) ) {
						tableBody( jMonth, nCntOfVar ) = RealToStr( curVal, 4 );
					} else {
						tableBody( jMonth, nCntOfVar ) = RealToStr( curVal, 2 );
					}
				}
				getMaxAndSum( iVar, sumVal, maximumVal );
				tableBody( 13, nCntOfVar ) = RealToStr( sumVal, 2 );
				tableBody( 14, nCntOfVar ) = RealToStr( maximumVal, 2 );
				if ( includeCategory ) {
					//first find category
					curCategory = 0;
					curIndex = econVar( iVar ).index;
					{ auto const SELECT_CASE_var( econVar( iVar ).kindOfObj );
					if ( SELECT_CASE_var == kindChargeSimple ) {
						if ( ( curIndex >= 1 ) && ( curIndex <= numChargeSimple ) ) {
							curCatPt = chargeSimple( curIndex ).categoryPt;
						}
					} else if ( SELECT_CASE_var == kindChargeBlock ) {
						if ( ( curIndex >= 1 ) && ( curIndex <= numChargeBlock ) ) {
							curCatPt = chargeBlock( curIndex ).categoryPt;
						}
					}}
					if ( ( curCatPt >= 1 ) && ( curCatPt <= numEconVar ) ) {
						curCategory = econVar( curCatPt ).specific;
					}
					{ auto const SELECT_CASE_var( curCategory );
					if ( SELECT_CASE_var == catEnergyCharges ) {
						tableBody( 15, nCntOfVar ) = "EnergyCharges";
					} else if ( SELECT_CASE_var == catDemandCharges ) {
						tableBody( 15, nCntOfVar ) = "DemandCharges";
					} else if ( SELECT_CASE_var == catServiceCharges ) {
						tableBody( 15, nCntOfVar ) = "ServiceCharges";
					} else if ( SELECT_CASE_var == catBasis ) {
						tableBody( 15, nCntOfVar ) = "Basis";
					} else if ( SELECT_CASE_var == catAdjustment ) {
						tableBody( 15, nCntOfVar ) = "Adjustment";
					} else if ( SELECT_CASE_var == catSurcharge ) {
						tableBody( 15, nCntOfVar ) = "Surcharge";
					} else if ( SELECT_CASE_var == catSubtotal ) {
						tableBody( 15, nCntOfVar ) = "Subtotal";
					} else if ( SELECT_CASE_var == catTaxes ) {
						tableBody( 15, nCntOfVar ) = "Taxes";
					} else if ( SELECT_CASE_var == catTotal ) {
						tableBody( 15, nCntOfVar ) = "Total";
					} else {
						tableBody( 15, nCntOfVar ) = "none";
					}}
				}
				econVar( iVar ).isReported = true;
			}
		}
		columnWidth = 14; //array assignment - same for all columns
		WriteSubtitle( titleString );
		WriteTable( tableBody, rowHead, columnHead, columnWidth );
		if ( sqlite ) {
			sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Tariff Report", forString, titleString );
		}
		columnHead.deallocate();
		rowHead.deallocate();
		columnWidth.deallocate();
		tableBody.deallocate();

	}

	void
	selectTariff()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    To select tariffs for each combination of meter and
		//    group.  If multipler tariffs have the same meter and
		//    group, then select the one with the lowest cost.
		//    For electric tariffs, since they may have buy, sell, or
		//    netmetering, they need to be combined more carefully.
		//    Multiple meters are used but buy + sell might be more or
		//    less expensive than netmeter.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputProcessor::EnergyMeters;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int totalVarPt;
		int totEneVarPt;
		Real64 annualTotal;
		Real64 annEneTotal;
		int iTariff;
		int jMonth;
		int kTariff;
		int lMin;
		int mGroup;
		Array1D_int groupIndex; // index number (in tariff) for the group name
		Array1D_int MinTariffIndex; // tariff index for the Minimum value
		int numMins;
		int curMinTariffIndex;
		bool isFound;
		int groupCount;
		int lowestSimpleTariff;
		int lowestPurchaseTariff;
		int lowestSurplusSoldTariff;
		int lowestNetMeterTariff;

		groupIndex.dimension( numTariff, 0 );
		groupCount = 0;
		numMins = 0;
		MinTariffIndex.dimension( numTariff, 0 );
		for ( iTariff = 1; iTariff <= numTariff; ++iTariff ) {
			//determine if this is meter related to electricity
			if ( tariff( iTariff ).reportMeterIndx != 0 ) {
				{ auto const SELECT_CASE_var( MakeUPPERCase( EnergyMeters( tariff( iTariff ).reportMeterIndx ).ResourceType ) );
				if ( SELECT_CASE_var == "ELECTRICITY" ) {
					tariff( iTariff ).kindElectricMtr = kindMeterElecSimple;
				} else if ( SELECT_CASE_var == "ELECTRICITYPRODUCED" ) {
					tariff( iTariff ).kindElectricMtr = kindMeterElecProduced;
				} else if ( SELECT_CASE_var == "ELECTRICITYPURCHASED" ) {
					tariff( iTariff ).kindElectricMtr = kindMeterElecPurchased;
				} else if ( SELECT_CASE_var == "ELECTRICITYSURPLUSSOLD" ) {
					tariff( iTariff ).kindElectricMtr = kindMeterElecSurplusSold;
				} else if ( SELECT_CASE_var == "ELECTRICITYNET" ) {
					tariff( iTariff ).kindElectricMtr = kindMeterElecNet;
				} else {
					tariff( iTariff ).kindElectricMtr = kindMeterNotElectric;
				}}
			} else {
				tariff( iTariff ).kindElectricMtr = kindMeterNotElectric;
			}
			// compute the total annual cost of each tariff
			totalVarPt = tariff( iTariff ).ptTotal;
			totEneVarPt = tariff( iTariff ).nativeTotalEnergy;
			annualTotal = 0.0;
			annEneTotal = 0.0;
			for ( jMonth = 1; jMonth <= MaxNumMonths; ++jMonth ) {
				annualTotal += econVar( totalVarPt ).values( jMonth );
				annEneTotal += econVar( totEneVarPt ).values( jMonth );
			}
			tariff( iTariff ).totalAnnualCost = annualTotal;
			tariff( iTariff ).totalAnnualEnergy = annEneTotal;
			// Set the groupIndex
			if ( groupIndex( iTariff ) == 0 ) {
				//set the current item to the tariff index
				++groupCount;
				groupIndex( iTariff ) = groupCount;
				//set all remaining matching items to the same index
				for ( kTariff = iTariff + 1; kTariff <= numTariff; ++kTariff ) {
					if ( SameString( tariff( kTariff ).groupName, tariff( iTariff ).groupName ) ) {
						groupIndex( kTariff ) = groupCount;
					}
				}
			}
		}
		// First process the all tariff and identify the lowest cost for each type of meter and group.
		for ( iTariff = 1; iTariff <= numTariff; ++iTariff ) {
			if ( tariff( iTariff ).isQualified ) {
				isFound = false;
				for ( lMin = 1; lMin <= numMins; ++lMin ) {
					curMinTariffIndex = MinTariffIndex( lMin );
					//find matching meter and group
					if ( tariff( iTariff ).reportMeterIndx == tariff( curMinTariffIndex ).reportMeterIndx ) {
						if ( groupIndex( iTariff ) == groupIndex( curMinTariffIndex ) ) {
							isFound = true;
							//found the matching mater and group now test if smaller Min is current tariff
							if ( tariff( iTariff ).totalAnnualCost < tariff( curMinTariffIndex ).totalAnnualCost ) {
								MinTariffIndex( lMin ) = iTariff;
								//select the new Minimum tariff and deselect the one that was just exceeded
								tariff( curMinTariffIndex ).isSelected = false;
								tariff( iTariff ).isSelected = true;
							}
						}
					}
				}
				if ( ! isFound ) {
					++numMins;
					if ( numMins > numTariff ) {
						ShowWarningError( "UtilityCost:Tariff Debugging error numMins greater than numTariff." );
					}
					MinTariffIndex( numMins ) = iTariff;
					// tariff(numMins)%isSelected = .TRUE.  !original
					tariff( iTariff ).isSelected = true; //BTG changed 2/7/2005     CR6573
				}
			}
		}
		// Now select for the electric meters. If electric buying and selling and netmetering all are going
		// on, need to determine which combination should be selected. Within each group select just one set
		// of electric results.  The electric results can be either the buy rate only, the buy rate plus the
		// sell rate, or the netmetering rate, whichever of these three is the lowest combination.
		for ( mGroup = 1; mGroup <= groupCount; ++mGroup ) {
			lowestSimpleTariff = 0;
			lowestPurchaseTariff = 0;
			lowestSurplusSoldTariff = 0;
			lowestNetMeterTariff = 0;
			for ( iTariff = 1; iTariff <= numTariff; ++iTariff ) {
				if ( tariff( iTariff ).isQualified ) {
					if ( tariff( iTariff ).isSelected ) {
						if ( groupIndex( iTariff ) == mGroup ) {
							{ auto const SELECT_CASE_var( tariff( iTariff ).kindElectricMtr );
							if ( SELECT_CASE_var == kindMeterElecSimple ) {
								lowestSimpleTariff = iTariff;
							} else if ( SELECT_CASE_var == kindMeterElecProduced ) {
								// don't show electric produced rates as ever selected since surplus sold is more relevant
								tariff( iTariff ).isSelected = false;
							} else if ( SELECT_CASE_var == kindMeterElecPurchased ) {
								lowestPurchaseTariff = iTariff;
							} else if ( SELECT_CASE_var == kindMeterElecSurplusSold ) {
								lowestSurplusSoldTariff = iTariff;
							} else if ( SELECT_CASE_var == kindMeterElecNet ) {
								lowestNetMeterTariff = iTariff;
							}}
						}
					}
				}
			}
			// compare the simple and purchased metered tariffs
			if ( ( lowestSimpleTariff > 0 ) && ( lowestPurchaseTariff > 0 ) ) {
				if ( tariff( lowestSimpleTariff ).totalAnnualCost < tariff( lowestPurchaseTariff ).totalAnnualCost ) {
					tariff( lowestPurchaseTariff ).isSelected = false;
					lowestPurchaseTariff = 0;
				} else {
					tariff( lowestSimpleTariff ).isSelected = false;
					lowestSimpleTariff = 0;
				}
			}
			// if surplus sold is negative use it otherwise don't
			if ( lowestSurplusSoldTariff > 0 ) {
				if ( tariff( lowestSurplusSoldTariff ).totalAnnualCost > 0 ) {
					tariff( lowestSurplusSoldTariff ).isSelected = false;
					lowestSurplusSoldTariff = 0;
				}
			}
			// if netmetering is used compare it to simple plus surplus
			if ( ( ( lowestNetMeterTariff > 0 ) && ( lowestSurplusSoldTariff > 0 ) ) && ( lowestSimpleTariff > 0 ) ) {
				if ( tariff( lowestNetMeterTariff ).totalAnnualCost < ( tariff( lowestSimpleTariff ).totalAnnualCost + tariff( lowestSurplusSoldTariff ).totalAnnualCost ) ) {
					tariff( lowestSimpleTariff ).isSelected = false;
					lowestSimpleTariff = 0;
					tariff( lowestSurplusSoldTariff ).isSelected = false;
					lowestSurplusSoldTariff = 0;
				} else {
					tariff( lowestNetMeterTariff ).isSelected = false;
					lowestNetMeterTariff = 0;
				}
			}
			// if netmetering is used compare it to purchased plus surplus
			if ( ( ( lowestNetMeterTariff > 0 ) && ( lowestSurplusSoldTariff > 0 ) ) && ( lowestPurchaseTariff > 0 ) ) {
				if ( tariff( lowestNetMeterTariff ).totalAnnualCost < ( tariff( lowestPurchaseTariff ).totalAnnualCost + tariff( lowestSurplusSoldTariff ).totalAnnualCost ) ) {
					tariff( lowestPurchaseTariff ).isSelected = false;
					lowestPurchaseTariff = 0;
					tariff( lowestSurplusSoldTariff ).isSelected = false;
					lowestSurplusSoldTariff = 0;
				} else {
					tariff( lowestNetMeterTariff ).isSelected = false;
					lowestNetMeterTariff = 0;
				}
			}
			// if netmetering is used compare it to simple only
			if ( ( lowestNetMeterTariff > 0 ) && ( lowestSimpleTariff > 0 ) ) {
				if ( tariff( lowestNetMeterTariff ).totalAnnualCost < tariff( lowestSimpleTariff ).totalAnnualCost ) {
					tariff( lowestSimpleTariff ).isSelected = false;
					lowestSimpleTariff = 0;
				} else {
					tariff( lowestNetMeterTariff ).isSelected = false;
					lowestNetMeterTariff = 0;
				}
			}
			// if netmetering is used compare it to purchased only
			if ( ( lowestNetMeterTariff > 0 ) && ( lowestPurchaseTariff > 0 ) ) {
				if ( tariff( lowestNetMeterTariff ).totalAnnualCost < tariff( lowestPurchaseTariff ).totalAnnualCost ) {
					tariff( lowestPurchaseTariff ).isSelected = false;
					lowestPurchaseTariff = 0;
				} else {
					tariff( lowestNetMeterTariff ).isSelected = false;
					lowestNetMeterTariff = 0;
				}
			}
		}
		groupIndex.deallocate();
		MinTariffIndex.deallocate();
	}

	void
	GetMonthlyCostForResource(
		int const inResourceNumber,
		Array1A< Real64 > outMonthlyCosts
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   May 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//  Return the total annual cost for a given resource number.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		outMonthlyCosts.dim( 12 );

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int iTariff;
		int jMonth;
		int totalVarPt;

		outMonthlyCosts = 0.0;
		for ( iTariff = 1; iTariff <= numTariff; ++iTariff ) {
			if ( tariff( iTariff ).isSelected ) {
				if ( tariff( iTariff ).resourceNum == inResourceNumber ) {
					totalVarPt = tariff( iTariff ).ptTotal;
					for ( jMonth = 1; jMonth <= 12; ++jMonth ) { //use 12 because LCC assume 12 months
						outMonthlyCosts( jMonth ) += econVar( totalVarPt ).values( jMonth );
					}
				}
			}
		}
	}

} // EconomicTariff

} // EnergyPlus
