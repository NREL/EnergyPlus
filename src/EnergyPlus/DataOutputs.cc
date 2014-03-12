// EnergyPlus Headers
#include <DataOutputs.hh>

namespace EnergyPlus {

namespace DataOutputs {

	// Module containing the data and routines dealing with prescanning for
	// requested output variables to limit the number being processed in OutputProcessor
	// Also any input counts (such as autosize counts/records that are used
	// by later program modules.

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   July 2010
	//       MODIFIED       April 2011; to include autosize counts
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// The module contains structure for output variables that are used in a small number of modules.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using DataGlobals::MaxNameLength;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const NumMonthlyReports( 62 );
	FArray1D_Fstring const MonthlyNamedReports( NumMonthlyReports, sFstring( 51 ), { "ZONECOOLINGSUMMARYMONTHLY                          ", "ZONEHEATINGSUMMARYMONTHLY                          ", "ZONEELECTRICSUMMARYMONTHLY                         ", "SPACEGAINSMONTHLY                                  ", "PEAKSPACEGAINSMONTHLY                              ", "SPACEGAINCOMPONENTSATCOOLINGPEAKMONTHLY            ", "ENERGYCONSUMPTIONELECTRICITYNATURALGASMONTHLY      ", "ENERGYCONSUMPTIONELECTRICITYGENERATEDPROPANEMONTHLY", "ENERGYCONSUMPTIONDIESELFUELOILMONTHLY              ", "ENERGYCONSUMPTIONDISTRICTHEATINGCOOLINGMONTHLY     ", "ENERGYCONSUMPTIONCOALGASOLINEMONTHLY               ", "ENERGYCONSUMPTIONOTHERFUELSMONTHLY                 ", "ENDUSEENERGYCONSUMPTIONELECTRICITYMONTHLY          ", "ENDUSEENERGYCONSUMPTIONNATURALGASMONTHLY           ", "ENDUSEENERGYCONSUMPTIONDIESELMONTHLY               ", "ENDUSEENERGYCONSUMPTIONFUELOILMONTHLY              ", "ENDUSEENERGYCONSUMPTIONCOALMONTHLY                 ", "ENDUSEENERGYCONSUMPTIONPROPANEMONTHLY              ", "ENDUSEENERGYCONSUMPTIONGASOLINEMONTHLY             ", "ENDUSEENERGYCONSUMPTIONOTHERFUELSMONTHLY           ", "PEAKENERGYENDUSEELECTRICITYPART1MONTHLY            ", "PEAKENERGYENDUSEELECTRICITYPART2MONTHLY            ", "ELECTRICCOMPONENTSOFPEAKDEMANDMONTHLY              ", "PEAKENERGYENDUSENATURALGASMONTHLY                  ", "PEAKENERGYENDUSEDIESELMONTHLY                      ", "PEAKENERGYENDUSEFUELOILMONTHLY                     ", "PEAKENERGYENDUSECOALMONTHLY                        ", "PEAKENERGYENDUSEPROPANEMONTHLY                     ", "PEAKENERGYENDUSEGASOLINEMONTHLY                    ", "PEAKENERGYENDUSEOTHERFUELSMONTHLY                  ", "SETPOINTSNOTMETWITHTEMPERATURESMONTHLY             ", "COMFORTREPORTSIMPLE55MONTHLY                       ", "UNGLAZEDTRANSPIREDSOLARCOLLECTORSUMMARYMONTHLY     ", "OCCUPANTCOMFORTDATASUMMARYMONTHLY                  ", "CHILLERREPORTMONTHLY                               ", "TOWERREPORTMONTHLY                                 ", "BOILERREPORTMONTHLY                                ", "DXREPORTMONTHLY                                    ", "WINDOWREPORTMONTHLY                                ", "WINDOWENERGYREPORTMONTHLY                          ", "WINDOWZONESUMMARYMONTHLY                           ", "WINDOWENERGYZONESUMMARYMONTHLY                     ", "AVERAGEOUTDOORCONDITIONSMONTHLY                    ", "OUTDOORCONDITIONSMAXIMUMDRYBULBMONTHLY             ", "OUTDOORCONDITIONSMINIMUMDRYBULBMONTHLY             ", "OUTDOORCONDITIONSMAXIMUMWETBULBMONTHLY             ", "OUTDOORCONDITIONSMAXIMUMDEWPOINTMONTHLY            ", "OUTDOORGROUNDCONDITIONSMONTHLY                     ", "WINDOWACREPORTMONTHLY                              ", "WATERHEATERREPORTMONTHLY                           ", "GENERATORREPORTMONTHLY                             ", "DAYLIGHTINGREPORTMONTHLY                           ", "COILREPORTMONTHLY                                  ", "PLANTLOOPDEMANDREPORTMONTHLY                       ", "FANREPORTMONTHLY                                   ", "PUMPREPORTMONTHLY                                  ", "CONDLOOPDEMANDREPORTMONTHLY                        ", "ZONETEMPERATUREOSCILLATIONREPORTMONTHLY            ", "AIRLOOPSYSTEMENERGYANDWATERUSEMONTHLY              ", "AIRLOOPSYSTEMCOMPONENTLOADSMONTHLY                 ", "AIRLOOPSYSTEMCOMPONENTENERGYUSEMONTHLY             ", "MECHANICALVENTILATIONLOADSMONTHLY                  " } );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int MaxConsideredOutputVariables( 0 ); // Max Array size for OutputVariable pre-scanned
	int NumConsideredOutputVariables( 0 ); // Number of variables - pre-scanned, allowed for output
	int iNumberOfRecords; // Number of records in input
	int iNumberOfDefaultedFields; // number of defaulted fields
	int iTotalFieldsWithDefaults; // number of fields that can be defaulted
	int iNumberOfAutoSizedFields; // number of autosized fields
	int iTotalAutoSizableFields; // number of fields that can be autosized
	int iNumberOfAutoCalcedFields; // number of autocalculated fields
	int iTotalAutoCalculatableFields; // number of fields that can be autocalculated

	// Object Data
	FArray1D< OutputReportingVariables > OutputVariablesForSimulation;
	FArray1D< OutputReportingVariables > TempOutputVariablesForSimulation;

	// Functions

	bool
	FindItemInVariableList(
		Fstring const & KeyedValue,
		Fstring const & VariableName
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   July 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up a key and variable name value and determines if they are
		// in the list of required variables for a simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool InVariableList;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Found;
		int Item;

		InVariableList = false;
		Found = 0;
		for ( Item = 1; Item <= NumConsideredOutputVariables; ++Item ) {
			if ( ! DOSameString( VariableName, OutputVariablesForSimulation( Item ).VarName ) ) continue;
			Found = Item;
			break;
		}
		if ( Found != 0 ) {
			if ( DOSameString( KeyedValue, OutputVariablesForSimulation( Found ).Key ) || OutputVariablesForSimulation( Found ).Key == "*" ) {
				InVariableList = true;
			} else {
				while ( Found != 0 ) {
					Found = OutputVariablesForSimulation( Found ).Next;
					if ( Found != 0 ) {
						if ( DOSameString( KeyedValue, OutputVariablesForSimulation( Found ).Key ) || OutputVariablesForSimulation( Found ).Key == "*" ) {
							InVariableList = true;
							break;
						}
					}
				}
			}
		}

		return InVariableList;

	}

	bool
	DOSameString(
		Fstring const & TestString1, // First String to Test
		Fstring const & TestString2 // Second String to Test
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   November 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This function returns true if the two strings are equal (case insensitively)

		// METHODOLOGY EMPLOYED:
		// Make both strings uppercase.  Do internal compare.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool DOSameString;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( len_trim( TestString1 ) != len_trim( TestString2 ) ) {
			DOSameString = false;
		} else if ( TestString1 == TestString2 ) {
			DOSameString = true;
		} else {
			DOSameString = DOMakeUPPERCase( TestString1 ) == DOMakeUPPERCase( TestString2 );
		}

		return DOSameString;

	}

	Fstring
	DOMakeUPPERCase( Fstring const & InputString ) // Input String
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This function returns the Upper Case representation of the InputString.

		// METHODOLOGY EMPLOYED:
		// Uses the Intrinsic SCAN function to scan the lowercase representation of
		// characters (DataStringGlobals) for each character in the given string.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Fstring ResultString( len( InputString ) ); // Result String, string is limited to

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// MaxInputLineLength because of PowerStation Compiler
		// otherwise could say (CHARACTER(len=LEN(InputString))

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int curCharVal;

		ResultString = InputString;

		for ( int i = 1; i <= len_trim( InputString ); ++i ) {
			curCharVal = ichar( InputString( i, i ) );
			{ auto const SELECT_CASE_var( curCharVal );
			if ( ( 97 <= SELECT_CASE_var && SELECT_CASE_var <= 122 ) || ( 224 <= SELECT_CASE_var && SELECT_CASE_var <= 255 ) ) { // lowercase ASCII and accented characters
				ResultString( i, i ) = CHAR( curCharVal - 32 );
			}}
		}
		//       ! first check for normal lowercase char, then normal uppercase char
		//       if(InputString(i:i) >= "a" .and. InputString(i:i) <= "z") then
		//          ResultString(i:i) = achar(iachar(InputString(i:i)) - 32)
		//       else if (InputString(i:i) >= "A" .and. InputString(i:i) <= "Z") then
		//          cycle !ResultString(i:i) = InputString(i:i)  ! leave as is
		//       else ! now see if it's an accented char that needs uppercaseing
		//          Pos=SCAN(AccentedLowerCase,InputString(i:i))
		//          if (Pos /= 0) THEN
		//             ResultString(i:i)=AccentedUpperCase(Pos:Pos)
		//          ELSE
		//             cycle !ResultString(i:i) = InputString(i:i)
		//          ENDIF
		//       end if
		//    end do

		//    do i = 1, LEN_TRIM(InputString)
		//       ! first check for normal lowercase char, then normal uppercase char
		//       if(InputString(i:i) >= "a" .and. InputString(i:i) <= "z") then
		//          ResultString(i:i) = achar(iachar(InputString(i:i)) - 32)
		//       else if (InputString(i:i) >= "A" .and. InputString(i:i) <= "Z") then
		//          cycle !ResultString(i:i) = InputString(i:i)  ! leave as is
		//       else ! now see if it's an accented char that needs uppercaseing
		//          Pos=SCAN(AccentedLowerCase,InputString(i:i))
		//          if (Pos /= 0) THEN
		//             ResultString(i:i)=AccentedUpperCase(Pos:Pos)
		//          ELSE
		//             cycle !ResultString(i:i) = InputString(i:i)
		//          ENDIF
		//       end if
		//    end do
		//    ResultString=TRIM(ResultString)

		//  ResultString=Blank
		//  Pos=SCAN(InputString,LowerCase)
		//  IF (POS /= 0) THEN
		//    LengthInputString=LEN_TRIM(InputString)
		//    DO Count=1,LengthInputString
		//      Pos=SCAN(LowerCase,InputString(Count:Count))
		//      IF (Pos /= 0) THEN
		//        ResultString(Count:Count)=UpperCase(Pos:Pos)
		//      ELSE
		//        ResultString(Count:Count)=InputString(Count:Count)
		//      ENDIF
		//    END DO
		//    ResultString=TRIM(ResultString)
		//  ELSE
		//    ! String already in Upper Case
		//    ResultString=TRIM(InputString)
		//  ENDIF

		return ResultString;

	}

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

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

} // DataOutputs

} // EnergyPlus
