// ObjexxFCL Headers
#include <ObjexxFCL/string.functions.hh>

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

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const NumMonthlyReports( 62 );
	Array1D_string const MonthlyNamedReports( NumMonthlyReports, { "ZONECOOLINGSUMMARYMONTHLY", "ZONEHEATINGSUMMARYMONTHLY", "ZONEELECTRICSUMMARYMONTHLY", "SPACEGAINSMONTHLY", "PEAKSPACEGAINSMONTHLY", "SPACEGAINCOMPONENTSATCOOLINGPEAKMONTHLY", "ENERGYCONSUMPTIONELECTRICITYNATURALGASMONTHLY", "ENERGYCONSUMPTIONELECTRICITYGENERATEDPROPANEMONTHLY", "ENERGYCONSUMPTIONDIESELFUELOILMONTHLY", "ENERGYCONSUMPTIONDISTRICTHEATINGCOOLINGMONTHLY", "ENERGYCONSUMPTIONCOALGASOLINEMONTHLY", "ENERGYCONSUMPTIONOTHERFUELSMONTHLY", "ENDUSEENERGYCONSUMPTIONELECTRICITYMONTHLY", "ENDUSEENERGYCONSUMPTIONNATURALGASMONTHLY", "ENDUSEENERGYCONSUMPTIONDIESELMONTHLY", "ENDUSEENERGYCONSUMPTIONFUELOILMONTHLY", "ENDUSEENERGYCONSUMPTIONCOALMONTHLY", "ENDUSEENERGYCONSUMPTIONPROPANEMONTHLY", "ENDUSEENERGYCONSUMPTIONGASOLINEMONTHLY", "ENDUSEENERGYCONSUMPTIONOTHERFUELSMONTHLY", "PEAKENERGYENDUSEELECTRICITYPART1MONTHLY", "PEAKENERGYENDUSEELECTRICITYPART2MONTHLY", "ELECTRICCOMPONENTSOFPEAKDEMANDMONTHLY", "PEAKENERGYENDUSENATURALGASMONTHLY", "PEAKENERGYENDUSEDIESELMONTHLY", "PEAKENERGYENDUSEFUELOILMONTHLY", "PEAKENERGYENDUSECOALMONTHLY", "PEAKENERGYENDUSEPROPANEMONTHLY", "PEAKENERGYENDUSEGASOLINEMONTHLY", "PEAKENERGYENDUSEOTHERFUELSMONTHLY", "SETPOINTSNOTMETWITHTEMPERATURESMONTHLY", "COMFORTREPORTSIMPLE55MONTHLY", "UNGLAZEDTRANSPIREDSOLARCOLLECTORSUMMARYMONTHLY", "OCCUPANTCOMFORTDATASUMMARYMONTHLY", "CHILLERREPORTMONTHLY", "TOWERREPORTMONTHLY", "BOILERREPORTMONTHLY", "DXREPORTMONTHLY", "WINDOWREPORTMONTHLY", "WINDOWENERGYREPORTMONTHLY", "WINDOWZONESUMMARYMONTHLY", "WINDOWENERGYZONESUMMARYMONTHLY", "AVERAGEOUTDOORCONDITIONSMONTHLY", "OUTDOORCONDITIONSMAXIMUMDRYBULBMONTHLY", "OUTDOORCONDITIONSMINIMUMDRYBULBMONTHLY", "OUTDOORCONDITIONSMAXIMUMWETBULBMONTHLY", "OUTDOORCONDITIONSMAXIMUMDEWPOINTMONTHLY", "OUTDOORGROUNDCONDITIONSMONTHLY", "WINDOWACREPORTMONTHLY", "WATERHEATERREPORTMONTHLY", "GENERATORREPORTMONTHLY", "DAYLIGHTINGREPORTMONTHLY", "COILREPORTMONTHLY", "PLANTLOOPDEMANDREPORTMONTHLY", "FANREPORTMONTHLY", "PUMPREPORTMONTHLY", "CONDLOOPDEMANDREPORTMONTHLY", "ZONETEMPERATUREOSCILLATIONREPORTMONTHLY", "AIRLOOPSYSTEMENERGYANDWATERUSEMONTHLY", "AIRLOOPSYSTEMCOMPONENTLOADSMONTHLY", "AIRLOOPSYSTEMCOMPONENTENERGYUSEMONTHLY", "MECHANICALVENTILATIONLOADSMONTHLY" } );

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
	Array1D< OutputReportingVariables > OutputVariablesForSimulation;

	// Functions

	// Clears the global data in DataOutputs.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		MaxConsideredOutputVariables = 0;
		NumConsideredOutputVariables = 0;
		iNumberOfRecords = int();
		iNumberOfDefaultedFields = int();
		iTotalFieldsWithDefaults = int();
		iNumberOfAutoSizedFields = int();
		iTotalAutoSizableFields = int();
		iNumberOfAutoCalcedFields = int();
		iTotalAutoCalculatableFields = int();
		OutputVariablesForSimulation.deallocate();
	}

	bool
	FindItemInVariableList(
		std::string const & KeyedValue,
		std::string const & VariableName
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
			if ( ! equali( VariableName, OutputVariablesForSimulation( Item ).VarName ) ) continue;
			Found = Item;
			break;
		}
		if ( Found != 0 ) {
			if ( equali( KeyedValue, OutputVariablesForSimulation( Found ).Key ) || OutputVariablesForSimulation( Found ).Key == "*" ) {
				InVariableList = true;
			} else {
				while ( Found != 0 ) {
					Found = OutputVariablesForSimulation( Found ).Next;
					if ( Found != 0 ) {
						if ( equali( KeyedValue, OutputVariablesForSimulation( Found ).Key ) || OutputVariablesForSimulation( Found ).Key == "*" ) {
							InVariableList = true;
							break;
						}
					}
				}
			}
		}

		return InVariableList;

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

} // DataOutputs

} // EnergyPlus
