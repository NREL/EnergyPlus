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

} // DataOutputs

} // EnergyPlus
