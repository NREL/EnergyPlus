#ifndef SQLiteProcedures_hh_INCLUDED
#define SQLiteProcedures_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/FArray2S.hh>
#include <ObjexxFCL/Fstring.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace SQLiteProcedures {

	// Data
	extern int const MaxMessageSize;
	extern bool WriteOutputToSQLite;
	extern bool WriteTabularDataToSQLite;

	extern int SQLdbTimeIndex;

	// Functions

	void
	SQLiteBegin();

	void
	SQLiteCommit();

	void
	CreateSQLiteDatabase();

	void
	CreateSQLiteReportVariableDictionaryRecord(
		int const reportVariableReportID,
		int const storeTypeIndex,
		Fstring const & indexGroup,
		Fstring const & keyedValueString,
		Fstring const & variableName,
		int const indexType,
		Fstring const & units,
		int const reportingFreq,
		Optional_Fstring_const ScheduleName = _
	);

	void
	CreateSQLiteReportVariableDataRecord(
		int const recordIndex,
		int const timeIndex,
		Real64 const value,
		Optional_int_const reportingInterval = _,
		Optional< Real64 const > minValue = _,
		Optional_int_const minValueDate = _,
		Optional< Real64 const > maxValue = _,
		Optional_int_const maxValueDate = _,
		Optional_int_const minutesPerTimeStep = _
	);

	int
	CreateSQLiteTimeIndexRecord(
		int const reportingInterval,
		int const recordIndex,
		int const CumlativeSimulationDays,
		Optional_int_const Month = _,
		Optional_int_const DayOfMonth = _,
		Optional_int_const Hour = _,
		Optional< Real64 const > EndMinute = _,
		Optional< Real64 const > StartMinute = _,
		Optional_int_const DST = _,
		Optional_Fstring_const DayType = _
	);

	void
	CreateSQLiteZoneTable();

	void
	CreateSQLiteNominalLightingTable();

	void
	CreateSQLiteNominalPeopleTable();

	void
	CreateSQLiteNominalElectricEquipmentTable();

	void
	CreateSQLiteNominalGasEquipmentTable();

	void
	CreateSQLiteNominalSteamEquipmentTable();

	void
	CreateSQLiteNominalHotWaterEquipmentTable();

	void
	CreateSQLiteNominalOtherEquipmentTable();

	void
	CreateSQLiteNominalBaseboardHeatTable();

	void
	CreateSQLiteSurfacesTable();

	void
	CreateSQLiteConstructionsTable();

	void
	CreateSQLiteMaterialsTable();

	void
	CreateSQLiteZoneListTable();

	void
	CreateSQLiteZoneGroupTable();

	void
	CreateSQLiteInfiltrationTable();

	void
	CreateSQLiteVentilationTable();

	void
	AddSQLiteZoneSizingRecord(
		Fstring const & ZoneName, // the name of the zone
		Fstring const & LoadType, // the description of the input variable
		Real64 const CalcDesLoad, // the value from the sizing calculation [W]
		Real64 const UserDesLoad, // the value from the sizing calculation modified by user input [W]
		Real64 const CalcDesFlow, // calculated design air flow rate [m3/s]
		Real64 const UserDesFlow, // user input or modified design air flow rate [m3/s]
		Fstring const & DesDayName, // the name of the design day that produced the peak
		Fstring const & PeakHrMin, // time stamp of the peak
		Real64 const PeakTemp, // temperature at peak [C]
		Real64 const PeakHumRat, // humidity ratio at peak [kg water/kg dry air]
		Real64 const MinOAVolFlow // zone design minimum outside air flow rate [m3/s]
	);

	void
	AddSQLiteSystemSizingRecord(
		Fstring const & SysName, // the name of the system
		Fstring const & VarDesc, // the description of the input variable
		Real64 const VarValue // the value from the sizing calculation
	);

	void
	AddSQLiteComponentSizingRecord(
		Fstring const & CompType, // the type of the component
		Fstring const & CompName, // the name of the component
		Fstring const & VarDesc, // the description of the input variable
		Real64 const VarValue // the value from the sizing calculation
	);

	void
	CreateSQLiteRoomAirModelTable();

	void
	CreateSQLiteMeterDictionaryRecord(
		int const meterReportID,
		int const storeTypeIndex,
		Fstring const & indexGroup,
		Fstring const & keyedValueString,
		Fstring const & variableName,
		int const indexType,
		Fstring const & units,
		int const reportingFreq,
		Optional_Fstring_const ScheduleName = _
	);

	void
	CreateSQLiteMeterRecord(
		int const recordIndex,
		int const timeIndex,
		Real64 const value,
		Optional_int_const reportingInterval = _,
		Optional< Real64 const > minValue = _,
		Optional_int_const minValueDate = _,
		Optional< Real64 const > maxValue = _,
		Optional_int_const maxValueDate = _,
		Optional_int_const minutesPerTimeStep = _
	);

	void
	SQLiteWriteMessageMacro( Fstring const & message );

	void
	CreateZoneExtendedOutput();

	void
	CreateSQLiteDaylightMapTitle(
		int const mapNum,
		Fstring const & mapName,
		Fstring const & environmentName,
		int const zone,
		Fstring const & refPt1,
		Fstring const & refPt2,
		Real64 const zCoord
	);

	void
	CreateSQLiteDaylightMap(
		int const mapNum,
		int const month,
		int const dayOfMonth,
		int const hourOfDay,
		int const nX,
		FArray1S< Real64 > const x,
		int const nY,
		FArray1S< Real64 > const y,
		FArray2S< Real64 > const illuminance
	);

	void
	CreateSQLiteTabularDataRecords(
		FArray2S_Fstring const body, // row,column
		FArray1S_Fstring const rowLabels,
		FArray1S_Fstring const columnLabels,
		Fstring const & ReportName,
		Fstring const & ReportForString,
		Fstring const & TableName
	);

	void
	InitializeIndexes();

	void
	InitializeTabularDataTable();

	void
	InitializeTabularDataView();

	void
	CreateSQLiteSimulationsRecord( int const ID );

	void
	CreateSQLiteEnvironmentPeriodRecord();

	void
	CreateSQLiteErrorRecord(
		int const simulationIndex,
		int const errorType,
		Fstring const & errorMessage,
		int const cnt
	);

	void
	UpdateSQLiteErrorRecord( Fstring const & errorMessage );

	void
	UpdateSQLiteSimulationRecord(
		bool const completed,
		bool const completedSuccessfully
	);

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

	//     Copyright © 2008 Building Synergies, LLC.  All rights reserved.

} // SQLiteProcedures

} // EnergyPlus

#endif
