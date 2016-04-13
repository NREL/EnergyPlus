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

#ifndef OutputReportTabular_hh_INCLUDED
#define OutputReportTabular_hh_INCLUDED

// C++ Headers
#include <fstream>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace OutputReportTabular {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS:

	extern int const MaxHeaderLength;
	extern int const MaxNoteLength;

	extern int const aggTypeSumOrAvg;
	extern int const aggTypeMaximum;
	extern int const aggTypeMinimum;
	extern int const aggTypeValueWhenMaxMin;
	extern int const aggTypeHoursZero;
	extern int const aggTypeHoursNonZero;
	extern int const aggTypeHoursPositive;
	extern int const aggTypeHoursNonPositive;
	extern int const aggTypeHoursNegative;
	extern int const aggTypeHoursNonNegative;
	extern int const aggTypeSumOrAverageHoursShown;
	extern int const aggTypeMaximumDuringHoursShown;
	extern int const aggTypeMinimumDuringHoursShown;

	extern int const tableStyleComma;
	extern int const tableStyleTab;
	extern int const tableStyleFixed;
	extern int const tableStyleHTML;
	extern int const tableStyleXML;

	extern int const unitsStyleNone; // no change to any units
	extern int const unitsStyleJtoKWH;
	extern int const unitsStyleJtoMJ;
	extern int const unitsStyleJtoGJ;
	extern int const unitsStyleInchPound;
	extern int const unitsStyleNotFound;

	extern int const isAverage;
	extern int const isSum;

	extern int const stepTypeZone;
	extern int const stepTypeHVAC;

	// BEPS Report Related Variables
	// From Report:Table:Predefined - BEPS
	extern int const numResourceTypes;
	extern int const numSourceTypes;

	//MODULE VARIABLE DECLARATIONS:

	// The Binned table type is different and only references one variable and its structure is very
	// different from the others so it is has its own type.

	// arrays for time binned results

	extern int OutputTableBinnedCount;
	extern int BinResultsTableCount;
	extern int BinResultsIntervalCount;

	extern int const numNamedMonthly;
	// These reports are detailed/named in routine InitializePredefinedMonthlyTitles

	extern int MonthlyInputCount;
	extern int sizeMonthlyInput;
	extern int MonthlyFieldSetInputCount;
	extern int sizeMonthlyFieldSetInput;
	extern int MonthlyTablesCount;
	extern int MonthlyColumnsCount;
	extern Array1D_bool IsMonthGathered; // shown as true for any month used

	extern int TOCEntriesCount;
	extern int TOCEntriesSize;

	extern int UnitConvSize;

	extern bool WriteTabularFiles;

	// Allow up to five output files to be created
	extern int const maxNumStyles;

	// From Report:Table:Style
	extern int unitsStyle; // see list of parameters
	extern int numStyles;
	extern std::ofstream csv_stream; // CSV table stream
	extern std::ofstream tab_stream; // Tab table stream
	extern std::ofstream fix_stream; // Fixed table stream
	extern std::ofstream htm_stream; // HTML table stream
	extern std::ofstream xml_stream; // XML table stream
	extern Array1D< std::ofstream * > TabularOutputFile; // Table stream array
	extern Array1D_string del; // the delimiter to use
	extern Array1D_int TableStyle; // see list of parameters

	extern Real64 timeInYear;

	// Flags for predefined tabular reports
	extern bool displayTabularBEPS;
	extern bool displayLEEDSummary;
	extern bool displayTabularCompCosts; // added BTG 5/6/04 for component cost summary
	extern bool displayTabularVeriSum; // added JG 2006-06-28 for input verification and summary report
	extern bool displayComponentSizing;
	extern bool displaySurfaceShadowing;
	extern bool displayDemandEndUse;
	extern bool displayAdaptiveComfort;
	extern bool displaySourceEnergyEndUseSummary;
	extern bool displayZoneComponentLoadSummary;

	// BEPS Report Related Variables
	// From Report:Table:Predefined - BEPS
	// arrays that hold the meter numbers that are initialized at get input
	extern Array1D_int meterNumTotalsBEPS;
	extern Array1D_int meterNumTotalsSource;
	extern Array1D_bool fuelfactorsused;
	extern Array1D_bool ffUsed;
	extern Array1D< Real64 > SourceFactors;
	extern Array1D_bool ffSchedUsed;
	extern Array1D_int ffSchedIndex;
	extern Array2D_int meterNumEndUseBEPS;
	extern Array3D_int meterNumEndUseSubBEPS;
	// arrays that hold the names of the resource and end uses
	extern Array1D_string resourceTypeNames;
	extern Array1D_string sourceTypeNames;
	extern Array1D_string endUseNames;
	// arrays that hold the actual values for the year
	extern Array1D< Real64 > gatherTotalsBEPS;
	extern Array1D< Real64 > gatherTotalsBySourceBEPS;
	extern Array1D< Real64 > gatherTotalsSource;
	extern Array1D< Real64 > gatherTotalsBySource;
	extern Array2D< Real64 > gatherEndUseBEPS;
	extern Array2D< Real64 > gatherEndUseBySourceBEPS;
	extern Array3D< Real64 > gatherEndUseSubBEPS;
	// arrays the hold the demand values
	extern Array1D< Real64 > gatherDemandTotal;
	extern Array2D< Real64 > gatherDemandEndUse;
	extern Array3D< Real64 > gatherDemandEndUseSub;
	extern Array1D_int gatherDemandTimeStamp;
	// to keep track of hours for the BEPS report gathering
	extern Real64 gatherElapsedTimeBEPS;
	// for normalization of results
	extern Real64 buildingGrossFloorArea;
	extern Real64 buildingConditionedFloorArea;
	// keep track if schedules are used in fuel factors
	extern bool fuelFactorSchedulesUsed;
	// for electic load components on BEPS report
	extern int meterNumPowerFuelFireGen;
	extern Real64 gatherPowerFuelFireGen;
	extern int meterNumPowerPV;
	extern Real64 gatherPowerPV;
	extern int meterNumPowerWind;
	extern Real64 gatherPowerWind;
	extern Real64 OverallNetEnergyFromStorage;
	extern int meterNumPowerHTGeothermal;
	extern Real64 gatherPowerHTGeothermal;
	extern int meterNumElecProduced;
	extern Real64 gatherElecProduced;
	extern int meterNumElecPurchased;
	extern Real64 gatherElecPurchased;
	extern int meterNumElecSurplusSold;
	extern Real64 gatherElecSurplusSold;
	extern int meterNumElecStorage;
	extern Real64 gatherElecStorage;
	extern int meterNumPowerConversion;
	extern Real64 gatherPowerConversion;
	// for on site thermal source components on BEPS report
	extern int meterNumWaterHeatRecovery;
	extern Real64 gatherWaterHeatRecovery;
	extern int meterNumAirHeatRecoveryCool;
	extern Real64 gatherAirHeatRecoveryCool;
	extern int meterNumAirHeatRecoveryHeat;
	extern Real64 gatherAirHeatRecoveryHeat;
	extern int meterNumHeatHTGeothermal;
	extern Real64 gatherHeatHTGeothermal;
	extern int meterNumHeatSolarWater;
	extern Real64 gatherHeatSolarWater;
	extern int meterNumHeatSolarAir;
	extern Real64 gatherHeatSolarAir;
	// for on site water components on BEPS report
	extern int meterNumRainWater;
	extern Real64 gatherRainWater;
	extern int meterNumCondensate;
	extern Real64 gatherCondensate;
	extern int meterNumGroundwater;
	extern Real64 gatherWellwater;
	extern int meterNumMains;
	extern Real64 gatherMains;
	extern int meterNumWaterEndUseTotal;
	extern Real64 gatherWaterEndUseTotal;
	// for source energy conversion factors on BEPS report
	extern Real64 sourceFactorElectric;
	extern Real64 sourceFactorNaturalGas;
	extern Real64 efficiencyDistrictCooling;
	extern Real64 efficiencyDistrictHeating;
	extern Real64 sourceFactorSteam;
	extern Real64 sourceFactorGasoline;
	extern Real64 sourceFactorDiesel;
	extern Real64 sourceFactorCoal;
	extern Real64 sourceFactorFuelOil1;
	extern Real64 sourceFactorFuelOil2;
	extern Real64 sourceFactorPropane;
	extern Real64 sourceFactorOtherFuel1;
	extern Real64 sourceFactorOtherFuel2;

	extern Array1D_int td;
	//(1)   Current year
	//(2)   Current month
	//(3)   Current day
	//(4)   Time difference with respect to UTC in minutes (0-59)
	//(5)   Hour of the day (0-23)
	//(6)   Minutes (0-59)
	//(7)   Seconds (0-59)
	//(8)   Milliseconds (0-999)

	// Design day name storage
	extern Array1D_string DesignDayName;
	extern int DesignDayCount;

	//arrays related to pulse and load component reporting
	extern Array2D< Real64 > radiantPulseUsed;
	extern Array2D_int radiantPulseTimestep;
	extern Array2D< Real64 > radiantPulseReceived;
	extern Array3D< Real64 > loadConvectedNormal;
	extern Array3D< Real64 > loadConvectedWithPulse;
	extern Array3D< Real64 > netSurfRadSeq;
	extern Array2D< Real64 > decayCurveCool;
	extern Array2D< Real64 > decayCurveHeat;
	extern Array3D< Real64 > ITABSFseq; // used for determining the radiant fraction on each surface
	extern Array3D< Real64 > TMULTseq; // used for determining the radiant fraction on each surface

	extern Array3D< Real64 > peopleInstantSeq;
	extern Array3D< Real64 > peopleLatentSeq;
	extern Array3D< Real64 > peopleRadSeq;
	extern Array3D< Real64 > peopleDelaySeq;

	extern Array3D< Real64 > lightInstantSeq;
	extern Array3D< Real64 > lightRetAirSeq;
	extern Array3D< Real64 > lightLWRadSeq; // long wave thermal radiation
	extern Array3D< Real64 > lightSWRadSeq; // short wave visible radiation
	extern Array3D< Real64 > lightDelaySeq;

	extern Array3D< Real64 > equipInstantSeq;
	extern Array3D< Real64 > equipLatentSeq;
	extern Array3D< Real64 > equipRadSeq;
	extern Array3D< Real64 > equipDelaySeq;

	extern Array3D< Real64 > refrigInstantSeq;
	extern Array3D< Real64 > refrigRetAirSeq;
	extern Array3D< Real64 > refrigLatentSeq;

	extern Array3D< Real64 > waterUseInstantSeq;
	extern Array3D< Real64 > waterUseLatentSeq;

	extern Array3D< Real64 > hvacLossInstantSeq;
	extern Array3D< Real64 > hvacLossRadSeq;
	extern Array3D< Real64 > hvacLossDelaySeq;

	extern Array3D< Real64 > powerGenInstantSeq;
	extern Array3D< Real64 > powerGenRadSeq;
	extern Array3D< Real64 > powerGenDelaySeq;

	extern Array3D< Real64 > infilInstantSeq;
	extern Array3D< Real64 > infilLatentSeq;

	extern Array3D< Real64 > zoneVentInstantSeq;
	extern Array3D< Real64 > zoneVentLatentSeq;

	extern Array3D< Real64 > interZoneMixInstantSeq;
	extern Array3D< Real64 > interZoneMixLatentSeq;

	extern Array3D< Real64 > feneCondInstantSeq;
	//REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: feneSolarInstantSeq
	extern Array3D< Real64 > feneSolarRadSeq;
	extern Array3D< Real64 > feneSolarDelaySeq;

	extern Array3D< Real64 > surfDelaySeq;

	extern int maxUniqueKeyCount;

	// for the XML report must keep track fo the active sub-table name and report set by other routines
	extern std::string activeSubTableName;
	extern std::string activeReportNameNoSpace;
	extern std::string activeReportName;
	extern std::string activeForName;
	extern std::string prevReportName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops
	//PRIVATE      DateToStr

	// Types

	struct OutputTableBinnedType
	{
		// Members
		std::string keyValue; // the key value (usually an asterisk to indicate all variables
		std::string varOrMeter; // the name of the variable or meter
		Real64 intervalStart; // The lowest value for the intervals being binned into.
		Real64 intervalSize; // The size of the bins starting with Interval start.
		int intervalCount; // The number of bins used. The number of hours below the start of
		// the lowest bin and above the value of the last bin are also shown.
		int resIndex; // result index - pointer to BinResults array
		int numTables;
		int typeOfVar; // 0=not found, 1=integer, 2=real, 3=meter
		int avgSum; // Variable  is Averaged=1 or Summed=2
		int stepType; // Variable time step is Zone=1 or HVAC=2
		std::string units; // the units string, may be blank
		std::string ScheduleName; // the name of the schedule
		int scheduleIndex; // index to the schedule specified - if no schedule use zero

		// Default Constructor
		OutputTableBinnedType() :
			intervalStart( 0.0 ),
			intervalSize( 0.0 ),
			intervalCount( 0 ),
			resIndex( 0 ),
			numTables( 0 ),
			typeOfVar( 0 ),
			avgSum( 0 ),
			stepType( 0 ),
			scheduleIndex( 0 )
		{}

	};

	struct BinResultsType
	{
		// Members
		Array1D< Real64 > mnth; // monthly bins
		Array1D< Real64 > hrly; // hourly bins

		// Default Constructor
		BinResultsType() :
			mnth( 12, 0.0 ),
			hrly( 24, 0.0 )
		{}

	};

	struct BinObjVarIDType
	{
		// Members
		std::string namesOfObj; // name of the object
		int varMeterNum; // variable or meter number

		// Default Constructor
		BinObjVarIDType() :
			varMeterNum( 0 )
		{}

	};

	struct BinStatisticsType
	{
		// Members
		Real64 sum; // sum of the variable
		Real64 sum2; // sum of the variable squared
		int n; // number of items in sum
		Real64 minimum; // minimum value
		Real64 maximum; // maximum value

		// Default Constructor
		BinStatisticsType() :
			sum( 0.0 ),
			sum2( 0.0 ),
			n( 0 ),
			minimum( 0.0 ),
			maximum( 0.0 )
		{}

	};

	struct NamedMonthlyType
	{
		// Members
		std::string title; // report title
		bool show; // if report should be shown

		// Default Constructor
		NamedMonthlyType() :
			show( false )
		{}

	};

	struct MonthlyInputType
	{
		// Members
		std::string name; // identifier
		int numFieldSet; // number of monthly field sets
		int firstFieldSet; // pointer to the first field set
		int numTables; // number of tables
		int firstTable; // pointer to the first table
		int showDigits; // the number of digits to be shown

		// Default Constructor
		MonthlyInputType() :
			numFieldSet( 0 ),
			firstFieldSet( 0 ),
			numTables( 0 ),
			firstTable( 0 ),
			showDigits( 0 )
		{}

	};

	struct MonthlyFieldSetInputType
	{
		// Members
		std::string variMeter; // the name of the variable or meter
		std::string colHead; // the column header to use instead of the variable name (only for predefined)
		int aggregate; // the type of aggregation for the variable (see aggType parameters)
		std::string varUnits; // Units sting, may be blank
		std::string variMeterUpper; // the name of the variable or meter uppercased
		int typeOfVar; // 0=not found, 1=integer, 2=real, 3=meter
		int keyCount; // noel
		int varAvgSum; // Variable  is Averaged=1 or Summed=2
		int varStepType; // Variable time step is Zone=1 or HVAC=2
		Array1D_string NamesOfKeys; // keyNames !noel
		Array1D_int IndexesForKeyVar; // keyVarIndexes !noel

		// Default Constructor
		MonthlyFieldSetInputType() :
			aggregate( 0 ),
			typeOfVar( 0 ),
			keyCount( 0 ),
			varAvgSum( 1 ),
			varStepType( 1 )
		{}

	};

	struct MonthlyTablesType
	{
		// Members
		std::string keyValue; // the key value - the object names that result in the variable
		int firstColumn; // pointer to the monthly column array for the first item
		int numColumns; // number of columns for the table

		// Default Constructor
		MonthlyTablesType() :
			firstColumn( 0 ),
			numColumns( 0 )
		{}

	};

	struct MonthlyColumnsType
	{
		// Members
		std::string varName; // name of variable
		std::string colHead; // column header (not used for user defined monthly)
		int varNum; // variable or meter number
		int typeOfVar; // 0=not found, 1=integer, 2=real, 3=meter
		int avgSum; // Variable  is Averaged=1 or Summed=2
		int stepType; // Variable time step is Zone=1 or HVAC=2
		std::string units; // the units string, may be blank
		int aggType; // index to the type of aggregation (see list of parameters)
		Array1D< Real64 > reslt; // monthly results
		Array1D< Real64 > duration; // the time during which results are summed for use in averages
		Array1D_int timeStamp; // encoded timestamp of max or min
		Real64 aggForStep; // holds the aggregation for the HVAC time steps when smaller than
		// the zone timestep

		// Default Constructor
		MonthlyColumnsType() :
			varNum( 0 ),
			typeOfVar( 0 ),
			avgSum( 0 ),
			stepType( 0 ),
			aggType( 0 ),
			reslt( 12, 0.0 ),
			duration( 12, 0.0 ),
			timeStamp( 12, 0 ),
			aggForStep( 0.0 )
		{}

	};

	struct TOCEntriesType
	{
		// Members
		std::string reportName; // the name of the individual report
		std::string sectionName; // the name of the section containing individual reports
		bool isWritten; // flag if the entry has been written to TOC

		// Default Constructor
		TOCEntriesType() :
			isWritten( false )
		{}

	};

	struct UnitConvType
	{
		// Members
		std::string siName; // the name abbreviation or symbol of the SI units
		std::string ipName; // the name abbreviation or symbol of the IP units
		Real64 mult; // the multiplier used to convert from SI to IP in IP = (SI * mult) + offset
		Real64 offset; // the offset used to convert from SI to IP in IP = (SI * mult) + offset
		std::string hint; // the string used when multiple SI units match
		bool several; // several different options for the SI unit to be converted into IP
		bool is_default; // if part of a set of "several" this should be used as default

		// Default Constructor
		UnitConvType() :
			mult( 1.0 ),
			offset( 0.0 ),
			several( false ),
			is_default( false )
		{}

	};

	// Object Data
	extern Array1D< OutputTableBinnedType > OutputTableBinned;
	extern Array2D< BinResultsType > BinResults; // table number, number of intervals
	extern Array1D< BinResultsType > BinResultsBelow; // time below the lowest defined bin
	extern Array1D< BinResultsType > BinResultsAbove; // time above the highest defined bin
	extern Array1D< BinObjVarIDType > BinObjVarID;
	extern Array1D< BinStatisticsType > BinStatistics;
	extern Array1D< NamedMonthlyType > namedMonthly; // for predefined monthly report titles
	extern Array1D< MonthlyFieldSetInputType > MonthlyFieldSetInput;
	extern Array1D< MonthlyInputType > MonthlyInput;
	extern Array1D< MonthlyTablesType > MonthlyTables;
	extern Array1D< MonthlyColumnsType > MonthlyColumns;
	extern Array1D< TOCEntriesType > TOCEntries;
	extern Array1D< UnitConvType > UnitConv;

	// Functions
	void
	clear_state();

	void
	UpdateTabularReports( int const IndexTypeKey ); // What kind of data to update (Zone, HVAC)

	//======================================================================================================================
	//======================================================================================================================

	//    GET INPUT ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	GetInputTabularMonthly();

	int
	AddMonthlyReport(
		std::string const & inReportName,
		int const inNumDigitsShown
	);

	void
	AddMonthlyFieldSetInput(
		int const inMonthReport,
		std::string const & inVariMeter,
		std::string const & inColHead,
		int const inAggregate
	);

	void
	InitializeTabularMonthly();

	void
	GetInputTabularTimeBins();

	bool
	warningAboutKeyNotFound( int foundIndex, int inObjIndex, std::string const & moduleName );

	void
	GetInputTabularStyle();

	int
	SetUnitsStyleFromString( std::string const & unitStringIn );


	void
	GetInputTabularPredefined();

	bool
	isCompLoadRepReq();

	void
	InitializePredefinedMonthlyTitles();

	void
	CreatePredefinedMonthlyReports();

	void
	GetInputFuelAndPollutionFactors();

	//======================================================================================================================
	//======================================================================================================================

	//    OTHER INITIALIZATION ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	OpenOutputTabularFile();

	void
	CloseOutputTabularFile();

	void
	WriteTableOfContents();

	//======================================================================================================================
	//======================================================================================================================

	//    GATHER DATA EACH TIME STEP ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	GatherBinResultsForTimestep( int const IndexTypeKey ); // What kind of data to update (Zone, HVAC)

	void
	GatherMonthlyResultsForTimestep( int const IndexTypeKey ); // What kind of data to update (Zone, HVAC)

	void
	GatherBEPSResultsForTimestep( int const IndexTypeKey ); // What kind of data to update (Zone, HVAC)

	void
	GatherSourceEnergyEndUseResultsForTimestep( int const IndexTypeKey ); // What kind of data to update (Zone, HVAC)

	void
	GatherPeakDemandForTimestep( int const IndexTypeKey ); // What kind of data to update (Zone, HVAC)

	void
	GatherHeatGainReport( int const IndexTypeKey ); // What kind of data to update (Zone, HVAC)

	//======================================================================================================================
	//======================================================================================================================

	//    WRITE OUTPUT FILE ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	WriteTabularReports();

	void
	FillWeatherPredefinedEntries();

	std::string
	GetColumnUsingTabs(
		std::string const & inString, // Input String
		int const colNum // Column number
	);

	void
	FillRemainingPredefinedEntries();

	void
	WriteMonthlyTables();

	void
	WriteTimeBinTables();

	void
	WriteBEPSTable();

	std::string
	ResourceWarningMessage(std::string resource);

	Real64
	WaterConversionFunct(Real64 WaterTotal, Real64 ConversionFactor);

	void
	WriteSourceEnergyEndUseSummary();

	void
	WriteDemandEndUseSummary();

	void
	WriteCompCostTable();

	void
	WriteVeriSumTable();

	void
	WriteAdaptiveComfortTable();

	void
	WritePredefinedTables();

	void
	WriteComponentSizing();

	void
	WriteSurfaceShadowing();

	void
	AddTOCZoneLoadComponentTable();

	void
	AllocateLoadComponentArrays();

	void
	DeallocateLoadComponentArrays();

	void
	ComputeLoadComponentDecayCurve();

	void
	GatherComponentLoadsSurface();

	void
	GatherComponentLoadsHVAC();

	void
	ComputeDelayedComponents();

	void
	WriteZoneLoadComponentTable();

	void
	WriteReportHeaders(
		std::string const & reportName,
		std::string const & objectName,
		int const averageOrSum
	);

	void
	WriteSubtitle( std::string const & subtitle );

	void
	WriteTextLine(
		std::string const & lineOfText,
		Optional_bool_const isBold = _
	);

	void
	WriteTable(
		Array2S_string const body, // row,column
		Array1S_string const rowLabels,
		Array1S_string const columnLabels,
		Array1S_int widthColumn,
		Optional_bool_const transposeXML = _,
		Optional_string_const footnoteText = _
	);

	std::string
	MakeAnchorName(
		std::string const & reportString,
		std::string const & objectString
	);

	std::string
	InsertCurrencySymbol(
		std::string const & inString, // Input String
		bool const isHTML // True if an HTML string
	);

	std::string
	ConvertToElementTag( std::string const & inString ); // Input String

	std::string
	ConvertUnicodeToUTF8( unsigned long const codepoint );

	std::string
	ConvertToEscaped( std::string const & inString ); // Input String

	void
	DetermineBuildingFloorArea();

	//======================================================================================================================
	//======================================================================================================================

	//    ROUTINES TO RESET GATHERED VALUES TO ZERO

	//======================================================================================================================
	//======================================================================================================================

	void
	ResetTabularReports();

	void
	ResetMonthlyGathering();

	void
	ResetBinGathering();

	void
	ResetBEPSGathering();

	void
	ResetSourceEnergyEndUseGathering();

	void
	ResetPeakDemandGathering();

	void
	ResetHeatGainGathering();

	void
	ResetRemainingPredefinedEntries();

	void
	ResetAdaptiveComfort();


	//======================================================================================================================
	//======================================================================================================================

	//    ROUTINES RELATED TO IF VALUE IS IN A RANGE

	//======================================================================================================================
	//======================================================================================================================

	bool
	isInTriangle(
		Real64 const qx,
		Real64 const qy,
		Real64 const x1,
		Real64 const y1,
		Real64 const x2,
		Real64 const y2,
		Real64 const x3,
		Real64 const y3
	);

	bool
	isInQuadrilateral(
		Real64 const qx,
		Real64 const qy,
		Real64 const ax,
		Real64 const ay,
		Real64 const bx,
		Real64 const by,
		Real64 const cx,
		Real64 const cy,
		Real64 const dx,
		Real64 const dy
	);

	//======================================================================================================================
	//======================================================================================================================

	//    SUPPORT ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	std::string
	RealToStr(
		Real64 const RealIn,
		int const numDigits
	);

	std::string
	IntToStr( int const intIn );

	Real64
	StrToReal( std::string const & stringIn );

	std::string
	DateToString( int const codedDate ); // word containing encoded month, day, hour, minute

	void
	AddTOCEntry(
		std::string const & nameSection,
		std::string const & nameReport
	);

	void
	SetupUnitConversions();

	std::string
	GetUnitSubString( std::string const & inString ); // Input String

	void
	LookupSItoIP(
		std::string const & stringInWithSI,
		int & unitConvIndex,
		std::string & stringOutWithIP
	);

	Real64
	ConvertIP(
		int const unitConvIndex,
		Real64 const SIvalue
	);

	Real64
	ConvertIPdelta(
		int const unitConvIndex,
		Real64 const SIvalue
	);

	void
	GetUnitConversion(
		int const unitConvIndex,
		Real64 & multiplier,
		Real64 & offset,
		std::string & IPunit
	);

	Real64
	getSpecificUnitMultiplier(
		std::string const & SIunit,
		std::string const & IPunit
	);

	Real64
	getSpecificUnitDivider(
		std::string const & SIunit,
		std::string const & IPunit
	);

	Real64
	getSpecificUnitIndex(
		std::string const & SIunit,
		std::string const & IPunit
	);

} // OutputReportTabular

} // EnergyPlus

#endif
