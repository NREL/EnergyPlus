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
#include <iomanip>
#include <map>
#include <utility>
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/member.functions.hh>
#include <ObjexxFCL/numeric.hh>
#include <ObjexxFCL/string.functions.hh>
#include <ObjexxFCL/Time_Date.hh>

// EnergyPlus Headers
#include <CommandLineInterface.hh>
#include <OutputReportTabular.hh>
#include <DataAirflowNetwork.hh>
#include <DataCostEstimate.hh>
#include <DataDefineEquip.hh>
#include <DataEnvironment.hh>
#include <DataErrorTracking.hh>
#include <DataGlobalConstants.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataOutputs.hh>
#include <DataPrecisionGlobals.hh>
#include <DataShadowingCombinations.hh>
#include <DataSizing.hh>
#include <DataStringGlobals.hh>
#include <DataSurfaces.hh>
#include <DataWater.hh>
#include <DataZoneEquipment.hh>
#include <DirectAirManager.hh>
#include <DisplayRoutines.hh>
#include <ExteriorEnergyUse.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <LowTempRadiantSystem.hh>
#include <ElectricPowerServiceManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <OutputReportTabularAnnual.hh>
#include <PollutionModule.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <SQLiteProcedures.hh>
#include <ThermalComfort.hh>
#include <UtilityRoutines.hh>
#include <VentilatedSlab.hh>
#include <ZonePlenum.hh>

namespace EnergyPlus {

namespace OutputReportTabular {

	// MODULE INFORMATION:
	//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
	//    DATE WRITTEN   July 2003
	//    MODIFIED       na
	//    RE-ENGINEERED  na
	// PURPOSE OF THIS MODULE:
	//    This module allows the user to define several different tabular
	//    report that have a specific format.
	// METHODOLOGY EMPLOYED:
	//    Generally aggregation. Specifically, the IDF objects are read into data
	//    structures on the first call to update the data.  The data structures
	//    include not only ones to hold the IDF data but also that initialize
	//    the structure used to gather data each iteration. The report:table:binned
	//    object is stored in OutputTableBinned.
	//    During initialization the TableResults data structure is created which contains
	//    all the information needed to perform the aggregation on a timestep basis.
	//    After the end of the simulation the original Output data structures
	//    are scanned and actual tables are created doing any scaling as necessary
	//    and placing all the results into an output table.  The output table
	//    is written in the selected format for each of the tables defined.
	// REFERENCES:
	//    None.
	// OTHER NOTES:.
	//                                      |--> BinResults
	//                                      |
	//                                      |--> BinResultsAbove
	//   OutputTableBinned ---------------->|
	//                                      |--> BinResultsBelow
	//                                      |
	//                                      |--> BinObjVarID
	//
	//                                      |--> MonthlyFieldSetInput
	//   MonthlyInput --------------------->|
	//                                      |--> MonthlyTable --> MonthlyColumns

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace InputProcessor;
	using DataGlobals::BigNumber;
	using DataGlobals::ZoneTSReporting;
	using DataGlobals::HVACTSReporting;
	using DataGlobals::KindOfSim;
	using DataGlobals::ksDesignDay;
	using DataGlobals::ksRunPeriodDesign;
	using DataGlobals::ksRunPeriodWeather;
	using DataGlobals::DoWeathSim;
	using DataGlobals::DoOutputReporting;
	using DataGlobals::DisplayExtraWarnings;
	using DataGlobals::OutputFileInits;
	using DataGlobals::HourOfDay;
	using DataGlobals::TimeStep;
	using DataGlobals::SecInHour;
	using DataGlobals::TimeStepZone;
	using DataGlobals::TimeStepZoneSec;
	using DataGlobals::CurrentTime;
	using DataGlobals::NumOfZones;
	using DataGlobals::OutputFileDebug;
	using namespace DataGlobalConstants;
	using namespace OutputReportPredefined;
	using namespace DataHeatBalance;

	// Data
	//MODULE PARAMETER DEFINITIONS:

	int const MaxHeaderLength( 50 );
	int const MaxNoteLength( 200 );

	int const aggTypeSumOrAvg( 1 );
	int const aggTypeMaximum( 2 );
	int const aggTypeMinimum( 3 );
	int const aggTypeValueWhenMaxMin( 4 );
	int const aggTypeHoursZero( 5 );
	int const aggTypeHoursNonZero( 6 );
	int const aggTypeHoursPositive( 7 );
	int const aggTypeHoursNonPositive( 8 );
	int const aggTypeHoursNegative( 9 );
	int const aggTypeHoursNonNegative( 10 );
	int const aggTypeSumOrAverageHoursShown( 11 );
	int const aggTypeMaximumDuringHoursShown( 12 );
	int const aggTypeMinimumDuringHoursShown( 13 );

	int const tableStyleComma( 1 );
	int const tableStyleTab( 2 );
	int const tableStyleFixed( 3 );
	int const tableStyleHTML( 4 );
	int const tableStyleXML( 5 );

	int const unitsStyleNone( 0 ); // no change to any units
	int const unitsStyleJtoKWH( 1 );
	int const unitsStyleJtoMJ( 2 );
	int const unitsStyleJtoGJ( 3 );
	int const unitsStyleInchPound( 4 );
	int const unitsStyleNotFound( 5 );

	int const isAverage( 1 );
	int const isSum( 2 );

	int const stepTypeZone( ZoneTSReporting );
	int const stepTypeHVAC( HVACTSReporting );

	// BEPS Report Related Variables
	// From Report:Table:Predefined - BEPS
	int const numResourceTypes( 14 );
	int const numSourceTypes( 12 );

	static std::string const validChars( "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_:." );
	static std::string const BlankString;

	//MODULE VARIABLE DECLARATIONS:

	// The Binned table type is different and only references one variable and its structure is very
	// different from the others so it is has its own type.

	// arrays for time binned results

	int OutputTableBinnedCount( 0 );
	int BinResultsTableCount( 0 );
	int BinResultsIntervalCount( 0 );

	int const numNamedMonthly( 62 );
	// These reports are detailed/named in routine InitializePredefinedMonthlyTitles

	int MonthlyInputCount( 0 );
	int sizeMonthlyInput( 0 );
	int MonthlyFieldSetInputCount( 0 );
	int sizeMonthlyFieldSetInput( 0 );
	int MonthlyTablesCount( 0 );
	int MonthlyColumnsCount( 0 );
	Array1D_bool IsMonthGathered( 12, false ); // shown as true for any month used

	int TOCEntriesCount( 0 );
	int TOCEntriesSize( 0 );

	int UnitConvSize( 0 );

	bool WriteTabularFiles( false );
	bool GetInput( true );
	bool firstTimeGatherHGReport( true );


	// Allow up to five output files to be created
	int const maxNumStyles( 5 );

	// From Report:Table:Style
	int unitsStyle( 0 ); // see list of parameters
	int numStyles( 0 );
	std::ofstream csv_stream; // CSV table stream
	std::ofstream tab_stream; // Tab table stream
	std::ofstream fix_stream; // Fixed table stream
	std::ofstream htm_stream; // HTML table stream
	std::ofstream xml_stream; // XML table stream
	Array1D< std::ofstream * > TabularOutputFile( maxNumStyles, { &csv_stream, &tab_stream, &fix_stream, &htm_stream, &xml_stream } ); // Table stream array
	Array1D_string del( maxNumStyles ); // the delimiter to use
	Array1D_int TableStyle( maxNumStyles, 0 ); // see list of parameters

	Real64 timeInYear( 0.0 );

	// Flags for predefined tabular reports
	bool displayTabularBEPS( false );
	bool displayLEEDSummary( false );
	bool displayTabularCompCosts( false ); // added BTG 5/6/04 for component cost summary
	bool displayTabularVeriSum( false ); // added JG 2006-06-28 for input verification and summary report
	bool displayComponentSizing( false );
	bool displaySurfaceShadowing( false );
	bool displayDemandEndUse( false );
	bool displayAdaptiveComfort( false );
	bool displaySourceEnergyEndUseSummary( false );
	bool displayZoneComponentLoadSummary( false );

	// BEPS Report Related Variables
	// From Report:Table:Predefined - BEPS
	// arrays that hold the meter numbers that are initialized at get input
	Array1D_int meterNumTotalsBEPS( numResourceTypes, 0 );
	Array1D_int meterNumTotalsSource( numSourceTypes, 0 );
	Array1D_bool fuelfactorsused( numSourceTypes, false );
	Array1D_bool ffUsed( numResourceTypes, false );
	Array1D< Real64 > SourceFactors( numResourceTypes, 0.0 );
	Array1D_bool ffSchedUsed( numResourceTypes, false );
	Array1D_int ffSchedIndex( numResourceTypes, 0 );
	Array2D_int meterNumEndUseBEPS( numResourceTypes, NumEndUses, 0 );
	Array3D_int meterNumEndUseSubBEPS;
	// arrays that hold the names of the resource and end uses
	Array1D_string resourceTypeNames( numResourceTypes );
	Array1D_string sourceTypeNames( numSourceTypes );
	Array1D_string endUseNames( NumEndUses );
	// arrays that hold the actual values for the year
	Array1D< Real64 > gatherTotalsBEPS( numResourceTypes, 0.0 );
	Array1D< Real64 > gatherTotalsBySourceBEPS( numResourceTypes, 0.0 );
	Array1D< Real64 > gatherTotalsSource( numSourceTypes, 0.0 );
	Array1D< Real64 > gatherTotalsBySource( numSourceTypes, 0.0 );
	Array2D< Real64 > gatherEndUseBEPS( numResourceTypes, NumEndUses, 0.0 );
	Array2D< Real64 > gatherEndUseBySourceBEPS( numResourceTypes, NumEndUses, 0.0 );
	Array3D< Real64 > gatherEndUseSubBEPS;
	// arrays the hold the demand values
	Array1D< Real64 > gatherDemandTotal( numResourceTypes, 0.0 );
	Array2D< Real64 > gatherDemandEndUse( numResourceTypes, NumEndUses, 0.0 );
	Array3D< Real64 > gatherDemandEndUseSub;
	Array1D_int gatherDemandTimeStamp( numResourceTypes, 0 );
	// to keep track of hours for the BEPS report gathering
	Real64 gatherElapsedTimeBEPS( 0.0 );
	// for normalization of results
	Real64 buildingGrossFloorArea( 0.0 );
	Real64 buildingConditionedFloorArea( 0.0 );
	// keep track if schedules are used in fuel factors
	bool fuelFactorSchedulesUsed( false );
	// for electic load components on BEPS report
	int meterNumPowerFuelFireGen( 0 );
	Real64 gatherPowerFuelFireGen( 0.0 );
	int meterNumPowerPV( 0 );
	Real64 gatherPowerPV( 0.0 );
	int meterNumPowerWind( 0 );
	Real64 gatherPowerWind( 0.0 );
	Real64 OverallNetEnergyFromStorage( 0.0 );
	int meterNumPowerHTGeothermal( 0 );
	Real64 gatherPowerHTGeothermal( 0.0 );
	int meterNumElecProduced( 0 );
	Real64 gatherElecProduced( 0.0 );
	int meterNumElecPurchased( 0 );
	Real64 gatherElecPurchased( 0.0 );
	int meterNumElecSurplusSold( 0 );
	Real64 gatherElecSurplusSold( 0.0 );
	int meterNumElecStorage = ( 0 );
	Real64 gatherElecStorage = ( 0.0 );
	int meterNumPowerConversion = ( 0 );
	Real64 gatherPowerConversion = ( 0.0 );
	// for on site thermal source components on BEPS report
	int meterNumWaterHeatRecovery( 0 );
	Real64 gatherWaterHeatRecovery( 0.0 );
	int meterNumAirHeatRecoveryCool( 0 );
	Real64 gatherAirHeatRecoveryCool( 0.0 );
	int meterNumAirHeatRecoveryHeat( 0 );
	Real64 gatherAirHeatRecoveryHeat( 0.0 );
	int meterNumHeatHTGeothermal( 0 );
	Real64 gatherHeatHTGeothermal( 0.0 );
	int meterNumHeatSolarWater( 0 );
	Real64 gatherHeatSolarWater( 0.0 );
	int meterNumHeatSolarAir( 0 );
	Real64 gatherHeatSolarAir( 0.0 );
	// for on site water components on BEPS report
	int meterNumRainWater( 0 );
	Real64 gatherRainWater( 0.0 );
	int meterNumCondensate( 0 );
	Real64 gatherCondensate( 0.0 );
	int meterNumGroundwater( 0 );
	Real64 gatherWellwater( 0.0 );
	int meterNumMains( 0 );
	Real64 gatherMains( 0.0 );
	int meterNumWaterEndUseTotal( 0 );
	Real64 gatherWaterEndUseTotal( 0.0 );
	// for source energy conversion factors on BEPS report
	Real64 sourceFactorElectric( 0.0 );
	Real64 sourceFactorNaturalGas( 0.0 );
	Real64 efficiencyDistrictCooling( 0.0 );
	Real64 efficiencyDistrictHeating( 0.0 );
	Real64 sourceFactorSteam( 0.0 );
	Real64 sourceFactorGasoline( 0.0 );
	Real64 sourceFactorDiesel( 0.0 );
	Real64 sourceFactorCoal( 0.0 );
	Real64 sourceFactorFuelOil1( 0.0 );
	Real64 sourceFactorFuelOil2( 0.0 );
	Real64 sourceFactorPropane( 0.0 );
	Real64 sourceFactorOtherFuel1( 0.0 );
	Real64 sourceFactorOtherFuel2( 0.0 );

	Array1D_int td( 8 );
	//(1)   Current year
	//(2)   Current month
	//(3)   Current day
	//(4)   Time difference with respect to UTC in minutes (0-59)
	//(5)   Hour of the day (0-23)
	//(6)   Minutes (0-59)
	//(7)   Seconds (0-59)
	//(8)   Milliseconds (0-999)

	// Design day name storage
	Array1D_string DesignDayName;
	int DesignDayCount( 0 );

	//arrays related to pulse and load component reporting
	Array2D< Real64 > radiantPulseUsed;
	Array2D_int radiantPulseTimestep;
	Array2D< Real64 > radiantPulseReceived;
	Array3D< Real64 > loadConvectedNormal;
	Array3D< Real64 > loadConvectedWithPulse;
	Array3D< Real64 > netSurfRadSeq;
	Array2D< Real64 > decayCurveCool;
	Array2D< Real64 > decayCurveHeat;
	Array3D< Real64 > ITABSFseq; // used for determining the radiant fraction on each surface
	Array3D< Real64 > TMULTseq; // used for determining the radiant fraction on each surface

	Array3D< Real64 > peopleInstantSeq;
	Array3D< Real64 > peopleLatentSeq;
	Array3D< Real64 > peopleRadSeq;

	Array3D< Real64 > lightInstantSeq;
	Array3D< Real64 > lightRetAirSeq;
	Array3D< Real64 > lightLWRadSeq; // long wave thermal radiation
	Array3D< Real64 > lightSWRadSeq; // short wave visible radiation

	Array3D< Real64 > equipInstantSeq;
	Array3D< Real64 > equipLatentSeq;
	Array3D< Real64 > equipRadSeq;

	Array3D< Real64 > refrigInstantSeq;
	Array3D< Real64 > refrigRetAirSeq;
	Array3D< Real64 > refrigLatentSeq;

	Array3D< Real64 > waterUseInstantSeq;
	Array3D< Real64 > waterUseLatentSeq;

	Array3D< Real64 > hvacLossInstantSeq;
	Array3D< Real64 > hvacLossRadSeq;

	Array3D< Real64 > powerGenInstantSeq;
	Array3D< Real64 > powerGenRadSeq;
	Array3D< Real64 > infilInstantSeq;
	Array3D< Real64 > infilLatentSeq;

	Array3D< Real64 > zoneVentInstantSeq;
	Array3D< Real64 > zoneVentLatentSeq;

	Array3D< Real64 > interZoneMixInstantSeq;
	Array3D< Real64 > interZoneMixLatentSeq;

	Array3D< Real64 > feneCondInstantSeq;
	//REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: feneSolarInstantSeq
	Array3D< Real64 > feneSolarRadSeq;
	int maxUniqueKeyCount( 0 );

	// for the XML report must keep track fo the active sub-table name and report set by other routines
	std::string activeSubTableName;
	std::string activeReportNameNoSpace;
	std::string activeReportName;
	std::string activeForName;
	std::string prevReportName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops
	//PRIVATE      DateToStr

	// Object Data
	Array1D< OutputTableBinnedType > OutputTableBinned;
	Array2D< BinResultsType > BinResults; // table number, number of intervals
	Array1D< BinResultsType > BinResultsBelow; // time below the lowest defined bin
	Array1D< BinResultsType > BinResultsAbove; // time above the highest defined bin
	Array1D< BinObjVarIDType > BinObjVarID;
	Array1D< BinStatisticsType > BinStatistics;
	Array1D< NamedMonthlyType > namedMonthly; // for predefined monthly report titles
	Array1D< MonthlyFieldSetInputType > MonthlyFieldSetInput;
	Array1D< MonthlyInputType > MonthlyInput;
	Array1D< MonthlyTablesType > MonthlyTables;
	Array1D< MonthlyColumnsType > MonthlyColumns;
	Array1D< TOCEntriesType > TOCEntries;
	Array1D< UnitConvType > UnitConv;

	static gio::Fmt fmtLD( "*" );
	static gio::Fmt fmtA( "(A)" );

	namespace {
		bool GatherMonthlyResultsForTimestepRunOnce( true );
		bool UpdateTabularReportsGetInput( true );
		bool GatherHeatGainReportfirstTime( true );
		bool AllocateLoadComponentArraysDoAllocate( true );
	}

	// Functions
	void
	clear_state(){
		GatherMonthlyResultsForTimestepRunOnce =  true;
		UpdateTabularReportsGetInput = true;
		GatherHeatGainReportfirstTime = true;
		AllocateLoadComponentArraysDoAllocate = true;
		OutputTableBinnedCount = 0;
		BinResultsTableCount = 0;
		BinResultsIntervalCount = 0;
		MonthlyInputCount = 0;
		sizeMonthlyInput = 0;
		MonthlyFieldSetInputCount = 0;
		sizeMonthlyFieldSetInput = 0;
		MonthlyTablesCount = 0;
		MonthlyColumnsCount = 0;
		IsMonthGathered = Array1D_bool ( 12, false );
		TOCEntriesCount = 0;
		TOCEntriesSize = 0;
		UnitConvSize = 0;
		WriteTabularFiles = false;
		unitsStyle = 0;
		numStyles = 0;
		TabularOutputFile = Array1D< std::ofstream * > ( maxNumStyles, { &csv_stream, &tab_stream, &fix_stream, &htm_stream, &xml_stream } );
		del = Array1D_string ( maxNumStyles );
		TableStyle = Array1D_int ( maxNumStyles, 0 );
		timeInYear = 0.0;
		displayTabularBEPS = false;
		displayLEEDSummary = false;
		displayTabularCompCosts = false;
		displayTabularVeriSum = false;
		displayComponentSizing = false;
		displaySurfaceShadowing = false;
		displayDemandEndUse = false;
		displayAdaptiveComfort = false;
		displaySourceEnergyEndUseSummary = false;
		displayZoneComponentLoadSummary = false;
		meterNumTotalsBEPS = Array1D_int ( numResourceTypes, 0 );
		meterNumTotalsSource = Array1D_int ( numSourceTypes, 0 );
		fuelfactorsused = Array1D_bool ( numSourceTypes, false );
		ffUsed = Array1D_bool ( numResourceTypes, false );
		SourceFactors = Array1D< Real64 > ( numResourceTypes, 0.0 );
		ffSchedUsed = Array1D_bool ( numResourceTypes, false );
		ffSchedIndex = Array1D_int ( numResourceTypes, 0 );
		meterNumEndUseBEPS = Array2D_int ( numResourceTypes, NumEndUses, 0 );
		meterNumEndUseSubBEPS.deallocate();
//		resourceTypeNames.deallocate();
//		sourceTypeNames.deallocate();
//		endUseNames.deallocate();
		gatherTotalsBEPS = Array1D< Real64 > ( numResourceTypes, 0.0 );
		gatherTotalsBySourceBEPS = Array1D< Real64 > ( numResourceTypes, 0.0 );
		gatherTotalsSource = Array1D< Real64 > ( numSourceTypes, 0.0 );
		gatherTotalsBySource= Array1D< Real64 > ( numSourceTypes, 0.0 );
		gatherEndUseBEPS = Array2D< Real64 > ( numResourceTypes, NumEndUses, 0.0 );
		gatherEndUseBySourceBEPS = Array2D< Real64 > ( numResourceTypes, NumEndUses, 0.0 );
		gatherEndUseSubBEPS.deallocate();
		gatherDemandTotal = Array1D< Real64 > ( numResourceTypes, 0.0 );
		gatherDemandEndUse = Array2D< Real64 > ( numResourceTypes, NumEndUses, 0.0 );
		gatherDemandEndUseSub.deallocate();
		gatherDemandTimeStamp = Array1D_int ( numResourceTypes, 0 );
		gatherElapsedTimeBEPS = 0.0;
		buildingGrossFloorArea = 0.0;
		buildingConditionedFloorArea = 0.0;
		fuelFactorSchedulesUsed = false;
		meterNumPowerFuelFireGen = 0;
		gatherPowerFuelFireGen = 0.0;
		meterNumPowerPV = 0;
		gatherPowerPV = 0.0;
		meterNumPowerWind = 0;
		gatherPowerWind = 0.0;
		OverallNetEnergyFromStorage = 0.0;
		meterNumPowerHTGeothermal = 0;
		gatherPowerHTGeothermal = 0.0;
		meterNumElecProduced = 0;
		gatherElecProduced = 0.0;
		meterNumElecPurchased = 0;
		gatherElecPurchased = 0.0;
		meterNumElecSurplusSold = 0;
		gatherElecSurplusSold = 0.0;
		meterNumWaterHeatRecovery = 0;
		gatherWaterHeatRecovery = 0.0;
		meterNumAirHeatRecoveryCool = 0;
		gatherAirHeatRecoveryCool = 0.0;
		meterNumAirHeatRecoveryHeat = 0;
		gatherAirHeatRecoveryHeat = 0.0;
		meterNumHeatHTGeothermal = 0;
		gatherHeatHTGeothermal = 0.0;
		meterNumHeatSolarWater = 0;
		gatherHeatSolarWater = 0.0;
		meterNumHeatSolarAir = 0;
		gatherHeatSolarAir = 0.0;
		meterNumRainWater = 0;
		gatherRainWater = 0.0;
		meterNumCondensate = 0;
		gatherCondensate = 0.0;
		meterNumGroundwater = 0;
		gatherWellwater = 0.0;
		meterNumMains = 0;
		gatherMains = 0.0;
		meterNumWaterEndUseTotal = 0;
		gatherWaterEndUseTotal = 0.0;
		sourceFactorElectric = 0.0;
		sourceFactorNaturalGas = 0.0;
		efficiencyDistrictCooling = 0.0;
		efficiencyDistrictHeating = 0.0;
		sourceFactorSteam = 0.0;
		sourceFactorGasoline = 0.0;
		sourceFactorDiesel = 0.0;
		sourceFactorCoal = 0.0;
		sourceFactorFuelOil1 = 0.0;
		sourceFactorFuelOil2 = 0.0;
		sourceFactorPropane = 0.0;
		sourceFactorOtherFuel1 = 0.0;
		sourceFactorOtherFuel2 = 0.0;
		DesignDayName.deallocate();
		DesignDayCount = 0;
		radiantPulseUsed.deallocate();
		radiantPulseTimestep.deallocate();
		radiantPulseReceived.deallocate();
		loadConvectedNormal.deallocate();
		loadConvectedWithPulse.deallocate();
		netSurfRadSeq.deallocate();
		decayCurveCool.deallocate();
		decayCurveHeat.deallocate();
		ITABSFseq.deallocate();
		TMULTseq.deallocate();
		peopleInstantSeq.deallocate();
		peopleLatentSeq.deallocate();
		peopleRadSeq.deallocate();
		lightInstantSeq.deallocate();
		lightRetAirSeq.deallocate();
		lightLWRadSeq.deallocate();
		lightSWRadSeq.deallocate();
		equipInstantSeq.deallocate();
		equipLatentSeq.deallocate();
		equipRadSeq.deallocate();
		refrigInstantSeq.deallocate();
		refrigRetAirSeq.deallocate();
		refrigLatentSeq.deallocate();
		waterUseInstantSeq.deallocate();
		waterUseLatentSeq.deallocate();
		hvacLossInstantSeq.deallocate();
		hvacLossRadSeq.deallocate();
		powerGenInstantSeq.deallocate();
		powerGenRadSeq.deallocate();
		infilInstantSeq.deallocate();
		infilLatentSeq.deallocate();
		zoneVentInstantSeq.deallocate();
		zoneVentLatentSeq.deallocate();
		interZoneMixInstantSeq.deallocate();
		interZoneMixLatentSeq.deallocate();
		feneCondInstantSeq.deallocate();
		feneSolarRadSeq.deallocate();
		maxUniqueKeyCount = 0;
		OutputTableBinned.deallocate();
		BinResults.deallocate();
		BinResultsBelow.deallocate();
		BinResultsAbove.deallocate();
		BinObjVarID.deallocate();
		BinStatistics.deallocate();
		namedMonthly.deallocate();
		MonthlyFieldSetInput.deallocate();
		MonthlyInput.deallocate();
		MonthlyTables.deallocate();
		MonthlyColumns.deallocate();
		TOCEntries.deallocate();
		UnitConv.deallocate();

		OutputReportTabular::ResetTabularReports();
	}

	void
	UpdateTabularReports( int const IndexTypeKey ) // What kind of data to update (Zone, HVAC)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This is the routine that is called at the end of the time step
		// loop and updates the arrays of data that will later being put
		// into the tabular reports.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:


		if ( IndexTypeKey != ZoneTSReporting && IndexTypeKey != HVACTSReporting ) {
			ShowFatalError( "Invalid reporting requested -- UpdateTabularReports" );
		}

		if ( UpdateTabularReportsGetInput ) {
			GetInputTabularMonthly();
			OutputReportTabularAnnual::GetInputTabularAnnual();
			GetInputTabularTimeBins();
			GetInputTabularStyle();
			GetInputTabularPredefined();
			// noel -- noticed this was called once and very slow -- sped up a little by caching keys
			InitializeTabularMonthly();
			GetInputFuelAndPollutionFactors();
			SetupUnitConversions();
			AddTOCZoneLoadComponentTable();
			UpdateTabularReportsGetInput = false;
			date_and_time( _, _, _, td );
		}
		if ( DoOutputReporting && WriteTabularFiles && ( KindOfSim == ksRunPeriodWeather ) ) {
			if ( IndexTypeKey == stepTypeZone ) {
				gatherElapsedTimeBEPS += TimeStepZone;
			}
			if ( DoWeathSim ) {
				GatherMonthlyResultsForTimestep( IndexTypeKey );
				OutputReportTabularAnnual::GatherAnnualResultsForTimeStep( IndexTypeKey );
				GatherBinResultsForTimestep( IndexTypeKey );
				GatherBEPSResultsForTimestep( IndexTypeKey );
				GatherSourceEnergyEndUseResultsForTimestep( IndexTypeKey );
				GatherPeakDemandForTimestep( IndexTypeKey );
				GatherHeatGainReport( IndexTypeKey );
			}
		}
	}

	//======================================================================================================================
	//======================================================================================================================

	//    GET INPUT ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	GetInputTabularMonthly()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   The routine assigns the input information for
		//   REPORT:TABLE:MONTHLY also known as tabular monthly
		//   reports that are defined by the user. The input
		//   information is assigned to a data structure that
		//   is used for both user defined monthly reports and
		//   predefined monthly reports.

		// METHODOLOGY EMPLOYED:
		//   Uses get input structure and call to build up
		//   data on monthly reports.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurrentModuleObject( "Output:Table:Monthly" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int curTable; // index of the current table being processed in MonthlyInput
		int curAggType; // kind of aggregation identified (see AggType parameters)
		std::string curAggString; // Current aggregation sting
		int jField;
		int NumParams; // Number of elements combined
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		Array1D_string AlphArray; // character string data
		Array1D< Real64 > NumArray; // numeric data
		int IOStat; // IO Status when calling get input subroutine
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name

		MonthlyInputCount = GetNumObjectsFound( CurrentModuleObject );
		if ( MonthlyInputCount > 0 ) {
			WriteTabularFiles = true;
			// if not a run period using weather do not create reports
			if ( ! DoWeathSim ) {
				ShowWarningError( CurrentModuleObject + " requested with SimulationControl Run Simulation for Weather File Run Periods set to No so " + CurrentModuleObject + " will not be generated" );
				return;
			}
		}
		GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNums );
		AlphArray.allocate( NumAlphas );
		NumArray.dimension( NumNums, 0.0 );
		for ( int TabNum = 1, TabNum_end = MonthlyInputCount; TabNum <= TabNum_end; ++TabNum ) { // MonthlyInputCount is modified in the loop
			GetObjectItem( CurrentModuleObject, TabNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat );
			IsNotOK = false;
			IsBlank = false;
			if ( TabNum - 1 > 0 ) {
				VerifyName( AlphArray( 1 ), MonthlyInput, &MonthlyInputType::name, TabNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "RTMBLANK";
				}
			}
			if ( NumAlphas < 2 ) {
				ShowSevereError( CurrentModuleObject + ": No fields specified." );
			}
			// add to the data structure
			curTable = AddMonthlyReport( AlphArray( 1 ), int( NumArray( 1 ) ) );
			for ( jField = 2; jField <= NumAlphas; jField += 2 ) {
				curAggString = AlphArray( jField + 1 );
				// set accumulator values to default as appropriate for aggregation type
				if ( SameString( curAggString, "SumOrAverage" ) ) {
					curAggType = aggTypeSumOrAvg;
				} else if ( SameString( curAggString, "Maximum" ) ) {
					curAggType = aggTypeMaximum;
				} else if ( SameString( curAggString, "Minimum" ) ) {
					curAggType = aggTypeMinimum;
				} else if ( SameString( curAggString, "ValueWhenMaximumOrMinimum" ) ) {
					curAggType = aggTypeValueWhenMaxMin;
				} else if ( SameString( curAggString, "HoursZero" ) ) {
					curAggType = aggTypeHoursZero;
				} else if ( SameString( curAggString, "HoursNonzero" ) ) {
					curAggType = aggTypeHoursNonZero;
				} else if ( SameString( curAggString, "HoursPositive" ) ) {
					curAggType = aggTypeHoursPositive;
				} else if ( SameString( curAggString, "HoursNonpositive" ) ) {
					curAggType = aggTypeHoursNonPositive;
				} else if ( SameString( curAggString, "HoursNegative" ) ) {
					curAggType = aggTypeHoursNegative;
				} else if ( SameString( curAggString, "HoursNonnegative" ) ) {
					curAggType = aggTypeHoursNonNegative;
				} else if ( SameString( curAggString, "SumOrAverageDuringHoursShown" ) ) {
					curAggType = aggTypeSumOrAverageHoursShown;
				} else if ( SameString( curAggString, "MaximumDuringHoursShown" ) ) {
					curAggType = aggTypeMaximumDuringHoursShown;
				} else if ( SameString( curAggString, "MinimumDuringHoursShown" ) ) {
					curAggType = aggTypeMinimumDuringHoursShown;
				} else {
					curAggType = aggTypeSumOrAvg;
					ShowWarningError( CurrentModuleObject + '=' + MonthlyInput( TabNum ).name + ", Variable name=" + AlphArray( jField ) );
					ShowContinueError( "Invalid aggregation type=\"" + curAggString + "\"  Defaulting to SumOrAverage." );
				}
				AddMonthlyFieldSetInput( curTable, AlphArray( jField ), "", curAggType );
			}
		}

	}

	int
	AddMonthlyReport(
		std::string const & inReportName,
		int const inNumDigitsShown
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2008
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Creates a monthly report

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int const SizeAdder( 25 );

		if ( ! allocated( MonthlyInput ) ) {
			MonthlyInput.allocate( SizeAdder );
			sizeMonthlyInput = SizeAdder;
			MonthlyInputCount = 1;
		} else {
			++MonthlyInputCount;
			// if larger than current size grow the array
			if ( MonthlyInputCount > sizeMonthlyInput ) {
				MonthlyInput.redimension( sizeMonthlyInput += SizeAdder );
			}
		}
		// initialize new record
		MonthlyInput( MonthlyInputCount ).name = inReportName;
		MonthlyInput( MonthlyInputCount ).showDigits = inNumDigitsShown;
		return MonthlyInputCount;
	}

	void
	AddMonthlyFieldSetInput(
		int const inMonthReport,
		std::string const & inVariMeter,
		std::string const & inColHead,
		int const inAggregate
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2008
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Assigns the column information for predefined
		//   monthly reports

		// METHODOLOGY EMPLOYED:
		//   Simple assignments to public variables.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const sizeIncrement( 50 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( ! allocated( MonthlyFieldSetInput ) ) {
			MonthlyFieldSetInput.allocate( sizeIncrement );
			sizeMonthlyFieldSetInput = sizeIncrement;
			MonthlyFieldSetInputCount = 1;
		} else {
			++MonthlyFieldSetInputCount;
			// if larger than current size grow the array
			if ( MonthlyFieldSetInputCount > sizeMonthlyFieldSetInput ) {
				MonthlyFieldSetInput.redimension( sizeMonthlyFieldSetInput *= 2 ); //Tuned Changed += sizeIncrement to *= 2 for reduced heap allocations (at some space cost)
			}
		}
		// initialize new record)
		MonthlyFieldSetInput( MonthlyFieldSetInputCount ).variMeter = inVariMeter;
		MonthlyFieldSetInput( MonthlyFieldSetInputCount ).colHead = inColHead;
		MonthlyFieldSetInput( MonthlyFieldSetInputCount ).aggregate = inAggregate;
		//update the references from the MonthlyInput array
		if ( ( inMonthReport > 0 ) && ( inMonthReport <= MonthlyInputCount ) ) {
			if ( MonthlyInput( inMonthReport ).firstFieldSet == 0 ) {
				MonthlyInput( inMonthReport ).firstFieldSet = MonthlyFieldSetInputCount;
				MonthlyInput( inMonthReport ).numFieldSet = 1;
			} else {
				++MonthlyInput( inMonthReport ).numFieldSet;
			}
		}
	}

	void
	InitializeTabularMonthly()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   July 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   This routine initializes the data structures based
		//   on input from either the IDF file or from the predefined
		//   monthly reports.  The data structures follow the IDD
		//   closely.  The routine initializes many of the arrays
		//   for monthly tables.

		// METHODOLOGY EMPLOYED:
		//   Process the data structures that define monthly tabular
		//   reports

		// NOTE:
		//   The bulk of this routine used to be part of the the
		//   GetInputTabularMonthly routine but when predefined
		//   monthly reports were added this routine was seperated
		//   from input.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int TabNum; // index when cycling through each table
		int NumColumns; // number of columns specified in the input for an object
		int FirstColumn; // the first column of the monthly input
		std::string curVariMeter; // current variable or meter
		int colNum; // loop index for columns
		int KeyCount;
		int TypeVar;
		int AvgSumVar;
		int StepTypeVar;
		std::string UnitsVar; // Units sting, may be blank
		//CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: NamesOfKeys      ! Specific key name
		//INTEGER, DIMENSION(:) , ALLOCATABLE                     :: IndexesForKeyVar ! Array index
		Array1D_string UniqueKeyNames;
		int UniqueKeyCount;
		int iKey;
		int jUnique;
		int found;
		int kUniqueKey;
		int lTable;
		int mColumn;
		int ColumnsRecount;
		int TablesRecount;
		static Real64 BigNum( 0.0 );
		bool environmentKeyFound;
		static bool VarWarning( true );
		static int ErrCount1( 0 );
		static int ErrCount2( 0 );
		//INTEGER       :: maxKeyCount

		// if not a running a weather simulation do not create reports
		if ( ! DoWeathSim ) return;
		maxUniqueKeyCount = 1500;
		UniqueKeyNames.allocate( maxUniqueKeyCount );
		// First pass through the input objects is to put the name of the report
		// into the array and count the number of unique keys found to allocate
		// the monthlyTables and monthlyColumns
		// This approach seems inefficient but I know of no other way to size
		// the arrays prior to filling them and to size the arrays basically
		// the same steps must be gone through as with filling the arrays.

		//#ifdef ITM_KEYCACHE
		// Noel comment:  How about allocating these variables once for the whole routine?
		//    Again, if a max value for key count can be agreed upon, we could use it here --
		//    otherwise, will have to have re-allocate logic.
		//maxKeyCount=1500 ! ?
		//ALLOCATE(NamesOfKeys(maxKeyCount))
		//ALLOCATE(IndexesForKeyVar(maxKeyCount))
		//#endif

		MonthlyColumnsCount = 0;
		MonthlyTablesCount = 0;
		for ( TabNum = 1; TabNum <= MonthlyInputCount; ++TabNum ) {
			// the number of columns based on number of alpha fields
			NumColumns = MonthlyInput( TabNum ).numFieldSet;
			FirstColumn = MonthlyInput( TabNum ).firstFieldSet;
			environmentKeyFound = false;
			UniqueKeyCount = 0;
			for ( colNum = 1; colNum <= NumColumns; ++colNum ) {

				//#ifdef ITM_KEYCACHE
				// Noel comment:  First time in this TabNum/ColNum loop, let's save the results
				//  of GetVariableKeyCountandType & GetVariableKeys.
				curVariMeter = MakeUPPERCase( MonthlyFieldSetInput( FirstColumn + colNum - 1 ).variMeter );
				// call the key count function but only need count during this pass
				GetVariableKeyCountandType( curVariMeter, KeyCount, TypeVar, AvgSumVar, StepTypeVar, UnitsVar );
				//    IF (KeyCount > maxKeyCount) THEN
				//      DEALLOCATE(NamesOfKeys)
				//      DEALLOCATE(IndexesForKeyVar)
				//      maxKeyCount=KeyCount
				//      ALLOCATE(NamesOfKeys(maxKeyCount))
				//      ALLOCATE(IndexesForKeyVar(maxKeyCount))
				//    ENDIF
				MonthlyFieldSetInput( FirstColumn + colNum - 1 ).NamesOfKeys.allocate( KeyCount );
				MonthlyFieldSetInput( FirstColumn + colNum - 1 ).IndexesForKeyVar.allocate( KeyCount );

				// fill keys?
				GetVariableKeys( curVariMeter, TypeVar, MonthlyFieldSetInput( FirstColumn + colNum - 1 ).NamesOfKeys, MonthlyFieldSetInput( FirstColumn + colNum - 1 ).IndexesForKeyVar );

				// save these values to use later -- noel
				MonthlyFieldSetInput( FirstColumn + colNum - 1 ).variMeterUpper = curVariMeter;
				MonthlyFieldSetInput( FirstColumn + colNum - 1 ).typeOfVar = TypeVar;
				MonthlyFieldSetInput( FirstColumn + colNum - 1 ).keyCount = KeyCount;
				MonthlyFieldSetInput( FirstColumn + colNum - 1 ).varAvgSum = AvgSumVar;
				MonthlyFieldSetInput( FirstColumn + colNum - 1 ).varStepType = StepTypeVar;
				MonthlyFieldSetInput( FirstColumn + colNum - 1 ).varUnits = UnitsVar;
				//    DO iKey = 1, KeyCount
				//      MonthlyFieldSetInput(FirstColumn + ColNum - 1)%NamesOfKeys(iKey) = NamesOfKeys(iKey)  !noel
				//      MonthlyFieldSetInput(FirstColumn + ColNum - 1)%IndexesForKeyVar(iKey) = IndexesForKeyVar(iKey)  !noel
				//    ENDDO
				//#else
				//    curVariMeter = MakeUPPERCase(MonthlyFieldSetInput(FirstColumn + ColNum - 1)%variMeter)
				//    ! call the key count function but only need count during this pass
				//    CALL GetVariableKeyCountandType(curVariMeter,KeyCount,TypeVar,AvgSumVar,StepTypeVar,UnitsVar)
				//    ALLOCATE(NamesOfKeys(KeyCount))
				//    ALLOCATE(IndexesForKeyVar(KeyCount))
				//    CALL GetVariableKeys(curVariMeter,TypeVar,NamesOfKeys,IndexesForKeyVar)
				//#endif

				for ( iKey = 1; iKey <= KeyCount; ++iKey ) {
					found = 0;
					// set a flag if environment variables are found
					if ( SameString( MonthlyFieldSetInput( FirstColumn + colNum - 1 ).NamesOfKeys( iKey ), "ENVIRONMENT" ) ) {
						environmentKeyFound = true;
						found = -1; //so not counted in list of unique keys
					}
					for ( jUnique = 1; jUnique <= UniqueKeyCount; ++jUnique ) {
						if ( SameString( UniqueKeyNames( jUnique ), MonthlyFieldSetInput( FirstColumn + colNum - 1 ).NamesOfKeys( iKey ) ) ) {
							found = jUnique;
							break;
						}
					}
					if ( found == 0 ) {
						++UniqueKeyCount;
						if ( UniqueKeyCount > maxUniqueKeyCount ) {
							UniqueKeyNames.redimension( maxUniqueKeyCount += 500 );
						}
						UniqueKeyNames( UniqueKeyCount ) = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).NamesOfKeys( iKey );
					}
				}
				//#ifdef ITM_KEYCACHE
				//    ! Don't deallocate here, only allocating/deallocating once for the whole routine
				//#else
				//    DEALLOCATE(NamesOfKeys)
				//    DEALLOCATE(IndexesForKeyVar)
				//#endif
			} //colNum
			// fix for CR8285 - when monthly report is only environmental variables
			if ( environmentKeyFound && UniqueKeyCount == 0 ) {
				UniqueKeyCount = 1;
			}
			// increment the number of tables based on the number of unique keys
			MonthlyTablesCount += UniqueKeyCount;
			MonthlyColumnsCount += UniqueKeyCount * NumColumns;
		} //TabNum the end of the loop through the inputs objects
		// Now that we have the maximum size of the number of tables (each table is
		// repeated for the number of keys found) and the number of total columns
		// of all of the tables, allocate the arrays to store this information.
		MonthlyTables.allocate( MonthlyTablesCount );
		MonthlyColumns.allocate( MonthlyColumnsCount );
		// Initialize tables and results
		for ( auto & e : MonthlyTables ) {
			e.keyValue.clear();
			e.firstColumn = 0;
			e.numColumns = 0;
		}

		for ( auto & e : MonthlyColumns ) {
			e.varName.clear();
			e.varNum = 0;
			e.typeOfVar = 0;
			e.avgSum = 0;
			e.stepType = 0;
			e.units.clear();
			e.aggType = 0;
		}
		for ( colNum = 1; colNum <= MonthlyColumnsCount; ++colNum ) {
			MonthlyColumns( colNum ).reslt = 0.0;
			MonthlyColumns( colNum ).timeStamp = 0;
			MonthlyColumns( colNum ).duration = 0.0;
		}

		ColumnsRecount = 0;
		TablesRecount = 0;
		for ( TabNum = 1; TabNum <= MonthlyInputCount; ++TabNum ) {
			// the number of columns based on number of alpha fields
			NumColumns = MonthlyInput( TabNum ).numFieldSet;
			FirstColumn = MonthlyInput( TabNum ).firstFieldSet;
			UniqueKeyCount = 0;
			environmentKeyFound = false;
			for ( colNum = 1; colNum <= NumColumns; ++colNum ) {
				//#ifdef ITM_KEYCACHE
				// Noel comment:  Here is where we could use the saved values
				curVariMeter = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).variMeterUpper;
				KeyCount = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).keyCount;
				TypeVar = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).typeOfVar;
				AvgSumVar = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).varAvgSum;
				StepTypeVar = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).varStepType;
				UnitsVar = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).varUnits;
				//    DO iKey = 1, KeyCount  !noel
				//       NamesOfKeys(iKey) = MonthlyFieldSetInput(FirstColumn + ColNum - 1)%NamesOfKeys(iKey)  !noel
				//       IndexesForKeyVar(iKey) = MonthlyFieldSetInput(FirstColumn + ColNum - 1)%IndexesForKeyVar(iKey) !noel
				//    ENDDO
				//#else
				//    curVariMeter = MakeUPPERCase(MonthlyFieldSetInput(FirstColumn + ColNum - 1)%variMeter)
				//    ! call the key count function but only need count during this pass
				//    CALL GetVariableKeyCountandType(curVariMeter,KeyCount,TypeVar,AvgSumVar,StepTypeVar,UnitsVar)
				//    ALLOCATE(NamesOfKeys(KeyCount))
				//    ALLOCATE(IndexesForKeyVar(KeyCount))
				//    CALL GetVariableKeys(curVariMeter,TypeVar,NamesOfKeys,IndexesForKeyVar)
				//#endif

				if ( KeyCount == 0 ) {
					++ErrCount1;
					if ( ErrCount1 == 1 && ! DisplayExtraWarnings && ! VarWarning && KindOfSim == ksRunPeriodWeather ) {
						ShowWarningError( "Processing Monthly Tabular Reports: Variable names not valid for this simulation" );
						ShowContinueError( "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual variables." );
					}
					//fixing CR5878 removed the showing of the warning once about a specific variable.
					if ( DisplayExtraWarnings && KindOfSim == ksRunPeriodWeather ) {
						ShowWarningError( "Processing Monthly Tabular Reports: " + MonthlyInput( TabNum ).name );
						ShowContinueError( "..Variable name=" + curVariMeter + " not valid for this simulation." );
						if ( VarWarning ) {
							ShowContinueError( "..Variables not valid for this simulation will have \"[Invalid/Undefined]\" in the Units Column of the Table Report." );
							VarWarning = false;
						}
					}
				}
				for ( iKey = 1; iKey <= KeyCount; ++iKey ) {
					found = 0;
					// set a flag if environment variables are found
					if ( SameString( MonthlyFieldSetInput( FirstColumn + colNum - 1 ).NamesOfKeys( iKey ), "ENVIRONMENT" ) ) {
						environmentKeyFound = true;
						found = -1; //so not counted in list of unique keys
					}
					for ( jUnique = 1; jUnique <= UniqueKeyCount; ++jUnique ) {
						if ( SameString( UniqueKeyNames( jUnique ), MonthlyFieldSetInput( FirstColumn + colNum - 1 ).NamesOfKeys( iKey ) ) ) {
							found = jUnique;
							break;
						}
					}
					if ( found == 0 ) {
						++UniqueKeyCount;
						UniqueKeyNames( UniqueKeyCount ) = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).NamesOfKeys( iKey );
					}
				}
				//#ifdef ITM_KEYCACHE
				//    ! Don't deallocate here, only allocating/deallocating once for the whole routine
				//#else
				//    DEALLOCATE(NamesOfKeys)
				//    DEALLOCATE(IndexesForKeyVar)
				//#endif
			}
			// fix for CR8285 - when monthly report is only environmental variables
			if ( environmentKeyFound && UniqueKeyCount == 0 ) {
				UniqueKeyCount = 1;
			}
			// increment the number of tables based on the number of unique keys
			MonthlyInput( TabNum ).firstTable = TablesRecount + 1;
			MonthlyInput( TabNum ).numTables = UniqueKeyCount;
			TablesRecount += UniqueKeyCount;
			// loop through the different unique keys since each user defined table
			// has that many instances - one for each unique key.
			// It is unusual that this loop is about 'keys' and an inner loop is also
			// about 'keys' but for this loop the keys are really instances of tables.
			for ( kUniqueKey = 1; kUniqueKey <= UniqueKeyCount; ++kUniqueKey ) {
				lTable = kUniqueKey + MonthlyInput( TabNum ).firstTable - 1;
				//use the term 'environment' for identifying the report if
				if ( environmentKeyFound && UniqueKeyCount == 1 ) {
					MonthlyTables( lTable ).keyValue = "Environment";
				} else { //this is the most common case is to use the unique key for the report
					MonthlyTables( lTable ).keyValue = UniqueKeyNames( kUniqueKey );
				}
				MonthlyTables( lTable ).firstColumn = ColumnsRecount + 1;
				MonthlyTables( lTable ).numColumns = NumColumns;
				ColumnsRecount += NumColumns;
				FirstColumn = MonthlyInput( TabNum ).firstFieldSet;
				for ( colNum = 1; colNum <= NumColumns; ++colNum ) {
					environmentKeyFound = false;
					mColumn = colNum + MonthlyTables( lTable ).firstColumn - 1;
					// when going through the columns this time, not all columns may have
					// a EP variable that corresponds to it.  In no variable is found
					// then set it to 0 to be skipped during data gathering

					//#ifdef ITM_KEYCACHE
					// Noel comment:  Here is where we could use the saved values
					curVariMeter = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).variMeterUpper;
					KeyCount = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).keyCount;
					TypeVar = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).typeOfVar;
					AvgSumVar = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).varAvgSum;
					StepTypeVar = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).varStepType;
					UnitsVar = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).varUnits;
					//    DO iKey = 1, KeyCount  !noel
					//       NamesOfKeys(iKey) = MonthlyFieldSetInput(FirstColumn + ColNum - 1)%NamesOfKeys(iKey)  !noel
					//       IndexesForKeyVar(iKey) = MonthlyFieldSetInput(FirstColumn + ColNum - 1)%IndexesForKeyVar(iKey) !noel
					//    ENDDO
					//#else
					//    curVariMeter = MakeUPPERCase(MonthlyFieldSetInput(FirstColumn + ColNum - 1)%variMeter)
					//    ! call the key count function but only need count during this pass
					//    CALL GetVariableKeyCountandType(curVariMeter,KeyCount,TypeVar,AvgSumVar,StepTypeVar,UnitsVar)
					//    ALLOCATE(NamesOfKeys(KeyCount))
					//    ALLOCATE(IndexesForKeyVar(KeyCount))
					//    CALL GetVariableKeys(curVariMeter,TypeVar,NamesOfKeys,IndexesForKeyVar)
					//#endif

					if ( KeyCount == 1 ) { // first test if KeyCount is one to avoid referencing a zero element array
						if ( SameString( MonthlyFieldSetInput( FirstColumn + colNum - 1 ).NamesOfKeys( 1 ), "ENVIRONMENT" ) ) {
							environmentKeyFound = true;
						}
					}
					// if this is an environment variable - don't bother searching
					if ( environmentKeyFound ) {
						found = 1; //only one instance of environment variables so use it.
					} else {
						// search through the keys for the currently active key "UniqueKeyNames(kUniqueKey)"
						found = 0;
						for ( iKey = 1; iKey <= KeyCount; ++iKey ) {
							if ( SameString( MonthlyFieldSetInput( FirstColumn + colNum - 1 ).NamesOfKeys( iKey ), UniqueKeyNames( kUniqueKey ) ) ) {
								found = iKey;
								break;
							}
						}
					}
					if ( ( found > 0 ) && ( KeyCount >= 1 ) ) {
						MonthlyColumns( mColumn ).varName = curVariMeter;
						MonthlyColumns( mColumn ).varNum = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).IndexesForKeyVar( found );
						MonthlyColumns( mColumn ).typeOfVar = TypeVar;
						MonthlyColumns( mColumn ).avgSum = AvgSumVar;
						MonthlyColumns( mColumn ).stepType = StepTypeVar;
						MonthlyColumns( mColumn ).units = UnitsVar;
						MonthlyColumns( mColumn ).aggType = MonthlyFieldSetInput( FirstColumn + colNum - 1 ).aggregate;
						// set accumulator values to default as appropriate for aggregation type
						{ auto const SELECT_CASE_var( MonthlyColumns( mColumn ).aggType );
						if ( SELECT_CASE_var == aggTypeSumOrAvg ) {
							MonthlyColumns( mColumn ).reslt = 0.0;
							MonthlyColumns( mColumn ).duration = 0.0;
						} else if ( SELECT_CASE_var == aggTypeMaximum ) {
							MonthlyColumns( mColumn ).reslt = -huge( BigNum );
							MonthlyColumns( mColumn ).timeStamp = 0;
						} else if ( SELECT_CASE_var == aggTypeMinimum ) {
							MonthlyColumns( mColumn ).reslt = huge( BigNum );
							MonthlyColumns( mColumn ).timeStamp = 0;
						} else if ( SELECT_CASE_var == aggTypeValueWhenMaxMin ) {
							MonthlyColumns( mColumn ).reslt = 0.0;
						} else if ( SELECT_CASE_var == aggTypeHoursZero ) {
							MonthlyColumns( mColumn ).reslt = 0.0;
						} else if ( SELECT_CASE_var == aggTypeHoursNonZero ) {
							MonthlyColumns( mColumn ).reslt = 0.0;
						} else if ( SELECT_CASE_var == aggTypeHoursPositive ) {
							MonthlyColumns( mColumn ).reslt = 0.0;
						} else if ( SELECT_CASE_var == aggTypeHoursNonPositive ) {
							MonthlyColumns( mColumn ).reslt = 0.0;
						} else if ( SELECT_CASE_var == aggTypeHoursNegative ) {
							MonthlyColumns( mColumn ).reslt = 0.0;
						} else if ( SELECT_CASE_var == aggTypeHoursNonNegative ) {
							MonthlyColumns( mColumn ).reslt = 0.0;
						} else if ( SELECT_CASE_var == aggTypeSumOrAverageHoursShown ) {
							MonthlyColumns( mColumn ).reslt = 0.0;
							MonthlyColumns( mColumn ).duration = 0.0;
						} else if ( SELECT_CASE_var == aggTypeMaximumDuringHoursShown ) {
							MonthlyColumns( mColumn ).reslt = -huge( BigNum );
							MonthlyColumns( mColumn ).timeStamp = 0;
						} else if ( SELECT_CASE_var == aggTypeMinimumDuringHoursShown ) {
							MonthlyColumns( mColumn ).reslt = huge( BigNum );
							MonthlyColumns( mColumn ).timeStamp = 0;
						}}
					} else { //if no key corresponds to this instance of the report
						++ErrCount2;
						if ( ErrCount2 == 1 && ! DisplayExtraWarnings && ! VarWarning && KindOfSim == ksRunPeriodWeather ) {
							ShowWarningError( "Processing Monthly Tabular Reports: Variable names not valid for this simulation" );
							ShowContinueError( "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual variables." );
						}
						//fixing CR5878 removed the showing of the warning once about a specific variable.
						if ( DisplayExtraWarnings && KindOfSim == ksRunPeriodWeather ) {
							ShowWarningError( "Processing Monthly Tabular Reports: " + MonthlyInput( TabNum ).name );
							ShowContinueError( "..Variable name=" + curVariMeter + " not valid for this simulation." );
							ShowContinueError( "..i.e., Variable name=" + UniqueKeyNames( kUniqueKey ) + ':' + curVariMeter + " not valid for this simulation." );
							if ( VarWarning ) {
								ShowContinueError( "..Variables not valid for this simulation will have \"[Invalid/Undefined]\" in the Units Column of the Table Report." );
								VarWarning = false;
							}
						}
						MonthlyColumns( mColumn ).varName = curVariMeter;
						MonthlyColumns( mColumn ).varNum = 0;
						MonthlyColumns( mColumn ).typeOfVar = 0;
						MonthlyColumns( mColumn ).avgSum = 0;
						MonthlyColumns( mColumn ).stepType = 0;
						MonthlyColumns( mColumn ).units = "Invalid/Undefined";
						MonthlyColumns( mColumn ).aggType = aggTypeSumOrAvg;
					}
					//#ifdef ITM_KEYCACHE
					//#else
					//    DEALLOCATE(NamesOfKeys)
					//    DEALLOCATE(IndexesForKeyVar)
					//#endif
				} //ColNum
			} //kUniqueKey
		} //TabNum the end of the loop through the inputs objects

		//#ifdef ITM_KEYCACHE
		//DEALLOCATE(NamesOfKeys)
		//DEALLOCATE(IndexesForKeyVar)
		//#endif
	}

	void
	GetInputTabularTimeBins()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   This routine initializes the data structures based
		//   on input from in the IDF file.  The data structures
		//   follow the IDD closely.

		// METHODOLOGY EMPLOYED:
		//   Uses get input structure similar to other objects

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using ScheduleManager::GetScheduleIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurrentModuleObject( "Output:Table:TimeBins" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iInObj; // index when cycling through each idf input object
		int NumParams; // Number of elements combined
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		Array1D_string AlphArray; // character string data
		Array1D< Real64 > NumArray; // numeric data
		int IOStat; // IO Status when calling get input subroutine
		int iTable;
		int firstReport;
		int repIndex;
		int found;
		Real64 const bigVal( 0.0 ); // used with HUGE: Value doesn't matter, only type: Initialize so compiler doesn't warn about use uninitialized

		Array1D_string objNames;
		Array1D_int objVarIDs;

		GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNums );
		AlphArray.allocate( NumAlphas );
		NumArray.dimension( NumNums, 0.0 );

		timeInYear = 0.0; //intialize the time in year counter
		// determine size of array that holds the IDF description
		OutputTableBinnedCount = GetNumObjectsFound( CurrentModuleObject );
		OutputTableBinned.allocate( OutputTableBinnedCount );
		if ( OutputTableBinnedCount > 0 ) {
			WriteTabularFiles = true;
			// if not a run period using weather do not create reports
			if ( ! DoWeathSim ) {
				ShowWarningError( CurrentModuleObject + " requested with SimulationControl Run Simulation for Weather File Run Periods set to No so " + CurrentModuleObject + " will not be generated" );
				return;
			}
		}
		// looking for maximum number of intervals for sizing
		BinResultsIntervalCount = 0;
		BinResultsTableCount = 0;
		for ( iInObj = 1; iInObj <= OutputTableBinnedCount; ++iInObj ) {
			GetObjectItem( CurrentModuleObject, iInObj, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			OutputTableBinned( iInObj ).keyValue = AlphArray( 1 );
			OutputTableBinned( iInObj ).varOrMeter = AlphArray( 2 );
			//if a schedule has been specified assign
			if ( len( AlphArray( 3 ) ) > 0 ) {
				OutputTableBinned( iInObj ).ScheduleName = AlphArray( 3 );
				OutputTableBinned( iInObj ).scheduleIndex = GetScheduleIndex( AlphArray( 3 ) );
				if ( OutputTableBinned( iInObj ).scheduleIndex == 0 ) {
					ShowWarningError( CurrentModuleObject + ": invalid " + cAlphaFieldNames( 3 ) + "=\"" + AlphArray( 3 ) + "\" - not found." );
				}
			} else {
				OutputTableBinned( iInObj ).scheduleIndex = 0; //flag value for no schedule used
			}
			//validate the kind of variable - not used internally except for validation
			if ( len( AlphArray( 4 ) ) > 0 ) {
				if ( ! ( SameString( AlphArray( 4 ), "ENERGY" ) || SameString( AlphArray( 4 ), "DEMAND" ) || SameString( AlphArray( 4 ), "TEMPERATURE" ) || SameString( AlphArray( 4 ), "FLOWRATE" ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + AlphArray( 1 ) + " the Variable Type was not energy, demand, temperature, or flowrate." );
				}
			}
			OutputTableBinned( iInObj ).intervalStart = NumArray( 1 );
			OutputTableBinned( iInObj ).intervalSize = NumArray( 2 );
			OutputTableBinned( iInObj ).intervalCount = int( NumArray( 3 ) );
			// valid range checking on inputs
			if ( OutputTableBinned( iInObj ).intervalCount < 1 ) {
				OutputTableBinned( iInObj ).intervalCount = 1;
			}
			if ( OutputTableBinned( iInObj ).intervalCount > 20 ) {
				OutputTableBinned( iInObj ).intervalCount = 20;
			}
			if ( OutputTableBinned( iInObj ).intervalSize < 0 ) {
				OutputTableBinned( iInObj ).intervalSize = 1000.0;
			}
			OutputTableBinned( iInObj ).resIndex = BinResultsTableCount + 1; //the next results report
			// find maximum number of intervals
			if ( OutputTableBinned( iInObj ).intervalCount > BinResultsIntervalCount ) {
				BinResultsIntervalCount = OutputTableBinned( iInObj ).intervalCount;
			}
			GetVariableKeyCountandType( OutputTableBinned( iInObj ).varOrMeter, OutputTableBinned( iInObj ).numTables, OutputTableBinned( iInObj ).typeOfVar, OutputTableBinned( iInObj ).avgSum, OutputTableBinned( iInObj ).stepType, OutputTableBinned( iInObj ).units );
			if ( OutputTableBinned( iInObj ).typeOfVar == 0 ) {
				ShowWarningError( CurrentModuleObject + ": User specified meter or variable not found: " + OutputTableBinned( iInObj ).varOrMeter );
			}
			// If only a single table key is requested than only one should be counted
			// later will reset the numTables array pointer but for now use it to know
			// how many items to scan through
			if ( OutputTableBinned( iInObj ).keyValue == "*" ) {
				BinResultsTableCount += OutputTableBinned( iInObj ).numTables;
			} else {
				++BinResultsTableCount; //if a particular key is requested then only one more report
			}
		}
		// size the arrays that holds the bin results
		BinResults.allocate( BinResultsIntervalCount, BinResultsTableCount );
		BinResultsBelow.allocate( BinResultsTableCount );
		BinResultsAbove.allocate( BinResultsTableCount );
		BinStatistics.allocate( BinResultsTableCount );
		BinObjVarID.allocate( BinResultsTableCount );
		// now that the arrays are sized go back and fill in
		// what ID numbers are used for each table
		for ( iInObj = 1; iInObj <= OutputTableBinnedCount; ++iInObj ) {
			firstReport = OutputTableBinned( iInObj ).resIndex;
			// allocate the arrays to the number of objects
			objNames.allocate( OutputTableBinned( iInObj ).numTables );
			objVarIDs.allocate( OutputTableBinned( iInObj ).numTables );
			GetVariableKeys( OutputTableBinned( iInObj ).varOrMeter, OutputTableBinned( iInObj ).typeOfVar, objNames, objVarIDs );
			if ( OutputTableBinned( iInObj ).keyValue == "*" ) {
				for ( iTable = 1; iTable <= OutputTableBinned( iInObj ).numTables; ++iTable ) {
					repIndex = firstReport + ( iTable - 1 );
					BinObjVarID( repIndex ).namesOfObj = objNames( iTable );
					BinObjVarID( repIndex ).varMeterNum = objVarIDs( iTable );
					// check if valid meter or number
					if ( objVarIDs( iTable ) == 0 ) {
						ShowWarningError( CurrentModuleObject + ": Specified variable or meter not found: " + objNames( iTable ) );
					}
				}
			} else {
				// scan through the keys and look for the user specified key
				found = 0;
				for ( iTable = 1; iTable <= OutputTableBinned( iInObj ).numTables; ++iTable ) {
					if ( SameString( objNames( iTable ), OutputTableBinned( iInObj ).keyValue ) ) {
						found = iTable;
						break;
					}
				}
				// the first and only report is assigned to the found object name
				if ( !warningAboutKeyNotFound( found, iInObj, CurrentModuleObject ) ) {
					BinObjVarID( firstReport ).namesOfObj = objNames( found );
					BinObjVarID( firstReport ).varMeterNum = objVarIDs( found );
				}
				// reset the number of tables to one
				OutputTableBinned( iInObj ).numTables = 1;
			}
		}
		// clear the binning arrays to zeros
		for ( auto & e : BinResults ) {
			e.mnth = 0.0;
			e.hrly = 0.0;
		}
		for ( auto & e : BinResultsBelow ) {
			e.mnth = 0.0;
			e.hrly = 0.0;
		}
		for ( auto & e : BinResultsAbove ) {
			e.mnth = 0.0;
			e.hrly = 0.0;
		}

		// initialize statistics counters
		for ( auto & e : BinStatistics ) {
			e.minimum = huge( bigVal );
			e.maximum = -huge( bigVal );
			e.n = 0;
			e.sum = 0.0;
			e.sum2 = 0.0;
		}
	}

	bool
	warningAboutKeyNotFound( int foundIndex, int inObjIndex, std::string const & moduleName )
	{
		if ( foundIndex == 0 ) {
			ShowWarningError( moduleName + ": Specified key not found: " + OutputTableBinned( inObjIndex ).keyValue + " for variable: " + OutputTableBinned( inObjIndex ).varOrMeter );
			return true;
		} else {
			return false;
		}

	}

	void
	GetInputTabularStyle()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   This routine set a flag for the output format for
		//   all tabular reports. This is a "unique" object.

		// METHODOLOGY EMPLOYED:
		//   Uses get input structure similar to other objects

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using DataStringGlobals::CharComma;
		using DataStringGlobals::CharTab;
		using DataStringGlobals::CharSpace;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurrentModuleObject( "OutputControl:Table:Style" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumTabularStyle;
		int NumParams; // Number of elements combined
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		Array1D_string AlphArray; // character string data
		Array1D< Real64 > NumArray; // numeric data
		int IOStat; // IO Status when calling get input subroutine

		GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNums );
		AlphArray.allocate( NumAlphas );
		NumArray.dimension( NumNums, 0.0 );

		NumTabularStyle = GetNumObjectsFound( CurrentModuleObject );

		if ( NumTabularStyle == 0 ) {
			AlphArray( 1 ) = "COMMA";
			numStyles = 1;
			TableStyle( 1 ) = tableStyleComma;
			del( 1 ) = CharComma; //comma
			unitsStyle = unitsStyleNone;
		} else if ( NumTabularStyle == 1 ) {
			GetObjectItem( CurrentModuleObject, 1, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			// ColumnSeparator
			if ( SameString( AlphArray( 1 ), "Comma" ) ) {
				numStyles = 1;
				TableStyle( 1 ) = tableStyleComma;
				del( 1 ) = CharComma; //comma
			} else if ( SameString( AlphArray( 1 ), "Tab" ) ) {
				numStyles = 1;
				TableStyle( 1 ) = tableStyleTab;
				del( 1 ) = CharTab; //tab
			} else if ( SameString( AlphArray( 1 ), "Fixed" ) ) {
				numStyles = 1;
				TableStyle( 1 ) = tableStyleFixed;
				del( 1 ) = CharSpace; // space
			} else if ( SameString( AlphArray( 1 ), "HTML" ) ) {
				numStyles = 1;
				TableStyle( 1 ) = tableStyleHTML;
				del( 1 ) = CharSpace; //space - this is not used much for HTML output
			} else if ( SameString( AlphArray( 1 ), "XML" ) ) {
				numStyles = 1;
				TableStyle( 1 ) = tableStyleXML;
				del( 1 ) = CharSpace; //space - this is not used much for XML output
			} else if ( SameString( AlphArray( 1 ), "CommaAndHTML" ) ) {
				numStyles = 2;
				TableStyle( 1 ) = tableStyleComma;
				del( 1 ) = CharComma; //comma
				TableStyle( 2 ) = tableStyleHTML;
				del( 2 ) = CharSpace; //space - this is not used much for HTML output
			} else if ( SameString( AlphArray( 1 ), "CommaAndXML" ) ) {
				numStyles = 2;
				TableStyle( 1 ) = tableStyleComma;
				del( 1 ) = CharComma; //comma
				TableStyle( 2 ) = tableStyleXML;
				del( 2 ) = CharSpace; //space - this is not used much for XML output
			} else if ( SameString( AlphArray( 1 ), "TabAndHTML" ) ) {
				numStyles = 2;
				TableStyle( 1 ) = tableStyleTab;
				del( 1 ) = CharTab; //tab
				TableStyle( 2 ) = tableStyleHTML;
				del( 2 ) = CharSpace; //space - this is not used much for HTML output
			} else if ( SameString( AlphArray( 1 ), "XMLandHTML" ) ) {
				numStyles = 2;
				TableStyle( 1 ) = tableStyleXML;
				del( 1 ) = CharSpace; //space - this is not used much for XML output
				TableStyle( 2 ) = tableStyleHTML;
				del( 2 ) = CharSpace; //space - this is not used much for HTML output
			} else if ( SameString( AlphArray( 1 ), "All" ) ) {
				numStyles = 5;
				TableStyle( 1 ) = tableStyleComma;
				del( 1 ) = CharComma; //comma
				TableStyle( 2 ) = tableStyleTab;
				del( 2 ) = CharTab; //tab
				TableStyle( 3 ) = tableStyleFixed;
				del( 3 ) = CharSpace; // space
				TableStyle( 4 ) = tableStyleHTML;
				del( 4 ) = CharSpace; //space - this is not used much for HTML output
				TableStyle( 5 ) = tableStyleXML;
				del( 5 ) = CharSpace; //space - this is not used much for XML output
			} else {
				ShowWarningError( CurrentModuleObject + ": Invalid " + cAlphaFieldNames( 1 ) + "=\"" + AlphArray( 1 ) + "\". Commas will be used." );
				numStyles = 1;
				TableStyle( 1 ) = tableStyleComma;
				del( 1 ) = CharComma; //comma
				AlphArray( 1 ) = "COMMA";
			}
			//MonthlyUnitConversion
			if ( NumAlphas >= 2 ) {
				unitsStyle = SetUnitsStyleFromString( AlphArray( 2 ) );
				if (unitsStyle == unitsStyleNotFound) {
					ShowWarningError( CurrentModuleObject + ": Invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphArray( 2 ) + "\". No unit conversion will be performed. Normal SI units will be shown." );
				}
			} else {
				unitsStyle = unitsStyleNone;
				AlphArray( 2 ) = "None";
			}
		} else if ( NumTabularStyle > 1 ) {
			ShowWarningError( CurrentModuleObject + ": Only one instance of this object is allowed. Commas will be used." );
			TableStyle = tableStyleComma;
			del = std::string( 1, CharComma ); //comma
			AlphArray( 1 ) = "COMMA";
			unitsStyle = unitsStyleNone;
			AlphArray( 2 ) = "None";
		}

		if ( WriteTabularFiles ) {
			gio::write( OutputFileInits, fmtA ) << "! <Tabular Report>,Style,Unit Conversion";
			if ( AlphArray( 1 ) != "HTML" ) {
				ConvertCaseToLower( AlphArray( 1 ), AlphArray( 2 ) );
				AlphArray( 1 ).erase( 1 );
				AlphArray( 1 ) += AlphArray( 2 ).substr( 1 );
			}
			gio::write( OutputFileInits, "('Tabular Report,',A,',',A)" ) << AlphArray( 1 ) << AlphArray( 2 );
		}

	}

	int
	SetUnitsStyleFromString( std::string const & unitStringIn )
	{
		int unitsStyleReturn;
		if ( SameString( unitStringIn, "None" ) ) {
			unitsStyleReturn = unitsStyleNone;
		} else if ( SameString( unitStringIn, "JTOKWH" ) ) {
			unitsStyleReturn = unitsStyleJtoKWH;
		} else if ( SameString( unitStringIn, "JTOMJ" ) ) {
			unitsStyleReturn = unitsStyleJtoMJ;
		} else if ( SameString( unitStringIn, "JTOGJ" ) ) {
			unitsStyleReturn = unitsStyleJtoGJ;
		} else if ( SameString( unitStringIn, "INCHPOUND" ) ) {
			unitsStyleReturn = unitsStyleInchPound;
		} else {
			unitsStyleReturn = unitsStyleNotFound;
		}
		return unitsStyleReturn;
	}

	void
	GetInputTabularPredefined()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   November 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   This routine flags if any of the predefined reports
		//   are requested by the user

		// METHODOLOGY EMPLOYED:
		//   Uses get input structure similar to other objects

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using OutputProcessor::EndUseCategory;
		using OutputProcessor::MaxNumSubcategories;
		using DataStringGlobals::CharComma;
		using DataStringGlobals::CharTab;
		using DataStringGlobals::CharSpace;
		using OutputReportPredefined::reportName;
		using OutputReportPredefined::numReportName;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurrentModuleObject( "Output:Table:SummaryReports" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int NumTabularPredefined;
		int NumParams;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		Array1D_string AlphArray;
		Array1D< Real64 > NumArray;
		int IOStat; // IO Status when calling get input subroutine
		int iReport;
		std::string meterName;
		int meterNumber;
		int iResource;
		int jEndUse;
		int kEndUseSub;
		int jReport;
		bool nameFound;
		bool ErrorsFound;

		ErrorsFound = false;
		NumTabularPredefined = GetNumObjectsFound( CurrentModuleObject );
		if ( NumTabularPredefined == 1 ) {
			// find out how many fields since the object is extensible
			GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNums );
			// allocate the temporary arrays for the call to get the filed
			AlphArray.allocate( NumAlphas );
			// don't really need the NumArray since not expecting any numbers but the call requires it
			NumArray.dimension( NumNums, 0.0 );
			// get the object
			GetObjectItem( CurrentModuleObject, 1, AlphArray, NumAlphas, NumArray, NumNums, IOStat );
			// default all report flags to false (do not get produced)
			displayTabularBEPS = false;
			// initialize the names of the predefined monthly report titles
			InitializePredefinedMonthlyTitles();
			// loop through the fields looking for matching report titles
			for ( iReport = 1; iReport <= NumAlphas; ++iReport ) {
				nameFound = false;
				if ( SameString( AlphArray( iReport ), "ABUPS" ) ) {
					displayTabularBEPS = true;
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "AnnualBuildingUtilityPerformanceSummary" ) ) {
					displayTabularBEPS = true;
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "BEPS" ) ) {
					displayTabularBEPS = true;
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "ComponentCostEconomicsSummary" ) ) {
					displayTabularCompCosts = true;
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "InputVerificationandResultsSummary" ) ) {
					displayTabularVeriSum = true;
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "IVRS" ) ) {
					displayTabularVeriSum = true;
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "ComponentSizingSummary" ) ) {
					displayComponentSizing = true;
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "CSS" ) ) {
					displayComponentSizing = true;
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "SurfaceShadowingSummary" ) ) {
					displaySurfaceShadowing = true;
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "SHAD" ) ) {
					displaySurfaceShadowing = true;
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "DemandEndUseComponentsSummary" ) ) {
					displayDemandEndUse = true;
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "AdaptiveComfortSummary" ) ) {
					displayAdaptiveComfort = true;
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "SourceEnergyEndUseComponentsSummary" ) ) {
					displaySourceEnergyEndUseSummary = true;
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "ZoneComponentLoadSummary" ) ) {
					displayZoneComponentLoadSummary = true;
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "LEEDSummary" ) ) {
					displayLEEDSummary = true;
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "EnergyMeters" ) ) {
					WriteTabularFiles = true;
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "AllSummary" ) ) {
					WriteTabularFiles = true;
					displayTabularBEPS = true;
					displayTabularVeriSum = true;
					displayTabularCompCosts = true;
					displaySurfaceShadowing = true;
					displayComponentSizing = true;
					displayDemandEndUse = true;
					displayAdaptiveComfort = true;
					displaySourceEnergyEndUseSummary = true;
					nameFound = true;
					for ( jReport = 1; jReport <= numReportName; ++jReport ) {
						reportName( jReport ).show = true;
					}
				} else if ( SameString( AlphArray( iReport ), "AllSummaryAndSizingPeriod" ) ) {
					WriteTabularFiles = true;
					displayTabularBEPS = true;
					displayTabularVeriSum = true;
					displayTabularCompCosts = true;
					displaySurfaceShadowing = true;
					displayComponentSizing = true;
					displayDemandEndUse = true;
					displayAdaptiveComfort = true;
					displaySourceEnergyEndUseSummary = true;
					nameFound = true;
					for ( jReport = 1; jReport <= numReportName; ++jReport ) {
						reportName( jReport ).show = true;
					}
					//the sizing period reports
					displayZoneComponentLoadSummary = true;
				} else if ( SameString( AlphArray( iReport ), "AllMonthly" ) ) {
					WriteTabularFiles = true;
					for ( jReport = 1; jReport <= numNamedMonthly; ++jReport ) {
						namedMonthly( jReport ).show = true;
					}
					nameFound = true;
				} else if ( SameString( AlphArray( iReport ), "AllSummaryAndMonthly" ) ) {
					WriteTabularFiles = true;
					displayTabularBEPS = true;
					displayTabularVeriSum = true;
					displayTabularCompCosts = true;
					displaySurfaceShadowing = true;
					displayComponentSizing = true;
					displayDemandEndUse = true;
					displayAdaptiveComfort = true;
					displaySourceEnergyEndUseSummary = true;
					nameFound = true;
					for ( jReport = 1; jReport <= numReportName; ++jReport ) {
						reportName( jReport ).show = true;
					}
					for ( jReport = 1; jReport <= numNamedMonthly; ++jReport ) {
						namedMonthly( jReport ).show = true;
					}
				} else if ( SameString( AlphArray( iReport ), "AllSummaryMonthlyAndSizingPeriod" ) ) {
					WriteTabularFiles = true;
					displayTabularBEPS = true;
					displayTabularVeriSum = true;
					displayTabularCompCosts = true;
					displaySurfaceShadowing = true;
					displayComponentSizing = true;
					displayDemandEndUse = true;
					displayAdaptiveComfort = true;
					displaySourceEnergyEndUseSummary = true;
					nameFound = true;
					for ( jReport = 1; jReport <= numReportName; ++jReport ) {
						reportName( jReport ).show = true;
					}
					for ( jReport = 1; jReport <= numNamedMonthly; ++jReport ) {
						namedMonthly( jReport ).show = true;
					}
					//the sizing period reports
					displayZoneComponentLoadSummary = true;
				}
				// check the reports that are predefined and are created by OutputReportPredefined
				for ( jReport = 1; jReport <= numReportName; ++jReport ) {
					if ( SameString( AlphArray( iReport ), reportName( jReport ).name ) ) {
						WriteTabularFiles = true;
						reportName( jReport ).show = true;
						nameFound = true;
					}
					if ( SameString( AlphArray( iReport ), reportName( jReport ).abrev ) ) {
						WriteTabularFiles = true;
						reportName( jReport ).show = true;
						nameFound = true;
					}
				}
				// check if the predefined monthly reports are used
				for ( jReport = 1; jReport <= numNamedMonthly; ++jReport ) {
					if ( SameString( AlphArray( iReport ), namedMonthly( jReport ).title ) ) {
						namedMonthly( jReport ).show = true;
						WriteTabularFiles = true;
						nameFound = true;
					}
				}
				if ( ! nameFound ) {
					ShowSevereError( CurrentModuleObject + " Field[" + RoundSigDigits( iReport ) + "]=\"" + AlphArray( iReport ) + "\", invalid report name -- will not be reported." );
					//      ErrorsFound=.TRUE.
				}
			}
			CreatePredefinedMonthlyReports();
		} else if ( NumTabularPredefined > 1 ) {
			ShowSevereError( CurrentModuleObject + ": Only one instance of this object is allowed." );
			ErrorsFound = true;
		}
		if ( ErrorsFound ) {
			ShowFatalError( CurrentModuleObject + ": Preceding errors cause termination." );
		}
		// if the BEPS report has been called for than initialize its arrays
		if ( displayTabularBEPS || displayDemandEndUse || displaySourceEnergyEndUseSummary || displayLEEDSummary ) {
			// initialize the resource type names
			resourceTypeNames( 1 ) = "Electricity";
			resourceTypeNames( 2 ) = "Gas";
			resourceTypeNames( 3 ) = "DistrictCooling";
			resourceTypeNames( 4 ) = "DistrictHeating";
			resourceTypeNames( 5 ) = "Steam";
			resourceTypeNames( 6 ) = "Gasoline";
			resourceTypeNames( 7 ) = "Water";
			resourceTypeNames( 8 ) = "Diesel";
			resourceTypeNames( 9 ) = "Coal";
			resourceTypeNames( 10 ) = "FuelOil#1";
			resourceTypeNames( 11 ) = "FuelOil#2";
			resourceTypeNames( 12 ) = "Propane";
			resourceTypeNames( 13 ) = "OtherFuel1";
			resourceTypeNames( 14 ) = "OtherFuel2";

			sourceTypeNames( 1 ) = "Electric";
			sourceTypeNames( 2 ) = "NaturalGas";
			sourceTypeNames( 3 ) = "Gasoline";
			sourceTypeNames( 4 ) = "Diesel";
			sourceTypeNames( 5 ) = "Coal";
			sourceTypeNames( 6 ) = "FuelOil#1";
			sourceTypeNames( 7 ) = "FuelOil#2";
			sourceTypeNames( 8 ) = "Propane";
			sourceTypeNames( 9 ) = "PurchasedElectric";
			sourceTypeNames( 10 ) = "SoldElectric";
			sourceTypeNames( 11 ) = "OtherFuel1";
			sourceTypeNames( 12 ) = "OtherFuel2";

			// initialize the end use names
			endUseNames( endUseHeating ) = "Heating";
			endUseNames( endUseCooling ) = "Cooling";
			endUseNames( endUseInteriorLights ) = "InteriorLights";
			endUseNames( endUseExteriorLights ) = "ExteriorLights";
			endUseNames( endUseInteriorEquipment ) = "InteriorEquipment";
			endUseNames( endUseExteriorEquipment ) = "ExteriorEquipment";
			endUseNames( endUseFans ) = "Fans";
			endUseNames( endUsePumps ) = "Pumps";
			endUseNames( endUseHeatRejection ) = "HeatRejection";
			endUseNames( endUseHumidification ) = "Humidifier";
			endUseNames( endUseHeatRecovery ) = "HeatRecovery";
			endUseNames( endUseWaterSystem ) = "WaterSystems";
			endUseNames( endUseRefrigeration ) = "Refrigeration";
			endUseNames( endUseCogeneration ) = "Cogeneration";

			// End use subs must be dynamically allocated to accomodate the end use with the most subcategories
			meterNumEndUseSubBEPS.allocate( MaxNumSubcategories, NumEndUses, numResourceTypes );
			meterNumEndUseSubBEPS = 0;

			// loop through all of the resources and end uses and sub end uses for the entire facility
			for ( iResource = 1; iResource <= numResourceTypes; ++iResource ) {
				meterName = resourceTypeNames( iResource ) + ":FACILITY";
				meterNumber = GetMeterIndex( meterName );
				meterNumTotalsBEPS( iResource ) = meterNumber;

				for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
					meterName = endUseNames( jEndUse ) + ':' + resourceTypeNames( iResource ); //// ':FACILITY'
					meterNumber = GetMeterIndex( meterName );
					meterNumEndUseBEPS( iResource, jEndUse ) = meterNumber;

					for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
						meterName = EndUseCategory( jEndUse ).SubcategoryName( kEndUseSub ) + ':' + endUseNames( jEndUse ) + ':' + resourceTypeNames( iResource );
						meterNumber = GetMeterIndex( meterName );
						meterNumEndUseSubBEPS( kEndUseSub, jEndUse, iResource ) = meterNumber;
					}
				}
			}

			for ( iResource = 1; iResource <= numSourceTypes; ++iResource ) {
				meterNumber = GetMeterIndex( sourceTypeNames( iResource ) + "Emissions:Source" );
				meterNumTotalsSource( iResource ) = meterNumber;
			}

			// initialize the gathering arrays to zero
			gatherTotalsBEPS = 0.0;
			gatherTotalsBySourceBEPS = 0.0;
			gatherTotalsSource = 0.0;
			gatherTotalsBySource = 0.0;
			gatherEndUseBEPS = 0.0;
			gatherEndUseBySourceBEPS = 0.0;
			// End use subs must be dynamically allocated to accomodate the end use with the most subcategories
			gatherEndUseSubBEPS.allocate( MaxNumSubcategories, NumEndUses, numResourceTypes );
			gatherEndUseSubBEPS = 0.0;
			gatherDemandEndUseSub.allocate( MaxNumSubcategories, NumEndUses, numResourceTypes );
			gatherDemandEndUseSub = 0.0;

			// get meter numbers for other meters relating to electric load components
			meterNumPowerFuelFireGen = GetMeterIndex( "Cogeneration:ElectricityProduced" );
			meterNumPowerPV = GetMeterIndex( "Photovoltaic:ElectricityProduced" );
			meterNumPowerWind = GetMeterIndex( "WindTurbine:ElectricityProduced" );
			meterNumPowerHTGeothermal = GetMeterIndex( "HTGeothermal:ElectricityProduced" );
			meterNumElecStorage = GetMeterIndex( "ElectricStorage:ElectricityProduced" );
			meterNumPowerConversion = GetMeterIndex( "PowerConversion:ElectricityProduced");
			meterNumElecProduced = GetMeterIndex( "ElectricityProduced:Facility" );
			meterNumElecPurchased = GetMeterIndex( "ElectricityPurchased:Facility" );
			meterNumElecSurplusSold = GetMeterIndex( "ElectricitySurplusSold:Facility" );
			// if no ElectricityPurchased:Facility meter is defined then no electric load center
			// was created by the user and no power generation will occur in the plant. The amount
			// purchased would be the total end use.
			if ( meterNumElecPurchased == 0 ) {
				meterNumElecPurchased = GetMeterIndex( "Electricity:Facility" );
			}

			// initialize the gathering variables for the electric load components
			gatherPowerFuelFireGen = 0.0;
			gatherPowerPV = 0.0;
			gatherPowerWind = 0.0;
			gatherPowerHTGeothermal = 0.0;
			gatherElecProduced = 0.0;
			gatherElecPurchased = 0.0;
			gatherElecSurplusSold = 0.0;
			gatherElecStorage = 0.0;
			gatherPowerConversion = 0.0;

			// get meter numbers for onsite thermal components on BEPS report
			meterNumWaterHeatRecovery = GetMeterIndex( "HeatRecovery:EnergyTransfer" );
			meterNumAirHeatRecoveryCool = GetMeterIndex( "HeatRecoveryForCooling:EnergyTransfer" );
			meterNumAirHeatRecoveryHeat = GetMeterIndex( "HeatRecoveryForHeating:EnergyTransfer" );
			meterNumHeatHTGeothermal = GetMeterIndex( "HTGeothermal:HeatProduced" );
			meterNumHeatSolarWater = GetMeterIndex( "SolarWater:Facility" );
			meterNumHeatSolarAir = GetMeterIndex( "HeatProduced:SolarAir" );
			// initialize the gathering variables for onsite thermal components on BEPS report
			gatherWaterHeatRecovery = 0.0;
			gatherAirHeatRecoveryCool = 0.0;
			gatherAirHeatRecoveryHeat = 0.0;
			gatherHeatHTGeothermal = 0.0;
			gatherHeatSolarWater = 0.0;
			gatherHeatSolarAir = 0.0;

			// get meter numbers for water components on BEPS report
			meterNumRainWater = GetMeterIndex( "Rainwater:OnSiteWater" );
			meterNumCondensate = GetMeterIndex( "Condensate:OnSiteWater" );
			meterNumGroundwater = GetMeterIndex( "Wellwater:OnSiteWater" );
			meterNumMains = GetMeterIndex( "MainsWater:Facility" );
			meterNumWaterEndUseTotal = GetMeterIndex( "Water:Facility" );

			// initialize the gathering variables for water components on BEPS report
			gatherRainWater = 0.0;
			gatherCondensate = 0.0;
			gatherWellwater = 0.0;
			gatherMains = 0.0;
			gatherWaterEndUseTotal = 0.0;

		}
	}

	bool
	isCompLoadRepReq()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   November 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Determine if the ZoneComponentLoadSummary or
		//   ZoneComponentLoadDetail reports are requested.

		// METHODOLOGY EMPLOYED:
		//   Uses get input structure similar to other objects

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool isCompLoadRepReq;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurrentModuleObject( "Output:Table:SummaryReports" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumTabularPredefined;
		int NumParams;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		Array1D_string AlphArray;
		Array1D< Real64 > NumArray;
		int IOStat; // IO Status when calling get input subroutine
		int iReport;
		bool isFound;

		isFound = false;
		NumTabularPredefined = GetNumObjectsFound( CurrentModuleObject );
		if ( NumTabularPredefined == 1 ) {
			// find out how many fields since the object is extensible
			GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNums );
			// allocate the temporary arrays for the call to get the filed
			AlphArray.allocate( NumAlphas );
			// don't really need the NumArray since not expecting any numbers but the call requires it
			NumArray.dimension( NumNums, 0.0 );
			// get the object
			GetObjectItem( CurrentModuleObject, 1, AlphArray, NumAlphas, NumArray, NumNums, IOStat );
			// loop through the fields looking for matching report titles
			for ( iReport = 1; iReport <= NumAlphas; ++iReport ) {
				if ( SameString( AlphArray( iReport ), "ZoneComponentLoadSummary" ) ) {
					isFound = true;
				}
				if ( SameString( AlphArray( iReport ), "AllSummaryAndSizingPeriod" ) ) {
					isFound = true;
				}
				if ( SameString( AlphArray( iReport ), "AllSummaryMonthlyAndSizingPeriod" ) ) {
					isFound = true;
				}
			}
		}
		isCompLoadRepReq = isFound; //return true if either report was found
		return isCompLoadRepReq;
	}

	void
	InitializePredefinedMonthlyTitles()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   September 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Initialize the NamedMonthly array for the titles
		//   of the monthly predefined reports

		// METHODOLOGY EMPLOYED:
		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataOutputs;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int xcount;

		namedMonthly.allocate( numNamedMonthly );
		namedMonthly( 1 ).title = "ZoneCoolingSummaryMonthly";
		namedMonthly( 2 ).title = "ZoneHeatingSummaryMonthly";
		namedMonthly( 3 ).title = "ZoneElectricSummaryMonthly";
		namedMonthly( 4 ).title = "SpaceGainsMonthly";
		namedMonthly( 5 ).title = "PeakSpaceGainsMonthly";
		namedMonthly( 6 ).title = "SpaceGainComponentsAtCoolingPeakMonthly";
		namedMonthly( 7 ).title = "EnergyConsumptionElectricityNaturalGasMonthly";
		namedMonthly( 8 ).title = "EnergyConsumptionElectricityGeneratedPropaneMonthly";
		namedMonthly( 9 ).title = "EnergyConsumptionDieselFuelOilMonthly";
		namedMonthly( 10 ).title = "EnergyConsumptionDistrictHeatingCoolingMonthly";
		namedMonthly( 11 ).title = "EnergyConsumptionCoalGasolineMonthly";
		namedMonthly( 12 ).title = "EnergyConsumptionOtherFuelsMonthly";
		namedMonthly( 13 ).title = "EndUseEnergyConsumptionElectricityMonthly";
		namedMonthly( 14 ).title = "EndUseEnergyConsumptionNaturalGasMonthly";
		namedMonthly( 15 ).title = "EndUseEnergyConsumptionDieselMonthly";
		namedMonthly( 16 ).title = "EndUseEnergyConsumptionFuelOilMonthly";
		namedMonthly( 17 ).title = "EndUseEnergyConsumptionCoalMonthly";
		namedMonthly( 18 ).title = "EndUseEnergyConsumptionPropaneMonthly";
		namedMonthly( 19 ).title = "EndUseEnergyConsumptionGasolineMonthly";
		namedMonthly( 20 ).title = "EndUseEnergyConsumptionOtherFuelsMonthly";
		namedMonthly( 21 ).title = "PeakEnergyEndUseElectricityPart1Monthly";
		namedMonthly( 22 ).title = "PeakEnergyEndUseElectricityPart2Monthly";
		namedMonthly( 23 ).title = "ElectricComponentsOfPeakDemandMonthly";
		namedMonthly( 24 ).title = "PeakEnergyEndUseNaturalGasMonthly";
		namedMonthly( 25 ).title = "PeakEnergyEndUseDieselMonthly";
		namedMonthly( 26 ).title = "PeakEnergyEndUseFuelOilMonthly";
		namedMonthly( 27 ).title = "PeakEnergyEndUseCoalMonthly";
		namedMonthly( 28 ).title = "PeakEnergyEndUsePropaneMonthly";
		namedMonthly( 29 ).title = "PeakEnergyEndUseGasolineMonthly";
		namedMonthly( 30 ).title = "PeakEnergyEndUseOtherFuelsMonthly";
		namedMonthly( 31 ).title = "SetpointsNotMetWithTemperaturesMonthly";
		namedMonthly( 32 ).title = "ComfortReportSimple55Monthly";
		namedMonthly( 33 ).title = "UnglazedTranspiredSolarCollectorSummaryMonthly";
		namedMonthly( 34 ).title = "OccupantComfortDataSummaryMonthly";
		namedMonthly( 35 ).title = "ChillerReportMonthly";
		namedMonthly( 36 ).title = "TowerReportMonthly";
		namedMonthly( 37 ).title = "BoilerReportMonthly";
		namedMonthly( 38 ).title = "DXReportMonthly";
		namedMonthly( 39 ).title = "WindowReportMonthly";
		namedMonthly( 40 ).title = "WindowEnergyReportMonthly";
		namedMonthly( 41 ).title = "WindowZoneSummaryMonthly";
		namedMonthly( 42 ).title = "WindowEnergyZoneSummaryMonthly";
		namedMonthly( 43 ).title = "AverageOutdoorConditionsMonthly";
		namedMonthly( 44 ).title = "OutdoorConditionsMaximumDryBulbMonthly";
		namedMonthly( 45 ).title = "OutdoorConditionsMinimumDryBulbMonthly";
		namedMonthly( 46 ).title = "OutdoorConditionsMaximumWetBulbMonthly";
		namedMonthly( 47 ).title = "OutdoorConditionsMaximumDewPointMonthly";
		namedMonthly( 48 ).title = "OutdoorGroundConditionsMonthly";
		namedMonthly( 49 ).title = "WindowACReportMonthly";
		namedMonthly( 50 ).title = "WaterHeaterReportMonthly";
		namedMonthly( 51 ).title = "GeneratorReportMonthly";
		namedMonthly( 52 ).title = "DaylightingReportMonthly";
		namedMonthly( 53 ).title = "CoilReportMonthly";
		namedMonthly( 54 ).title = "PlantLoopDemandReportMonthly";
		namedMonthly( 55 ).title = "FanReportMonthly";
		namedMonthly( 56 ).title = "PumpReportMonthly";
		namedMonthly( 57 ).title = "CondLoopDemandReportMonthly";
		namedMonthly( 58 ).title = "ZoneTemperatureOscillationReportMonthly";
		namedMonthly( 59 ).title = "AirLoopSystemEnergyAndWaterUseMonthly";
		namedMonthly( 60 ).title = "AirLoopSystemComponentLoadsMonthly";
		namedMonthly( 61 ).title = "AirLoopSystemComponentEnergyUseMonthly";
		namedMonthly( 62 ).title = "MechanicalVentilationLoadsMonthly";

		if ( numNamedMonthly != NumMonthlyReports ) {
			ShowFatalError( "InitializePredefinedMonthlyTitles: Number of Monthly Reports in OutputReportTabular=[" + RoundSigDigits( numNamedMonthly ) + "] does not match number in DataOutputs=[" + RoundSigDigits( NumMonthlyReports ) + "]." );
		} else {
			for ( xcount = 1; xcount <= numNamedMonthly; ++xcount ) {
				if ( ! SameString( MonthlyNamedReports( xcount ), namedMonthly( xcount ).title ) ) {
					ShowSevereError( "InitializePredefinedMonthlyTitles: Monthly Report Titles in OutputReportTabular do not match titles in DataOutput." );
					ShowContinueError( "first mismatch at ORT [" + RoundSigDigits( numNamedMonthly ) + "] =\"" + namedMonthly( xcount ).title + "\"." );
					ShowContinueError( "same location in DO =\"" + MonthlyNamedReports( xcount ) + "\"." );
					ShowFatalError( "Preceding condition causes termination." );
				}
			}
		}
	}

	void
	CreatePredefinedMonthlyReports()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   September 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   For any predefined monthly reports that have been
		//   called out, define the individual columns.

		// METHODOLOGY EMPLOYED:
		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int curReport;

		// ----------------------------------------------------------------------------------------
		// If any variable are added to these reports they also need to be added to the
		// AddVariablesForMonthlyReport routine in InputProcessor.
		// ----------------------------------------------------------------------------------------

		if ( namedMonthly( 1 ).show ) {
			curReport = AddMonthlyReport( "ZoneCoolingSummaryMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Zone Air System Sensible Cooling Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Air System Sensible Cooling Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Drybulb Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Wetbulb Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Zone Total Internal Latent Gain Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Total Internal Latent Gain Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Drybulb Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Wetbulb Temperature", "", aggTypeValueWhenMaxMin );
		}
		if ( namedMonthly( 2 ).show ) {
			curReport = AddMonthlyReport( "ZoneHeatingSummaryMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Zone Air System Sensible Heating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Air System Sensible Heating Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Drybulb Temperature", "", aggTypeValueWhenMaxMin );
		}
		if ( namedMonthly( 3 ).show ) {
			curReport = AddMonthlyReport( "ZoneElectricSummaryMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Zone Lights Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Lights Electric Energy", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Zone Electric Equipment Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Electric Equipment Electric Energy", "", aggTypeMaximum );
		}
		if ( namedMonthly( 4 ).show ) {
			curReport = AddMonthlyReport( "SpaceGainsMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Zone People Total Heating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Lights Total Heating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Electric Equipment Total Heating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Gas Equipment Total Heating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Hot Water Equipment Total Heating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Steam Equipment Total Heating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Other Equipment Total Heating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Infiltration Sensible Heat Gain Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Infiltration Sensible Heat Loss Energy", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 5 ).show ) {
			curReport = AddMonthlyReport( "PeakSpaceGainsMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Zone People Total Heating Energy", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Zone Lights Total Heating Energy", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Zone Electric Equipment Total Heating Energy", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Zone Gas Equipment Total Heating Energy", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Zone Hot Water Equipment Total Heating Energy", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Zone Steam Equipment Total Heating Energy", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Zone Other Equipment Total Heating Energy", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Zone Infiltration Sensible Heat Gain Energy", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Zone Infiltration Sensible Heat Loss Energy", "", aggTypeMaximum );
		}
		if ( namedMonthly( 6 ).show ) {
			curReport = AddMonthlyReport( "SpaceGainComponentsAtCoolingPeakMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Zone Air System Sensible Cooling Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Zone People Total Heating Energy", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Zone Lights Total Heating Energy", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Zone Electric Equipment Total Heating Energy", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Zone Gas Equipment Total Heating Energy", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Zone Hot Water Equipment Total Heating Energy", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Zone Steam Equipment Total Heating Energy", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Zone Other Equipment Total Heating Energy", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Zone Infiltration Sensible Heat Gain Energy", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Zone Infiltration Sensible Heat Loss Energy", "", aggTypeValueWhenMaxMin );
		}
		if ( namedMonthly( 7 ).show ) {
			curReport = AddMonthlyReport( "EnergyConsumptionElectricityNaturalGasMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Electricity:Facility", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Electricity:Facility", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Gas:Facility", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Gas:Facility", "", aggTypeMaximum );
		}
		if ( namedMonthly( 8 ).show ) {
			curReport = AddMonthlyReport( "EnergyConsumptionElectricityGeneratedPropaneMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "ElectricityProduced:Facility", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "ElectricityProduced:Facility", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Propane:Facility", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Propane:Facility", "", aggTypeMaximum );
		}
		if ( namedMonthly( 9 ).show ) {
			curReport = AddMonthlyReport( "EnergyConsumptionDieselFuelOilMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Diesel:Facility", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Diesel:Facility", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "FuelOil#1:Facility", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "FuelOil#1:Facility", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "FuelOil#2:Facility", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "FuelOil#2:Facility", "", aggTypeMaximum );
		}
		if ( namedMonthly( 10 ).show ) {
			curReport = AddMonthlyReport( "EnergyConsumptionDistrictHeatingCoolingMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "DistrictCooling:Facility", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "DistrictCooling:Facility", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "DistrictHeating:Facility", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "DistrictHeating:Facility", "", aggTypeMaximum );
		}
		if ( namedMonthly( 11 ).show ) {
			curReport = AddMonthlyReport( "EnergyConsumptionCoalGasolineMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Coal:Facility", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Coal:Facility", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Gasoline:Facility", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Gasoline:Facility", "", aggTypeMaximum );
		}
		if ( namedMonthly( 12 ).show ) {
			curReport = AddMonthlyReport( "EnergyConsumptionOtherFuelsMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "OtherFuel1:Facility", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "OtherFuel1:Facility", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "OtherFuel2:Facility", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "OtherFuel2:Facility", "", aggTypeMaximum );
		}
		if ( namedMonthly( 13 ).show ) {
			curReport = AddMonthlyReport( "EndUseEnergyConsumptionElectricityMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "InteriorLights:Electricity", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "ExteriorLights:Electricity", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "InteriorEquipment:Electricity", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:Electricity", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Fans:Electricity", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Pumps:Electricity", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Heating:Electricity", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling:Electricity", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "HeatRejection:Electricity", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Humidifier:Electricity", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "HeatRecovery:Electricity", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:Electricity", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:Electricity", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 14 ).show ) {
			curReport = AddMonthlyReport( "EndUseEnergyConsumptionNaturalGasMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "InteriorEquipment:Gas", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:Gas", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Heating:Gas", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling:Gas", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:Gas", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:Gas", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Humidifier:Gas", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 15 ).show ) {
			curReport = AddMonthlyReport( "EndUseEnergyConsumptionDieselMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:Diesel", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling:Diesel", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Heating:Diesel", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:Diesel", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:Diesel", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 16 ).show ) {
			curReport = AddMonthlyReport( "EndUseEnergyConsumptionFuelOilMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:FuelOil#1", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling:FuelOil#1", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Heating:FuelOil#1", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:FuelOil#1", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:FuelOil#1", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:FuelOil#2", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling:FuelOil#2", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Heating:FuelOil#2", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:FuelOil#2", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:FuelOil#2", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 17 ).show ) {
			curReport = AddMonthlyReport( "EndUseEnergyConsumptionCoalMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:Coal", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Heating:Coal", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:Coal", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 18 ).show ) {
			curReport = AddMonthlyReport( "EndUseEnergyConsumptionPropaneMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:Propane", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling:Propane", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Heating:Propane", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:Propane", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:Propane", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Humidifier:Propane", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 19 ).show ) {
			curReport = AddMonthlyReport( "EndUseEnergyConsumptionGasolineMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:Gasoline", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling:Gasoline", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Heating:Gasoline", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:Gasoline", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:Gasoline", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 20 ).show ) {
			curReport = AddMonthlyReport( "EndUseEnergyConsumptionOtherFuelsMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:OtherFuel1", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling:OtherFuel1", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Heating:OtherFuel1", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:OtherFuel1", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:OtherFuel1", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:OtherFuel2", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling:OtherFuel2", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Heating:OtherFuel2", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:OtherFuel2", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:OtherFuel2", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 21 ).show ) {
			curReport = AddMonthlyReport( "PeakEnergyEndUseElectricityPart1Monthly", 2 );
			AddMonthlyFieldSetInput( curReport, "InteriorLights:Electricity", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "ExteriorLights:Electricity", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "InteriorEquipment:Electricity", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:Electricity", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Fans:Electricity", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Pumps:Electricity", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Heating:Electricity", "", aggTypeMaximum );
		}
		if ( namedMonthly( 22 ).show ) {
			curReport = AddMonthlyReport( "PeakEnergyEndUseElectricityPart2Monthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Cooling:Electricity", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "HeatRejection:Electricity", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Humidifier:Electricity", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "HeatRecovery:Electricity", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:Electricity", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:Electricity", "", aggTypeMaximum );
		}
		if ( namedMonthly( 23 ).show ) {
			curReport = AddMonthlyReport( "ElectricComponentsOfPeakDemandMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Electricity:Facility", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "InteriorLights:Electricity", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "InteriorEquipment:Electricity", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "ExteriorLights:Electricity", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:Electricity", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Fans:Electricity", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Pumps:Electricity", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Heating:Electricity", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Cooling:Electricity", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "HeatRejection:Electricity", "", aggTypeValueWhenMaxMin );
		}
		if ( namedMonthly( 24 ).show ) {
			curReport = AddMonthlyReport( "PeakEnergyEndUseNaturalGasMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "InteriorEquipment:Gas", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:Gas", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Heating:Gas", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling:Gas", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:Gas", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:Gas", "", aggTypeMaximum );
		}
		if ( namedMonthly( 25 ).show ) {
			curReport = AddMonthlyReport( "PeakEnergyEndUseDieselMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:Diesel", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling:Diesel", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Heating:Diesel", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:Diesel", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:Diesel", "", aggTypeMaximum );
		}
		if ( namedMonthly( 26 ).show ) {
			curReport = AddMonthlyReport( "PeakEnergyEndUseFuelOilMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:FuelOil#1", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling:FuelOil#1", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Heating:FuelOil#1", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:FuelOil#1", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:FuelOil#1", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:FuelOil#2", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling:FuelOil#2", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Heating:FuelOil#2", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:FuelOil#2", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:FuelOil#2", "", aggTypeMaximum );
		}
		if ( namedMonthly( 27 ).show ) {
			curReport = AddMonthlyReport( "PeakEnergyEndUseCoalMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:Coal", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Heating:Coal", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:Coal", "", aggTypeMaximum );
		}
		if ( namedMonthly( 28 ).show ) {
			curReport = AddMonthlyReport( "PeakEnergyEndUsePropaneMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:Propane", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling:Propane", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Heating:Propane", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:Propane", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:Propane", "", aggTypeMaximum );
		}
		if ( namedMonthly( 29 ).show ) {
			curReport = AddMonthlyReport( "PeakEnergyEndUseGasolineMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:Gasoline", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling:Gasoline", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Heating:Gasoline", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:Gasoline", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:Gasoline", "", aggTypeMaximum );
		}
		if ( namedMonthly( 30 ).show ) {
			curReport = AddMonthlyReport( "PeakEnergyEndUseOtherFuelsMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:OtherFuel1", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling:OtherFuel1", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Heating:OtherFuel1", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:OtherFuel1", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:OtherFuel1", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "ExteriorEquipment:OtherFuel2", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling:OtherFuel2", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Heating:OtherFuel2", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "WaterSystems:OtherFuel2", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cogeneration:OtherFuel2", "", aggTypeMaximum );
		}
		if ( namedMonthly( 31 ).show ) {
			curReport = AddMonthlyReport( "SetpointsNotMetWithTemperaturesMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Zone Heating Setpoint Not Met Time", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Zone Mean Air Temperature", "", aggTypeSumOrAverageHoursShown );
			AddMonthlyFieldSetInput( curReport, "Zone Heating Setpoint Not Met While Occupied Time", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Zone Mean Air Temperature", "", aggTypeSumOrAverageHoursShown );
			AddMonthlyFieldSetInput( curReport, "Zone Cooling Setpoint Not Met Time", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Zone Mean Air Temperature", "", aggTypeSumOrAverageHoursShown );
			AddMonthlyFieldSetInput( curReport, "Zone Cooling Setpoint Not Met While Occupied Time", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Zone Mean Air Temperature", "", aggTypeSumOrAverageHoursShown );
		}
		if ( namedMonthly( 32 ).show ) {
			curReport = AddMonthlyReport( "ComfortReportSimple55Monthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Zone Thermal Comfort ASHRAE 55 Simple Model Summer Clothes Not Comfortable Time", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Zone Mean Air Temperature", "", aggTypeSumOrAverageHoursShown );
			AddMonthlyFieldSetInput( curReport, "Zone Thermal Comfort ASHRAE 55 Simple Model Winter Clothes Not Comfortable Time", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Zone Mean Air Temperature", "", aggTypeSumOrAverageHoursShown );
			AddMonthlyFieldSetInput( curReport, "Zone Thermal Comfort ASHRAE 55 Simple Model Summer or Winter Clothes Not Comfortable Time", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Zone Mean Air Temperature", "", aggTypeSumOrAverageHoursShown );
		}
		if ( namedMonthly( 33 ).show ) {
			curReport = AddMonthlyReport( "UnglazedTranspiredSolarCollectorSummaryMonthly", 5 );
			AddMonthlyFieldSetInput( curReport, "Solar Collector System Efficiency", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Solar Collector System Efficiency", "", aggTypeSumOrAverageHoursShown );
			AddMonthlyFieldSetInput( curReport, "Solar Collector Outside Face Suction Velocity", "", aggTypeSumOrAverageHoursShown );
			AddMonthlyFieldSetInput( curReport, "Solar Collector Sensible Heating Rate", "", aggTypeSumOrAverageHoursShown );
		}
		if ( namedMonthly( 34 ).show ) {
			curReport = AddMonthlyReport( "OccupantComfortDataSummaryMonthly", 5 );
			AddMonthlyFieldSetInput( curReport, "People Occupant Count", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "People Air Temperature", "", aggTypeSumOrAverageHoursShown );
			AddMonthlyFieldSetInput( curReport, "People Air Relative Humidity", "", aggTypeSumOrAverageHoursShown );
			AddMonthlyFieldSetInput( curReport, "Zone Thermal Comfort Fanger Model PMV", "", aggTypeSumOrAverageHoursShown );
			AddMonthlyFieldSetInput( curReport, "Zone Thermal Comfort Fanger Model PPD", "", aggTypeSumOrAverageHoursShown );
		}
		if ( namedMonthly( 35 ).show ) {
			curReport = AddMonthlyReport( "ChillerReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Chiller Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Chiller Electric Power", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Chiller Electric Energy", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Chiller Evaporator Cooling Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Chiller Evaporator Cooling Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Chiller Condenser Heat Transfer Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Chiller COP", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Chiller COP", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Chiller Part Load Ratio", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Chiller Part Load Ratio", "", aggTypeMaximum );
		}
		if ( namedMonthly( 36 ).show ) {
			curReport = AddMonthlyReport( "TowerReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Cooling Tower Fan Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling Tower Fan Electric Energy", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Cooling Tower Fan Electric Power", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling Tower Heat Transfer Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling Tower Inlet Temperature", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling Tower Outlet Temperature", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling Tower Mass Flow Rate", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 37 ).show ) {
			curReport = AddMonthlyReport( "BoilerReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Boiler Heating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Boiler Gas Consumption", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Boiler Heating Energy", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Boiler Heating Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Boiler Gas Consumption Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Boiler Inlet Temperature", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Boiler Outlet Temperature", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Boiler Mass Flow Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Boiler Ancillary Electric Power", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Boiler Part Load Ratio", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Boiler Part Load Ratio", "", aggTypeMaximum );
		}
		if ( namedMonthly( 38 ).show ) {
			curReport = AddMonthlyReport( "DXReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Total Cooling Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Total Cooling Energy", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Sensible Cooling Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Latent Cooling Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Crankcase Heater Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Runtime Fraction", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Runtime Fraction", "", aggTypeMinimum );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Total Cooling Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Sensible Cooling Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Latent Cooling Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Electric Power", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Crankcase Heater Electric Power", "", aggTypeMaximum );
		}
		if ( namedMonthly( 39 ).show ) {
			curReport = AddMonthlyReport( "WindowReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Surface Window Transmitted Solar Radiation Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Surface Window Transmitted Beam Solar Radiation Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Surface Window Transmitted Diffuse Solar Radiation Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Surface Window Heat Gain Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Surface Window Heat Loss Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Surface Window Inside Face Glazing Condensation Status", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Surface Shading Device Is On Time Fraction", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Surface Storm Window On Off Status", "", aggTypeHoursNonZero );
		}
		if ( namedMonthly( 40 ).show ) {
			curReport = AddMonthlyReport( "WindowEnergyReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Surface Window Transmitted Solar Radiation Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Surface Window Transmitted Beam Solar Radiation Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Surface Window Transmitted Diffuse Solar Radiation Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Surface Window Heat Gain Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Surface Window Heat Loss Energy", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 41 ).show ) {
			curReport = AddMonthlyReport( "WindowZoneSummaryMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Zone Windows Total Heat Gain Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Windows Total Heat Loss Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Windows Total Transmitted Solar Radiation Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Exterior Windows Total Transmitted Beam Solar Radiation Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Exterior Windows Total Transmitted Diffuse Solar Radiation Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Interior Windows Total Transmitted Diffuse Solar Radiation Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Interior Windows Total Transmitted Beam Solar Radiation Rate", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 42 ).show ) {
			curReport = AddMonthlyReport( "WindowEnergyZoneSummaryMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Zone Windows Total Heat Gain Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Windows Total Heat Loss Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Windows Total Transmitted Solar Radiation Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Exterior Windows Total Transmitted Beam Solar Radiation Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Exterior Windows Total Transmitted Diffuse Solar Radiation Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Interior Windows Total Transmitted Diffuse Solar Radiation Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Interior Windows Total Transmitted Beam Solar Radiation Energy", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 43 ).show ) {
			curReport = AddMonthlyReport( "AverageOutdoorConditionsMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Drybulb Temperature", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Wetbulb Temperature", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Dewpoint Temperature", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Site Wind Speed", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Site Sky Temperature", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Site Diffuse Solar Radiation Rate per Area", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Site Direct Solar Radiation Rate per Area", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Site Rain Status", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 44 ).show ) {
			curReport = AddMonthlyReport( "OutdoorConditionsMaximumDryBulbMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Drybulb Temperature", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Wetbulb Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Dewpoint Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Wind Speed", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Sky Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Diffuse Solar Radiation Rate per Area", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Direct Solar Radiation Rate per Area", "", aggTypeValueWhenMaxMin );
		}
		if ( namedMonthly( 45 ).show ) {
			curReport = AddMonthlyReport( "OutdoorConditionsMinimumDryBulbMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Drybulb Temperature", "", aggTypeMinimum );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Wetbulb Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Dewpoint Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Wind Speed", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Sky Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Diffuse Solar Radiation Rate per Area", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Direct Solar Radiation Rate per Area", "", aggTypeValueWhenMaxMin );
		}
		if ( namedMonthly( 46 ).show ) {
			curReport = AddMonthlyReport( "OutdoorConditionsMaximumWetBulbMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Wetbulb Temperature", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Drybulb Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Dewpoint Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Wind Speed", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Sky Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Diffuse Solar Radiation Rate per Area", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Direct Solar Radiation Rate per Area", "", aggTypeValueWhenMaxMin );
		}
		if ( namedMonthly( 47 ).show ) {
			curReport = AddMonthlyReport( "OutdoorConditionsMaximumDewPointMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Dewpoint Temperature", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Drybulb Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Outdoor Air Wetbulb Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Wind Speed", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Sky Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Diffuse Solar Radiation Rate per Area", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Site Direct Solar Radiation Rate per Area", "", aggTypeValueWhenMaxMin );
		}
		if ( namedMonthly( 48 ).show ) {
			curReport = AddMonthlyReport( "OutdoorGroundConditionsMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Site Ground Temperature", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Site Surface Ground Temperature", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Site Deep Ground Temperature", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Site Mains Water Temperature", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Site Ground Reflected Solar Radiation Rate per Area", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Site Snow on Ground Status", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 49 ).show ) {
			curReport = AddMonthlyReport( "WindowACReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Zone Window Air Conditioner Total Cooling Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Window Air Conditioner Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Window Air Conditioner Total Cooling Energy", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Zone Window Air Conditioner Sensible Cooling Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Window Air Conditioner Latent Cooling Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Window Air Conditioner Total Cooling Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Zone Window Air Conditioner Sensible Cooling Rate", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Zone Window Air Conditioner Latent Cooling Rate", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Zone Window Air Conditioner Electric Power", "", aggTypeValueWhenMaxMin );
		}
		if ( namedMonthly( 50 ).show ) {
			curReport = AddMonthlyReport( "WaterHeaterReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Water Heater Total Demand Heat Transfer Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Water Heater Use Side Heat Transfer Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Water Heater Burner Heating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Water Heater Gas Consumption", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Water Heater Total Demand Heat Transfer Energy", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Water Heater Loss Demand Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Water Heater Heat Loss Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Water Heater Tank Temperature", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Water Heater Heat Recovery Supply Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Water Heater Source Energy", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 51 ).show ) {
			curReport = AddMonthlyReport( "GeneratorReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Generator Produced Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Generator Diesel Consumption", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Generator Gas Consumption", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Generator Produced Electric Energy", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Generator Total Heat Recovery", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Generator Jacket Heat Recovery Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Generator Lube Heat Recovery", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Generator Exhaust Heat Recovery Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Generator Exhaust Air Temperature", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 52 ).show ) {
			curReport = AddMonthlyReport( "DaylightingReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Site Exterior Beam Normal Illuminance", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Daylighting Lighting Power Multiplier", "", aggTypeSumOrAverageHoursShown );
			AddMonthlyFieldSetInput( curReport, "Daylighting Lighting Power Multiplier", "", aggTypeMinimumDuringHoursShown );
			AddMonthlyFieldSetInput( curReport, "Daylighting Reference Point 1 Illuminance", "", aggTypeSumOrAverageHoursShown );
			AddMonthlyFieldSetInput( curReport, "Daylighting Reference Point 1 Glare Index", "", aggTypeSumOrAverageHoursShown );
			AddMonthlyFieldSetInput( curReport, "Daylighting Reference Point 1 Glare Index Setpoint Exceeded Time", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Daylighting Reference Point 1 Daylight Illuminance Setpoint Exceeded Time", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Daylighting Reference Point 2 Illuminance", "", aggTypeSumOrAverageHoursShown );
			AddMonthlyFieldSetInput( curReport, "Daylighting Reference Point 2 Glare Index", "", aggTypeSumOrAverageHoursShown );
			AddMonthlyFieldSetInput( curReport, "Daylighting Reference Point 2 Glare Index Setpoint Exceeded Time", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Daylighting Reference Point 2 Daylight Illuminance Setpoint Exceeded Time", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 53 ).show ) {
			curReport = AddMonthlyReport( "CoilReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Heating Coil Heating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Heating Coil Heating Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Total Cooling Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Sensible Cooling Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Total Cooling Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Sensible Cooling Rate", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Cooling Coil Wetted Area Fraction", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 54 ).show ) {
			curReport = AddMonthlyReport( "PlantLoopDemandReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Plant Supply Side Cooling Demand Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Plant Supply Side Cooling Demand Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Plant Supply Side Heating Demand Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Plant Supply Side Heating Demand Rate", "", aggTypeMaximum );
		}
		if ( namedMonthly( 55 ).show ) {
			curReport = AddMonthlyReport( "FanReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Fan Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Fan Rise in Air Temperature", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Fan Electric Power", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Fan Rise in Air Temperature", "", aggTypeValueWhenMaxMin );
		}
		if ( namedMonthly( 56 ).show ) {
			curReport = AddMonthlyReport( "PumpReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Pump Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Pump Fluid Heat Gain Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Pump Electric Power", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Pump Shaft Power", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Pump Fluid Heat Gain Rate", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Pump Outlet Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Pump Mass Flow Rate", "", aggTypeValueWhenMaxMin );
		}
		if ( namedMonthly( 57 ).show ) {
			curReport = AddMonthlyReport( "CondLoopDemandReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Plant Supply Side Cooling Demand Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Plant Supply Side Cooling Demand Rate", "", aggTypeMaximum );
			AddMonthlyFieldSetInput( curReport, "Plant Supply Side Inlet Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Plant Supply Side Outlet Temperature", "", aggTypeValueWhenMaxMin );
			AddMonthlyFieldSetInput( curReport, "Plant Supply Side Heating Demand Rate", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Plant Supply Side Heating Demand Rate", "", aggTypeMaximum );
		}
		if ( namedMonthly( 58 ).show ) {
			curReport = AddMonthlyReport( "ZoneTemperatureOscillationReportMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Zone Oscillating Temperatures Time", "", aggTypeHoursNonZero );
			AddMonthlyFieldSetInput( curReport, "Zone People Occupant Count", "", aggTypeSumOrAverageHoursShown );
		}
		if ( namedMonthly( 59 ).show ) {
			curReport = AddMonthlyReport( "AirLoopSystemEnergyAndWaterUseMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Air System Hot Water Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Steam Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Chilled Water Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Gas Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Water Volume", "", aggTypeSumOrAvg );
		}

		if ( namedMonthly( 60 ).show ) {
			curReport = AddMonthlyReport( "AirLoopSystemComponentLoadsMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Air System Fan Air Heating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Cooling Coil Total Cooling Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Heating Coil Total Heating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Heat Exchanger Total Heating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Heat Exchanger Total Cooling Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Humidifier Total Heating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Evaporative Cooler Total Cooling Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Desiccant Dehumidifier Total Cooling Energy", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 61 ).show ) {
			curReport = AddMonthlyReport( "AirLoopSystemComponentEnergyUseMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Air System Fan Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Heating Coil Hot Water Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Cooling Coil Chilled Water Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System DX Heating Coil Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System DX Cooling Coil Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Heating Coil Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Heating Coil Gas Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Heating Coil Steam Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Humidifier Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Humidifier Gas Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Evaporative Cooler Electric Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Air System Desiccant Dehumidifier Electric Energy", "", aggTypeSumOrAvg );
		}
		if ( namedMonthly( 62 ).show ) {
			curReport = AddMonthlyReport( "MechanicalVentilationLoadsMonthly", 2 );
			AddMonthlyFieldSetInput( curReport, "Zone Mechanical Ventilation No Load Heat Removal Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Mechanical Ventilation Cooling Load Increase Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Mechanical Ventilation Cooling Load Increase Due to Overheating Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Mechanical Ventilation Cooling Load Decrease Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Mechanical Ventilation No Load Heat Addition Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Mechanical Ventilation Heating Load Increase Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Mechanical Ventilation Heating Load Increase Due to Overcooling Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Mechanical Ventilation Heating Load Decrease Energy", "", aggTypeSumOrAvg );
			AddMonthlyFieldSetInput( curReport, "Zone Mechanical Ventilation Air Changes per Hour", "", aggTypeSumOrAvg );
		}
	}

	void
	GetInputFuelAndPollutionFactors()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Read the Fuel Factor inputs by the user to
		//   get the source energy conversion factors
		//   Also reads PolutionCalculationFactors to
		//   get information on district cooling and heating

		// METHODOLOGY EMPLOYED:
		//   Uses get input structure similar to other objects

		// REFERENCES:
		// na

		// Using/Aliasing
		using PollutionModule::GetFuelFactorInfo;
		using PollutionModule::GetEnvironmentalImpactFactorInfo;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 curSourceFactor;
		bool fuelFactorUsed;
		bool fFScheduleUsed;
		int ffScheduleIndex;

		//set the default factors for source energy - they will be overwritten if the user sets any values
		sourceFactorElectric = 3.167;
		sourceFactorNaturalGas = 1.084;
		sourceFactorSteam = 0.3;
		sourceFactorGasoline = 1.05;
		sourceFactorDiesel = 1.05;
		sourceFactorCoal = 1.05;
		sourceFactorFuelOil1 = 1.05;
		sourceFactorFuelOil2 = 1.05;
		sourceFactorPropane = 1.05;
		sourceFactorOtherFuel1 = 1.0;
		sourceFactorOtherFuel2 = 1.0;
		// the following should be kept consistent with the assumptions in the pollution calculation routines
		efficiencyDistrictCooling = 3.0;
		efficiencyDistrictHeating = 0.3;

		//  TotalSourceEnergyUse = (gatherTotalsSource(1) & !total source from electricity
		//                  +  gatherTotalsSource(2)   & !natural gas
		//                  + gatherTotalsSource(3)    & !gasoline
		//                  + gatherTotalsSource(4)    & !diesel
		//                  + gatherTotalsSource(5)    & !coal
		//                  + gatherTotalsSource(6)    & !fuel oil #1
		//                  + gatherTotalsSource(7)    & !fuel oil #2
		//                  + gatherTotalsSource(8)    &  !propane
		//                  + gatherTotalsBEPS(3)*sourceFactorElectric/efficiencyDistrictCooling  & !district cooling
		//                  + gatherTotalsBEPS(4)*sourceFactorNaturalGas/efficiencyDistrictHeating  & !district heating
		//                  + gatherTotalsBEPS(5)*sourceFactorSteam  & !steam
		//                                          ) / largeConversionFactor

		GetFuelFactorInfo( "NaturalGas", fuelFactorUsed, curSourceFactor, fFScheduleUsed, ffScheduleIndex );
		if ( fuelFactorUsed ) {
			sourceFactorNaturalGas = curSourceFactor;
			fuelfactorsused( 2 ) = true;
			ffUsed( 2 ) = true;
		}
		SourceFactors( 2 ) = curSourceFactor;
		if ( fFScheduleUsed ) {
			fuelFactorSchedulesUsed = true;
			ffSchedUsed( 2 ) = true;
			ffSchedIndex( 2 ) = ffScheduleIndex;
		}

		GetFuelFactorInfo( "ResidualOil", fuelFactorUsed, curSourceFactor, fFScheduleUsed, ffScheduleIndex );
		if ( fuelFactorUsed ) {
			sourceFactorFuelOil2 = curSourceFactor;
			fuelfactorsused( 7 ) = true;
			ffUsed( 11 ) = true;
		}
		SourceFactors( 11 ) = curSourceFactor;
		if ( fFScheduleUsed ) {
			fuelFactorSchedulesUsed = true;
			ffSchedUsed( 11 ) = true;
			ffSchedIndex( 11 ) = ffScheduleIndex;
		}

		GetFuelFactorInfo( "DistillateOil", fuelFactorUsed, curSourceFactor, fFScheduleUsed, ffScheduleIndex );
		if ( fuelFactorUsed ) {
			sourceFactorFuelOil1 = curSourceFactor;
			fuelfactorsused( 6 ) = true;
			ffUsed( 10 ) = true;
		}
		SourceFactors( 10 ) = curSourceFactor;
		if ( fFScheduleUsed ) {
			fuelFactorSchedulesUsed = true;
			ffSchedUsed( 10 ) = true;
			ffSchedIndex( 10 ) = ffScheduleIndex;
		}

		GetFuelFactorInfo( "Coal", fuelFactorUsed, curSourceFactor, fFScheduleUsed, ffScheduleIndex );
		if ( fuelFactorUsed ) {
			sourceFactorCoal = curSourceFactor;
			fuelfactorsused( 5 ) = true;
			ffUsed( 9 ) = true;
		}
		SourceFactors( 9 ) = curSourceFactor;
		if ( fFScheduleUsed ) {
			fuelFactorSchedulesUsed = true;
			ffSchedUsed( 9 ) = true;
			ffSchedIndex( 9 ) = ffScheduleIndex;
		}

		GetFuelFactorInfo( "Electricity", fuelFactorUsed, curSourceFactor, fFScheduleUsed, ffScheduleIndex );
		if ( fuelFactorUsed ) {
			sourceFactorElectric = curSourceFactor;
			fuelfactorsused( 1 ) = true;
			ffUsed( 1 ) = true;
		}
		SourceFactors( 1 ) = curSourceFactor;
		if ( fFScheduleUsed ) {
			fuelFactorSchedulesUsed = true;
			ffSchedUsed( 1 ) = true;
			ffSchedIndex( 1 ) = ffScheduleIndex;
		}

		GetFuelFactorInfo( "Gasoline", fuelFactorUsed, curSourceFactor, fFScheduleUsed, ffScheduleIndex );
		if ( fuelFactorUsed ) {
			sourceFactorGasoline = curSourceFactor;
			fuelfactorsused( 3 ) = true;
			ffUsed( 6 ) = true;
		}
		SourceFactors( 6 ) = curSourceFactor;
		if ( fFScheduleUsed ) {
			fuelFactorSchedulesUsed = true;
			ffSchedUsed( 6 ) = true;
			ffSchedIndex( 6 ) = ffScheduleIndex;
		}

		GetFuelFactorInfo( "Propane", fuelFactorUsed, curSourceFactor, fFScheduleUsed, ffScheduleIndex );
		if ( fuelFactorUsed ) {
			sourceFactorPropane = curSourceFactor;
			fuelfactorsused( 8 ) = true;
			ffUsed( 12 ) = true;
		}
		SourceFactors( 12 ) = curSourceFactor;
		if ( fFScheduleUsed ) {
			fuelFactorSchedulesUsed = true;
			ffSchedUsed( 12 ) = true;
			ffSchedIndex( 12 ) = ffScheduleIndex;
		}

		GetFuelFactorInfo( "Diesel", fuelFactorUsed, curSourceFactor, fFScheduleUsed, ffScheduleIndex );
		if ( fuelFactorUsed ) {
			sourceFactorDiesel = curSourceFactor;
			fuelfactorsused( 4 ) = true;
			ffUsed( 8 ) = true;
		}
		SourceFactors( 8 ) = curSourceFactor;
		if ( fFScheduleUsed ) {
			fuelFactorSchedulesUsed = true;
			ffSchedUsed( 8 ) = true;
			ffSchedIndex( 8 ) = ffScheduleIndex;
		}

		GetFuelFactorInfo( "DistrictCooling", fuelFactorUsed, curSourceFactor, fFScheduleUsed, ffScheduleIndex );
		if ( fuelFactorUsed ) {
			ffUsed( 3 ) = true;
		}
		SourceFactors( 3 ) = curSourceFactor;
		if ( fFScheduleUsed ) {
			ffSchedUsed( 3 ) = true;
			ffSchedIndex( 3 ) = ffScheduleIndex;
		}

		GetFuelFactorInfo( "DistrictHeating", fuelFactorUsed, curSourceFactor, fFScheduleUsed, ffScheduleIndex );
		if ( fuelFactorUsed ) {
			ffUsed( 4 ) = true;
		}
		SourceFactors( 4 ) = curSourceFactor;
		if ( fFScheduleUsed ) {
			ffSchedUsed( 4 ) = true;
			ffSchedIndex( 4 ) = ffScheduleIndex;
		}

		GetFuelFactorInfo( "Steam", fuelFactorUsed, curSourceFactor, fFScheduleUsed, ffScheduleIndex );
		if ( fuelFactorUsed ) {
			ffUsed( 5 ) = true;
		}
		SourceFactors( 5 ) = curSourceFactor;
		if ( fFScheduleUsed ) {
			ffSchedUsed( 5 ) = true;
			ffSchedIndex( 5 ) = ffScheduleIndex;
		}

		GetFuelFactorInfo( "OtherFuel1", fuelFactorUsed, curSourceFactor, fFScheduleUsed, ffScheduleIndex );
		if ( fuelFactorUsed ) {
			sourceFactorOtherFuel1 = curSourceFactor;
			fuelfactorsused( 11 ) = true; // should be source number
			ffUsed( 13 ) = true;
		}
		SourceFactors( 13 ) = curSourceFactor;
		if ( fFScheduleUsed ) {
			fuelFactorSchedulesUsed = true;
			ffSchedUsed( 13 ) = true;
			ffSchedIndex( 13 ) = ffScheduleIndex;
		}

		GetFuelFactorInfo( "OtherFuel2", fuelFactorUsed, curSourceFactor, fFScheduleUsed, ffScheduleIndex );
		if ( fuelFactorUsed ) {
			sourceFactorOtherFuel2 = curSourceFactor;
			fuelfactorsused( 12 ) = true; // should be source number
			ffUsed( 14 ) = true;
		}
		SourceFactors( 14 ) = curSourceFactor;
		if ( fFScheduleUsed ) {
			fuelFactorSchedulesUsed = true;
			ffSchedUsed( 14 ) = true;
			ffSchedIndex( 14 ) = ffScheduleIndex;
		}

		GetEnvironmentalImpactFactorInfo( efficiencyDistrictHeating, efficiencyDistrictCooling, sourceFactorSteam );

	}

	//======================================================================================================================
	//======================================================================================================================

	//    OTHER INITIALIZATION ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	OpenOutputTabularFile()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Create a file that holds the output from the tabular reports
		//   the output is in a CSV file if it is comma delimited otherwise
		//   it is in a TXT file.

		// METHODOLOGY EMPLOYED:
		//   Uses get input structure similar to other objects

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataStringGlobals::VerString;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::WeatherFileLocationTitle;
		using DataHeatBalance::BuildingName;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iStyle;
		std::string curDel;

		// get a new file unit number
		// create a file to hold the results
		// Use a CSV file if comma seperated but otherwise use TXT file
		// extension.
		if ( WriteTabularFiles ) {
			for ( iStyle = 1; iStyle <= numStyles; ++iStyle ) {
				std::ofstream & tbl_stream( *TabularOutputFile( iStyle ) );
				curDel = del( iStyle );
				if ( TableStyle( iStyle ) == tableStyleComma ) {
					DisplayString( "Writing tabular output file results using comma format." );
					tbl_stream.open( DataStringGlobals::outputTblCsvFileName );
					if ( ! tbl_stream ) {
						ShowFatalError( "OpenOutputTabularFile: Could not open file \"" + DataStringGlobals::outputTblCsvFileName + "\" for output (write)." );
					}
					tbl_stream << "Program Version:" << curDel << VerString << '\n';
					tbl_stream << "Tabular Output Report in Format: " << curDel << "Comma\n";
					tbl_stream << '\n';
					tbl_stream << "Building:" << curDel << BuildingName << '\n';
					if ( EnvironmentName == WeatherFileLocationTitle ) {
						tbl_stream << "Environment:" << curDel << EnvironmentName << '\n';
					} else {
						tbl_stream << "Environment:" << curDel << EnvironmentName << " ** " << WeatherFileLocationTitle << '\n';
					}
					tbl_stream << '\n';
				} else if ( TableStyle( iStyle ) == tableStyleTab ) {
					DisplayString( "Writing tabular output file results using tab format." );
					tbl_stream.open( DataStringGlobals::outputTblTabFileName );
					if ( ! tbl_stream ) {
						ShowFatalError( "OpenOutputTabularFile: Could not open file \"" + DataStringGlobals::outputTblTabFileName + "\" for output (write)." );
					}
					tbl_stream << "Program Version" << curDel << VerString << '\n';
					tbl_stream << "Tabular Output Report in Format: " << curDel << "Tab\n";
					tbl_stream << '\n';
					tbl_stream << "Building:" << curDel << BuildingName << '\n';
					if ( EnvironmentName == WeatherFileLocationTitle ) {
						tbl_stream << "Environment:" << curDel << EnvironmentName << '\n';
					} else {
						tbl_stream << "Environment:" << curDel << EnvironmentName << " ** " << WeatherFileLocationTitle << '\n';
					}
					tbl_stream << '\n';
				} else if ( TableStyle( iStyle ) == tableStyleHTML ) {
					DisplayString( "Writing tabular output file results using HTML format." );
					tbl_stream.open( DataStringGlobals::outputTblHtmFileName );
					if ( ! tbl_stream ) {
						ShowFatalError( "OpenOutputTabularFile: Could not open file \"" + DataStringGlobals::outputTblHtmFileName + "\" for output (write)." );
					}
					tbl_stream << "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\"http://www.w3.org/TR/html4/loose.dtd\">\n";
					tbl_stream << "<html>\n";
					tbl_stream << "<head>\n";
					if ( EnvironmentName == WeatherFileLocationTitle ) {
						tbl_stream << "<title> " << BuildingName << ' ' << EnvironmentName << '\n';
					} else {
						tbl_stream << "<title> " << BuildingName << ' ' << EnvironmentName << " ** " << WeatherFileLocationTitle << '\n';
					}
					tbl_stream << "  " << std::setw( 4 ) << td( 1 ) << '-' << std::setfill( '0' ) << std::setw( 2 ) << td( 2 ) << '-' << std::setw( 2 ) << td( 3 ) << '\n';
					tbl_stream << "  " << std::setw( 2 ) << td( 5 ) << ':' << std::setw( 2 ) << td( 6 ) << ':' << std::setw( 2 ) << td( 7 ) << std::setfill( ' ' ) << '\n';
					tbl_stream << " - EnergyPlus</title>\n";
					tbl_stream << "</head>\n";
					tbl_stream << "<body>\n";
					tbl_stream << "<p><a href=\"#toc\" style=\"float: right\">Table of Contents</a></p>\n";
					tbl_stream << "<a name=top></a>\n";
					tbl_stream << "<p>Program Version:<b>" << VerString << "</b></p>\n";
					tbl_stream << "<p>Tabular Output Report in Format: <b>HTML</b></p>\n";
					tbl_stream << "<p>Building: <b>" << BuildingName << "</b></p>\n";
					if ( EnvironmentName == WeatherFileLocationTitle ) {
						tbl_stream << "<p>Environment: <b>" << EnvironmentName << "</b></p>\n";
					} else {
						tbl_stream << "<p>Environment: <b>" << EnvironmentName << " ** " << WeatherFileLocationTitle << "</b></p>\n";
					}
					tbl_stream << "<p>Simulation Timestamp: <b>" << std::setw( 4 ) << td( 1 ) << '-' << std::setfill( '0' ) << std::setw( 2 ) << td( 2 ) << '-' << std::setw( 2 ) << td( 3 ) << '\n';
					tbl_stream << "  " << std::setw( 2 ) << td( 5 ) << ':' << std::setw( 2 ) << td( 6 ) << ':' << std::setw( 2 ) << td( 7 ) << std::setfill( ' ' ) << "</b></p>\n";
				} else if ( TableStyle( iStyle ) == tableStyleXML ) {
					DisplayString( "Writing tabular output file results using XML format." );
					tbl_stream.open( DataStringGlobals::outputTblXmlFileName );
					if ( ! tbl_stream ) {
						ShowFatalError( "OpenOutputTabularFile: Could not open file \"" + DataStringGlobals::outputTblXmlFileName + "\" for output (write)." );
					}
					tbl_stream << "<?xml version=\"1.0\"?>\n";
					tbl_stream << "<EnergyPlusTabularReports>\n";
					tbl_stream << "  <BuildingName>" << BuildingName << "</BuildingName>\n";
					tbl_stream << "  <EnvironmentName>" << EnvironmentName << "</EnvironmentName>\n";
					tbl_stream << "  <WeatherFileLocationTitle>" << WeatherFileLocationTitle << "</WeatherFileLocationTitle>\n";
					tbl_stream << "  <ProgramVersion>" << VerString << "</ProgramVersion>\n";
					tbl_stream << "  <SimulationTimestamp>\n";
					tbl_stream << "    <Date>\n";
					tbl_stream << "      " << std::setw( 4 ) << td( 1 ) << '-' << std::setfill( '0' ) << std::setw( 2 ) << td( 2 ) << '-' << std::setw( 2 ) << td( 3 ) << '\n';
					tbl_stream << "    </Date>\n";
					tbl_stream << "    <Time>\n";
					tbl_stream << "      " << std::setw( 2 ) << td( 5 ) << ':' << std::setw( 2 ) << td( 6 ) << ':' << std::setw( 2 ) << td( 7 ) << std::setfill( ' ' ) << '\n';
					tbl_stream << "    </Time>\n";
					tbl_stream << "  </SimulationTimestamp>\n";
					tbl_stream << '\n';
				} else {
					DisplayString( "Writing tabular output file results using text format." );
					tbl_stream.open( DataStringGlobals::outputTblTxtFileName );
					if ( ! tbl_stream ) {
						ShowFatalError( "OpenOutputTabularFile: Could not open file \"" + DataStringGlobals::outputTblTxtFileName + "\" for output (write)." );
					}
					tbl_stream << "Program Version: " << VerString << '\n';
					tbl_stream << "Tabular Output Report in Format: " << curDel << "Fixed\n";
					tbl_stream << '\n';
					tbl_stream << "Building:        " << BuildingName << '\n';
					if ( EnvironmentName == WeatherFileLocationTitle ) {
						tbl_stream << "Environment:     " << EnvironmentName << '\n';
					} else {
						tbl_stream << "Environment:     " << EnvironmentName << " ** " << WeatherFileLocationTitle << '\n';
					}
					tbl_stream << '\n';
				}
			}
		}
	}

	void
	CloseOutputTabularFile()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Create a file that holds the output from the tabular reports
		//   the output is in a CSV file if it is comma delimited otherwise
		//   it is in a TXT file.

		// METHODOLOGY EMPLOYED:
		//   Uses get input structure similar to other objects

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iStyle;

		if ( WriteTabularFiles ) {
			for ( iStyle = 1; iStyle <= numStyles; ++iStyle ) {
				std::ofstream & tbl_stream( *TabularOutputFile( iStyle ) );
				if ( TableStyle( iStyle ) == tableStyleHTML ) { // if HTML file put ending info
					tbl_stream << "</body>\n";
					tbl_stream << "</html>\n";
				} else if ( TableStyle( iStyle ) == tableStyleXML ) {
					if ( ! prevReportName.empty() ) {
						tbl_stream << "</" << prevReportName << ">\n"; //close the last element if it was used.
					}
					tbl_stream << "</EnergyPlusTabularReports>\n";
				}
				tbl_stream.close();
			}
		}
	}

	void
	WriteTableOfContents()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   June 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Creates hyperlinks for table of contents

		// METHODOLOGY EMPLOYED:
		//   Go through the reports and create links

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputReportPredefined::reportName;
		using OutputReportPredefined::numReportName;
		using DataCostEstimate::DoCostEstimate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const Entire_Facility( "Entire Facility" );
		static std::string const Annual_Building_Utility_Performance_Summary( "Annual Building Utility Performance Summary" );
		static std::string const Input_Verification_and_Results_Summary( "Input Verification and Results Summary" );
		static std::string const Demand_End_Use_Components_Summary( "Demand End Use Components Summary" );
		static std::string const Source_Energy_End_Use_Components_Summary( "Source Energy End Use Components Summary" );
		static std::string const Component_Cost_Economics_Summary( "Component Cost Economics Summary" );
		static std::string const Component_Sizing_Summary( "Component Sizing Summary" );
		static std::string const Surface_Shadowing_Summary( "Surface Shadowing Summary" );
		static std::string const Adaptive_Comfort_Summary( "Adaptive Comfort Summary" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iInput;
		int jTable;
		int curTable;
		int iEntry;
		int jEntry;
		int kReport;
		std::string curSection;
		int iStyle;
		std::string origName;
		std::string curName;
		int indexUnitConv;

		for ( iStyle = 1; iStyle <= numStyles; ++iStyle ) {
			if ( TableStyle( iStyle ) == tableStyleHTML ) {
				std::ostream & tbl_stream( *TabularOutputFile( iStyle ) );
				tbl_stream << "<hr>\n";
				tbl_stream << "<a name=toc></a>\n";
				tbl_stream << "<p><b>Table of Contents</b></p>\n";
				tbl_stream << "<a href=\"#top\">Top</a>\n";
				if ( displayTabularBEPS ) {
					tbl_stream << "<br><a href=\"#" << MakeAnchorName( Annual_Building_Utility_Performance_Summary, Entire_Facility ) << "\">Annual Building Utility Performance Summary</a>\n";
				}
				if ( displayTabularVeriSum ) {
					tbl_stream << "<br><a href=\"#" << MakeAnchorName( Input_Verification_and_Results_Summary, Entire_Facility ) << "\">Input Verification and Results Summary</a>\n";
				}
				if ( displayDemandEndUse ) {
					tbl_stream << "<br><a href=\"#" << MakeAnchorName( Demand_End_Use_Components_Summary, Entire_Facility ) << "\">Demand End Use Components Summary</a>\n";
				}
				if ( displaySourceEnergyEndUseSummary ) {
					tbl_stream << "<br><a href=\"#" << MakeAnchorName( Source_Energy_End_Use_Components_Summary, Entire_Facility ) << "\">Source Energy End Use Components Summary</a>\n";
				}
				if ( DoCostEstimate ) {
					tbl_stream << "<br><a href=\"#" << MakeAnchorName( Component_Cost_Economics_Summary, Entire_Facility ) << "\">Component Cost Economics Summary</a>\n";
				}
				if ( displayComponentSizing ) {
					tbl_stream << "<br><a href=\"#" << MakeAnchorName( Component_Sizing_Summary, Entire_Facility ) << "\">Component Sizing Summary</a>\n";
				}
				if ( displaySurfaceShadowing ) {
					tbl_stream << "<br><a href=\"#" << MakeAnchorName( Surface_Shadowing_Summary, Entire_Facility ) << "\">Surface Shadowing Summary</a>\n";
				}
				if ( displayAdaptiveComfort ){
					tbl_stream << "<br><a href=\"#" << MakeAnchorName( Adaptive_Comfort_Summary, Entire_Facility ) << "\">Adaptive Comfort Summary</a>\n";
				}
				for ( kReport = 1; kReport <= numReportName; ++kReport ) {
					if ( reportName( kReport ).show ) {
						tbl_stream << "<br><a href=\"#" << MakeAnchorName( reportName( kReport ).namewithspaces, Entire_Facility ) << "\">" << reportName( kReport ).namewithspaces << "</a>\n";
					}
				}
				if ( DoWeathSim ) {
					for ( iInput = 1; iInput <= MonthlyInputCount; ++iInput ) {
						if ( MonthlyInput( iInput ).numTables > 0 ) {
							tbl_stream << "<p><b>" << MonthlyInput( iInput ).name << "</b></p> |\n";
							for ( jTable = 1; jTable <= MonthlyInput( iInput ).numTables; ++jTable ) {
								curTable = jTable + MonthlyInput( iInput ).firstTable - 1;
								tbl_stream << "<a href=\"#" << MakeAnchorName( MonthlyInput( iInput ).name, MonthlyTables( curTable ).keyValue ) << "\">" << MonthlyTables( curTable ).keyValue << "</a>    |   \n";
							}
						}
					}
					for ( iInput = 1; iInput <= OutputTableBinnedCount; ++iInput ) {
						if ( OutputTableBinned( iInput ).numTables > 0 ) {
							if ( OutputTableBinned( iInput ).scheduleIndex == 0 ) {
								tbl_stream << "<p><b>" << OutputTableBinned( iInput ).varOrMeter << "</b></p> |\n";
							} else {
								tbl_stream << "<p><b>" << OutputTableBinned( iInput ).varOrMeter << " [" << OutputTableBinned( iInput ).ScheduleName << "]</b></p> |\n";
							}
							for ( jTable = 1; jTable <= OutputTableBinned( iInput ).numTables; ++jTable ) {
								curTable = OutputTableBinned( iInput ).resIndex + ( jTable - 1 );
								curName = "";
								if ( unitsStyle == unitsStyleInchPound ) {
									origName = OutputTableBinned( iInput ).varOrMeter + " [" + OutputTableBinned( iInput ).units + ']';
									LookupSItoIP( origName, indexUnitConv, curName );
								} else {
									curName = OutputTableBinned( iInput ).varOrMeter + " [" + OutputTableBinned( iInput ).units + ']';
								}
								if ( OutputTableBinned( iInput ).scheduleIndex == 0 ) {
									tbl_stream << "<a href=\"#" << MakeAnchorName( curName, BinObjVarID( curTable ).namesOfObj ) << "\">" << BinObjVarID( curTable ).namesOfObj << "</a>   |  \n";
								} else {
									tbl_stream << "<a href=\"#" << MakeAnchorName( curName + OutputTableBinned( iInput ).ScheduleName, BinObjVarID( curTable ).namesOfObj ) << "\">" << BinObjVarID( curTable ).namesOfObj << "</a>   |  \n";
								}
							}
						}
					}
					OutputReportTabularAnnual::AddAnnualTableOfContents( tbl_stream );
				}
				//add entries specifically added using AddTOCEntry
				for ( iEntry = 1; iEntry <= TOCEntriesCount; ++iEntry ) {
					if ( ! TOCEntries( iEntry ).isWritten ) {
						curSection = TOCEntries( iEntry ).sectionName;
						tbl_stream << "<p><b>" << curSection << "</b></p> |\n";
						for ( jEntry = iEntry; jEntry <= TOCEntriesCount; ++jEntry ) {
							if ( ! TOCEntries( jEntry ).isWritten ) {
								if ( TOCEntries( jEntry ).sectionName == curSection ) {
									tbl_stream << "<a href=\"#" << MakeAnchorName( TOCEntries( jEntry ).sectionName, TOCEntries( jEntry ).reportName ) << "\">" << TOCEntries( jEntry ).reportName << "</a>   |  \n";
									TOCEntries( jEntry ).isWritten = true;
								}
							}
						}
					}
				}
			}
		}
	}

	//======================================================================================================================
	//======================================================================================================================

	//    GATHER DATA EACH TIME STEP ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	GatherBinResultsForTimestep( int const IndexTypeKey ) // What kind of data to update (Zone, HVAC)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Gathers the data each timesetp and adds the length of the
		//   timestep to the appropriate bin.

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using DataEnvironment::Month;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iInObj;
		int jTable;
		Real64 curValue;
		// values of OutputTableBinned array for current index
		Real64 curIntervalStart;
		Real64 curIntervalSize;
		int curIntervalCount;
		int curResIndex;
		int curNumTables;
		int curTypeOfVar;
		int curScheduleIndex;
		Real64 elapsedTime;
		bool gatherThisTime;
		Real64 topValue;
		int binNum;
		int repIndex;
		int curStepType;

		//REAL(r64), external :: GetInternalVariableValue

		if ( ! DoWeathSim ) return;
		elapsedTime = TimeStepSys;
		timeInYear += elapsedTime;
		for ( iInObj = 1; iInObj <= OutputTableBinnedCount; ++iInObj ) {
			// get values of array for current object being referenced
			curIntervalStart = OutputTableBinned( iInObj ).intervalStart;
			curIntervalSize = OutputTableBinned( iInObj ).intervalSize;
			curIntervalCount = OutputTableBinned( iInObj ).intervalCount;
			curResIndex = OutputTableBinned( iInObj ).resIndex;
			curNumTables = OutputTableBinned( iInObj ).numTables;
			topValue = curIntervalStart + curIntervalSize * curIntervalCount;
			curTypeOfVar = OutputTableBinned( iInObj ).typeOfVar;
			curStepType = OutputTableBinned( iInObj ).stepType;
			curScheduleIndex = OutputTableBinned( iInObj ).scheduleIndex;
			//if a schedule was used, check if it was non-zero value
			if ( curScheduleIndex != 0 ) {
				if ( GetCurrentScheduleValue( curScheduleIndex ) != 0.0 ) {
					gatherThisTime = true;
				} else {
					gatherThisTime = false;
				}
			} else {
				gatherThisTime = true;
			}
			if ( gatherThisTime ) {
				for ( jTable = 1; jTable <= curNumTables; ++jTable ) {
					repIndex = curResIndex + ( jTable - 1 );
					if ( ( ( curStepType == stepTypeZone ) && ( IndexTypeKey == ZoneTSReporting ) ) || ( ( curStepType == stepTypeHVAC ) && ( IndexTypeKey == HVACTSReporting ) ) ) {
						// put actual value from OutputProcesser arrays
						curValue = GetInternalVariableValue( curTypeOfVar, BinObjVarID( repIndex ).varMeterNum );
						// per MJW when a summed variable is used divide it by the length of the time step
						if ( IndexTypeKey == HVACTSReporting ) {
							elapsedTime = TimeStepSys;
						} else {
							elapsedTime = TimeStepZone;
						}
						if ( OutputTableBinned( iInObj ).avgSum == isSum ) { // if it is a summed variable
							curValue /= ( elapsedTime * SecInHour );
						}
						// round the value to the number of signficant digits used in the final output report
						if ( curIntervalSize < 1 ) {
							curValue = round( curValue * 10000.0 ) / 10000.0; // four significant digits
						}
						else if ( curIntervalSize >= 10 ) {
							curValue = round( curValue ); // zero significant digits
						}
						else {
							curValue = round( curValue * 100.0 ) / 100.0; // two significant digits
						}
						// check if the value is above the maximum or below the minimum value
						// first before binning the value within the range.
						if ( curValue < curIntervalStart ) {
							BinResultsBelow( repIndex ).mnth( Month ) += elapsedTime;
							BinResultsBelow( repIndex ).hrly( HourOfDay ) += elapsedTime;
						} else if ( curValue >= topValue ) {
							BinResultsAbove( repIndex ).mnth( Month ) += elapsedTime;
							BinResultsAbove( repIndex ).hrly( HourOfDay ) += elapsedTime;
						} else {
							// determine which bin the results are in
							binNum = int( ( curValue - curIntervalStart ) / curIntervalSize ) + 1;
							BinResults( binNum, repIndex ).mnth( Month ) += elapsedTime;
							BinResults( binNum, repIndex ).hrly( HourOfDay ) += elapsedTime;
						}
						// add to statistics array
						++BinStatistics( repIndex ).n;
						BinStatistics( repIndex ).sum += curValue;
						BinStatistics( repIndex ).sum2 += curValue * curValue;
						if ( curValue < BinStatistics( repIndex ).minimum ) {
							BinStatistics( repIndex ).minimum = curValue;
						}
						if ( curValue > BinStatistics( repIndex ).maximum ) {
							BinStatistics( repIndex ).maximum = curValue;
						}
					}
				}
			}
		}
	}

	void
	GatherMonthlyResultsForTimestep( int const IndexTypeKey ) // What kind of data to update (Zone, HVAC)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   September 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Gathers the data each timestep and updates the arrays
		//   holding the data that will be reported later.

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using DataEnvironment::Month;
		using DataEnvironment::DayOfMonth;
		using General::EncodeMonDayHrMin;
		using General::DetermineMinuteForReporting;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iTable; // loop variable for monthlyTables
		int jColumn; // loop variable for monthlyColumns
		int curCol;
		Real64 curValue;
		int curTypeOfVar;
		int curVarNum;
		Real64 elapsedTime;
		Real64 oldResultValue;
		int oldTimeStamp;
		Real64 oldDuration;
		Real64 newResultValue;
		int newTimeStamp;
		Real64 newDuration;
		int timestepTimeStamp;
		bool activeMinMax;
		//LOGICAL,SAVE  :: activeHoursShown=.FALSE.  !fix by LKL addressing CR6482
		bool activeHoursShown;
		bool activeNewValue;
		int curStepType;
		int minuteCalculated;
		int kOtherColumn; // variable used in loop to scan through additional columns
		int scanColumn;
		Real64 scanValue;
		int scanTypeOfVar;
		int scanVarNum;
		Real64 oldScanValue;
		// local copies of some of the MonthlyColumns array references since
		// profiling showed that they were slow.

		static Array1D_int MonthlyColumnsTypeOfVar;
		static Array1D_int MonthlyColumnsStepType;
		static Array1D_int MonthlyColumnsAggType;
		static Array1D_int MonthlyColumnsVarNum;
		static Array1D_int MonthlyTablesNumColumns;
		static int curFirstColumn( 0 );

		if ( ! DoWeathSim ) return;

		// create temporary arrays to speed processing of these arrays
		if ( GatherMonthlyResultsForTimestepRunOnce ) {
			// MonthlyColumns
			MonthlyColumnsTypeOfVar.allocate( MonthlyColumns.I() );
			MonthlyColumnsStepType.allocate( MonthlyColumns.I() );
			MonthlyColumnsAggType.allocate( MonthlyColumns.I() );
			MonthlyColumnsVarNum.allocate( MonthlyColumns.I() );
			for ( int i = MonthlyColumns.l(), e = MonthlyColumns.u(); i <= e; ++i ) {
				auto const & col( MonthlyColumns( i ) );
				MonthlyColumnsTypeOfVar( i ) = col.typeOfVar;
				MonthlyColumnsStepType( i ) = col.stepType;
				MonthlyColumnsAggType( i ) = col.aggType;
				MonthlyColumnsVarNum( i ) = col.varNum;
			}

			// MonthlyTables
			MonthlyTablesNumColumns.allocate( MonthlyTables.I() );
			for ( int i = MonthlyTables.l(), e = MonthlyTables.u(); i <= e; ++i ) {
				MonthlyTablesNumColumns( i ) = MonthlyTables( i ).numColumns;
			}

			// set flag so this block is only executed once
			GatherMonthlyResultsForTimestepRunOnce = false;
		}

		elapsedTime = TimeStepSys;
		if ( IndexTypeKey == HVACTSReporting ) {
			elapsedTime = TimeStepSys;
		} else {
			elapsedTime = TimeStepZone;
		}
		IsMonthGathered( Month ) = true;
		for ( iTable = 1; iTable <= MonthlyTablesCount; ++iTable ) {
			activeMinMax = false; //at the beginning of the new timestep
			activeHoursShown = false; //fix by JG addressing CR6482
			curFirstColumn = MonthlyTables( iTable ).firstColumn;
			for ( jColumn = 1; jColumn <= MonthlyTablesNumColumns( iTable ); ++jColumn ) {
				curCol = jColumn + curFirstColumn - 1;
				curTypeOfVar = MonthlyColumnsTypeOfVar( curCol );
				curStepType = MonthlyColumnsStepType( curCol );
				if ( ( ( curStepType == stepTypeZone ) && ( IndexTypeKey == ZoneTSReporting ) ) || ( ( curStepType == stepTypeHVAC ) && ( IndexTypeKey == HVACTSReporting ) ) ) {
					//  the above condition used to include the following prior to new scan method
					//  (MonthlyColumns(curCol)%aggType .EQ. aggTypeValueWhenMaxMin)
					curVarNum = MonthlyColumnsVarNum( curCol );
					curValue = GetInternalVariableValue( curTypeOfVar, curVarNum );
					// Get the value from the result array
					oldResultValue = MonthlyColumns( curCol ).reslt( Month );
					oldTimeStamp = MonthlyColumns( curCol ).timeStamp( Month );
					oldDuration = MonthlyColumns( curCol ).duration( Month );
					// Zero the revised values (as default if not set later in SELECT)
					newResultValue = 0.0;
					newTimeStamp = 0;
					newDuration = 0.0;
					activeNewValue = false;
					// the current timestamp
					minuteCalculated = DetermineMinuteForReporting( IndexTypeKey );
					//      minuteCalculated = (CurrentTime - INT(CurrentTime))*60
					//      IF (IndexTypeKey .EQ. stepTypeHVAC) minuteCalculated = minuteCalculated + SysTimeElapsed * 60
					//      minuteCalculated = INT((TimeStep-1) * TimeStepZone * 60) + INT((SysTimeElapsed + TimeStepSys) * 60)
					EncodeMonDayHrMin( timestepTimeStamp, Month, DayOfMonth, HourOfDay, minuteCalculated );
					// perform the selected aggregation type
					// use next lines since it is faster was: SELECT CASE (MonthlyColumns(curCol)%aggType)
					{ auto const SELECT_CASE_var( MonthlyColumnsAggType( curCol ) );
					if ( SELECT_CASE_var == aggTypeSumOrAvg ) {
						if ( MonthlyColumns( curCol ).avgSum == isSum ) { // if it is a summed variable
							newResultValue = oldResultValue + curValue;
						} else {
							newResultValue = oldResultValue + curValue * elapsedTime; //for averaging - weight by elapsed time
						}
						newDuration = oldDuration + elapsedTime;
						activeNewValue = true;
					} else if ( SELECT_CASE_var == aggTypeMaximum ) {
						// per MJW when a summed variable is used divide it by the length of the time step
						if ( MonthlyColumns( curCol ).avgSum == isSum ) { // if it is a summed variable
							if ( IndexTypeKey == HVACTSReporting ) {
								curValue /= ( TimeStepSys * SecInHour );
							} else {
								curValue /= TimeStepZoneSec;
							}
						}
						if ( curValue > oldResultValue ) {
							newResultValue = curValue;
							newTimeStamp = timestepTimeStamp;
							activeMinMax = true;
							activeNewValue = true;
						} else {
							activeMinMax = false; //reset this
						}
					} else if ( SELECT_CASE_var == aggTypeMinimum ) {
						// per MJW when a summed variable is used divide it by the length of the time step
						if ( MonthlyColumns( curCol ).avgSum == isSum ) { // if it is a summed variable
							if ( IndexTypeKey == HVACTSReporting ) {
								curValue /= ( TimeStepSys * SecInHour );
							} else {
								curValue /= TimeStepZoneSec;
							}
						}
						if ( curValue < oldResultValue ) {
							newResultValue = curValue;
							newTimeStamp = timestepTimeStamp;
							activeMinMax = true;
							activeNewValue = true;
						} else {
							activeMinMax = false; //reset this
						}
					} else if ( SELECT_CASE_var == aggTypeHoursZero ) {
						if ( curValue == 0 ) {
							newResultValue = oldResultValue + elapsedTime;
							activeHoursShown = true;
							activeNewValue = true;
						} else {
							activeHoursShown = false;
						}
					} else if ( SELECT_CASE_var == aggTypeHoursNonZero ) {
						if ( curValue != 0 ) {
							newResultValue = oldResultValue + elapsedTime;
							activeHoursShown = true;
							activeNewValue = true;
						} else {
							activeHoursShown = false;
						}
					} else if ( SELECT_CASE_var == aggTypeHoursPositive ) {
						if ( curValue > 0 ) {
							newResultValue = oldResultValue + elapsedTime;
							activeHoursShown = true;
							activeNewValue = true;
						} else {
							activeHoursShown = false;
						}
					} else if ( SELECT_CASE_var == aggTypeHoursNonPositive ) {
						if ( curValue <= 0 ) {
							newResultValue = oldResultValue + elapsedTime;
							activeHoursShown = true;
							activeNewValue = true;
						} else {
							activeHoursShown = false;
						}
					} else if ( SELECT_CASE_var == aggTypeHoursNegative ) {
						if ( curValue < 0 ) {
							newResultValue = oldResultValue + elapsedTime;
							activeHoursShown = true;
							activeNewValue = true;
						} else {
							activeHoursShown = false;
						}
					} else if ( SELECT_CASE_var == aggTypeHoursNonNegative ) {
						if ( curValue >= 0 ) {
							newResultValue = oldResultValue + elapsedTime;
							activeHoursShown = true;
							activeNewValue = true;
						} else {
							activeHoursShown = false;
						}
						// The valueWhenMaxMin is picked up now during the activeMinMax if block below.
						//CASE (aggTypeValueWhenMaxMin)
						//CASE (aggTypeSumOrAverageHoursShown)
						//CASE (aggTypeMaximumDuringHoursShown)
						//CASE (aggTypeMinimumDuringHoursShown)
					}}
					// if the new value has been set then set the monthly values to the
					// new columns. This skips the aggregation types that don't even get
					// triggered now such as valueWhenMinMax and all the agg*HoursShown
					if ( activeNewValue ) {
						MonthlyColumns( curCol ).reslt( Month ) = newResultValue;
						MonthlyColumns( curCol ).timeStamp( Month ) = newTimeStamp;
						MonthlyColumns( curCol ).duration( Month ) = newDuration;
					}
					// if a minimum or maximum value was set this timeStep then
					// scan the remaining columns of the table looking for values
					// that are aggregation type "ValueWhenMaxMin" and set their values
					// if another minimum or maximum column is found then end
					// the scan (it will be taken care of when that column is done)
					if ( activeMinMax ) {
						for ( kOtherColumn = jColumn + 1; kOtherColumn <= MonthlyTables( iTable ).numColumns; ++kOtherColumn ) {
							scanColumn = kOtherColumn + MonthlyTables( iTable ).firstColumn - 1;
							{ auto const SELECT_CASE_var( MonthlyColumns( scanColumn ).aggType );
							if ( ( SELECT_CASE_var == aggTypeMaximum ) || ( SELECT_CASE_var == aggTypeMinimum ) ) {
								// end scanning since these might reset
								break; //do
							} else if ( SELECT_CASE_var == aggTypeValueWhenMaxMin ) {
								// this case is when the value should be set
								scanTypeOfVar = MonthlyColumns( scanColumn ).typeOfVar;
								scanVarNum = MonthlyColumns( scanColumn ).varNum;
								scanValue = GetInternalVariableValue( scanTypeOfVar, scanVarNum );
								// When a summed variable is used divide it by the length of the time step
								if ( MonthlyColumns( scanColumn ).avgSum == isSum ) { // if it is a summed variable
									if ( IndexTypeKey == HVACTSReporting ) {
										scanValue /= ( TimeStepSys * SecInHour );
									} else {
										scanValue /= TimeStepZoneSec;
									}
								}
								MonthlyColumns( scanColumn ).reslt( Month ) = scanValue;
							} else {
								// do nothing
							}}
						}
					}
					// If the hours variable is active then scan through the rest of the variables
					// and accumulate
					if ( activeHoursShown ) {
						for ( kOtherColumn = jColumn + 1; kOtherColumn <= MonthlyTables( iTable ).numColumns; ++kOtherColumn ) {
							scanColumn = kOtherColumn + MonthlyTables( iTable ).firstColumn - 1;
							scanTypeOfVar = MonthlyColumns( scanColumn ).typeOfVar;
							scanVarNum = MonthlyColumns( scanColumn ).varNum;
							scanValue = GetInternalVariableValue( scanTypeOfVar, scanVarNum );
							oldScanValue = MonthlyColumns( scanColumn ).reslt( Month );
							{ auto const SELECT_CASE_var( MonthlyColumns( scanColumn ).aggType );
							if ( ( SELECT_CASE_var == aggTypeHoursZero ) || ( SELECT_CASE_var == aggTypeHoursNonZero ) ) {
								// end scanning since these might reset
								break; //do
							} else if ( ( SELECT_CASE_var == aggTypeHoursPositive ) || ( SELECT_CASE_var == aggTypeHoursNonPositive ) ) {
								// end scanning since these might reset
								break; //do
							} else if ( ( SELECT_CASE_var == aggTypeHoursNegative ) || ( SELECT_CASE_var == aggTypeHoursNonNegative ) ) {
								// end scanning since these might reset
								break; //do
							} else if ( SELECT_CASE_var == aggTypeSumOrAverageHoursShown ) {
								// this case is when the value should be set
								if ( MonthlyColumns( scanColumn ).avgSum == isSum ) { // if it is a summed variable
									MonthlyColumns( scanColumn ).reslt( Month ) = oldScanValue + scanValue;
								} else {
									//for averaging - weight by elapsed time
									MonthlyColumns( scanColumn ).reslt( Month ) = oldScanValue + scanValue * elapsedTime;
								}
								MonthlyColumns( scanColumn ).duration( Month ) += elapsedTime;
							} else if ( SELECT_CASE_var == aggTypeMaximumDuringHoursShown ) {
								if ( MonthlyColumns( scanColumn ).avgSum == isSum ) { // if it is a summed variable
									if ( IndexTypeKey == HVACTSReporting ) {
										scanValue /= ( TimeStepSys * SecInHour );
									} else {
										scanValue /= TimeStepZoneSec;
									}
								}
								if ( scanValue > oldScanValue ) {
									MonthlyColumns( scanColumn ).reslt( Month ) = scanValue;
									MonthlyColumns( scanColumn ).timeStamp( Month ) = timestepTimeStamp;
								}
							} else if ( SELECT_CASE_var == aggTypeMinimumDuringHoursShown ) {
								if ( MonthlyColumns( scanColumn ).avgSum == isSum ) { // if it is a summed variable
									if ( IndexTypeKey == HVACTSReporting ) {
										scanValue /= ( TimeStepSys * SecInHour );
									} else {
										scanValue /= TimeStepZoneSec;
									}
								}
								if ( scanValue < oldScanValue ) {
									MonthlyColumns( scanColumn ).reslt( Month ) = scanValue;
									MonthlyColumns( scanColumn ).timeStamp( Month ) = timestepTimeStamp;
								}
							} else {
								// do nothing
							}}
							activeHoursShown = false; //fixed CR8317
						}
					}
				}
			}
		}
	}

	void
	GatherBEPSResultsForTimestep( int const IndexTypeKey ) // What kind of data to update (Zone, HVAC)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   November 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   This routine gathers data for producing the BEPS report

		// METHODOLOGY EMPLOYED:
		//   Uses get input structure similar to other objects
		//   Meter names are of two forms:
		//         <ResourceType>:<name>
		//   or
		//         <EndUseType>:<ResourceType>
		//   For the purposes of this routine, only the facility <name>
		//   is used.  Remember that 'Building' is actually the sum of
		//   the zones only without system,plant and exterior. The only
		//   way to get them all is to use 'facility'
		//   The <EndUseType> are:
		//          Heating
		//          Cooling
		//          InteriorLights
		//          ExteriorLights
		//          InteriorEquipment
		//          ExteriorEquipment
		//          Fans
		//          Pumps
		//          HeatRejection
		//          Humidifier
		//          HeatRecovery
		//          DHW
		//          Refrigeration
		//          Cogeneration
		//   The <ResourceType> are:
		//          Electricity
		//          Gas
		//          Gasoline
		//          Diesel
		//          Coal
		//          FuelOil#1
		//          FuelOil#2
		//          Propane
		//          Water
		//          Steam
		//          DistrictCooling
		//          DistrictHeating

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputProcessor::EndUseCategory;
		using DataStringGlobals::CharComma;
		using DataStringGlobals::CharTab;
		using DataStringGlobals::CharSpace;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iResource;
		int jEndUse;
		int kEndUseSub;
		Real64 curMeterValue;
		int curMeterNumber;

		// if no beps report is called then skip

		if ( ( displayTabularBEPS || displayLEEDSummary ) && ( IndexTypeKey == stepTypeZone ) ) {
			// add the current time to the total elapsed time
			//FOLLOWING LINE MOVED TO UPDATETABULARREPORTS because used even when beps is not called
			//gatherElapsedTimeBEPS = gatherElapsedTimeBEPS + TimeStepZone
			// loop through all of the resource types for the entire facility
			//  DO iResource = 1, numResourceTypes
			//    curMeterNumber = meterNumTotalsBEPS(iResource)
			//    IF (curMeterNumber .GT. 0) THEN
			//      curMeterValue = GetCurrentMeterValue(curMeterNumber)
			//      gatherTotalsBEPS(iResource) = gatherTotalsBEPS(iResource) + curMeterValue
			//    END IF
			//  END DO

			// loop through all of the resources and end uses for the entire facility
			for ( iResource = 1; iResource <= numResourceTypes; ++iResource ) {
				curMeterNumber = meterNumTotalsBEPS( iResource );
				if ( curMeterNumber > 0 ) {
					curMeterValue = GetCurrentMeterValue( curMeterNumber );
					gatherTotalsBEPS( iResource ) += curMeterValue;
				}

				for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
					curMeterNumber = meterNumEndUseBEPS( iResource, jEndUse );
					if ( curMeterNumber > 0 ) {
						curMeterValue = GetCurrentMeterValue( curMeterNumber );
						gatherEndUseBEPS( iResource, jEndUse ) += curMeterValue;

						for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
							curMeterNumber = meterNumEndUseSubBEPS( kEndUseSub, jEndUse, iResource );
							if ( curMeterNumber > 0 ) {
								curMeterValue = GetCurrentMeterValue( curMeterNumber );
								gatherEndUseSubBEPS( kEndUseSub, jEndUse, iResource ) += curMeterValue;
							}
						}
					}
				}
			}

			for ( iResource = 1; iResource <= numSourceTypes; ++iResource ) {
				curMeterNumber = meterNumTotalsSource( iResource );
				if ( curMeterNumber > 0 ) {
					curMeterValue = GetCurrentMeterValue( curMeterNumber );
					gatherTotalsSource( iResource ) += curMeterValue;
				}
			}

			// gather the electric load components
			gatherPowerFuelFireGen += GetCurrentMeterValue( meterNumPowerFuelFireGen );
			gatherPowerPV += GetCurrentMeterValue( meterNumPowerPV );
			gatherPowerWind += GetCurrentMeterValue( meterNumPowerWind );
			gatherPowerHTGeothermal += GetCurrentMeterValue( meterNumPowerHTGeothermal );
			gatherElecProduced += GetCurrentMeterValue( meterNumElecProduced );
			gatherElecPurchased += GetCurrentMeterValue( meterNumElecPurchased );
			gatherElecSurplusSold += GetCurrentMeterValue( meterNumElecSurplusSold );
			gatherElecStorage += GetCurrentMeterValue( meterNumElecStorage );
			gatherPowerConversion += GetCurrentMeterValue( meterNumPowerConversion );
			// gather the onsite thermal components
			gatherWaterHeatRecovery += GetCurrentMeterValue( meterNumWaterHeatRecovery );
			gatherAirHeatRecoveryCool += GetCurrentMeterValue( meterNumAirHeatRecoveryCool );
			gatherAirHeatRecoveryHeat += GetCurrentMeterValue( meterNumAirHeatRecoveryHeat );
			gatherHeatHTGeothermal += GetCurrentMeterValue( meterNumHeatHTGeothermal );
			gatherHeatSolarWater += GetCurrentMeterValue( meterNumHeatSolarWater );
			gatherHeatSolarAir += GetCurrentMeterValue( meterNumHeatSolarAir );
			// gather the water supply components
			gatherRainWater += GetCurrentMeterValue( meterNumRainWater );
			gatherCondensate += GetCurrentMeterValue( meterNumCondensate );
			gatherWellwater += GetCurrentMeterValue( meterNumGroundwater );
			gatherMains += GetCurrentMeterValue( meterNumMains );
			gatherWaterEndUseTotal += GetCurrentMeterValue( meterNumWaterEndUseTotal );

		}
	}

	void
	GatherSourceEnergyEndUseResultsForTimestep( int const IndexTypeKey ) // What kind of data to update (Zone, HVAC)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Mangesh Basarkar
		//       DATE WRITTEN   September 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   This routine gathers data for producing the end uses report in source energy

		// METHODOLOGY EMPLOYED:
		//   Uses get input structure similar to other objects
		//   Meter names are of two forms:
		//         <ResourceType>:<name>
		//   or
		//         <EndUseType>:<ResourceType>
		//   The <EndUseType> are:
		//          Heating
		//          Cooling
		//          InteriorLights
		//          ExteriorLights
		//          InteriorEquipment
		//          ExteriorEquipment
		//          Fans
		//          Pumps
		//          HeatRejection
		//          Humidifier
		//          HeatRecovery
		//          DHW
		//          Refrigeration
		//          Cogeneration
		//   The <ResourceType> are:
		//          Electricity 1
		//          Gas 2
		//          Gasoline 6
		//          Diesel 8
		//          Coal 9
		//          FuelOil#1 10
		//          FuelOil#2 11
		//          Propane 12
		//          Water 7
		//          Steam 5
		//          DistrictCooling 3
		//          DistrictHeating 4

		//          sourceTypeNames(1)='Electric'
		//          sourceTypeNames(2)='NaturalGas'
		//          sourceTypeNames(3)='Gasoline'
		//          sourceTypeNames(4)='Diesel'
		//          sourceTypeNames(5)='Coal'
		//          sourceTypeNames(6)='FuelOil#1'
		//          sourceTypeNames(7)='FuelOil#2'
		//          sourceTypeNames(8)='Propane'
		//          sourceTypeNames(9)='PurchasedElectric'
		//          sourceTypeNames(10)='SoldElectric'
		//          sourceTypeNames(11)='OtherFuel1'
		//          sourceTypeNames(12)='OtherFuel2'

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputProcessor::EndUseCategory;
		using DataStringGlobals::CharComma;
		using DataStringGlobals::CharTab;
		using DataStringGlobals::CharSpace;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iResource;
		int jEndUse;
		Real64 curMeterValue;
		int curMeterNumber;

		// if no beps by source report is called then skip

		if ( ( displaySourceEnergyEndUseSummary ) && ( IndexTypeKey == stepTypeZone ) ) {
			// loop through all of the resources and end uses for the entire facility
			for ( iResource = 1; iResource <= numResourceTypes; ++iResource ) {

				if ( ffSchedUsed( iResource ) ) {
					curMeterNumber = meterNumTotalsBEPS( iResource );
					if ( curMeterNumber > 0 ) {
						curMeterValue = GetCurrentMeterValue( curMeterNumber ) * GetCurrentScheduleValue( ffSchedIndex( iResource ) ) * SourceFactors( iResource );
						gatherTotalsBySourceBEPS( iResource ) += curMeterValue;
					}
				} else {
					curMeterNumber = meterNumTotalsBEPS( iResource );
					if ( curMeterNumber > 0 ) {
						curMeterValue = GetCurrentMeterValue( curMeterNumber ) * SourceFactors( iResource );
						gatherTotalsBySourceBEPS( iResource ) += curMeterValue;
					}
				}

				for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
					if ( ffSchedUsed( iResource ) ) {
						curMeterNumber = meterNumEndUseBEPS( iResource, jEndUse );
						if ( curMeterNumber > 0 ) {
							curMeterValue = GetCurrentMeterValue( curMeterNumber ) * GetCurrentScheduleValue( ffSchedIndex( iResource ) ) * SourceFactors( iResource );
							gatherEndUseBySourceBEPS( iResource, jEndUse ) += curMeterValue;
						}
					} else {
						curMeterNumber = meterNumEndUseBEPS( iResource, jEndUse );
						if ( curMeterNumber > 0 ) {
							curMeterValue = GetCurrentMeterValue( curMeterNumber ) * SourceFactors( iResource );
							gatherEndUseBySourceBEPS( iResource, jEndUse ) += curMeterValue;
						}
					}
				}
			}

		}
	}

	void
	GatherPeakDemandForTimestep( int const IndexTypeKey ) // What kind of data to update (Zone, HVAC)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   January 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   This routine gathers data for producing the Peak Demand
		//   by end-use report

		// METHODOLOGY EMPLOYED:
		//   Uses get input structure similar to other objects
		//   Meter names are of two forms:
		//         <ResourceType>:<name>
		//   or
		//         <EndUseType>:<ResourceType>
		//   For the purposes of this routine, only the facility <name>
		//   is used.  Remember that 'Building' is actually the sum of
		//   the zones only without system,plant and exterior. The only
		//   way to get them all is to use 'facility'
		//   The <EndUseType> are:
		//          Heating
		//          Cooling
		//          InteriorLights
		//          ExteriorLights
		//          InteriorEquipment
		//          ExteriorEquipment
		//          Fans
		//          Pumps
		//          HeatRejection
		//          Humidifier
		//          HeatRecovery
		//          DHW
		//          Refrigeration
		//          Cogeneration
		//   The <ResourceType> are:
		//          Electricity
		//          Gas
		//          Gasoline
		//          Diesel
		//          Coal
		//          FuelOil#1
		//          FuelOil#2
		//          Propane
		//          Water
		//          Steam
		//          DistrictCooling
		//          DistrictHeating

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputProcessor::EndUseCategory;
		using DataStringGlobals::CharComma;
		using DataStringGlobals::CharTab;
		using DataStringGlobals::CharSpace;
		using DataEnvironment::Month;
		using DataEnvironment::DayOfMonth;
		using General::EncodeMonDayHrMin;
		using General::DetermineMinuteForReporting;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iResource;
		int jEndUse;
		int kEndUseSub;
		Real64 curDemandValue;
		int curMeterNumber;
		int minuteCalculated;
		int timestepTimeStamp;

		if ( ( displayDemandEndUse ) && ( IndexTypeKey == stepTypeZone ) ) {
			// loop through all of the resources and end uses for the entire facility
			for ( iResource = 1; iResource <= numResourceTypes; ++iResource ) {
				curMeterNumber = meterNumTotalsBEPS( iResource );
				if ( curMeterNumber > 0 ) {
					curDemandValue = GetCurrentMeterValue( curMeterNumber ) / TimeStepZoneSec;
					// check if current value is greater than existing peak demand value
					if ( curDemandValue > gatherDemandTotal( iResource ) ) {
						gatherDemandTotal( iResource ) = curDemandValue;
						// save the time that the peak demand occured
						//        minuteCalculated = (CurrentTime - INT(CurrentTime))*60
						minuteCalculated = DetermineMinuteForReporting( IndexTypeKey );
						EncodeMonDayHrMin( timestepTimeStamp, Month, DayOfMonth, HourOfDay, minuteCalculated );
						gatherDemandTimeStamp( iResource ) = timestepTimeStamp;
						// if new peak demand is set, then gather all of the end use values at this particular
						// time to find the components of the peak demand
						for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
							curMeterNumber = meterNumEndUseBEPS( iResource, jEndUse );
							if ( curMeterNumber > 0 ) {
								curDemandValue = GetCurrentMeterValue( curMeterNumber ) / TimeStepZoneSec;
								gatherDemandEndUse( iResource, jEndUse ) = curDemandValue;
								for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
									curMeterNumber = meterNumEndUseSubBEPS( kEndUseSub, jEndUse, iResource );
									if ( curMeterNumber > 0 ) {
										curDemandValue = GetCurrentMeterValue( curMeterNumber ) / TimeStepZoneSec;
										gatherDemandEndUseSub( kEndUseSub, jEndUse, iResource ) = curDemandValue;
									}
								}
							}
						}
					}
				}
			}
		}
	}

	void
	GatherHeatGainReport( int const IndexTypeKey ) // What kind of data to update (Zone, HVAC)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		//PURPOSE OF THIS SUBROUTINE:
		//   Gathers the data each zone timestep for the heat gain report.
		// The routine generates an annual table with the following columns which correspond to
		// the output variables and data structures shown:
		// Column                               Output Variable                                Internal Data Structure      Timestep Type
		// ------                               ---------------                                -----------------------      -------- -----
		// HVAC Input Sensible Air Heating      Zone Air Heat Balance System Air Transfer Rate ZnAirRpt()%SumMCpDTsystem    HVAC     Rate
		//                                   Zone Air Heat Balance System Convective Heat Gain Rate ZnAirRpt()%SumNonAirSystem HVAC   Rate
		// HVAC Input Sensible Air Cooling      Zone Air Heat Balance System Air Transfer Rate ZnAirRpt()%SumMCpDTsystem    HVAC     Rate
		// HVAC sensible heating by ATU         sensible heating by the air terminal unit                                   HVAC     Rate
		// HVAC sensible cooling by ATU         sensible cooling by the air terminal unit                                   HVAC     Rate
		//                                    Zone Air Heat Balance System Convective Heat Gain Rate ZnAirRpt()%SumNonAirSystem HVAC  Rate
		// HVAC Input Heated Surface Heating    Electric Low Temp Radiant Heating Energy       ElecRadSys()%HeatEnergy      HVAC     Energy
		//                                      Zone Ventilated Slab Radiant Heating Energy    VentSlab()%RadHeatingEnergy  HVAC     Energy
		//                                      Hydronic Low Temp Radiant Heating Energy       HydrRadSys()%HeatEnergy      HVAC     Energy
		//                                      Constant Flow Low Temp Radiant Heating Energy  CFloRadSys()%HeatEnergy      HVAC     Energy
		// HVAC Input Cooled Surface Cooling    Zone Ventilated Slab Radiant Cooling Energy    -VentSlab()%RadCoolingEnergy HVAC     Energy
		//                                      Hydronic Low Temp Radiant Cooling Energy       -HydrRadSys()%CoolEnergy     HVAC     Energy
		//                                      Constant Flow Low Temp Radiant Cooling Energy  -CFloRadSys()%CoolEnergy     HVAC     Energy
		// People Sensible Heat Addition        Zone People Sensible Heating Energy            ZnRpt()%PeopleSenGain        Zone     Energy
		// Lights Sensible Heat Addition        Zone Lights Total Heating Energy               ZnRpt()%LtsTotGain           Zone     Energy
		// Equipment Sensible Heat Addition     Zone Electric Equipment Radiant Heating Energy ZnRpt()%ElecRadGain          Zone     Energy
		//                                      Zone Gas Equipment Radiant Heating Energy      ZnRpt()%GasRadGain           Zone     Energy
		//                                      Zone Steam Equipment Radiant Heating Energy    ZnRpt()%SteamRadGain         Zone     Energy
		//                                      Zone Hot Water Equipment Radiant Heating Energy ZnRpt()%HWRadGain           Zone     Energy
		//                                      Zone Other Equipment Radiant Heating Energy    ZnRpt()%OtherRadGain         Zone     Energy
		//                                   Zone Electric Equipment Convective Heating Energy ZnRpt()%ElecConGain          Zone     Energy
		//                                      Zone Gas Equipment Convective Heating Energy   ZnRpt()%GasConGain           Zone     Energy
		//                                      Zone Steam Equipment Convective Heating Energy ZnRpt()%SteamConGain         Zone     Energy
		//                                    Zone Hot Water Equipment Convective Heating Energy ZnRpt()%HWConGain          Zone     Energy
		//                                      Zone Other Equipment Convective Heating Energy ZnRpt()%OtherConGain         Zone     Energy
		// Window Heat Addition                 Zone Windows Total Heat Gain Energy            ZoneWinHeatGainRepEnergy()   Zone     Energy
		// Interzone Air Transfer Heat Addition Zone Air Heat Balance Interzone Air Transfer Rate  ZnAirRpt()%SumMCpDTzones HVAC     Rate
		// Infiltration Heat Addition           Zone Air Heat Balance Outdoor Air Transfer Rate ZnAirRpt()%SumMCpDtInfil     HVAC     Rate
		// Equipment Sensible Heat Removal      Zone Electric Equipment Radiant Heating Energy ZnRpt()%ElecRadGain          Zone     Energy
		//                                      Zone Gas Equipment Radiant Heating Energy      ZnRpt()%GasRadGain           Zone     Energy
		//                                      Zone Steam Equipment Radiant Heating Energy    ZnRpt()%SteamRadGain         Zone     Energy
		//                                      Zone Hot Water Equipment Radiant Heating Energy ZnRpt()%HWRadGain           Zone     Energy
		//                                      Zone Other Equipment Radiant Heating Energy    ZnRpt()%OtherRadGain         Zone     Energy
		//                                   Zone Electric Equipment Convective Heating Energy ZnRpt()%ElecConGain          Zone     Energy
		//                                      Zone Gas Equipment Convective Heating Energy   ZnRpt()%GasConGain           Zone     Energy
		//                                      Zone Steam Equipment Convective Heating Energy ZnRpt()%SteamConGain         Zone     Energy
		//                                     Zone Hot Water Equipment Convective Heating Energy ZnRpt()%HWConGain         Zone     Energy
		//                                      Zone Other Equipment Convective Heating Energy ZnRpt()%OtherConGain         Zone     Energy
		// Window Heat Removal                  Zone Windows Total Heat Loss Energy            -ZoneWinHeatLossRepEnergy()  Zone     Energy
		// Interzone Air Transfer Heat Removal  Zone Air Heat Balance Interzone Air Transfer Rate ZnAirRpt()%SumMCpDTzones  HVAC     Rate
		// Infiltration Heat Removal            Zone Air Heat Balance Outdoor Air Transfer Rate ZnAirRpt()%SumMCpDtInfil     HVAC     Rate
		// The following two columns are derived based on the values of the other columns and need to be computed on every HVAC timestep.
		//   Opaque Surface Conduction and Other Heat Addition
		//   Opaque Surface Conduction and Other Heat Removal
		// For variables that are updated on a zone timestep basis, the values are used on the HVAC timestep but are ratioed by the
		// timestep lengths.
		// The peak reports follow a similar example.

		// Using/Aliasing
		using DataHeatBalance::ZonePreDefRep;
		using DataHeatBalance::ZnAirRpt;
		using DataHeatBalance::ZnRpt;
		using DataHeatBalance::ZoneWinHeatGainRepEnergy;
		using DataHeatBalance::ZoneWinHeatLossRepEnergy;
		using DataHeatBalance::ZoneWinHeatGainRep;
		using DataHeatBalance::ZoneWinHeatLossRep;
		using DataHeatBalance::BuildingPreDefRep;
		using DataHeatBalance::Zone;
		using VentilatedSlab::VentSlab;
		using VentilatedSlab::NumOfVentSlabs;
		using LowTempRadiantSystem::HydrRadSys;
		using LowTempRadiantSystem::NumOfHydrLowTempRadSys;
		using LowTempRadiantSystem::CFloRadSys;
		using LowTempRadiantSystem::NumOfCFloLowTempRadSys;
		using LowTempRadiantSystem::ElecRadSys;
		using LowTempRadiantSystem::NumOfElecLowTempRadSys;
		using DataDefineEquip::AirDistUnit;
		using DataDefineEquip::NumAirDistUnits;
		using DirectAirManager::DirectAir;
		using DirectAirManager::NumDirectAir;
		using DataEnvironment::Month;
		using DataEnvironment::DayOfMonth;
		using OutputReportPredefined::pdrSensibleGain;
		using OutputReportPredefined::reportName;
		using DataHVACGlobals::TimeStepSys;
		using General::EncodeMonDayHrMin;
		using General::DetermineMinuteForReporting;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int iZone( 0 );
		static int iRadiant( 0 );
		static int iunit( 0 );
		static int curZone( 0 );
		static Real64 eqpSens( 0.0 );
		static Real64 total( 0.0 );
		// the following arrays store the radiant total for each timestep
		static Array1D< Real64 > radiantHeat;
		static Array1D< Real64 > radiantCool;
		static Array1D< Real64 > ATUHeat;
		static Array1D< Real64 > ATUCool;
		static int timestepTimeStamp( 0 );
		static Real64 bldgHtPk( 0.0 );
		static Real64 bldgClPk( 0.0 );
		static Real64 timeStepRatio( 0.0 );

		Real64 mult; // zone list and group multipliers

		int ActualTimeMin;

		if ( ! DoWeathSim ) return;

		if ( ! reportName( pdrSensibleGain ).show ) return; //don't gather data if report isn't requested

		if ( IndexTypeKey == stepTypeZone ) return; //only add values over the HVAC timestep basis

		if ( GatherHeatGainReportfirstTime ) {
			radiantHeat.allocate( NumOfZones );
			radiantCool.allocate( NumOfZones );
			ATUHeat.allocate( NumOfZones );
			ATUCool.allocate( NumOfZones );
			GatherHeatGainReportfirstTime = false;
		}
		//clear the radiant surface accumulation variables
		radiantHeat = 0.0;
		radiantCool = 0.0;
		// clear the ATU accumulation variables
		ATUHeat = 0.0;
		ATUCool = 0.0;
		//--------------------
		//     ANNUAL
		//--------------------
		timeStepRatio = TimeStepSys / TimeStepZone; //the fraction of the zone time step used by the system timestep
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			mult = Zone( iZone ).Multiplier * Zone( iZone ).ListMultiplier;
			//People Sensible Heat Addition
			ZonePreDefRep( iZone ).SHGSAnPeoplAdd += ZnRpt( iZone ).PeopleSenGain * mult * timeStepRatio;
			//Lights Sensible Heat Addition
			ZonePreDefRep( iZone ).SHGSAnLiteAdd += ZnRpt( iZone ).LtsTotGain * mult * timeStepRatio;
			//HVAC Input Sensible Air Heating
			//HVAC Input Sensible Air Cooling
			if ( ( ZnAirRpt( iZone ).SumMCpDTsystem + ZnAirRpt( iZone ).SumNonAirSystem ) > 0.0 ) {
				ZonePreDefRep( iZone ).SHGSAnHvacHt += ZnAirRpt( iZone ).SumMCpDTsystem * TimeStepSys * SecInHour + ZnAirRpt( iZone ).SumNonAirSystem * mult * TimeStepSys * SecInHour;
			} else {
				ZonePreDefRep( iZone ).SHGSAnHvacCl += ZnAirRpt( iZone ).SumMCpDTsystem * TimeStepSys * SecInHour + ZnAirRpt( iZone ).SumNonAirSystem * mult * TimeStepSys * SecInHour;
			}
			//Interzone Air Transfer Heat Addition
			//Interzone Air Transfer Heat Removal
			if ( ZnAirRpt( iZone ).SumMCpDTzones > 0.0 ) {
				ZonePreDefRep( iZone ).SHGSAnIzaAdd += ZnAirRpt( iZone ).SumMCpDTzones * mult * TimeStepSys * SecInHour;
			} else {
				ZonePreDefRep( iZone ).SHGSAnIzaRem += ZnAirRpt( iZone ).SumMCpDTzones * mult * TimeStepSys * SecInHour;
			}
			//Window Heat Addition
			//Window Heat Removal
			ZonePreDefRep( iZone ).SHGSAnWindAdd += ZoneWinHeatGainRepEnergy( iZone ) * mult * timeStepRatio;
			ZonePreDefRep( iZone ).SHGSAnWindRem -= ZoneWinHeatLossRepEnergy( iZone ) * mult * timeStepRatio;
			//Infiltration Heat Addition
			//Infiltration Heat Removal
			if ( ZnAirRpt( iZone ).SumMCpDtInfil > 0.0 ) {
				ZonePreDefRep( iZone ).SHGSAnInfilAdd += ZnAirRpt( iZone ).SumMCpDtInfil * mult * TimeStepSys * SecInHour;
			} else {
				ZonePreDefRep( iZone ).SHGSAnInfilRem += ZnAirRpt( iZone ).SumMCpDtInfil * mult * TimeStepSys * SecInHour;
			}
			//Equipment Sensible Heat Addition
			//Equipment Sensible Heat Removal
			// the following variables are already gains so they do not need to be converted by multiplying by time.
			eqpSens = ( ZnRpt( iZone ).ElecRadGain + ZnRpt( iZone ).GasRadGain + ZnRpt( iZone ).HWRadGain + ZnRpt( iZone ).SteamRadGain + ZnRpt( iZone ).OtherRadGain + ZnRpt( iZone ).ElecConGain + ZnRpt( iZone ).GasConGain + ZnRpt( iZone ).HWConGain + ZnRpt( iZone ).SteamConGain + ZnRpt( iZone ).OtherConGain ) * timeStepRatio;
			if ( eqpSens > 0.0 ) {
				ZonePreDefRep( iZone ).SHGSAnEquipAdd += eqpSens * mult;
			} else {
				ZonePreDefRep( iZone ).SHGSAnEquipRem += eqpSens * mult;
			}
		}
		// HVAC annual heating by ATU
		// HVAC annual cooling by ATU
		for ( iunit = 1; iunit <= NumAirDistUnits; ++iunit ){
			// HVAC equipment should already have the multipliers included, no "* mult" needed (assumes autosized or multiplied hard-sized air flow).
			curZone = AirDistUnit( iunit ).ZoneNum;
			if ( ( curZone > 0 ) && ( curZone <= NumOfZones ) ) {
				ZonePreDefRep( curZone ).SHGSAnHvacATUHt += AirDistUnit( iunit ).HeatGain;
				ZonePreDefRep( curZone ).SHGSAnHvacATUCl -= AirDistUnit( iunit ).CoolGain;
				ATUHeat( curZone ) = AirDistUnit( iunit ).HeatRate;
				ATUCool( curZone ) = -AirDistUnit( iunit ).CoolRate;
			}
		}
		iunit = 0;
		for ( iunit = 1; iunit <= NumDirectAir; ++iunit ){
			// HVAC equipment should already have the multipliers included, no "* mult" needed (assumes autosized or multiplied hard-sized air flow).
			curZone = DirectAir( iunit ).ZoneNum;
			if ( ( curZone > 0 ) && ( curZone <= NumOfZones ) ) {
				ZonePreDefRep( curZone ).SHGSAnHvacATUHt += DirectAir( iunit ).HeatEnergy;
				ZonePreDefRep( curZone ).SHGSAnHvacATUCl -= DirectAir( iunit ).CoolEnergy;
				ATUHeat( curZone ) += DirectAir( iunit ).HeatRate;
				ATUCool( curZone ) -= DirectAir( iunit ).CoolRate;
			}
		}
		curZone = 0;
		// HVAC Input Heated Surface Heating
		// HVAC Input Cooled Surface Cooling
		for ( iRadiant = 1; iRadiant <= NumOfVentSlabs; ++iRadiant ) {
			curZone = VentSlab( iRadiant ).ZonePtr;
			mult = Zone( curZone ).Multiplier * Zone( curZone ).ListMultiplier;
			if( ( curZone > 0 ) && ( curZone <= NumOfZones ) ) {
				ZonePreDefRep( curZone ).SHGSAnSurfHt += VentSlab( iRadiant ).RadHeatingEnergy * mult;
				ZonePreDefRep( curZone ).SHGSAnSurfCl -= VentSlab( iRadiant ).RadCoolingEnergy * mult;
				radiantHeat( curZone ) = VentSlab( iRadiant ).RadHeatingPower * mult;
				radiantCool( curZone ) = -VentSlab( iRadiant ).RadCoolingPower * mult;
			}
		}
		for ( iRadiant = 1; iRadiant <= NumOfHydrLowTempRadSys; ++iRadiant ) {
			curZone = HydrRadSys( iRadiant ).ZonePtr;
			mult = Zone( curZone ).Multiplier * Zone( curZone ).ListMultiplier;
			if( ( curZone > 0 ) && ( curZone <= NumOfZones ) ) {
				ZonePreDefRep( curZone ).SHGSAnSurfHt += HydrRadSys( iRadiant ).HeatEnergy * mult;
				ZonePreDefRep( curZone ).SHGSAnSurfCl -= HydrRadSys( iRadiant ).CoolEnergy * mult;
				radiantHeat( curZone ) += HydrRadSys( iRadiant ).HeatPower * mult;
				radiantCool( curZone ) -= HydrRadSys( iRadiant ).CoolPower * mult;
			}
		}
		for ( iRadiant = 1; iRadiant <= NumOfCFloLowTempRadSys; ++iRadiant ) {
			curZone = CFloRadSys( iRadiant ).ZonePtr;
			mult = Zone( curZone ).Multiplier * Zone( curZone ).ListMultiplier;
			if( ( curZone > 0 ) && ( curZone <= NumOfZones ) ) {
				ZonePreDefRep( curZone ).SHGSAnSurfHt += CFloRadSys( iRadiant ).HeatEnergy * mult;
				ZonePreDefRep( curZone ).SHGSAnSurfCl -= CFloRadSys( iRadiant ).CoolEnergy * mult;
				radiantHeat( curZone ) += CFloRadSys( iRadiant ).HeatPower * mult;
				radiantCool( curZone ) -= CFloRadSys( iRadiant ).CoolPower * mult;
			}
		}
		for ( iRadiant = 1; iRadiant <= NumOfElecLowTempRadSys; ++iRadiant ) {
			curZone = ElecRadSys( iRadiant ).ZonePtr;
			mult = Zone( curZone ).Multiplier * Zone( curZone ).ListMultiplier;
			if( ( curZone > 0 ) && ( curZone <= NumOfZones ) ) {
				ZonePreDefRep( curZone ).SHGSAnSurfHt += ElecRadSys( iRadiant ).HeatEnergy * mult;
				radiantHeat( curZone ) += ElecRadSys( iRadiant ).HeatPower * mult;
			}
		}
		// Opaque Surface Conduction and Other Heat Addition
		// Opaque Surface Conduction and Other Heat Removal
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			// ZonePreDefRep variables above already inlude zone list and group multipliers
			total = ZonePreDefRep( iZone ).SHGSAnPeoplAdd + ZonePreDefRep( iZone ).SHGSAnLiteAdd + ZonePreDefRep( iZone ).SHGSAnHvacHt + ZonePreDefRep( iZone ).SHGSAnHvacCl + ZonePreDefRep( iZone ).SHGSAnIzaAdd + ZonePreDefRep( iZone ).SHGSAnIzaRem + ZonePreDefRep( iZone ).SHGSAnWindAdd + ZonePreDefRep( iZone ).SHGSAnWindRem + ZonePreDefRep( iZone ).SHGSAnInfilAdd + ZonePreDefRep( iZone ).SHGSAnInfilRem + ZonePreDefRep( iZone ).SHGSAnEquipAdd + ZonePreDefRep( iZone ).SHGSAnEquipRem + ZonePreDefRep( iZone ).SHGSAnSurfHt + ZonePreDefRep( iZone ).SHGSAnSurfCl;
			total = -total; //want to know the negative value of the sum since the row should add up to zero
			if ( total > 0 ) {
				ZonePreDefRep( iZone ).SHGSAnOtherAdd = total;
			} else {
				ZonePreDefRep( iZone ).SHGSAnOtherRem = total;
			}
		}
		//--------------------------------
		// ZONE PEAK COOLING AND HEATING
		//--------------------------------
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			mult = Zone( iZone ).Multiplier * Zone( iZone ).ListMultiplier;
			// RR I can't get the Infiltration Heat Addition/Removal columns to add up.
			// THis is the only suspect line, mixing MCpDt terms and a power term looks fishy.
			if( ( ZnAirRpt( iZone ).SumMCpDTsystem + radiantHeat( iZone ) + ZnAirRpt( iZone ).SumNonAirSystem * mult ) > 0 ) {
				if( ( ZnAirRpt( iZone ).SumMCpDTsystem + radiantHeat( iZone ) + ZnAirRpt( iZone ).SumNonAirSystem * mult ) > ZonePreDefRep( iZone ).htPeak ) {
					ZonePreDefRep( iZone ).htPeak = ZnAirRpt( iZone ).SumMCpDTsystem + radiantHeat( iZone ) + ZnAirRpt( iZone ).SumNonAirSystem * mult;
					//determine timestamp
					//      ActualTimeS = CurrentTime-TimeStepZone+SysTimeElapsed
					//      ActualtimeE = ActualTimeS+TimeStepSys
					//      ActualTimeHrS=INT(ActualTimeS)
					//      ActualTimeMin=NINT((ActualtimeE - ActualTimeHrS)*FracToMin)
					ActualTimeMin = DetermineMinuteForReporting( IndexTypeKey );
					EncodeMonDayHrMin( timestepTimeStamp, Month, DayOfMonth, HourOfDay, ActualTimeMin );
					ZonePreDefRep( iZone ).htPtTimeStamp = timestepTimeStamp;
					//HVAC Input Sensible Air Heating
					//HVAC Input Sensible Air Cooling
					// non-HVAC ZnAirRpt variables DO NOT include zone multipliers
					ZonePreDefRep( iZone ).SHGSHtHvacHt = ZnAirRpt( iZone ).SumMCpDTsystem + ZnAirRpt( iZone ).SumNonAirSystem * mult;
					ZonePreDefRep( iZone ).SHGSHtHvacCl = 0.0;
					// HVAC Input Heated Surface Heating
					// HVAC Input Cooled Surface Cooling
					ZonePreDefRep( iZone ).SHGSHtSurfHt = radiantHeat( iZone ); // multipliers included above
					ZonePreDefRep( iZone ).SHGSHtSurfCl = radiantCool( iZone ); // multipliers included above
					// HVAC ATU Heating at Heat Peak
					// HVAC ATU Cooling at Heat Peak
					ZonePreDefRep( iZone ).SHGSHtHvacATUHt = ATUHeat( iZone ); // multipliers included above
					ZonePreDefRep( iZone ).SHGSHtHvacATUCl = ATUCool( iZone ); // multipliers included above
					//People Sensible Heat Addition
					ZonePreDefRep( iZone ).SHGSHtPeoplAdd = ZnRpt( iZone ).PeopleSenGainRate * mult;
					//Lights Sensible Heat Addition
					ZonePreDefRep( iZone ).SHGSHtLiteAdd = ZnRpt( iZone ).LtsTotGainRate * mult;
					//Equipment Sensible Heat Addition
					//Equipment Sensible Heat Removal
					// non-HVAC ZnAirRpt variables DO NOT include zone multipliers
					eqpSens = ZnRpt( iZone ).ElecRadGainRate + ZnRpt( iZone ).GasRadGainRate + ZnRpt( iZone ).HWRadGainRate + ZnRpt( iZone ).SteamRadGainRate + ZnRpt( iZone ).OtherRadGainRate + ZnRpt( iZone ).ElecConGainRate + ZnRpt( iZone ).GasConGainRate + ZnRpt( iZone ).HWConGainRate + ZnRpt( iZone ).SteamConGainRate + ZnRpt( iZone ).OtherConGainRate;
					if ( eqpSens > 0.0 ) {
						ZonePreDefRep( iZone ).SHGSHtEquipAdd = eqpSens * mult;
						ZonePreDefRep( iZone ).SHGSHtEquipRem = 0.0;
					} else {
						ZonePreDefRep( iZone ).SHGSHtEquipAdd = 0.0;
						ZonePreDefRep( iZone ).SHGSHtEquipRem = eqpSens * mult;
					}
					//Window Heat Addition
					//Window Heat Removal
					ZonePreDefRep( iZone ).SHGSHtWindAdd = ZoneWinHeatGainRep( iZone ) * mult;
					ZonePreDefRep( iZone ).SHGSHtWindRem = -ZoneWinHeatLossRep( iZone ) * mult;
					// mixing object heat addition and removal
					if ( ZnAirRpt( iZone ).SumMCpDTzones > 0.0 ) {
						ZonePreDefRep( iZone ).SHGSHtIzaAdd = ZnAirRpt( iZone ).SumMCpDTzones * mult;
						ZonePreDefRep( iZone ).SHGSHtIzaRem = 0.0;
					} else {
						ZonePreDefRep( iZone ).SHGSHtIzaAdd = 0.0;
						ZonePreDefRep( iZone ).SHGSHtIzaRem = ZnAirRpt( iZone ).SumMCpDTzones * mult;
					}
					//Infiltration Heat Addition
					//Infiltration Heat Removal
					if ( ZnAirRpt( iZone ).SumMCpDtInfil > 0.0 ) {
						ZonePreDefRep( iZone ).SHGSHtInfilAdd = ZnAirRpt( iZone ).SumMCpDtInfil * mult;
						ZonePreDefRep( iZone ).SHGSHtInfilRem = 0.0;
					} else {
						ZonePreDefRep( iZone ).SHGSHtInfilAdd = 0.0;
						ZonePreDefRep( iZone ).SHGSHtInfilRem = ZnAirRpt( iZone ).SumMCpDtInfil * mult;
					}
					// Opaque Surface Conduction and Other Heat Addition
					// Opaque Surface Conduction and Other Heat Removal
					total = ZonePreDefRep( iZone ).SHGSHtPeoplAdd + ZonePreDefRep( iZone ).SHGSHtLiteAdd + ZonePreDefRep( iZone ).SHGSHtHvacHt + ZonePreDefRep( iZone ).SHGSHtHvacCl + ZonePreDefRep( iZone ).SHGSHtIzaAdd + ZonePreDefRep( iZone ).SHGSHtIzaRem + ZonePreDefRep( iZone ).SHGSHtWindAdd + ZonePreDefRep( iZone ).SHGSHtWindRem + ZonePreDefRep( iZone ).SHGSHtInfilAdd + ZonePreDefRep( iZone ).SHGSHtInfilRem + ZonePreDefRep( iZone ).SHGSHtEquipAdd + ZonePreDefRep( iZone ).SHGSHtEquipRem + ZonePreDefRep( iZone ).SHGSHtSurfHt + ZonePreDefRep( iZone ).SHGSHtSurfCl;
					total = -total; //want to know the negative value of the sum since the row should add up to zero
					if ( total > 0 ) {
						ZonePreDefRep( iZone ).SHGSHtOtherAdd = total;
						ZonePreDefRep( iZone ).SHGSHtOtherRem = 0.0;
					} else {
						ZonePreDefRep( iZone ).SHGSHtOtherAdd = 0.0;
						ZonePreDefRep( iZone ).SHGSHtOtherRem = total;
					}
				}
			} else {
				if( ( ZnAirRpt( iZone ).SumMCpDTsystem + radiantCool( iZone ) + ZnAirRpt( iZone ).SumNonAirSystem * mult ) < ZonePreDefRep( iZone ).clPeak ) {
					ZonePreDefRep( iZone ).clPeak = ZnAirRpt( iZone ).SumMCpDTsystem + radiantCool( iZone ) + ZnAirRpt( iZone ).SumNonAirSystem * mult;
					//determine timestamp
					//      ActualTimeS = CurrentTime-TimeStepZone+SysTimeElapsed
					//      ActualtimeE = ActualTimeS+TimeStepSys
					//      ActualTimeHrS=INT(ActualTimeS)
					//      ActualTimeMin=NINT((ActualtimeE - ActualTimeHrS)*FracToMin)
					ActualTimeMin = DetermineMinuteForReporting( IndexTypeKey );
					EncodeMonDayHrMin( timestepTimeStamp, Month, DayOfMonth, HourOfDay, ActualTimeMin );
					ZonePreDefRep( iZone ).clPtTimeStamp = timestepTimeStamp;
					//HVAC Input Sensible Air Heating
					//HVAC Input Sensible Air Cooling
					ZonePreDefRep( iZone ).SHGSClHvacHt = 0.0;
					ZonePreDefRep( iZone ).SHGSClHvacCl = ZnAirRpt( iZone ).SumMCpDTsystem + ZnAirRpt( iZone ).SumNonAirSystem * mult;
					// HVAC Input Heated Surface Heating
					// HVAC Input Cooled Surface Cooling
					ZonePreDefRep( iZone ).SHGSClSurfHt = radiantHeat( iZone );
					ZonePreDefRep( iZone ).SHGSClSurfCl = radiantCool( iZone );
					// HVAC heating by ATU at cool peak
					// HVAC cooling by ATU at cool peak
					ZonePreDefRep( iZone ).SHGSClHvacATUHt = ATUHeat( iZone );
					ZonePreDefRep( iZone ).SHGSClHvacATUCl = ATUCool( iZone );
					//People Sensible Heat Addition
					ZonePreDefRep( iZone ).SHGSClPeoplAdd = ZnRpt( iZone ).PeopleSenGainRate * mult;
					//Lights Sensible Heat Addition
					ZonePreDefRep( iZone ).SHGSClLiteAdd = ZnRpt( iZone ).LtsTotGainRate * mult;
					//Equipment Sensible Heat Addition
					//Equipment Sensible Heat Removal
					eqpSens = ZnRpt( iZone ).ElecRadGainRate + ZnRpt( iZone ).GasRadGainRate + ZnRpt( iZone ).HWRadGainRate + ZnRpt( iZone ).SteamRadGainRate + ZnRpt( iZone ).OtherRadGainRate + ZnRpt( iZone ).ElecConGainRate + ZnRpt( iZone ).GasConGainRate + ZnRpt( iZone ).HWConGainRate + ZnRpt( iZone ).SteamConGainRate + ZnRpt( iZone ).OtherConGainRate;
					if ( eqpSens > 0.0 ) {
						ZonePreDefRep( iZone ).SHGSClEquipAdd = eqpSens * mult;
						ZonePreDefRep( iZone ).SHGSClEquipRem = 0.0;
					} else {
						ZonePreDefRep( iZone ).SHGSClEquipAdd = 0.0;
						ZonePreDefRep( iZone ).SHGSClEquipRem = eqpSens * mult;
					}
					//Window Heat Addition
					//Window Heat Removal
					ZonePreDefRep( iZone ).SHGSClWindAdd = ZoneWinHeatGainRep( iZone ) * mult;
					ZonePreDefRep( iZone ).SHGSClWindRem = -ZoneWinHeatLossRep( iZone ) * mult;
					// mixing object cool addition and removal
					if( ZnAirRpt( iZone ).SumMCpDTzones > 0.0 ) {
						ZonePreDefRep( iZone ).SHGSClIzaAdd = ZnAirRpt( iZone ).SumMCpDTzones * mult;
						ZonePreDefRep( iZone ).SHGSClIzaRem = 0.0;
					} else {
						ZonePreDefRep( iZone ).SHGSClIzaAdd = 0.0;
						ZonePreDefRep( iZone ).SHGSClIzaRem = ZnAirRpt( iZone ).SumMCpDTzones * mult;
					}
					//Infiltration Heat Addition
					//Infiltration Heat Removal
					if ( ZnAirRpt( iZone ).SumMCpDtInfil > 0.0 ) {
						ZonePreDefRep( iZone ).SHGSClInfilAdd = ZnAirRpt( iZone ).SumMCpDtInfil * mult;
						ZonePreDefRep( iZone ).SHGSClInfilRem = 0.0;
					} else {
						ZonePreDefRep( iZone ).SHGSClInfilAdd = 0.0;
						ZonePreDefRep( iZone ).SHGSClInfilRem = ZnAirRpt( iZone ).SumMCpDtInfil * mult;
					}
					// Opaque Surface Conduction and Other Heat Addition
					// Opaque Surface Conduction and Other Heat Removal
					total = ZonePreDefRep( iZone ).SHGSClPeoplAdd + ZonePreDefRep( iZone ).SHGSClLiteAdd + ZonePreDefRep( iZone ).SHGSClHvacHt + ZonePreDefRep( iZone ).SHGSClHvacCl + ZonePreDefRep( iZone ).SHGSClIzaAdd + ZonePreDefRep( iZone ).SHGSClIzaRem + ZonePreDefRep( iZone ).SHGSClWindAdd + ZonePreDefRep( iZone ).SHGSClWindRem + ZonePreDefRep( iZone ).SHGSClInfilAdd + ZonePreDefRep( iZone ).SHGSClInfilRem + ZonePreDefRep( iZone ).SHGSClEquipAdd + ZonePreDefRep( iZone ).SHGSClEquipRem + ZonePreDefRep( iZone ).SHGSClSurfHt + ZonePreDefRep( iZone ).SHGSClSurfCl;
					total = -total; //want to know the negative value of the sum since the row should add up to zero
					if ( total > 0 ) {
						ZonePreDefRep( iZone ).SHGSClOtherAdd = total;
						ZonePreDefRep( iZone ).SHGSClOtherRem = 0.0;
					} else {
						ZonePreDefRep( iZone ).SHGSClOtherAdd = 0.0;
						ZonePreDefRep( iZone ).SHGSClOtherRem = total;
					}
				}
			}
		}
		//------------------------------------
		// BUILDING PEAK COOLING AND HEATING
		//------------------------------------
		bldgHtPk = 0.0;
		bldgClPk = 0.0;
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			mult = Zone( iZone ).Multiplier * Zone( iZone ).ListMultiplier;
			if( ( ZnAirRpt( iZone ).SumMCpDTsystem + radiantHeat( iZone ) + ZnAirRpt( iZone ).SumNonAirSystem * mult ) > 0 ) {
				bldgHtPk += ZnAirRpt( iZone ).SumMCpDTsystem + radiantHeat( iZone ) + ZnAirRpt( iZone ).SumNonAirSystem * mult;
			} else {
				bldgClPk += ZnAirRpt( iZone ).SumMCpDTsystem + radiantCool( iZone ) + ZnAirRpt( iZone ).SumNonAirSystem * mult;
			}
		}
		if ( bldgHtPk > BuildingPreDefRep.htPeak ) {
			BuildingPreDefRep.htPeak = bldgHtPk;
			//determine timestamp
			//  ActualTimeS = CurrentTime-TimeStepZone+SysTimeElapsed
			//  ActualtimeE = ActualTimeS+TimeStepSys
			//  ActualTimeHrS=INT(ActualTimeS)
			//  ActualTimeMin=NINT((ActualtimeE - ActualTimeHrS)*FracToMin)
			ActualTimeMin = DetermineMinuteForReporting( IndexTypeKey );
			EncodeMonDayHrMin( timestepTimeStamp, Month, DayOfMonth, HourOfDay, ActualTimeMin );
			BuildingPreDefRep.htPtTimeStamp = timestepTimeStamp;
			//reset building level results to zero prior to accumulating across zones
			BuildingPreDefRep.SHGSHtHvacHt = 0.0;
			BuildingPreDefRep.SHGSHtHvacCl = 0.0;
			BuildingPreDefRep.SHGSHtHvacATUHt = 0.0;
			BuildingPreDefRep.SHGSHtHvacATUCl = 0.0;
			BuildingPreDefRep.SHGSHtSurfHt = 0.0;
			BuildingPreDefRep.SHGSHtSurfCl = 0.0;
			BuildingPreDefRep.SHGSHtPeoplAdd = 0.0;
			BuildingPreDefRep.SHGSHtLiteAdd = 0.0;
			BuildingPreDefRep.SHGSHtEquipAdd = 0.0;
			BuildingPreDefRep.SHGSHtWindAdd = 0.0;
			BuildingPreDefRep.SHGSHtIzaAdd = 0.0;
			BuildingPreDefRep.SHGSHtInfilAdd = 0.0;
			BuildingPreDefRep.SHGSHtOtherAdd = 0.0;
			BuildingPreDefRep.SHGSHtEquipRem = 0.0;
			BuildingPreDefRep.SHGSHtWindRem = 0.0;
			BuildingPreDefRep.SHGSHtIzaRem = 0.0;
			BuildingPreDefRep.SHGSHtInfilRem = 0.0;
			BuildingPreDefRep.SHGSHtOtherRem = 0.0;
			for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
				mult = Zone( iZone ).Multiplier * Zone( iZone ).ListMultiplier;
				//HVAC Input Sensible Air Heating
				//HVAC Input Sensible Air Cooling
				BuildingPreDefRep.SHGSHtHvacHt += ZnAirRpt( iZone ).SumMCpDTsystem + ZnAirRpt( iZone ).SumNonAirSystem * mult;
				// HVAC Input Heated Surface Heating
				// HVAC Input Cooled Surface Cooling
				BuildingPreDefRep.SHGSHtSurfHt += radiantHeat( iZone );
				BuildingPreDefRep.SHGSHtSurfCl += radiantCool( iZone );
				// HVAC ATU Heating
				// HVAC ATU Cooling
				BuildingPreDefRep.SHGSHtHvacATUHt += ATUHeat( iZone );
				BuildingPreDefRep.SHGSHtHvacATUCl += ATUCool( iZone );
				//People Sensible Heat Addition
				BuildingPreDefRep.SHGSHtPeoplAdd += ZnRpt( iZone ).PeopleSenGainRate * mult;
				//Lights Sensible Heat Addition
				BuildingPreDefRep.SHGSHtLiteAdd += ZnRpt( iZone ).LtsTotGainRate * mult;
				//Equipment Sensible Heat Addition
				//Equipment Sensible Heat Removal
				eqpSens = ZnRpt( iZone ).ElecRadGainRate + ZnRpt( iZone ).GasRadGainRate + ZnRpt( iZone ).HWRadGainRate + ZnRpt( iZone ).SteamRadGainRate + ZnRpt( iZone ).OtherRadGainRate + ZnRpt( iZone ).ElecConGainRate + ZnRpt( iZone ).GasConGainRate + ZnRpt( iZone ).HWConGainRate + ZnRpt( iZone ).SteamConGainRate + ZnRpt( iZone ).OtherConGainRate;
				if ( eqpSens > 0.0 ) {
					BuildingPreDefRep.SHGSHtEquipAdd += eqpSens * mult;
				} else {
					BuildingPreDefRep.SHGSHtEquipRem += eqpSens * mult;
				}
				//Window Heat Addition
				//Window Heat Removal
				BuildingPreDefRep.SHGSHtWindAdd += ZoneWinHeatGainRep( iZone ) * mult;
				BuildingPreDefRep.SHGSHtWindRem -= ZoneWinHeatLossRep( iZone ) * mult;
				// mixing object heat addition and removal
				if( ZnAirRpt( iZone ).SumMCpDTzones > 0.0 ) {
					BuildingPreDefRep.SHGSHtIzaAdd += ZnAirRpt( iZone ).SumMCpDTzones * mult;
				} else {
					BuildingPreDefRep.SHGSHtIzaRem += ZnAirRpt( iZone ).SumMCpDTzones * mult;
				}
				//Infiltration Heat Addition
				//Infiltration Heat Removal
				if ( ZnAirRpt( iZone ).SumMCpDtInfil > 00 ) {
					BuildingPreDefRep.SHGSHtInfilAdd += ZnAirRpt( iZone ).SumMCpDtInfil * mult;
				} else {
					BuildingPreDefRep.SHGSHtInfilRem += ZnAirRpt( iZone ).SumMCpDtInfil * mult;
				}
			}
			// Opaque Surface Conduction and Other Heat Addition
			// Opaque Surface Conduction and Other Heat Removal
			total = BuildingPreDefRep.SHGSHtPeoplAdd + BuildingPreDefRep.SHGSHtLiteAdd + BuildingPreDefRep.SHGSHtHvacHt + BuildingPreDefRep.SHGSHtHvacCl + BuildingPreDefRep.SHGSHtIzaAdd + BuildingPreDefRep.SHGSHtIzaRem + BuildingPreDefRep.SHGSHtWindAdd + BuildingPreDefRep.SHGSHtWindRem + BuildingPreDefRep.SHGSHtInfilAdd + BuildingPreDefRep.SHGSHtInfilRem + BuildingPreDefRep.SHGSHtEquipAdd + BuildingPreDefRep.SHGSHtEquipRem + BuildingPreDefRep.SHGSHtSurfHt + BuildingPreDefRep.SHGSHtSurfCl;
			total = -total; //want to know the negative value of the sum since the row should add up to zero
			if ( total > 0 ) {
				BuildingPreDefRep.SHGSHtOtherAdd += total;
			} else {
				BuildingPreDefRep.SHGSHtOtherRem += total;
			}
		}
		if ( bldgClPk < BuildingPreDefRep.clPeak ) {
			BuildingPreDefRep.clPeak = bldgClPk;
			//determine timestamp
			//  ActualTimeS = CurrentTime-TimeStepZone+SysTimeElapsed
			//  ActualtimeE = ActualTimeS+TimeStepSys
			//  ActualTimeHrS=INT(ActualTimeS)
			//  ActualTimeMin=NINT((ActualtimeE - ActualTimeHrS)*FracToMin)
			ActualTimeMin = DetermineMinuteForReporting( IndexTypeKey );
			EncodeMonDayHrMin( timestepTimeStamp, Month, DayOfMonth, HourOfDay, ActualTimeMin );
			BuildingPreDefRep.clPtTimeStamp = timestepTimeStamp;
			//reset building level results to zero prior to accumulating across zones
			BuildingPreDefRep.SHGSClHvacHt = 0.0;
			BuildingPreDefRep.SHGSClHvacCl = 0.0;
			BuildingPreDefRep.SHGSClSurfHt = 0.0;
			BuildingPreDefRep.SHGSClSurfCl = 0.0;
			BuildingPreDefRep.SHGSClHvacATUHt = 0.0;
			BuildingPreDefRep.SHGSClHvacATUCl = 0.0;
			BuildingPreDefRep.SHGSClPeoplAdd = 0.0;
			BuildingPreDefRep.SHGSClLiteAdd = 0.0;
			BuildingPreDefRep.SHGSClEquipAdd = 0.0;
			BuildingPreDefRep.SHGSClWindAdd = 0.0;
			BuildingPreDefRep.SHGSClIzaAdd = 0.0;
			BuildingPreDefRep.SHGSClInfilAdd = 0.0;
			BuildingPreDefRep.SHGSClOtherAdd = 0.0;
			BuildingPreDefRep.SHGSClEquipRem = 0.0;
			BuildingPreDefRep.SHGSClWindRem = 0.0;
			BuildingPreDefRep.SHGSClIzaRem = 0.0;
			BuildingPreDefRep.SHGSClInfilRem = 0.0;
			BuildingPreDefRep.SHGSClOtherRem = 0.0;
			for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
				mult = Zone( iZone ).Multiplier * Zone( iZone ).ListMultiplier;
				//HVAC Input Sensible Air Heating
				//HVAC Input Sensible Air Cooling
				BuildingPreDefRep.SHGSClHvacCl += ZnAirRpt( iZone ).SumMCpDTsystem + ZnAirRpt( iZone ).SumNonAirSystem * mult;
				// HVAC Input Heated Surface Heating
				// HVAC Input Cooled Surface Cooling
				BuildingPreDefRep.SHGSClSurfHt += radiantHeat( iZone );
				BuildingPreDefRep.SHGSClSurfCl += radiantCool( iZone );
				// HVAC ATU Heating
				// HVAC ATU Cooling
				BuildingPreDefRep.SHGSClHvacATUHt += ATUHeat( iZone );
				BuildingPreDefRep.SHGSClHvacATUCl += ATUCool( iZone );
				//People Sensible Heat Addition
				BuildingPreDefRep.SHGSClPeoplAdd += ZnRpt( iZone ).PeopleSenGainRate * mult;
				//Lights Sensible Heat Addition
				BuildingPreDefRep.SHGSClLiteAdd += ZnRpt( iZone ).LtsTotGainRate * mult;
				//Equipment Sensible Heat Addition
				//Equipment Sensible Heat Removal
				eqpSens = ZnRpt( iZone ).ElecRadGainRate + ZnRpt( iZone ).GasRadGainRate + ZnRpt( iZone ).HWRadGainRate + ZnRpt( iZone ).SteamRadGainRate + ZnRpt( iZone ).OtherRadGainRate + ZnRpt( iZone ).ElecConGainRate + ZnRpt( iZone ).GasConGainRate + ZnRpt( iZone ).HWConGainRate + ZnRpt( iZone ).SteamConGainRate + ZnRpt( iZone ).OtherConGainRate;
				if ( eqpSens > 0.0 ) {
					BuildingPreDefRep.SHGSClEquipAdd += eqpSens * mult;
				} else {
					BuildingPreDefRep.SHGSClEquipRem += eqpSens * mult;
				}
				//Window Heat Addition
				//Window Heat Removal
				BuildingPreDefRep.SHGSClWindAdd += ZoneWinHeatGainRep( iZone ) * mult;
				BuildingPreDefRep.SHGSClWindRem -= ZoneWinHeatLossRep( iZone ) * mult;
				// mixing object cool addition and removal
				if( ZnAirRpt( iZone ).SumMCpDTzones > 0.0 ) {
					BuildingPreDefRep.SHGSClIzaAdd += ZnAirRpt( iZone ).SumMCpDTzones * mult;
				} else {
					BuildingPreDefRep.SHGSClIzaRem += ZnAirRpt( iZone ).SumMCpDTzones * mult;
				}
				//Infiltration Heat Addition
				//Infiltration Heat Removal
				if ( ZnAirRpt( iZone ).SumMCpDtInfil > 00 ) {
					BuildingPreDefRep.SHGSClInfilAdd += ZnAirRpt( iZone ).SumMCpDtInfil * mult;
				} else {
					BuildingPreDefRep.SHGSClInfilRem += ZnAirRpt( iZone ).SumMCpDtInfil * mult;
				}
			}
			// Opaque Surface Conduction and Other Heat Addition
			// Opaque Surface Conduction and Other Heat Removal
			total = BuildingPreDefRep.SHGSClPeoplAdd + BuildingPreDefRep.SHGSClLiteAdd + BuildingPreDefRep.SHGSClHvacHt + BuildingPreDefRep.SHGSClHvacCl + BuildingPreDefRep.SHGSClIzaAdd + BuildingPreDefRep.SHGSClIzaRem + BuildingPreDefRep.SHGSClWindAdd + BuildingPreDefRep.SHGSClWindRem + BuildingPreDefRep.SHGSClInfilAdd + BuildingPreDefRep.SHGSClInfilRem + BuildingPreDefRep.SHGSClEquipAdd + BuildingPreDefRep.SHGSClEquipRem + BuildingPreDefRep.SHGSClSurfHt + BuildingPreDefRep.SHGSClSurfCl;
			total = -total; //want to know the negative value of the sum since the row should add up to zero
			if ( total > 0 ) {
				BuildingPreDefRep.SHGSClOtherAdd += total;
			} else {
				BuildingPreDefRep.SHGSClOtherRem += total;
			}
		}
	}

	//======================================================================================================================
	//======================================================================================================================

	//    WRITE OUTPUT FILE ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	WriteTabularReports()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   This routine hides from the main simulation that four specific
		//   types of tabular reports are each created. If another type of
		//   report is added it can be added to the list here.

		// Locals
		int EchoInputFile; // found unit number for 'eplusout.audit'

		FillWeatherPredefinedEntries();
		FillRemainingPredefinedEntries();

		if ( WriteTabularFiles ) {
			// call each type of report in turn
			WriteBEPSTable();
			WriteTableOfContents();
			WriteVeriSumTable();
			WriteDemandEndUseSummary();
			WriteSourceEnergyEndUseSummary();
			WritePredefinedTables();
			WriteComponentSizing();
			WriteSurfaceShadowing();
			WriteCompCostTable();
			WriteAdaptiveComfortTable();
			WriteZoneLoadComponentTable();
			if ( DoWeathSim ) {
				WriteMonthlyTables();
				WriteTimeBinTables();
				OutputReportTabularAnnual::WriteAnnualTables();
			}
		}
		EchoInputFile = FindUnitNumber( DataStringGlobals::outputAuditFileName );
		gio::write( EchoInputFile, fmtLD ) << "MonthlyInputCount=" << MonthlyInputCount;
		gio::write( EchoInputFile, fmtLD ) << "sizeMonthlyInput=" << sizeMonthlyInput;
		gio::write( EchoInputFile, fmtLD ) << "MonthlyFieldSetInputCount=" << MonthlyFieldSetInputCount;
		gio::write( EchoInputFile, fmtLD ) << "sizeMonthlyFieldSetInput=" << sizeMonthlyFieldSetInput;
		gio::write( EchoInputFile, fmtLD ) << "MonthlyTablesCount=" << MonthlyTablesCount;
		gio::write( EchoInputFile, fmtLD ) << "MonthlyColumnsCount=" << MonthlyColumnsCount;
		gio::write( EchoInputFile, fmtLD ) << "sizeReportName=" << sizeReportName;
		gio::write( EchoInputFile, fmtLD ) << "numReportName=" << numReportName;
		gio::write( EchoInputFile, fmtLD ) << "sizeSubTable=" << sizeSubTable;
		gio::write( EchoInputFile, fmtLD ) << "numSubTable=" << numSubTable;
		gio::write( EchoInputFile, fmtLD ) << "sizeColumnTag=" << sizeColumnTag;
		gio::write( EchoInputFile, fmtLD ) << "numColumnTag=" << numColumnTag;
		gio::write( EchoInputFile, fmtLD ) << "sizeTableEntry=" << sizeTableEntry;
		gio::write( EchoInputFile, fmtLD ) << "numTableEntry=" << numTableEntry;
		gio::write( EchoInputFile, fmtLD ) << "sizeCompSizeTableEntry=" << sizeCompSizeTableEntry;
		gio::write( EchoInputFile, fmtLD ) << "numCompSizeTableEntry=" << numCompSizeTableEntry;

	}

	void
	FillWeatherPredefinedEntries()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   Feb 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Read the STAT file for the active weather file and summarize in a predefined report.
		//   The stat file that is attached may have several formats -- from evolution of the
		//   stat file from the weather converter (or others that produce a similar stat file).

		// METHODOLOGY EMPLOYED:
		//   na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace OutputReportPredefined;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const degChar( "°" );

		// LineTypes for reading the stat file
		int const StatisticsLine( 1 );
		int const LocationLine( 2 );
		int const LatLongLine( 3 );
		int const ElevationLine( 4 );
		int const StdPressureLine( 5 );
		int const DataSourceLine( 6 );
		int const WMOStationLine( 7 );
		int const DesignConditionsLine( 8 );
		int const heatingConditionsLine( 9 );
		int const coolingConditionsLine( 10 );
		int const stdHDDLine( 11 );
		int const stdCDDLine( 12 );
		int const maxDryBulbLine( 13 );
		int const minDryBulbLine( 14 );
		int const maxDewPointLine( 15 );
		int const minDewPointLine( 16 );
		int const wthHDDLine( 17 );
		int const wthCDDLine( 18 );
		int const KoppenLine( 19 );
		int const KoppenDes1Line( 20 );
		int const KoppenDes2Line( 21 );
		int const AshStdLine( 22 );
		int const AshStdDes1Line( 23 );
		int const AshStdDes2Line( 24 );
		int const AshStdDes3Line( 25 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		std::string lineIn;
		int statFile;
		bool fileExists;
		static int lineType( 0 );
		static int lineTypeinterim( 0 );
		int readStat;
		bool isASHRAE;
		bool iscalc;
		bool isKoppen;
		std::string::size_type ashPtr;
		std::string::size_type lnPtr;
		int col1;
		int col2;
		int col3;
		std::string::size_type sposlt;
		std::string::size_type eposlt;
		std::string::size_type sposlg;
		std::string::size_type eposlg;
		std::string::size_type spostz;
		std::string::size_type epostz;
		std::string ashDesYear;
		std::string ashZone; // ashrae climate zone
		std::string curNameWithSIUnits;
		std::string curNameAndUnits;
		int indexUnitConv;
		std::string storeASHRAEHDD;
		std::string storeASHRAECDD;
		bool heatingDesignlinepassed;
		bool coolingDesignlinepassed;
		bool desConditionlinepassed;

		{ IOFlags flags; gio::inquire( DataStringGlobals::inStatFileName, flags ); fileExists = flags.exists(); }
		readStat = 0;
		isASHRAE = false;
		iscalc = false;
		isKoppen = false;
		heatingDesignlinepassed = false;
		coolingDesignlinepassed = false;
		desConditionlinepassed = false;
		storeASHRAEHDD = "";
		storeASHRAECDD = "";
		lineTypeinterim = 0;
		if ( fileExists ) {
			statFile = GetNewUnitNumber();
			{ IOFlags flags; flags.ACTION( "READ" ); gio::open( statFile, DataStringGlobals::inStatFileName, flags ); readStat = flags.ios(); }
			if ( readStat != 0 ) {
				ShowFatalError( "FillWeatherPredefinedEntries: Could not open file "+DataStringGlobals::inStatFileName+" for input (read)." );
			}
			IOFlags flags;
			while ( readStat == 0 ) { //end of file, or error
				lineType = lineTypeinterim;
				gio::read( statFile, fmtA, flags ) >> lineIn; readStat = flags.ios();
				// reconcile line with different versions of stat file
				// v7.1 added version as first line.
				strip( lineIn );
				if ( has_prefix( lineIn, "Statistics" ) ) {
					lineType = StatisticsLine;
				} else if ( has_prefix( lineIn, "Location" ) ) {
					lineType = LocationLine;
				} else if ( has_prefix( lineIn, "{" ) ) {
					lineType = LatLongLine;
				} else if ( has_prefix( lineIn, "Elevation" ) ) {
					lineType = ElevationLine;
				} else if ( has_prefix( lineIn, "Standard Pressure" ) ) {
					lineType = StdPressureLine;
				} else if ( has_prefix( lineIn, "Data Source" ) ) {
					lineType = DataSourceLine;
				} else if ( has_prefix( lineIn, "WMO Station" ) ) {
					lineType = WMOStationLine;
				} else if ( has( lineIn, "Design Conditions" ) ) {
					if ( ! desConditionlinepassed ) {
						desConditionlinepassed = true;
						lineType = DesignConditionsLine;
					}
				} else if ( has_prefix( lineIn, "\tHeating" ) ) {
					if ( ! heatingDesignlinepassed ) {
						heatingDesignlinepassed = true;
						lineType = heatingConditionsLine;
					}
				} else if ( has_prefix( lineIn, "\tCooling" ) ) {
					if ( ! coolingDesignlinepassed ) {
						coolingDesignlinepassed = true;
						lineType = coolingConditionsLine;
					}
				} else if ( has( lineIn, "(standard) heating degree-days (10°C baseline)" ) ) {
					lineType = stdHDDLine;
				} else if ( has( lineIn, "(standard) cooling degree-days (18.3°C baseline)" ) ) {
					lineType = stdCDDLine;
				} else if ( has( lineIn, "Maximum Dry Bulb" ) ) {
					lineType = maxDryBulbLine;
				} else if ( has( lineIn, "Minimum Dry Bulb" ) ) {
					lineType = minDryBulbLine;
				} else if ( has( lineIn, "Maximum Dew Point" ) ) {
					lineType = maxDewPointLine;
				} else if ( has( lineIn, "Minimum Dew Point" ) ) {
					lineType = minDewPointLine;
				} else if ( has( lineIn, "(wthr file) heating degree-days (10°C baseline)" ) || has( lineIn, "heating degree-days (10°C baseline)" ) ) {
					lineType = wthHDDLine;
				} else if ( has( lineIn, "(wthr file) cooling degree-days (18°C baseline)" ) || has( lineIn, "cooling degree-days (18°C baseline)" ) ) {
					lineType = wthCDDLine;
				}
				// these not part of big if/else because sequential
				if ( lineType == KoppenDes1Line && isKoppen ) lineType = KoppenDes2Line;
				if ( lineType == KoppenLine && isKoppen ) lineType = KoppenDes1Line;
				if ( has( lineIn, "(Köppen classification)" ) ) lineType = KoppenLine;
				if ( lineType == AshStdDes2Line ) lineType = AshStdDes3Line;
				if ( lineType == AshStdDes1Line ) lineType = AshStdDes2Line;
				if ( lineType == AshStdLine ) lineType = AshStdDes1Line;
				if ( has( lineIn, "ASHRAE Standard" ) ) lineType = AshStdLine;

				{ auto const SELECT_CASE_var( lineType );
				if ( SELECT_CASE_var == StatisticsLine ) { // Statistics for USA_CA_San.Francisco_TMY2
					PreDefTableEntry( pdchWthrVal, "Reference", lineIn.substr( 15 ) );
				} else if ( SELECT_CASE_var == LocationLine ) { // Location -- SAN_FRANCISCO CA USA
					PreDefTableEntry( pdchWthrVal, "Site:Location", lineIn.substr( 11 ) );
				} else if ( SELECT_CASE_var == LatLongLine ) { //      {N 37° 37'} {W 122° 22'} {GMT -8.0 Hours}
					// find the {}
					sposlt = index( lineIn, '{' );
					eposlt = index( lineIn, '}' );
					if ( sposlt != std::string::npos && eposlt!= std::string::npos ) {
						PreDefTableEntry( pdchWthrVal, "Latitude", lineIn.substr( sposlt, eposlt - sposlt + 1 ) );
						// redefine so next scan can go with {}
						lineIn[ sposlt ] = '[';
						lineIn[ eposlt ] = ']';
					} else {
						PreDefTableEntry( pdchWthrVal, "Latitude", "not found" );
					}
					sposlg = index( lineIn, '{' );
					eposlg = index( lineIn, '}' );
					if ( sposlg != std::string::npos && eposlg != std::string::npos ) {
						PreDefTableEntry( pdchWthrVal, "Longitude", lineIn.substr( sposlg, eposlg - sposlg + 1 ) );
						// redefine so next scan can go with {}
						lineIn[ sposlg ] = '[';
						lineIn[ eposlg ] = ']';
					} else {
						PreDefTableEntry( pdchWthrVal, "Longitude", "not found" );
					}
					spostz = index( lineIn, '{' );
					epostz = index( lineIn, '}' );
					if ( spostz != std::string::npos && epostz != std::string::npos ) {
						PreDefTableEntry( pdchWthrVal, "Time Zone", lineIn.substr( spostz, epostz - spostz + 1 ) );
						// redefine so next scan can go with {}
						lineIn[ spostz ] = '[';
						lineIn[ epostz ] = ']';
					} else {
						PreDefTableEntry( pdchWthrVal, "Time Zone", "not found" );
					}
				} else if ( SELECT_CASE_var == ElevationLine ) { // Elevation --     5m above sea level
					lnPtr = index( lineIn.substr( 12 ), 'm' );
					if ( lnPtr != std::string::npos ) {
						curNameWithSIUnits = "Elevation (m) " + lineIn.substr( 12 + lnPtr + 2 );
						if ( unitsStyle == unitsStyleInchPound ) {
							LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
							PreDefTableEntry( pdchWthrVal, curNameAndUnits, RealToStr( ConvertIP( indexUnitConv, StrToReal( lineIn.substr( 12, lnPtr ) ) ), 1 ) );
						} else {
							PreDefTableEntry( pdchWthrVal, curNameWithSIUnits, lineIn.substr( 12, lnPtr ) );
						}
					} else {
						PreDefTableEntry( pdchWthrVal, "Elevation", "not found" );
					}
				} else if ( SELECT_CASE_var == StdPressureLine ) { // Standard Pressure at Elevation -- 101265Pa
					PreDefTableEntry( pdchWthrVal, "Standard Pressure at Elevation", lineIn.substr( 34 ) );
				} else if ( SELECT_CASE_var == DataSourceLine ) { // Data Source -- TMY2-23234
					PreDefTableEntry( pdchWthrVal, "Data Source", lineIn.substr( 15 ) );
				} else if ( SELECT_CASE_var == WMOStationLine ) { // WMO Station 724940
					PreDefTableEntry( pdchWthrVal, "WMO Station", lineIn.substr( 12 ) );
				} else if ( SELECT_CASE_var == DesignConditionsLine ) { //  - Using Design Conditions from "Climate Design Data 2005 ASHRAE Handbook"
					ashPtr = index( lineIn, "ASHRAE" );
					if ( ashPtr != std::string::npos ) {
						isASHRAE = true;
						iscalc = true;
						if ( ashPtr > 4u ) { //Autodesk:BoundsViolation IF block added to protect against ashPtr<=5
							ashDesYear = lineIn.substr( ashPtr - 5, 5 );
						} else {
							ashDesYear = "";
						}
						PreDefTableEntry( pdchWthrVal, "Weather File Design Conditions", "Climate Design Data " + ashDesYear + "ASHRAE Handbook" );
					} else if ( has( lineIn, "not calculated" ) || lineIn == "" ) {
						iscalc = false;
						PreDefTableEntry( pdchWthrVal, "Weather File Design Conditions", "not calculated, Number of days < 1 year" );
					} else {
						isASHRAE = false;
						iscalc = true;
						PreDefTableEntry( pdchWthrVal, "Weather File Design Conditions", "Calculated from the weather file" );
					}
				} else if ( SELECT_CASE_var == heatingConditionsLine ) { //  winter/heating design conditions
					if ( iscalc ) {
						if ( isASHRAE ) {
							if ( ashDesYear == "2001" ) {
								if ( unitsStyle == unitsStyleInchPound ) {
									curNameWithSIUnits = "Heating Design Temperature 99.6% (C)";
									LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
									PreDefTableEntry( pdchWthrVal, curNameAndUnits, RealToStr( ConvertIP( indexUnitConv, StrToReal( GetColumnUsingTabs( lineIn, 2 ) ) ), 1 ) + degChar );
									PreDefTableEntry( pdchWthrVal, "Heating Design Temperature 99% (F)", RealToStr( ConvertIP( indexUnitConv, StrToReal( GetColumnUsingTabs( lineIn, 3 ) ) ), 1 ) + degChar );
								} else {
									PreDefTableEntry( pdchWthrVal, "Heating Design Temperature 99.6% (C)", GetColumnUsingTabs( lineIn, 2 ) + degChar );
									PreDefTableEntry( pdchWthrVal, "Heating Design Temperature 99% (C)", GetColumnUsingTabs( lineIn, 3 ) + degChar );
								}
							} else { // 2005 and 2009 are the same
								if ( unitsStyle == unitsStyleInchPound ) {
									curNameWithSIUnits = "Heating Design Temperature 99.6% (C)";
									LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
									PreDefTableEntry( pdchWthrVal, curNameAndUnits, RealToStr( ConvertIP( indexUnitConv, StrToReal( GetColumnUsingTabs( lineIn, 4 ) ) ), 1 ) + degChar );
									PreDefTableEntry( pdchWthrVal, "Heating Design Temperature 99% (F)", RealToStr( ConvertIP( indexUnitConv, StrToReal( GetColumnUsingTabs( lineIn, 5 ) ) ), 1 ) + degChar );
								} else {
									PreDefTableEntry( pdchWthrVal, "Heating Design Temperature 99.6% (C)", GetColumnUsingTabs( lineIn, 4 ) + degChar );
									PreDefTableEntry( pdchWthrVal, "Heating Design Temperature 99% (C)", GetColumnUsingTabs( lineIn, 5 ) + degChar );
								}
							}
						} else { // from weather file
							if ( is_blank( GetColumnUsingTabs( lineIn, 5 ) ) ) {
								col1 = 3;
								col2 = 4;
							} else {
								col1 = 4;
								col2 = 5;
							}
							if ( unitsStyle == unitsStyleInchPound ) {
								curNameWithSIUnits = "Heating Design Temperature 99.6% (C)";
								LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
								PreDefTableEntry( pdchWthrVal, curNameAndUnits, RealToStr( ConvertIP( indexUnitConv, StrToReal( GetColumnUsingTabs( lineIn, col1 ) ) ), 1 ) + degChar );
								PreDefTableEntry( pdchWthrVal, "Heating Design Temperature 99% (F)", RealToStr( ConvertIP( indexUnitConv, StrToReal( GetColumnUsingTabs( lineIn, col2 ) ) ), 1 ) + degChar );
							} else {
								PreDefTableEntry( pdchWthrVal, "Heating Design Temperature 99.6% (C)", GetColumnUsingTabs( lineIn, col1 ) + degChar );
								PreDefTableEntry( pdchWthrVal, "Heating Design Temperature 99% (C)", GetColumnUsingTabs( lineIn, col2 ) + degChar );
							}
						}
					}
				} else if ( SELECT_CASE_var == coolingConditionsLine ) { //  summer/cooling design conditions
					if ( iscalc ) {
						if ( isASHRAE ) {
							if ( ashDesYear == "2001" ) {
								if ( unitsStyle == unitsStyleInchPound ) {
									curNameWithSIUnits = "Cooling Design Temperature 0.4% (C)";
									LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
									PreDefTableEntry( pdchWthrVal, curNameAndUnits, RealToStr( ConvertIP( indexUnitConv, StrToReal( GetColumnUsingTabs( lineIn, 2 ) ) ), 1 ) + degChar );
									PreDefTableEntry( pdchWthrVal, "Cooling Design Temperature 1% (F)", RealToStr( ConvertIP( indexUnitConv, StrToReal( GetColumnUsingTabs( lineIn, 4 ) ) ), 1 ) + degChar );
									PreDefTableEntry( pdchWthrVal, "Cooling Design Temperature 2% (F)", RealToStr( ConvertIP( indexUnitConv, StrToReal( GetColumnUsingTabs( lineIn, 6 ) ) ), 1 ) + degChar );
								} else {
									PreDefTableEntry( pdchWthrVal, "Cooling Design Temperature 0.4% (C)", GetColumnUsingTabs( lineIn, 2 ) + degChar );
									PreDefTableEntry( pdchWthrVal, "Cooling Design Temperature 1% (C)", GetColumnUsingTabs( lineIn, 4 ) + degChar );
									PreDefTableEntry( pdchWthrVal, "Cooling Design Temperature 2% (C)", GetColumnUsingTabs( lineIn, 6 ) + degChar );
								}
							} else { // 2005 and 2009 are the same
								if ( unitsStyle == unitsStyleInchPound ) {
									curNameWithSIUnits = "Cooling Design Temperature 0.4% (C)";
									LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
									PreDefTableEntry( pdchWthrVal, curNameAndUnits, RealToStr( ConvertIP( indexUnitConv, StrToReal( GetColumnUsingTabs( lineIn, 5 ) ) ), 1 ) + degChar );
									PreDefTableEntry( pdchWthrVal, "Cooling Design Temperature 1% (F)", RealToStr( ConvertIP( indexUnitConv, StrToReal( GetColumnUsingTabs( lineIn, 7 ) ) ), 1 ) + degChar );
									PreDefTableEntry( pdchWthrVal, "Cooling Design Temperature 2% (F)", RealToStr( ConvertIP( indexUnitConv, StrToReal( GetColumnUsingTabs( lineIn, 9 ) ) ), 1 ) + degChar );
								} else {
									PreDefTableEntry( pdchWthrVal, "Cooling Design Temperature 0.4% (C)", GetColumnUsingTabs( lineIn, 5 ) + degChar );
									PreDefTableEntry( pdchWthrVal, "Cooling Design Temperature 1% (C)", GetColumnUsingTabs( lineIn, 7 ) + degChar );
									PreDefTableEntry( pdchWthrVal, "Cooling Design Temperature 2% (C)", GetColumnUsingTabs( lineIn, 9 ) + degChar );
								}
							}
						} else { // from weather file
							if ( is_blank( GetColumnUsingTabs( lineIn, 6 ) ) ) {
								col1 = 3;
								col2 = 4;
								col3 = 5;
							} else {
								col1 = 4;
								col2 = 5;
								col3 = 6;
							}
							if ( unitsStyle == unitsStyleInchPound ) {
								curNameWithSIUnits = "Cooling Design Temperature 0.4% (C)";
								LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
								PreDefTableEntry( pdchWthrVal, curNameAndUnits, RealToStr( ConvertIP( indexUnitConv, StrToReal( GetColumnUsingTabs( lineIn, col1 ) ) ), 1 ) + degChar );
								PreDefTableEntry( pdchWthrVal, "Cooling Design Temperature 1% (F)", RealToStr( ConvertIP( indexUnitConv, StrToReal( GetColumnUsingTabs( lineIn, col2 ) ) ), 1 ) + degChar );
								PreDefTableEntry( pdchWthrVal, "Cooling Design Temperature 2% (F)", RealToStr( ConvertIP( indexUnitConv, StrToReal( GetColumnUsingTabs( lineIn, col3 ) ) ), 1 ) + degChar );
							} else {
								PreDefTableEntry( pdchWthrVal, "Cooling Design Temperature 0.4% (C)", GetColumnUsingTabs( lineIn, col1 ) + degChar );
								PreDefTableEntry( pdchWthrVal, "Cooling Design Temperature 1% (C)", GetColumnUsingTabs( lineIn, col2 ) + degChar );
								PreDefTableEntry( pdchWthrVal, "Cooling Design Temperature 2% (C)", GetColumnUsingTabs( lineIn, col3 ) + degChar );
							}
						}
					}
				} else if ( SELECT_CASE_var == stdHDDLine ) { //  - 1745 annual (standard) heating degree-days (10°C baseline)
					storeASHRAEHDD = lineIn.substr( 2, 4 );
				} else if ( SELECT_CASE_var == stdCDDLine ) { //  -  464 annual (standard) cooling degree-days (18.3°C baseline)
					storeASHRAECDD = lineIn.substr( 2, 4 );
				} else if ( SELECT_CASE_var == maxDryBulbLine ) { //   - Maximum Dry Bulb temperature of  35.6°C on Jul  9
					sposlt = index( lineIn, "of" );
					eposlt = index( lineIn, 'C' );
					sposlt += 2;
					eposlt -= 2;
					if ( sposlt != std::string::npos && eposlt != std::string::npos ) {
						if ( unitsStyle == unitsStyleInchPound ) {
							curNameWithSIUnits = "Maximum Dry Bulb Temperature (C)";
							LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
							PreDefTableEntry( pdchWthrVal, curNameAndUnits, RealToStr( ConvertIP( indexUnitConv, StrToReal( lineIn.substr( sposlt, eposlt - sposlt + 1 ) ) ), 1 ) + degChar );
						} else {
							PreDefTableEntry( pdchWthrVal, "Maximum Dry Bulb Temperature (C)", lineIn.substr( sposlt, eposlt - sposlt + 1 ) + degChar );
						}
					} else {
						PreDefTableEntry( pdchWthrVal, "Maximum Dry Bulb Temperature", "not found" );
					}
					sposlt = index( lineIn, "on" );
					sposlt += 2;
					if ( sposlt != std::string::npos ) {
						PreDefTableEntry( pdchWthrVal, "Maximum Dry Bulb Occurs on", lineIn.substr( sposlt ) );
					} else {
						PreDefTableEntry( pdchWthrVal, "Maximum Dry Bulb Occurs on", "not found" );
					}
				} else if ( SELECT_CASE_var == minDryBulbLine ) { //   - Minimum Dry Bulb temperature of -22.8°C on Jan  7
					sposlt = index( lineIn, "of" );
					eposlt = index( lineIn, 'C' );
					sposlt += 2;
					eposlt -= 2;
					if ( sposlt != std::string::npos && eposlt != std::string::npos ) {
						if ( unitsStyle == unitsStyleInchPound ) {
							curNameWithSIUnits = "Minimum Dry Bulb Temperature (C)";
							LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
							PreDefTableEntry( pdchWthrVal, curNameAndUnits, RealToStr( ConvertIP( indexUnitConv, StrToReal( lineIn.substr( sposlt, eposlt - sposlt + 1 ) ) ), 1 ) + degChar );
						} else {
							PreDefTableEntry( pdchWthrVal, "Minimum Dry Bulb Temperature (C)", lineIn.substr( sposlt, eposlt - sposlt + 1 ) + degChar );
						}
					} else {
						PreDefTableEntry( pdchWthrVal, "Minimum Dry Bulb Temperature", "not found" );
					}
					sposlt = index( lineIn, "on" );
					sposlt += 2;
					if ( sposlt != std::string::npos ) {
						PreDefTableEntry( pdchWthrVal, "Minimum Dry Bulb Occurs on", lineIn.substr( sposlt ) );
					} else {
						PreDefTableEntry( pdchWthrVal, "Minimum Dry Bulb Occurs on", "not found" );
					}
				} else if ( SELECT_CASE_var == maxDewPointLine ) { //   - Maximum Dew Point temperature of  25.6°C on Aug  4
					sposlt = index( lineIn, "of" );
					eposlt = index( lineIn, 'C' );
					sposlt += 2;
					eposlt -= 2;
					if ( sposlt != std::string::npos && eposlt != std::string::npos ) {
						if ( unitsStyle == unitsStyleInchPound ) {
							curNameWithSIUnits = "Maximum Dew Point Temperature (C)";
							LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
							PreDefTableEntry( pdchWthrVal, curNameAndUnits, RealToStr( ConvertIP( indexUnitConv, StrToReal( lineIn.substr( sposlt, eposlt - sposlt + 1 ) ) ), 1 ) + degChar );
						} else {
							PreDefTableEntry( pdchWthrVal, "Maximum Dew Point Temperature (C)", lineIn.substr( sposlt, eposlt - sposlt + 1 ) + degChar );
						}
					} else {
						PreDefTableEntry( pdchWthrVal, "Maximum Dew Point Temperature", "not found" );
					}
					sposlt = index( lineIn, "on" );
					sposlt += 2;
					if ( sposlt != std::string::npos ) {
						PreDefTableEntry( pdchWthrVal, "Maximum Dew Point Occurs on", lineIn.substr( sposlt ) );
					} else {
						PreDefTableEntry( pdchWthrVal, "Maximum Dew Point Occurs on", "not found" );
					}
				} else if ( SELECT_CASE_var == minDewPointLine ) { //   - Minimum Dew Point temperature of -28.9°C on Dec 31
					sposlt = index( lineIn, "of" );
					eposlt = index( lineIn, 'C' );
					sposlt += 2;
					eposlt -= 2;
					if ( sposlt != std::string::npos && eposlt != std::string::npos ) {
						if ( unitsStyle == unitsStyleInchPound ) {
							curNameWithSIUnits = "Minimum Dew Point Temperature (C)";
							LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
							PreDefTableEntry( pdchWthrVal, curNameAndUnits, RealToStr( ConvertIP( indexUnitConv, StrToReal( lineIn.substr( sposlt, eposlt - sposlt + 1 ) ) ), 1 ) + degChar );
						} else {
							PreDefTableEntry( pdchWthrVal, "Minimum Dew Point Temperature (C)", lineIn.substr( sposlt, eposlt - sposlt + 1 ) + degChar );
						}
					} else {
						PreDefTableEntry( pdchWthrVal, "Minimum Dew Point Temperature", "not found" );
					}
					sposlt = index( lineIn, "on" );
					sposlt += 2;
					if ( sposlt != std::string::npos ) {
						PreDefTableEntry( pdchWthrVal, "Minimum Dew Point Occurs on", lineIn.substr( sposlt ) );
					} else {
						PreDefTableEntry( pdchWthrVal, "Minimum Dew Point Occurs on", "not found" );
					}
				} else if ( SELECT_CASE_var == wthHDDLine ) { //  - 1745 (wthr file) annual heating degree-days (10°C baseline)
					if ( storeASHRAEHDD != "" ) {
						if ( unitsStyle == unitsStyleInchPound ) {
							curNameWithSIUnits = "Standard Heating Degree-Days - base 50°(C)";
							LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
							PreDefTableEntry( pdchWthrVal, curNameAndUnits, RealToStr( ConvertIPdelta( indexUnitConv, StrToReal( storeASHRAEHDD ) ), 1 ) );
						} else {
							PreDefTableEntry( pdchWthrVal, "Standard Heating Degree-Days (base 10°C)", storeASHRAEHDD );
						}
					} else {
						if ( unitsStyle == unitsStyleInchPound ) {
							PreDefTableEntry( pdchWthrVal, "Standard Heating Degree-Days (base 50°F)", "not found" );
						} else {
							PreDefTableEntry( pdchWthrVal, "Standard Heating Degree-Days (base 10°C)", "not found" );
						}
					}
					if ( unitsStyle == unitsStyleInchPound ) {
						curNameWithSIUnits = "Weather File Heating Degree-Days - base 50°(C)";
						LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
						PreDefTableEntry( pdchWthrVal, curNameAndUnits, RealToStr( ConvertIPdelta( indexUnitConv, StrToReal( lineIn.substr( 2, 4 ) ) ), 1 ) );
						PreDefTableEntry( pdchLeedGenData, "Heating Degree Days", RealToStr( ConvertIPdelta( indexUnitConv, StrToReal( lineIn.substr( 2, 4 ) ) ), 1 ) );
					} else {
						PreDefTableEntry( pdchWthrVal, "Weather File Heating Degree-Days (base 10°C)", lineIn.substr( 2, 4 ) );
						PreDefTableEntry( pdchLeedGenData, "Heating Degree Days", lineIn.substr( 2, 4 ) );
					}
				} else if ( SELECT_CASE_var == wthCDDLine ) { //  -  464 (wthr file) annual cooling degree-days (18°C baseline)
					if ( storeASHRAECDD != "" ) {
						if ( unitsStyle == unitsStyleInchPound ) {
							curNameWithSIUnits = "Standard Cooling Degree-Days - base 65°(C)";
							LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
							PreDefTableEntry( pdchWthrVal, curNameAndUnits, RealToStr( ConvertIPdelta( indexUnitConv, StrToReal( storeASHRAECDD ) ), 1 ) );
						} else {
							PreDefTableEntry( pdchWthrVal, "Standard Cooling Degree-Days (base 18.3°C)", storeASHRAECDD );
						}
					} else {
						if ( unitsStyle == unitsStyleInchPound ) {
							PreDefTableEntry( pdchWthrVal, "Standard Cooling Degree-Days (base 65°F)", "not found" );
						} else {
							PreDefTableEntry( pdchWthrVal, "Standard Cooling Degree-Days (base 18.3°C)", "not found" );
						}
					}
					if ( unitsStyle == unitsStyleInchPound ) {
						curNameWithSIUnits = "Weather File Cooling Degree-Days - base 64.4°(C)";
						LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
						PreDefTableEntry( pdchWthrVal, curNameAndUnits, RealToStr( ConvertIPdelta( indexUnitConv, StrToReal( lineIn.substr( 2, 4 ) ) ), 1 ) );
						PreDefTableEntry( pdchLeedGenData, "Cooling Degree Days", RealToStr( ConvertIPdelta( indexUnitConv, StrToReal( lineIn.substr( 2, 4 ) ) ), 1 ) );
					} else {
						PreDefTableEntry( pdchWthrVal, "Weather File Cooling Degree-Days (base 18°C)", lineIn.substr( 2, 4 ) );
						PreDefTableEntry( pdchLeedGenData, "Cooling Degree Days", lineIn.substr( 2, 4 ) );
					}
				} else if ( SELECT_CASE_var == KoppenLine ) { // - Climate type "BSk" (Köppen classification)
					if ( ! has( lineIn, "not shown" ) ) {
						isKoppen = true;
						if ( lineIn[ 18 ] == '"' ) { // two character classification
							PreDefTableEntry( pdchWthrVal, "Köppen Classification", lineIn.substr( 16, 2 ) );
						} else {
							PreDefTableEntry( pdchWthrVal, "Köppen Classification", lineIn.substr( 16, 3 ) );
						}
					} else {
						isKoppen = false;
						PreDefTableEntry( pdchWthrVal, "Köppen Recommendation", lineIn.substr( 2 ) );
					}
				} else if ( SELECT_CASE_var == KoppenDes1Line ) { // - Tropical monsoonal or tradewind-coastal (short dry season, lat. 5-25°)
					if ( isKoppen ) {
						PreDefTableEntry( pdchWthrVal, "Köppen Description", lineIn.substr( 2 ) );
					}
				} else if ( SELECT_CASE_var == KoppenDes2Line ) { // - Unbearably humid periods in summer, but passive cooling is possible
					if ( isKoppen ) {
						if ( len( lineIn ) > 3 ) { // avoid blank lines
							if ( lineIn.substr( 2, 2 ) != "**" ) { // avoid line with warning
								PreDefTableEntry( pdchWthrVal, "Köppen Recommendation", lineIn.substr( 2 ) );
							} else {
								PreDefTableEntry( pdchWthrVal, "Köppen Recommendation", "" );
							}
						} else {
							PreDefTableEntry( pdchWthrVal, "Köppen Recommendation", "" );
						}
					}
				} else if ( ( SELECT_CASE_var == AshStdLine ) || ( SELECT_CASE_var == AshStdDes1Line ) || ( SELECT_CASE_var == AshStdDes2Line ) || ( SELECT_CASE_var == AshStdDes3Line ) ) {
					//  - Climate type "1A" (ASHRAE Standards 90.1-2004 and 90.2-2004 Climate Zone)**
					if ( has( lineIn, "Standard" ) ) {
						ashZone = lineIn.substr( 16, 2 );
						if ( ashZone[ 1 ] == '"' ) ashZone[ 1 ] = ' ';
						PreDefTableEntry( pdchWthrVal, "ASHRAE Climate Zone", ashZone );
						PreDefTableEntry( pdchLeedGenData, "Climate Zone", ashZone );
						if ( ashZone == "1A" ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Very Hot-Humid" );
						} else if ( ashZone == "1B" ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Very Hot-Dry" );
						} else if ( ashZone == "2A" ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Hot-Humid" );
						} else if ( ashZone == "2B" ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Hot-Dry" );
						} else if ( ashZone == "3A" ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Warm-Humid" );
						} else if ( ashZone == "3B" ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Warm-Dry" );
						} else if ( ashZone == "3C" ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Warm-Marine" );
						} else if ( ashZone == "4A" ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Mixed-Humid" );
						} else if ( ashZone == "4B" ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Mixed-Dry" );
						} else if ( ashZone == "4C" ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Mixed-Marine" );
						} else if ( ashZone == "5A" ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Cool-Humid" );
						} else if ( ashZone == "5B" ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Cool-Dry" );
						} else if ( ashZone == "5C" ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Cool-Marine" );
						} else if ( ashZone == "6A" ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Cold-Humid" );
						} else if ( ashZone == "6B" ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Cold-Dry" );
						} else if ( ashZone == "7 " ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Very Cold" );
						} else if ( ashZone == "8 " ) {
							PreDefTableEntry( pdchWthrVal, "ASHRAE Description", "Subarctic" );
						}
					}
				}}
				lineIn = "";
				lineTypeinterim = 0;
				if ( lineType == AshStdDes3Line ) lineTypeinterim = 0;
				if ( lineType == AshStdDes2Line ) lineTypeinterim = AshStdDes2Line;
				if ( lineType == AshStdDes1Line ) lineTypeinterim = AshStdDes1Line;
				if ( lineType == AshStdLine ) lineTypeinterim = AshStdLine;
				if ( lineType == KoppenDes2Line ) lineTypeinterim = 0;
				if ( lineType == KoppenDes1Line ) lineTypeinterim = KoppenDes1Line;
				if ( lineType == KoppenLine ) lineTypeinterim = KoppenLine;
			}
			gio::close( statFile );
		}
	}

	std::string
	GetColumnUsingTabs(
		std::string const & inString, // Input String
		int const colNum // Column number
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   March 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Assumes that the input string contains tabs that mark the
		//   separation between columns. Returns the string that appears
		//   in the column specified.

		// METHODOLOGY EMPLOYED:
		//   na

		// Return value

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static char const tb( '\t' ); // tab character

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string::size_type startPos = 0;

		auto endPos = inString.find_first_of( tb );
		if ( colNum == 1 ) {
			if ( endPos == std::string::npos ) return inString;
			return inString.substr( startPos, endPos - startPos );
		}
		if ( endPos == std::string::npos ) return "";

		int numCols = 1;
		while ( numCols < colNum ) {
			startPos = endPos + 1;
			endPos = inString.find_first_of( tb, startPos );
			++numCols;
			if ( endPos == std::string::npos ) break;
		}
		if ( colNum > numCols ) return "";
		if ( endPos == std::string::npos ) endPos = inString.size();
		return inString.substr( startPos, endPos - startPos );
	}

	void
	FillRemainingPredefinedEntries()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   May 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Just before writing the output reports, will gather up
		//   any additional report entries for the predefined reports.

		// METHODOLOGY EMPLOYED:
		//   na

		// Using/Aliasing
		using DataHeatBalance::Zone;
		using DataHeatBalance::TotLights;
		using DataHeatBalance::Lights;
		using DataHeatBalance::ZonePreDefRep;
		using DataHeatBalance::ZnAirRpt;
		using DataHeatBalance::BuildingPreDefRep;
		using ExteriorEnergyUse::ExteriorLights;
		using ExteriorEnergyUse::NumExteriorLights;
		using ScheduleManager::ScheduleAverageHoursPerWeek;
		using ScheduleManager::GetScheduleName;
		using DataEnvironment::RunPeriodStartDayOfWeek;
		using DataEnvironment::CurrentYearIsLeapYear;
		using DataHVACGlobals::NumPrimaryAirSys;
		using DataOutputs::iNumberOfRecords;
		using DataOutputs::iNumberOfDefaultedFields;
		using DataOutputs::iTotalFieldsWithDefaults;
		using DataOutputs::iNumberOfAutoSizedFields;
		using DataOutputs::iTotalAutoSizableFields;
		using DataOutputs::iNumberOfAutoCalcedFields;
		using DataOutputs::iTotalAutoCalculatableFields;
		using ZonePlenum::NumZoneReturnPlenums;
		using ZonePlenum::NumZoneSupplyPlenums;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::WeatherFileLocationTitle;
		using General::RoundSigDigits;
		using DataAirflowNetwork::SimulateAirflowNetwork;
		using DataAirflowNetwork::AirflowNetworkControlMultizone;
		using DataAirflowNetwork::AirflowNetworkControlMultiADS;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iLight;
		int zonePt;
		int iZone;
		Real64 mult; // zone list and group multipliers
		static Real64 totalVolume( 0.0 );
		static int numUncondZones( 0 );
		static int numCondZones( 0 );
		int StartOfWeek;
		static Real64 HrsPerWeek( 0.0 );
		Real64 consumptionTotal;
		Real64 convertJtoGJ;
		// sensible heat gain report totals
		static Real64 totalHvacHt( 0.0 );
		static Real64 totalHvacCl( 0.0 );
		static Real64 totalHvacATUHt( 0.0 );
		static Real64 totalHvacATUCl( 0.0 );
		static Real64 totalSurfHt( 0.0 );
		static Real64 totalSurfCl( 0.0 );
		static Real64 totalPeoplAdd( 0.0 );
		static Real64 totalLiteAdd( 0.0 );
		static Real64 totalEquipAdd( 0.0 );
		static Real64 totalWindAdd( 0.0 );
		static Real64 totalIzaAdd( 0.0 );
		static Real64 totalInfilAdd( 0.0 );
		static Real64 totalOtherAdd( 0.0 );
		static Real64 totalEquipRem( 0.0 );
		static Real64 totalWindRem( 0.0 );
		static Real64 totalIzaRem( 0.0 );
		static Real64 totalInfilRem( 0.0 );
		static Real64 totalOtherRem( 0.0 );

		convertJtoGJ = 1.0 / 1000000000.0;
		StartOfWeek = RunPeriodStartDayOfWeek;
		if ( StartOfWeek == 0 ) StartOfWeek = 2; //if the first day of the week has not been set yet, assume monday

		//Interior Connected Lighting Power
		consumptionTotal = 0.0;
		for ( iLight = 1; iLight <= TotLights; ++iLight ) {
			zonePt = Lights( iLight ).ZonePtr;
			mult = Zone( zonePt ).Multiplier * Zone( zonePt ).ListMultiplier;
			if ( Zone( zonePt ).SystemZoneNodeNumber > 0 ) { //conditioned y/n
				PreDefTableEntry( pdchInLtCond, Lights( iLight ).Name, "Y" );
			} else {
				PreDefTableEntry( pdchInLtCond, Lights( iLight ).Name, "N" );
			}
			PreDefTableEntry( pdchInLtAvgHrSchd, Lights( iLight ).Name, ScheduleAverageHoursPerWeek( Lights( iLight ).SchedPtr, StartOfWeek, CurrentYearIsLeapYear ) );
			// average operating hours per week
			if ( gatherElapsedTimeBEPS > 0 ) {
				HrsPerWeek = 24 * 7 * Lights( iLight ).SumTimeNotZeroCons / gatherElapsedTimeBEPS;
				PreDefTableEntry( pdchInLtAvgHrOper, Lights( iLight ).Name, HrsPerWeek );
			}
			// full load hours per week
			if ( ( Lights( iLight ).DesignLevel * gatherElapsedTimeBEPS ) > 0 ) {
				HrsPerWeek = 24 * 7 * Lights( iLight ).SumConsumption / ( Lights( iLight ).DesignLevel * gatherElapsedTimeBEPS * SecInHour );
				PreDefTableEntry( pdchInLtFullLoadHrs, Lights( iLight ).Name, HrsPerWeek );
			}
			PreDefTableEntry( pdchInLtConsump, Lights( iLight ).Name, Lights( iLight ).SumConsumption * mult / 1000000000.0 );
			consumptionTotal += Lights( iLight ).SumConsumption / 1000000000.0;
		}
		PreDefTableEntry( pdchInLtConsump, "Interior Lighting Total", consumptionTotal );

		//Exterior Lighting
		consumptionTotal = 0.0;
		for ( iLight = 1; iLight <= NumExteriorLights; ++iLight ) {
			if ( ExteriorLights( iLight ).ControlMode == 1 ) { //photocell/schedule
				PreDefTableEntry( pdchExLtAvgHrSchd, ExteriorLights( iLight ).Name, ScheduleAverageHoursPerWeek( ExteriorLights( iLight ).SchedPtr, StartOfWeek, CurrentYearIsLeapYear ) );
			}
			// average operating hours per week
			if ( gatherElapsedTimeBEPS > 0 ) {
				HrsPerWeek = 24 * 7 * ExteriorLights( iLight ).SumTimeNotZeroCons / gatherElapsedTimeBEPS;
				PreDefTableEntry( pdchExLtAvgHrOper, ExteriorLights( iLight ).Name, HrsPerWeek );
			}
			// full load hours per week
			if ( ( ExteriorLights( iLight ).DesignLevel * gatherElapsedTimeBEPS ) > 0 ) {
				HrsPerWeek = 24 * 7 * ExteriorLights( iLight ).SumConsumption / ( ExteriorLights( iLight ).DesignLevel * gatherElapsedTimeBEPS * SecInHour );
				PreDefTableEntry( pdchExLtFullLoadHrs, ExteriorLights( iLight ).Name, HrsPerWeek );
			}
			PreDefTableEntry( pdchExLtConsump, ExteriorLights( iLight ).Name, ExteriorLights( iLight ).SumConsumption / 1000000000.0 );
			consumptionTotal += ExteriorLights( iLight ).SumConsumption / 1000000000.0;
		}
		PreDefTableEntry( pdchExLtConsump, "Exterior Lighting Total", consumptionTotal );

		//outside air ventilation
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			if ( Zone( iZone ).SystemZoneNodeNumber >= 0 ) { //conditioned zones only
				if ( Zone( iZone ).isNominalOccupied ) {
					//occupants
					if ( ZonePreDefRep( iZone ).NumOccAccumTime > 0 ) {
						PreDefTableEntry( pdchOaoAvgNumOcc1, Zone( iZone ).Name, ZonePreDefRep( iZone ).NumOccAccum / ZonePreDefRep( iZone ).NumOccAccumTime );
						PreDefTableEntry( pdchOaoAvgNumOcc2, Zone( iZone ).Name, ZonePreDefRep( iZone ).NumOccAccum / ZonePreDefRep( iZone ).NumOccAccumTime );
					}
					//Mechanical ventilation
					if ( Zone( iZone ).Volume > 0 && ZonePreDefRep( iZone ).TotTimeOcc > 0 ) {
						PreDefTableEntry( pdchOaoAvgMechVent, Zone( iZone ).Name, ZonePreDefRep( iZone ).MechVentVolTotal / ( ZonePreDefRep( iZone ).TotTimeOcc * Zone( iZone ).Volume * Zone( iZone ).Multiplier * Zone( iZone ).ListMultiplier ), 3 );
					}
					if ( ( Zone( iZone ).Volume > 0 ) && ( ZonePreDefRep( iZone ).TotTimeOcc > 0 ) ) {
						PreDefTableEntry( pdchOaoMinMechVent, Zone( iZone ).Name, ZonePreDefRep( iZone ).MechVentVolMin / ( Zone( iZone ).Volume * Zone( iZone ).Multiplier * Zone( iZone ).ListMultiplier ), 3 );
					}
					//infiltration
					if( Zone( iZone ).Volume > 0 && ZonePreDefRep( iZone ).TotTimeOcc > 0 ) {
						PreDefTableEntry( pdchOaoAvgInfil, Zone( iZone ).Name, ZonePreDefRep( iZone ).InfilVolTotal / ( ZonePreDefRep( iZone ).TotTimeOcc * Zone( iZone ).Volume ), 3 );
					}
					if ( ( Zone( iZone ).Volume > 0 ) && ( ZonePreDefRep( iZone ).TotTimeOcc > 0 ) ) {
						PreDefTableEntry( pdchOaoMinInfil, Zone( iZone ).Name, ZonePreDefRep( iZone ).InfilVolMin / ( Zone( iZone ).Volume ), 3 );
					}
					//AFN infiltration -- check that afn sim is being done.
					if ( SimulateAirflowNetwork < AirflowNetworkControlMultizone ) {
						ZonePreDefRep( iZone ).AFNInfilVolMin = 0.0;
						ZonePreDefRep( iZone ).AFNInfilVolTotal = 0.0;
						if ( ! ( SimulateAirflowNetwork == AirflowNetworkControlMultizone || SimulateAirflowNetwork == AirflowNetworkControlMultiADS ) ) {
							ZonePreDefRep( iZone ).AFNInfilVolMin = 0.0;
							ZonePreDefRep( iZone ).AFNInfilVolTotal = 0.0;
						}
					}
					if( Zone( iZone ).Volume > 0 && ZonePreDefRep( iZone ).TotTimeOcc > 0 ) {
						PreDefTableEntry( pdchOaoAvgAFNInfil, Zone( iZone ).Name, ZonePreDefRep( iZone ).AFNInfilVolTotal / ( ZonePreDefRep( iZone ).TotTimeOcc * Zone( iZone ).Volume ), 3 );
					}
					if ( ( Zone( iZone ).Volume > 0 ) && ( ZonePreDefRep( iZone ).TotTimeOcc > 0 ) ) {
						PreDefTableEntry( pdchOaoMinAFNInfil, Zone( iZone ).Name, ZonePreDefRep( iZone ).AFNInfilVolMin / ( Zone( iZone ).Volume ), 3 );
					}
					//simple 'ZoneVentilation'
					if( Zone( iZone ).Volume > 0 && ZonePreDefRep( iZone ).TotTimeOcc > 0 ) {
						PreDefTableEntry( pdchOaoAvgSimpVent, Zone( iZone ).Name, ZonePreDefRep( iZone ).SimpVentVolTotal / ( ZonePreDefRep( iZone ).TotTimeOcc * Zone( iZone ).Volume ), 3 );
					}
					if ( ( Zone( iZone ).Volume > 0 ) && ( ZonePreDefRep( iZone ).TotTimeOcc > 0 ) ) {
						PreDefTableEntry( pdchOaoMinSimpVent, Zone( iZone ).Name, ZonePreDefRep( iZone ).SimpVentVolMin / ( Zone( iZone ).Volume ), 3 );
					}

					//Zone volume
					PreDefTableEntry( pdchOaoZoneVol1, Zone( iZone ).Name, Zone( iZone ).Volume );
					PreDefTableEntry( pdchOaoZoneVol2, Zone( iZone ).Name, Zone( iZone ).Volume );
					totalVolume += Zone( iZone ).Volume;
				}
			}
		}

		// Add the number of central air distributions system to the count report
		PreDefTableEntry( pdchHVACcntVal, "HVAC Air Loops", NumPrimaryAirSys );
		// Add the number of conditioned and unconditioned zones to the count report
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			if ( Zone( iZone ).SystemZoneNodeNumber > 0 ) { //conditioned zones only
				++numCondZones;
			} else {
				++numUncondZones;
			}
		}
		PreDefTableEntry( pdchHVACcntVal, "Conditioned Zones", numCondZones );
		PreDefTableEntry( pdchHVACcntVal, "Unconditioned Zones", numUncondZones );
		//add the number of plenums to the count report
		PreDefTableEntry( pdchHVACcntVal, "Supply Plenums", NumZoneSupplyPlenums );
		PreDefTableEntry( pdchHVACcntVal, "Return Plenums", NumZoneReturnPlenums );

		// Started to create a total row but did not fully implement
		//CALL PreDefTableEntry(pdchOaoZoneVol1,'Total OA Avg', totalVolume)
		//CALL PreDefTableEntry(pdchOaoZoneVol2,'Total OA Min', totalVolume)

		// Add footnote saying if it is a design day or other kind of environment

		// Field counts
		PreDefTableEntry( pdchFieldCntVal, "IDF Objects", iNumberOfRecords );
		PreDefTableEntry( pdchFieldCntVal, "Defaulted Fields", iNumberOfDefaultedFields );
		PreDefTableEntry( pdchFieldCntVal, "Fields with Defaults", iTotalFieldsWithDefaults );
		PreDefTableEntry( pdchFieldCntVal, "Autosized Fields", iNumberOfAutoSizedFields );
		PreDefTableEntry( pdchFieldCntVal, "Autosizable Fields", iTotalAutoSizableFields );
		PreDefTableEntry( pdchFieldCntVal, "Autocalculated Fields", iNumberOfAutoCalcedFields );
		PreDefTableEntry( pdchFieldCntVal, "Autocalculatable Fields", iTotalAutoCalculatableFields );

		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			//annual
			// PreDefTableEntry( pdchSHGSAnHvacHt, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnHvacHt * convertJtoGJ, 3 );
			// PreDefTableEntry( pdchSHGSAnHvacCl, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnHvacCl * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnHvacHt, Zone( iZone ).Name, ( ZonePreDefRep( iZone ).SHGSAnHvacHt -
				ZonePreDefRep( iZone ).SHGSAnHvacATUHt ) * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnHvacCl, Zone( iZone ).Name, ( ZonePreDefRep( iZone ).SHGSAnHvacCl -
				ZonePreDefRep( iZone ).SHGSAnHvacATUCl ) * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnHvacATUHt, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnHvacATUHt * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnHvacATUCl, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnHvacATUCl * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnSurfHt, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnSurfHt * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnSurfCl, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnSurfCl * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnPeoplAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnPeoplAdd * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnLiteAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnLiteAdd * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnEquipAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnEquipAdd * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnWindAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnWindAdd * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnIzaAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnIzaAdd * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnInfilAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnInfilAdd * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnOtherAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnOtherAdd * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnEquipRem, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnEquipRem * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnWindRem, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnWindRem * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnIzaRem, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnIzaRem * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnInfilRem, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnInfilRem * convertJtoGJ, 3 );
			PreDefTableEntry( pdchSHGSAnOtherRem, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSAnOtherRem * convertJtoGJ, 3 );
			//peak cooling
			PreDefTableEntry( pdchSHGSClTimePeak, Zone( iZone ).Name, DateToString( ZonePreDefRep( iZone ).clPtTimeStamp ) );
			// PreDefTableEntry( pdchSHGSClHvacHt, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClHvacHt );
			// PreDefTableEntry( pdchSHGSClHvacCl, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClHvacCl );
			PreDefTableEntry( pdchSHGSClHvacHt, Zone( iZone ).Name, ( ZonePreDefRep( iZone ).SHGSClHvacHt - ZonePreDefRep( iZone ).SHGSClHvacATUHt ) );
			PreDefTableEntry( pdchSHGSClHvacCl, Zone( iZone ).Name, ( ZonePreDefRep( iZone ).SHGSClHvacCl - ZonePreDefRep( iZone ).SHGSClHvacATUCl ) );
			PreDefTableEntry( pdchSHGSClHvacATUHt, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClHvacATUHt );
			PreDefTableEntry( pdchSHGSClHvacATUCl, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClHvacATUCl );
			PreDefTableEntry( pdchSHGSClSurfHt, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClSurfHt );
			PreDefTableEntry( pdchSHGSClSurfCl, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClSurfCl );
			PreDefTableEntry( pdchSHGSClPeoplAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClPeoplAdd );
			PreDefTableEntry( pdchSHGSClLiteAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClLiteAdd );
			PreDefTableEntry( pdchSHGSClEquipAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClEquipAdd );
			PreDefTableEntry( pdchSHGSClWindAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClWindAdd );
			PreDefTableEntry( pdchSHGSClIzaAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClIzaAdd );
			PreDefTableEntry( pdchSHGSClInfilAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClInfilAdd );
			PreDefTableEntry( pdchSHGSClOtherAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClOtherAdd );
			PreDefTableEntry( pdchSHGSClEquipRem, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClEquipRem );
			PreDefTableEntry( pdchSHGSClWindRem, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClWindRem );
			PreDefTableEntry( pdchSHGSClIzaRem, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClIzaRem );
			PreDefTableEntry( pdchSHGSClInfilRem, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClInfilRem );
			PreDefTableEntry( pdchSHGSClOtherRem, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSClOtherRem );
			//peak heating
			PreDefTableEntry( pdchSHGSHtTimePeak, Zone( iZone ).Name, DateToString( ZonePreDefRep( iZone ).htPtTimeStamp ) );
			// PreDefTableEntry( pdchSHGSHtHvacHt, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtHvacHt );
			// PreDefTableEntry( pdchSHGSHtHvacCl, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtHvacCl );
			PreDefTableEntry( pdchSHGSHtHvacHt, Zone( iZone ).Name, ( ZonePreDefRep( iZone ).SHGSHtHvacHt - ZonePreDefRep( iZone ).SHGSHtHvacATUHt ) );
			PreDefTableEntry( pdchSHGSHtHvacCl, Zone( iZone ).Name, ( ZonePreDefRep( iZone ).SHGSHtHvacCl - ZonePreDefRep( iZone ).SHGSHtHvacATUCl ) );
			PreDefTableEntry( pdchSHGSHtHvacATUHt, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtHvacATUHt );
			PreDefTableEntry( pdchSHGSHtHvacATUCl, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtHvacATUCl );
			PreDefTableEntry( pdchSHGSHtSurfHt, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtSurfHt );
			PreDefTableEntry( pdchSHGSHtSurfCl, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtSurfCl );
			PreDefTableEntry( pdchSHGSHtPeoplAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtPeoplAdd );
			PreDefTableEntry( pdchSHGSHtLiteAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtLiteAdd );
			PreDefTableEntry( pdchSHGSHtEquipAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtEquipAdd );
			PreDefTableEntry( pdchSHGSHtWindAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtWindAdd );
			PreDefTableEntry( pdchSHGSHtIzaAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtIzaAdd );
			PreDefTableEntry( pdchSHGSHtInfilAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtInfilAdd );
			PreDefTableEntry( pdchSHGSHtOtherAdd, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtOtherAdd );
			PreDefTableEntry( pdchSHGSHtEquipRem, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtEquipRem );
			PreDefTableEntry( pdchSHGSHtWindRem, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtWindRem );
			PreDefTableEntry( pdchSHGSHtIzaRem, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtIzaRem );
			PreDefTableEntry( pdchSHGSHtInfilRem, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtInfilRem );
			PreDefTableEntry( pdchSHGSHtOtherRem, Zone( iZone ).Name, ZonePreDefRep( iZone ).SHGSHtOtherRem );
		}
		//totals for annual report
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			totalHvacHt += ZonePreDefRep( iZone ).SHGSAnHvacHt;
			totalHvacCl += ZonePreDefRep( iZone ).SHGSAnHvacCl;
			totalHvacATUHt += ZonePreDefRep( iZone ).SHGSAnHvacATUHt;
			totalHvacATUCl += ZonePreDefRep( iZone ).SHGSAnHvacATUCl;
			totalSurfHt += ZonePreDefRep( iZone ).SHGSAnSurfHt;
			totalSurfCl += ZonePreDefRep( iZone ).SHGSAnSurfCl;
			totalPeoplAdd += ZonePreDefRep( iZone ).SHGSAnPeoplAdd;
			totalLiteAdd += ZonePreDefRep( iZone ).SHGSAnLiteAdd;
			totalEquipAdd += ZonePreDefRep( iZone ).SHGSAnEquipAdd;
			totalWindAdd += ZonePreDefRep( iZone ).SHGSAnWindAdd;
			totalIzaAdd += ZonePreDefRep( iZone ).SHGSAnIzaAdd;
			totalInfilAdd += ZonePreDefRep( iZone ).SHGSAnInfilAdd;
			totalOtherAdd += ZonePreDefRep( iZone ).SHGSAnOtherAdd;
			totalEquipRem += ZonePreDefRep( iZone ).SHGSAnEquipRem;
			totalWindRem += ZonePreDefRep( iZone ).SHGSAnWindRem;
			totalIzaRem += ZonePreDefRep( iZone ).SHGSAnIzaRem;
			totalInfilRem += ZonePreDefRep( iZone ).SHGSAnInfilRem;
			totalOtherRem += ZonePreDefRep( iZone ).SHGSAnOtherRem;
		}
		// PreDefTableEntry( pdchSHGSAnHvacHt, "Total Facility", totalHvacHt * convertJtoGJ, 3 );
		// PreDefTableEntry( pdchSHGSAnHvacCl, "Total Facility", totalHvacCl * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnHvacHt, "Total Facility", ( totalHvacHt - totalHvacATUHt ) * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnHvacCl, "Total Facility", ( totalHvacCl - totalHvacATUCl ) * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnHvacATUHt, "Total Facility", totalHvacATUHt * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnHvacATUCl, "Total Facility", totalHvacATUCl * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnSurfHt, "Total Facility", totalSurfHt * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnSurfCl, "Total Facility", totalSurfCl * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnPeoplAdd, "Total Facility", totalPeoplAdd * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnLiteAdd, "Total Facility", totalLiteAdd * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnEquipAdd, "Total Facility", totalEquipAdd * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnWindAdd, "Total Facility", totalWindAdd * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnIzaAdd, "Total Facility", totalIzaAdd * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnInfilAdd, "Total Facility", totalInfilAdd * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnOtherAdd, "Total Facility", totalOtherAdd * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnEquipRem, "Total Facility", totalEquipRem * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnWindRem, "Total Facility", totalWindRem * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnIzaRem, "Total Facility", totalIzaRem * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnInfilRem, "Total Facility", totalInfilRem * convertJtoGJ, 3 );
		PreDefTableEntry( pdchSHGSAnOtherRem, "Total Facility", totalOtherRem * convertJtoGJ, 3 );
		// building level results for peak cooling
		PreDefTableEntry( pdchSHGSClTimePeak, "Total Facility", DateToString( BuildingPreDefRep.clPtTimeStamp ) );
		// PreDefTableEntry( pdchSHGSClHvacHt, "Total Facility", BuildingPreDefRep.SHGSClHvacHt );
		// PreDefTableEntry( pdchSHGSClHvacCl, "Total Facility", BuildingPreDefRep.SHGSClHvacCl );
		PreDefTableEntry( pdchSHGSClHvacHt, "Total Facility", ( BuildingPreDefRep.SHGSClHvacHt - BuildingPreDefRep.SHGSClHvacATUHt ) );
		PreDefTableEntry( pdchSHGSClHvacCl, "Total Facility", ( BuildingPreDefRep.SHGSClHvacCl - BuildingPreDefRep.SHGSClHvacATUCl ) );
		PreDefTableEntry( pdchSHGSClHvacATUHt, "Total Facility", BuildingPreDefRep.SHGSClHvacATUHt );
		PreDefTableEntry( pdchSHGSClHvacATUCl, "Total Facility", BuildingPreDefRep.SHGSClHvacATUCl );
		PreDefTableEntry( pdchSHGSClSurfHt, "Total Facility", BuildingPreDefRep.SHGSClSurfHt );
		PreDefTableEntry( pdchSHGSClSurfCl, "Total Facility", BuildingPreDefRep.SHGSClSurfCl );
		PreDefTableEntry( pdchSHGSClPeoplAdd, "Total Facility", BuildingPreDefRep.SHGSClPeoplAdd );
		PreDefTableEntry( pdchSHGSClLiteAdd, "Total Facility", BuildingPreDefRep.SHGSClLiteAdd );
		PreDefTableEntry( pdchSHGSClEquipAdd, "Total Facility", BuildingPreDefRep.SHGSClEquipAdd );
		PreDefTableEntry( pdchSHGSClWindAdd, "Total Facility", BuildingPreDefRep.SHGSClWindAdd );
		PreDefTableEntry( pdchSHGSClIzaAdd, "Total Facility", BuildingPreDefRep.SHGSClIzaAdd );
		PreDefTableEntry( pdchSHGSClInfilAdd, "Total Facility", BuildingPreDefRep.SHGSClInfilAdd );
		PreDefTableEntry( pdchSHGSClOtherAdd, "Total Facility", BuildingPreDefRep.SHGSClOtherAdd );
		PreDefTableEntry( pdchSHGSClEquipRem, "Total Facility", BuildingPreDefRep.SHGSClEquipRem );
		PreDefTableEntry( pdchSHGSClWindRem, "Total Facility", BuildingPreDefRep.SHGSClWindRem );
		PreDefTableEntry( pdchSHGSClIzaRem, "Total Facility", BuildingPreDefRep.SHGSClIzaRem );
		PreDefTableEntry( pdchSHGSClInfilRem, "Total Facility", BuildingPreDefRep.SHGSClInfilRem );
		PreDefTableEntry( pdchSHGSClOtherRem, "Total Facility", BuildingPreDefRep.SHGSClOtherRem );
		// building level results for peak heating
		PreDefTableEntry( pdchSHGSHtTimePeak, "Total Facility", DateToString( BuildingPreDefRep.htPtTimeStamp ) );
		// PreDefTableEntry( pdchSHGSHtHvacHt, "Total Facility", BuildingPreDefRep.SHGSHtHvacHt );
		// PreDefTableEntry( pdchSHGSHtHvacCl, "Total Facility", BuildingPreDefRep.SHGSHtHvacCl );
		PreDefTableEntry( pdchSHGSHtHvacHt, "Total Facility", ( BuildingPreDefRep.SHGSHtHvacHt - BuildingPreDefRep.SHGSHtHvacATUHt ) );
		PreDefTableEntry( pdchSHGSHtHvacCl, "Total Facility", ( BuildingPreDefRep.SHGSHtHvacCl - BuildingPreDefRep.SHGSHtHvacATUCl ) );
		PreDefTableEntry( pdchSHGSHtHvacATUHt, "Total Facility", BuildingPreDefRep.SHGSHtHvacATUHt );
		PreDefTableEntry( pdchSHGSHtHvacATUCl, "Total Facility", BuildingPreDefRep.SHGSHtHvacATUCl );
		PreDefTableEntry( pdchSHGSHtSurfHt, "Total Facility", BuildingPreDefRep.SHGSHtSurfHt );
		PreDefTableEntry( pdchSHGSHtSurfCl, "Total Facility", BuildingPreDefRep.SHGSHtSurfCl );
		PreDefTableEntry( pdchSHGSHtPeoplAdd, "Total Facility", BuildingPreDefRep.SHGSHtPeoplAdd );
		PreDefTableEntry( pdchSHGSHtLiteAdd, "Total Facility", BuildingPreDefRep.SHGSHtLiteAdd );
		PreDefTableEntry( pdchSHGSHtEquipAdd, "Total Facility", BuildingPreDefRep.SHGSHtEquipAdd );
		PreDefTableEntry( pdchSHGSHtWindAdd, "Total Facility", BuildingPreDefRep.SHGSHtWindAdd );
		PreDefTableEntry( pdchSHGSHtIzaAdd, "Total Facility", BuildingPreDefRep.SHGSHtIzaAdd );
		PreDefTableEntry( pdchSHGSHtInfilAdd, "Total Facility", BuildingPreDefRep.SHGSHtInfilAdd );
		PreDefTableEntry( pdchSHGSHtOtherAdd, "Total Facility", BuildingPreDefRep.SHGSHtOtherAdd );
		PreDefTableEntry( pdchSHGSHtEquipRem, "Total Facility", BuildingPreDefRep.SHGSHtEquipRem );
		PreDefTableEntry( pdchSHGSHtWindRem, "Total Facility", BuildingPreDefRep.SHGSHtWindRem );
		PreDefTableEntry( pdchSHGSHtIzaRem, "Total Facility", BuildingPreDefRep.SHGSHtIzaRem );
		PreDefTableEntry( pdchSHGSHtInfilRem, "Total Facility", BuildingPreDefRep.SHGSHtInfilRem );
		PreDefTableEntry( pdchSHGSHtOtherRem, "Total Facility", BuildingPreDefRep.SHGSHtOtherRem );

		// LEED Report
		// 1.1A-General Information
		//CALL PreDefTableEntry(pdchLeedGenData,'Principal Heating Source','-')
		if ( EnvironmentName == WeatherFileLocationTitle ) {
			PreDefTableEntry( pdchLeedGenData, "Weather File", EnvironmentName );
		} else {
			PreDefTableEntry( pdchLeedGenData, "Weather File", EnvironmentName + " ** " + WeatherFileLocationTitle );
		}

		//CALL PreDefTableEntry(pdchLeedGenData,'Climate Zone','-')
		//CALL PreDefTableEntry(pdchLeedGenData,'Heating Degree Days','-')
		//CALL PreDefTableEntry(pdchLeedGenData,'Cooling Degree Days','-')
		PreDefTableEntry( pdchLeedGenData, "HDD and CDD data source", "Weather File Stat" );
		if ( unitsStyle == unitsStyleInchPound ) {
			PreDefTableEntry( pdchLeedGenData, "Total gross floor area [ft2]", "-" );
		} else {
			PreDefTableEntry( pdchLeedGenData, "Total gross floor area [m2]", "-" );
		}
	}

	void
	WriteMonthlyTables()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2003
		//       MODIFIED       January 2010, Kyle Benne
		//                      Added SQLite output
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Set up the monthly tabular report results

		// METHODOLOGY EMPLOYED:
		//   Creates several arrays that are passed to the WriteTable
		//   routine.  All arrays are strings so numbers need to be
		//   converted prior to calling WriteTable.

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_string columnHead;
		Array1D_int columnWidth;
		Array1D_string rowHead( 16 );
		Array2D_string tableBody;
		Array1D_string aggString( 13 );
		std::string curAggString;
		int iInput;
		int jTable;
		int kColumn;
		int lMonth;
		int curTable;
		int curCol;
		Real64 curVal;
		Real64 curConversionFactor;
		static Real64 curConversionOffset( 0.0 );
		int columnUsedCount;
		int columnRecount;
		int digitsShown;
		Real64 minVal;
		Real64 maxVal;
		Real64 sumVal;
		Real64 sumDuration;
		std::string curUnits;
		std::string energyUnitsString;
		Real64 energyUnitsConversionFactor;
		int indexUnitConv;
		std::string varNameWithUnits;
		Real64 veryLarge;
		Real64 verySmall;

		static Real64 const storedMaxVal( std::numeric_limits< Real64 >::max() );
		static Real64 const storedMinVal( std::numeric_limits< Real64 >::lowest() );

		rowHead( 1 ) = "January";
		rowHead( 2 ) = "February";
		rowHead( 3 ) = "March";
		rowHead( 4 ) = "April";
		rowHead( 5 ) = "May";
		rowHead( 6 ) = "June";
		rowHead( 7 ) = "July";
		rowHead( 8 ) = "August";
		rowHead( 9 ) = "September";
		rowHead( 10 ) = "October";
		rowHead( 11 ) = "November";
		rowHead( 12 ) = "December";
		rowHead( 13 ) = "";
		rowHead( 14 ) = "Annual Sum or Average";
		rowHead( 15 ) = "Minimum of Months";
		rowHead( 16 ) = "Maximum of Months";

		aggString( aggTypeSumOrAvg ) = "";
		aggString( aggTypeMaximum ) = " Maximum ";
		aggString( aggTypeMinimum ) = " MINIMUM ";
		aggString( aggTypeValueWhenMaxMin ) = " AT MAX/MIN ";
		aggString( aggTypeHoursZero ) = " HOURS ZERO ";
		aggString( aggTypeHoursNonZero ) = " HOURS NON-ZERO ";
		aggString( aggTypeHoursPositive ) = " HOURS POSITIVE ";
		aggString( aggTypeHoursNonPositive ) = " HOURS NON-POSITIVE ";
		aggString( aggTypeHoursNegative ) = " HOURS NEGATIVE ";
		aggString( aggTypeHoursNonNegative ) = " HOURS NON-NEGATIVE ";
		aggString( aggTypeSumOrAverageHoursShown ) = " FOR HOURS SHOWN ";
		aggString( aggTypeMaximumDuringHoursShown ) = " MAX FOR HOURS SHOWN ";
		aggString( aggTypeMinimumDuringHoursShown ) = " MIN FOR HOURS SHOWN ";

		veryLarge = 1.0E280;
		verySmall = -1.0E280;

		// set the unit conversion
		if ( unitsStyle == unitsStyleNone ) {
			energyUnitsString = "J";
			energyUnitsConversionFactor = 1.0;
		} else if ( unitsStyle == unitsStyleJtoKWH ) {
			energyUnitsString = "kWh";
			energyUnitsConversionFactor = 1.0 / 3600000.0;
		} else if ( unitsStyle == unitsStyleJtoMJ ) {
			energyUnitsString = "MJ";
			energyUnitsConversionFactor = 1.0 / 1000000.0;
		} else if ( unitsStyle == unitsStyleJtoGJ ) {
			energyUnitsString = "GJ";
			energyUnitsConversionFactor = 1.0 / 1000000000.0;
		} else { // Should never happen but assures compilers of initialization
			energyUnitsString = "J";
			energyUnitsConversionFactor = 1.0;
		}

		// loop through each input to get the name of the tables
		for ( iInput = 1; iInput <= MonthlyInputCount; ++iInput ) {
			// loop through each report and
			digitsShown = MonthlyInput( iInput ).showDigits;
			for ( jTable = 1; jTable <= MonthlyInput( iInput ).numTables; ++jTable ) {
				curTable = jTable + MonthlyInput( iInput ).firstTable - 1;
				// first loop through and count how many 'columns' are defined
				// since max and min actually define two columns (the value
				// and the timestamp).
				columnUsedCount = 0;
				for ( kColumn = 1; kColumn <= MonthlyTables( curTable ).numColumns; ++kColumn ) {
					curCol = kColumn + MonthlyTables( curTable ).firstColumn - 1;
					{ auto const SELECT_CASE_var( MonthlyColumns( curCol ).aggType );
					if ( ( SELECT_CASE_var == aggTypeSumOrAvg ) || ( SELECT_CASE_var == aggTypeValueWhenMaxMin ) || ( SELECT_CASE_var == aggTypeHoursZero ) || ( SELECT_CASE_var == aggTypeHoursNonZero ) || ( SELECT_CASE_var == aggTypeHoursPositive ) || ( SELECT_CASE_var == aggTypeHoursNonPositive ) || ( SELECT_CASE_var == aggTypeHoursNegative ) || ( SELECT_CASE_var == aggTypeHoursNonNegative ) || ( SELECT_CASE_var == aggTypeSumOrAverageHoursShown ) ) {
						++columnUsedCount;
					} else if ( ( SELECT_CASE_var == aggTypeMaximum ) || ( SELECT_CASE_var == aggTypeMinimum ) || ( SELECT_CASE_var == aggTypeMaximumDuringHoursShown ) || ( SELECT_CASE_var == aggTypeMinimumDuringHoursShown ) ) {
						columnUsedCount += 2;
					}}
				} //jColumn
				columnHead.allocate( columnUsedCount );
				columnWidth.dimension( columnUsedCount, 14 ); //array assignment - same for all columns
				tableBody.allocate( columnUsedCount, 16 );
				tableBody = ""; //set entire table to blank as default
				columnRecount = 0;
				for ( kColumn = 1; kColumn <= MonthlyTables( curTable ).numColumns; ++kColumn ) {
					curCol = kColumn + MonthlyTables( curTable ).firstColumn - 1;
					curAggString = aggString( MonthlyColumns( curCol ).aggType );
					if ( len( curAggString ) > 0 ) {
						curAggString = " {" + stripped( curAggString ) + '}';
					}
					//do the unit conversions
					if ( unitsStyle == unitsStyleInchPound ) {
						varNameWithUnits = MonthlyColumns( curCol ).varName + '[' + MonthlyColumns( curCol ).units + ']';
						LookupSItoIP( varNameWithUnits, indexUnitConv, curUnits );
						GetUnitConversion( indexUnitConv, curConversionFactor, curConversionOffset, curUnits );
					} else { //just do the Joule conversion
						//if units is in Joules, convert if specified
						if ( SameString( MonthlyColumns( curCol ).units, "J" ) ) {
							curUnits = energyUnitsString;
							curConversionFactor = energyUnitsConversionFactor;
							curConversionOffset = 0.0;
						} else { //if not joules don't perform conversion
							curUnits = MonthlyColumns( curCol ).units;
							curConversionFactor = 1.0;
							curConversionOffset = 0.0;
						}
					}
					{ auto const SELECT_CASE_var( MonthlyColumns( curCol ).aggType );
					if ( ( SELECT_CASE_var == aggTypeSumOrAvg ) || ( SELECT_CASE_var == aggTypeSumOrAverageHoursShown ) ) {
						++columnRecount;
						// put in the name of the variable for the column
						columnHead( columnRecount ) = MonthlyColumns( curCol ).varName + curAggString + " [" + curUnits + ']';
						sumVal = 0.0;
						sumDuration = 0.0;
						minVal = storedMaxVal;
						maxVal = storedMinVal;
						for ( lMonth = 1; lMonth <= 12; ++lMonth ) {
							if ( MonthlyColumns( curCol ).avgSum == isAverage ) { // if it is a average variable divide by duration
								if ( MonthlyColumns( curCol ).duration( lMonth ) != 0 ) {
									curVal = ( ( MonthlyColumns( curCol ).reslt( lMonth ) / MonthlyColumns( curCol ).duration( lMonth ) ) * curConversionFactor ) + curConversionOffset;
								} else {
									curVal = 0.0;
								}
								sumVal += ( MonthlyColumns( curCol ).reslt( lMonth ) * curConversionFactor ) + curConversionOffset;
								sumDuration += MonthlyColumns( curCol ).duration( lMonth );
							} else {
								curVal = ( MonthlyColumns( curCol ).reslt( lMonth ) * curConversionFactor ) + curConversionOffset;
								sumVal += curVal;
							}
							if ( IsMonthGathered( lMonth ) ) {
								tableBody( columnRecount, lMonth ) = RealToStr( curVal, digitsShown );
								if ( curVal > maxVal ) maxVal = curVal;
								if ( curVal < minVal ) minVal = curVal;
							} else {
								tableBody( columnRecount, lMonth ) = "-";
							}
						} //lMonth
						// add the summary to bottom
						if ( MonthlyColumns( curCol ).avgSum == isAverage ) { // if it is a average variable divide by duration
							if ( sumDuration > 0 ) {
								tableBody( columnRecount, 14 ) = RealToStr( sumVal / sumDuration, digitsShown );
							} else {
								tableBody( columnRecount, 14 ) = "";
							}
						} else {
							tableBody( columnRecount, 14 ) = RealToStr( sumVal, digitsShown );
						}
						if ( minVal != storedMaxVal ) {
							tableBody( columnRecount, 15 ) = RealToStr( minVal, digitsShown );
						}
						if ( maxVal != storedMinVal ) {
							tableBody( columnRecount, 16 ) = RealToStr( maxVal, digitsShown );
						}
					} else if ( ( SELECT_CASE_var == aggTypeHoursZero ) || ( SELECT_CASE_var == aggTypeHoursNonZero ) || ( SELECT_CASE_var == aggTypeHoursPositive ) || ( SELECT_CASE_var == aggTypeHoursNonPositive ) || ( SELECT_CASE_var == aggTypeHoursNegative ) || ( SELECT_CASE_var == aggTypeHoursNonNegative ) ) {

						++columnRecount;
						// put in the name of the variable for the column
						columnHead( columnRecount ) = MonthlyColumns( curCol ).varName + curAggString + " [HOURS]";
						sumVal = 0.0;
						minVal = storedMaxVal;
						maxVal = storedMinVal;
						for ( lMonth = 1; lMonth <= 12; ++lMonth ) {
							curVal = MonthlyColumns( curCol ).reslt( lMonth );
							if ( IsMonthGathered( lMonth ) ) {
								tableBody( columnRecount, lMonth ) = RealToStr( curVal, digitsShown );
								sumVal += curVal;
								if ( curVal > maxVal ) maxVal = curVal;
								if ( curVal < minVal ) minVal = curVal;
							} else {
								tableBody( columnRecount, lMonth ) = "-";
							}
						} //lMonth
						// add the summary to bottom
						tableBody( columnRecount, 14 ) = RealToStr( sumVal, digitsShown );
						if ( minVal != storedMaxVal ) {
							tableBody( columnRecount, 15 ) = RealToStr( minVal, digitsShown );
						}
						if ( maxVal != storedMinVal ) {
							tableBody( columnRecount, 16 ) = RealToStr( maxVal, digitsShown );
						}
					} else if ( SELECT_CASE_var == aggTypeValueWhenMaxMin ) {
						++columnRecount;
						if ( MonthlyColumns( curCol ).avgSum == isSum ) {
							curUnits += "/s";
						}
						if ( SameString( curUnits, "J/s" ) ) {
							curUnits = "W";
						}
						//CR7783 fix
						if ( SameString( curUnits, "kWh/s" ) ) {
							curUnits = "W";
							curConversionFactor *= 3600000.0;
						}
						if ( SameString( curUnits, "GJ/s" ) ) {
							curUnits = "kW";
							curConversionFactor *= 1000000.0;
						}
						if ( SameString( curUnits, "MJ/s" ) ) {
							curUnits = "kW";
							curConversionFactor *= 1000.0;
						}
						if ( SameString( curUnits, "therm/s" ) ) {
							curUnits = "kBtu/h";
							curConversionFactor *= 360000.0;
						}
						if ( SameString( curUnits, "kBtu/s" ) ) {
							curUnits = "kBtu/h";
							curConversionFactor *= 3600.0;
						}
						if ( SameString( curUnits, "ton-hrs/s" ) ) {
							curUnits = "ton";
							curConversionFactor *= 3600.0;
						}
						columnHead( columnRecount ) = MonthlyColumns( curCol ).varName + curAggString + " [" + curUnits + ']';
						minVal = storedMaxVal;
						maxVal = storedMinVal;
						for ( lMonth = 1; lMonth <= 12; ++lMonth ) {
							curVal = MonthlyColumns( curCol ).reslt( lMonth ) * curConversionFactor + curConversionOffset;
							if ( IsMonthGathered( lMonth ) ) {
								tableBody( columnRecount, lMonth ) = RealToStr( curVal, digitsShown );
								if ( curVal > maxVal ) maxVal = curVal;
								if ( curVal < minVal ) minVal = curVal;
							} else {
								tableBody( columnRecount, lMonth ) = "-";
							}
						} //lMonth
						// add the summary to bottom
						if ( minVal != storedMaxVal ) {
							tableBody( columnRecount, 15 ) = RealToStr( minVal, digitsShown );
						}
						if ( maxVal != storedMinVal ) {
							tableBody( columnRecount, 16 ) = RealToStr( maxVal, digitsShown );
						}
					} else if ( ( SELECT_CASE_var == aggTypeMaximum ) || ( SELECT_CASE_var == aggTypeMinimum ) || ( SELECT_CASE_var == aggTypeMaximumDuringHoursShown ) || ( SELECT_CASE_var == aggTypeMinimumDuringHoursShown ) ) {
						columnRecount += 2;
						// put in the name of the variable for the column
						if ( MonthlyColumns( curCol ).avgSum == isSum ) { // if it is a summed variable
							curUnits += "/s";
						}
						if ( SameString( curUnits, "J/s" ) ) {
							curUnits = "W";
						}
						//CR7783 fix
						if ( SameString( curUnits, "kWh/s" ) ) {
							curUnits = "W";
							curConversionFactor *= 3600000.0;
						}
						if ( SameString( curUnits, "GJ/s" ) ) {
							curUnits = "kW";
							curConversionFactor *= 1000000.0;
						}
						if ( SameString( curUnits, "MJ/s" ) ) {
							curUnits = "kW";
							curConversionFactor *= 1000.0;
						}
						if ( SameString( curUnits, "therm/s" ) ) {
							curUnits = "kBtu/h";
							curConversionFactor *= 360000.0;
						}
						if ( SameString( curUnits, "kBtu/s" ) ) {
							curUnits = "kBtu/h";
							curConversionFactor *= 3600.0;
						}
						if ( SameString( curUnits, "ton-hrs/s" ) ) {
							curUnits = "ton";
							curConversionFactor *= 3600.0;
						}
						columnHead( columnRecount - 1 ) = MonthlyColumns( curCol ).varName + curAggString + '[' + curUnits + ']';
						columnHead( columnRecount ) = MonthlyColumns( curCol ).varName + " {TIMESTAMP} ";
						minVal = storedMaxVal;
						maxVal = storedMinVal;
						for ( lMonth = 1; lMonth <= 12; ++lMonth ) {
							if ( IsMonthGathered( lMonth ) ) {
								curVal = MonthlyColumns( curCol ).reslt( lMonth );
								//CR7788 the conversion factors were causing an overflow for the InchPound case since the
								//value was very small
								//restructured the following lines to hide showing HUGE and -HUGE values in output table CR8154 Glazer
								if ( ( curVal < veryLarge ) && ( curVal > verySmall ) ) {
									curVal = curVal * curConversionFactor + curConversionOffset;
									if ( curVal > maxVal ) maxVal = curVal;
									if ( curVal < minVal ) minVal = curVal;
									if ( curVal < veryLarge && curVal > verySmall ) {
										tableBody( columnRecount - 1, lMonth ) = RealToStr( curVal, digitsShown );
									} else {
										tableBody( columnRecount - 1, lMonth ) = "-";
									}
									tableBody( columnRecount, lMonth ) = DateToString( MonthlyColumns( curCol ).timeStamp( lMonth ) );
								} else {
									tableBody( columnRecount - 1, lMonth ) = "-";
									tableBody( columnRecount, lMonth ) = "-";
								}
							} else {
								tableBody( columnRecount - 1, lMonth ) = "-";
								tableBody( columnRecount, lMonth ) = "-";
							}
						} //lMonth
						// add the summary to bottom
						// Don't include if the original min and max values are still present
						if ( minVal < veryLarge ) {
							tableBody( columnRecount - 1, 15 ) = RealToStr( minVal, digitsShown );
						} else {
							tableBody( columnRecount - 1, 15 ) = "-";
						}
						if ( maxVal > verySmall ) {
							tableBody( columnRecount - 1, 16 ) = RealToStr( maxVal, digitsShown );
						} else {
							tableBody( columnRecount - 1, 15 ) = "-";
						}
					}}
				} //KColumn
				WriteReportHeaders( MonthlyInput( iInput ).name, MonthlyTables( curTable ).keyValue, isAverage );
				WriteSubtitle( "Custom Monthly Report" );
				WriteTable( tableBody, rowHead, columnHead, columnWidth, true ); //transpose monthly XML tables.
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, MonthlyInput( iInput ).name, MonthlyTables( curTable ).keyValue, "Custom Monthly Report" );
				}
			} //jTables
		} // iInput
	}

	void
	WriteTimeBinTables()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2003
		//       MODIFIED       January 2010, Kyle Benne
		//                      Added SQLite output
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Set up the time bin tabular report results

		// METHODOLOGY EMPLOYED:
		//   Creates several arrays that are passed to the WriteTable
		//   routine.  All arrays are strings so numbers need to be
		//   converted prior to calling WriteTable.
		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iInObj;
		int iTable;
		int kHour;
		int kMonth;
		int nCol;
		//main table
		Array1D_string columnHead;
		Array1D_int columnWidth;
		Array1D_string rowHead( 39 );
		Array2D_string tableBody;
		//stat table
		Array1D_string columnHeadStat( 1 );
		Array1D_int columnWidthStat( 1 );
		Array1D_string rowHeadStat( 6 );
		Array2D_string tableBodyStat( 1, 6 );

		Real64 curIntervalStart;
		Real64 curIntervalSize;
		int curIntervalCount;
		int curResIndex;
		int curNumTables;
		int numIntervalDigits;
		int firstReport;
		Real64 topValue;
		int repIndex;
		Real64 rowTotal;
		Real64 colTotal;
		Real64 aboveTotal;
		Real64 belowTotal;
		Real64 tableTotal;
		//CHARACTER(len=MaxNameLength):: repNameWithUnits ! For time bin reports, varible name with units
		std::string repNameWithUnitsandscheduleName;
		Real64 repStDev; // standard deviation
		Real64 repMean;
		std::string curNameWithSIUnits;
		std::string curNameAndUnits;
		int indexUnitConv;

		rowHead( 1 ) = "Interval Start";
		rowHead( 2 ) = "Interval End";
		rowHead( 3 ) = "January";
		rowHead( 4 ) = "February";
		rowHead( 5 ) = "March";
		rowHead( 6 ) = "April";
		rowHead( 7 ) = "May";
		rowHead( 8 ) = "June";
		rowHead( 9 ) = "July";
		rowHead( 10 ) = "August";
		rowHead( 11 ) = "September";
		rowHead( 12 ) = "October";
		rowHead( 13 ) = "November";
		rowHead( 14 ) = "December";
		rowHead( 15 ) = "12:01 to  1:00 am";
		rowHead( 16 ) = " 1:01 to  2:00 am";
		rowHead( 17 ) = " 2:01 to  3:00 am";
		rowHead( 18 ) = " 3:01 to  4:00 am";
		rowHead( 19 ) = " 4:01 to  5:00 am";
		rowHead( 20 ) = " 5:01 to  6:00 am";
		rowHead( 21 ) = " 6:01 to  7:00 am";
		rowHead( 22 ) = " 7:01 to  8:00 am";
		rowHead( 23 ) = " 8:01 to  9:00 am";
		rowHead( 24 ) = " 9:01 to 10:00 am";
		rowHead( 25 ) = "10:01 to 11:00 am";
		rowHead( 26 ) = "11:01 to 12:00 pm";
		rowHead( 27 ) = "12:01 to  1:00 pm";
		rowHead( 28 ) = " 1:01 to  2:00 pm";
		rowHead( 29 ) = " 2:01 to  3:00 pm";
		rowHead( 30 ) = " 3:01 to  4:00 pm";
		rowHead( 31 ) = " 4:01 to  5:00 pm";
		rowHead( 32 ) = " 5:01 to  6:00 pm";
		rowHead( 33 ) = " 6:01 to  7:00 pm";
		rowHead( 34 ) = " 7:01 to  8:00 pm";
		rowHead( 35 ) = " 8:01 to  9:00 pm";
		rowHead( 36 ) = " 9:01 to 10:00 pm";
		rowHead( 37 ) = "10:01 to 11:00 pm";
		rowHead( 38 ) = "11:01 to 12:00 am";
		rowHead( 39 ) = "Total";
		for ( iInObj = 1; iInObj <= OutputTableBinnedCount; ++iInObj ) {
			firstReport = OutputTableBinned( iInObj ).resIndex;
			curNameWithSIUnits = OutputTableBinned( iInObj ).varOrMeter + " [" + OutputTableBinned( iInObj ).units + ']';
			if ( unitsStyle == unitsStyleInchPound ) {
				LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
				curIntervalStart = ConvertIP( indexUnitConv, OutputTableBinned( iInObj ).intervalStart );
				curIntervalSize = ConvertIPdelta( indexUnitConv, OutputTableBinned( iInObj ).intervalSize );
			} else {
				curNameAndUnits = curNameWithSIUnits;
				curIntervalStart = OutputTableBinned( iInObj ).intervalStart;
				curIntervalSize = OutputTableBinned( iInObj ).intervalSize;
			}
			curIntervalCount = OutputTableBinned( iInObj ).intervalCount;
			curResIndex = OutputTableBinned( iInObj ).resIndex;
			curNumTables = OutputTableBinned( iInObj ).numTables;
			topValue = curIntervalStart + curIntervalSize * curIntervalCount;
			if ( curIntervalSize < 1 ) {
				numIntervalDigits = 4;
			} else if ( curIntervalSize >= 10 ) {
				numIntervalDigits = 0;
			} else {
				numIntervalDigits = 2;
			}
			// make arrays two columns wider for below and above bin range
			columnHead.allocate( curIntervalCount + 3 );
			columnWidth.allocate( curIntervalCount + 3 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( curIntervalCount + 3, 39 );
			tableBody = "";
			columnHead = "- [hr]";
			tableBody( 1, 1 ) = "less than";
			tableBody( 1, 2 ) = RealToStr( curIntervalStart, numIntervalDigits );
			for ( nCol = 1; nCol <= curIntervalCount; ++nCol ) {
				columnHead( nCol + 1 ) = IntToStr( nCol ) + " [hr]";
				//beginning of interval
				tableBody( nCol + 1, 1 ) = RealToStr( curIntervalStart + ( nCol - 1 ) * curIntervalSize, numIntervalDigits ) + "<=";
				//end of interval
				tableBody( nCol + 1, 2 ) = RealToStr( curIntervalStart + nCol * curIntervalSize, numIntervalDigits ) + '>';
			}
			tableBody( curIntervalCount + 2, 1 ) = "equal to or more than";
			tableBody( curIntervalCount + 2, 2 ) = RealToStr( topValue, numIntervalDigits );
			tableBody( curIntervalCount + 3, 1 ) = "Row";
			tableBody( curIntervalCount + 3, 2 ) = "Total";
			for ( iTable = 1; iTable <= curNumTables; ++iTable ) {
				repIndex = firstReport + ( iTable - 1 );
				if ( OutputTableBinned( iInObj ).scheduleIndex == 0 ) {
					repNameWithUnitsandscheduleName = curNameAndUnits;
				} else {
					repNameWithUnitsandscheduleName = curNameAndUnits + " [" + OutputTableBinned( iInObj ).ScheduleName + ']';
				}
				WriteReportHeaders( repNameWithUnitsandscheduleName, BinObjVarID( repIndex ).namesOfObj, OutputTableBinned( iInObj ).avgSum );
				for ( kHour = 1; kHour <= 24; ++kHour ) {
					tableBody( 1, 14 + kHour ) = RealToStr( BinResultsBelow( repIndex ).hrly( kHour ), 2 );
					tableBody( curIntervalCount + 2, 14 + kHour ) = RealToStr( BinResultsAbove( repIndex ).hrly( kHour ), 2 );
					rowTotal = BinResultsBelow( repIndex ).hrly( kHour ) + BinResultsAbove( repIndex ).hrly( kHour );
					for ( nCol = 1; nCol <= curIntervalCount; ++nCol ) {
						tableBody( nCol + 1, 14 + kHour ) = RealToStr( BinResults( nCol, repIndex ).hrly( kHour ), 2 );
						// sum the total for all columns
						rowTotal += BinResults( nCol, repIndex ).hrly( kHour );
					}
					tableBody( nCol + 2, 14 + kHour ) = RealToStr( rowTotal, 2 );
				}
				tableTotal = 0.0;
				for ( kMonth = 1; kMonth <= 12; ++kMonth ) {
					tableBody( 1, 2 + kMonth ) = RealToStr( BinResultsBelow( repIndex ).mnth( kMonth ), 2 );
					tableBody( curIntervalCount + 2, 2 + kMonth ) = RealToStr( BinResultsAbove( repIndex ).mnth( kMonth ), 2 );
					rowTotal = BinResultsBelow( repIndex ).mnth( kMonth ) + BinResultsAbove( repIndex ).mnth( kMonth );
					for ( nCol = 1; nCol <= curIntervalCount; ++nCol ) {
						tableBody( nCol + 1, 2 + kMonth ) = RealToStr( BinResults( nCol, repIndex ).mnth( kMonth ), 2 );
						// sum the total for all columns
						rowTotal += BinResults( nCol, repIndex ).mnth( kMonth );
					}
					tableBody( nCol + 2, 2 + kMonth ) = RealToStr( rowTotal, 2 );
					tableTotal += rowTotal;
				}
				// compute total row
				for ( nCol = 1; nCol <= curIntervalCount; ++nCol ) {
					colTotal = 0.0;
					for ( kMonth = 1; kMonth <= 12; ++kMonth ) {
						colTotal += BinResults( nCol, repIndex ).mnth( kMonth );
					}
					tableBody( nCol + 1, 39 ) = RealToStr( colTotal, 2 );
				}
				aboveTotal = 0.0;
				belowTotal = 0.0;
				for ( kMonth = 1; kMonth <= 12; ++kMonth ) {
					aboveTotal += BinResultsAbove( repIndex ).mnth( kMonth );
					belowTotal += BinResultsBelow( repIndex ).mnth( kMonth );
				}
				tableBody( 1, 39 ) = RealToStr( belowTotal, 2 );
				tableBody( curIntervalCount + 2, 39 ) = RealToStr( aboveTotal, 2 );
				tableBody( curIntervalCount + 3, 39 ) = RealToStr( tableTotal, 2 );
				WriteTextLine( "Values in table are in hours." );
				WriteTextLine( "" );
				WriteSubtitle( "Time Bin Results" );
				WriteTable( tableBody, rowHead, columnHead, columnWidth, true ); //transpose XML tables
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, repNameWithUnitsandscheduleName, BinObjVarID( repIndex ).namesOfObj, "Time Bin Results" );
				}
				//create statistics table
				rowHeadStat( 1 ) = "Minimum";
				rowHeadStat( 2 ) = "Mean minus two standard deviations";
				rowHeadStat( 3 ) = "Mean";
				rowHeadStat( 4 ) = "Mean plus two standard deviations";
				rowHeadStat( 5 ) = "Maximum";
				rowHeadStat( 6 ) = "Standard deviation";
				columnHeadStat( 1 ) = "Statistic";
				columnWidthStat( 1 ) = 14;
				//per Applied Regression Analysis and Other Multivariate Methods, Kleinburger/Kupper, 1978
				//first check if very large constant number has caused the second part to be larger than the first
				if ( BinStatistics( repIndex ).n > 1 ) {
					if ( BinStatistics( repIndex ).sum2 > ( pow_2( BinStatistics( repIndex ).sum ) / BinStatistics( repIndex ).n ) ) {
						repStDev = std::sqrt( ( BinStatistics( repIndex ).sum2 - ( pow_2( BinStatistics( repIndex ).sum ) / BinStatistics( repIndex ).n ) ) / ( BinStatistics( repIndex ).n - 1 ) );
					} else {
						repStDev = 0.0;
					}
					repMean = BinStatistics( repIndex ).sum / BinStatistics( repIndex ).n;
				} else {
					repStDev = 0.0;
					repMean = 0.0;
				}
				if ( unitsStyle == unitsStyleInchPound ) {
					tableBodyStat( 1, 1 ) = RealToStr( ConvertIP( indexUnitConv, BinStatistics( repIndex ).minimum ), 2 );
					tableBodyStat( 1, 2 ) = RealToStr( ConvertIP( indexUnitConv, repMean - 2 * repStDev ), 2 );
					tableBodyStat( 1, 3 ) = RealToStr( ConvertIP( indexUnitConv, repMean ), 2 );
					tableBodyStat( 1, 4 ) = RealToStr( ConvertIP( indexUnitConv, repMean + 2 * repStDev ), 2 );
					tableBodyStat( 1, 5 ) = RealToStr( ConvertIP( indexUnitConv, BinStatistics( repIndex ).maximum ), 2 );
					tableBodyStat( 1, 6 ) = RealToStr( ConvertIPdelta( indexUnitConv, repStDev ), 2 );
				} else {
					tableBodyStat( 1, 1 ) = RealToStr( BinStatistics( repIndex ).minimum, 2 );
					tableBodyStat( 1, 2 ) = RealToStr( repMean - 2 * repStDev, 2 );
					tableBodyStat( 1, 3 ) = RealToStr( repMean, 2 );
					tableBodyStat( 1, 4 ) = RealToStr( repMean + 2 * repStDev, 2 );
					tableBodyStat( 1, 5 ) = RealToStr( BinStatistics( repIndex ).maximum, 2 );
					tableBodyStat( 1, 6 ) = RealToStr( repStDev, 2 );
				}
				WriteSubtitle( "Statistics" );
				WriteTable( tableBodyStat, rowHeadStat, columnHeadStat, columnWidthStat, true ); //transpose XML table
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, repNameWithUnitsandscheduleName, BinObjVarID( repIndex ).namesOfObj, "Statistics" );
				}
			}
		}
	}

	void
	WriteBEPSTable()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   November 2003
		//       MODIFIED       January 2010, Kyle Benne
		//                      Added SQLite output
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Take the gathered total and enduse meter data and structure
		//   the results into a tabular report for output.

		// METHODOLOGY EMPLOYED:
		//   Create arrays for the call to WriteTable and then call it.
		//   This report actually consists of many sub-tables each with
		//   its own call to WriteTable.  Anytime that column headings are
		//   desired they are done in a new table because the only place
		//   that will split up very long header lines for the fixed width
		//   table is the header rows.

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputProcessor::MaxNumSubcategories;
		using OutputProcessor::EndUseCategory;
		using DataWater::WaterStorage;
		using DataWater::StorageTankDataStruct;
		using DataHVACGlobals::deviationFromSetPtThresholdHtg;
		using DataHVACGlobals::deviationFromSetPtThresholdClg;
		using ScheduleManager::GetScheduleName;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const colElectricity( 1 );
		int const colGas( 2 );
		int const colAdditionalFuel( 3 );
		int const colPurchCool( 4 );
		int const colPurchHeat( 5 );

		Real64 const SmallValue( 1.e-14 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// all arrays are in the format: (row, column)
		Array1D_string columnHead;
		Array1D_int columnWidth;
		Array1D_string rowHead;
		Array2D_string tableBody;

		// all arrays are in the format: (row, columnm)
		Array2D< Real64 > useVal( 6, 15 );
		Array2D< Real64 > normalVal( 6, 4 );
		Array1D< Real64 > collapsedTotal( 6 );
		Array2D< Real64 > collapsedEndUse( 6, NumEndUses );
		Array3D< Real64 > collapsedEndUseSub( MaxNumSubcategories, NumEndUses, 6 );
		Array2D< Real64 > endUseSubOther( 6, NumEndUses );
		Array1D_bool needOtherRow( NumEndUses );
		Real64 totalOnsiteHeat;
		Real64 totalOnsiteWater;
		Real64 totalWater;
		Real64 netElecPurchasedSold;
		Real64 totalSiteEnergyUse;
		Real64 netSiteEnergyUse;
		Real64 totalSourceEnergyUse;
		Real64 netSourceEnergyUse;
		Real64 netSourceElecPurchasedSold;
		int iResource;
		int jEndUse;
		int kEndUseSub;
		int i;
		Real64 largeConversionFactor;
		Real64 kConversionFactor;
		int numRows;
		Real64 initialStorage;
		Real64 finalStorage;
		Real64 StorageChange;
		int resourcePrimaryHeating;
		Real64 heatingMaximum;
		std::string footnote;
		Real64 waterConversionFactor;
		Real64 areaConversionFactor;
		Real64 convBldgGrossFloorArea;
		Real64 convBldgCondFloorArea;
		std::string curNameWithSIUnits;
		std::string curNameAndUnits;
		int indexUnitConv;
		Real64 processFraction;
		Real64 processElecCost;
		Real64 processGasCost;
		Real64 processOthrCost;

		Array1D< Real64 > leedFansParkFromFan( 6 );
		Array1D< Real64 > leedFansParkFromExtFuelEquip( 6 );
		Array1D< Real64 > leedIntLightProc( 6 );
		Array1D< Real64 > leedCook( 6 );
		Array1D< Real64 > leedIndProc( 6 );
		Array1D< Real64 > leedElevEsc( 6 );
		std::string subCatName;
		Real64 nonMisc;
		static Real64 leedSiteIntLite( 0.0 );
		static Real64 leedSiteSpHeat( 0.0 );
		static Real64 leedSiteSpCool( 0.0 );
		static Real64 leedSiteFanInt( 0.0 );
		static Real64 leedSiteSrvWatr( 0.0 );
		static Real64 leedSiteRecept( 0.0 );
		static Real64 leedSiteTotal( 0.0 );
		Real64 unconvert;

		if ( displayTabularBEPS || displayLEEDSummary ) {
			// show the headers of the report
			if ( displayTabularBEPS ) {
				WriteReportHeaders( "Annual Building Utility Performance Summary", "Entire Facility", isAverage );
				// show the number of hours that the table applies to
				WriteTextLine( "Values gathered over " + RealToStr( gatherElapsedTimeBEPS, 2 ) + " hours", true );
				if ( gatherElapsedTimeBEPS < 8759.0 ) { // might not add up to 8760 exactly but can't be more than 1 hour diff.
					WriteTextLine( "WARNING: THE REPORT DOES NOT REPRESENT A FULL ANNUAL SIMULATION.", true );
				}
				WriteTextLine( "", true );
			}
			// determine building floor areas
			DetermineBuildingFloorArea();
			// collapse the gatherEndUseBEPS array to the resource groups displayed
			for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
				collapsedEndUse( 1, jEndUse ) = gatherEndUseBEPS( 1, jEndUse ); //electricity
				collapsedEndUse( 2, jEndUse ) = gatherEndUseBEPS( 2, jEndUse ); //natural gas
				collapsedEndUse( 3, jEndUse ) = gatherEndUseBEPS( 6, jEndUse ) + gatherEndUseBEPS( 8, jEndUse ) + gatherEndUseBEPS( 9, jEndUse ) + gatherEndUseBEPS( 10, jEndUse ) + gatherEndUseBEPS( 11, jEndUse ) + gatherEndUseBEPS( 12, jEndUse ) + gatherEndUseBEPS( 13, jEndUse ) + gatherEndUseBEPS( 14, jEndUse ); //additional fuel  <- gasoline | <- diesel | <- coal | <- fuel oil #1 | <- fuel oil #2 | <- propane | <- otherfuel1 | <- otherfuel2
				collapsedEndUse( 4, jEndUse ) = gatherEndUseBEPS( 3, jEndUse ); //district cooling <- purchased cooling
				collapsedEndUse( 5, jEndUse ) = gatherEndUseBEPS( 4, jEndUse ) + gatherEndUseBEPS( 5, jEndUse ); //district heating <- purchased heating | <- steam
				collapsedEndUse( 6, jEndUse ) = gatherEndUseBEPS( 7, jEndUse ); //water
			}
			// repeat with totals
			collapsedTotal( 1 ) = gatherTotalsBEPS( 1 ); //electricity
			collapsedTotal( 2 ) = gatherTotalsBEPS( 2 ); //natural gas
			collapsedTotal( 3 ) = gatherTotalsBEPS( 6 ) + gatherTotalsBEPS( 8 ) + gatherTotalsBEPS( 9 ) + gatherTotalsBEPS( 10 ) + gatherTotalsBEPS( 11 ) + gatherTotalsBEPS( 12 ) + gatherTotalsBEPS( 13 ) + gatherTotalsBEPS( 14 ); //additional fuel  <- gasoline | <- diesel | <- coal | <- fuel oil #1 | <- fuel oil #2 | <- propane | <- otherfuel1 | <- otherfuel2
			collapsedTotal( 4 ) = gatherTotalsBEPS( 3 ); //district cooling <- purchased cooling
			collapsedTotal( 5 ) = gatherTotalsBEPS( 4 ) + gatherTotalsBEPS( 5 ); //district heating <- purchased heating | <- steam
			collapsedTotal( 6 ) = gatherTotalsBEPS( 7 ); //water

			for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
				for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
					collapsedEndUseSub( kEndUseSub, jEndUse, 1 ) = gatherEndUseSubBEPS( kEndUseSub, jEndUse, 1 ); //electricity
					collapsedEndUseSub( kEndUseSub, jEndUse, 2 ) = gatherEndUseSubBEPS( kEndUseSub, jEndUse, 2 ); //natural gas
					collapsedEndUseSub( kEndUseSub, jEndUse, 3 ) = gatherEndUseSubBEPS( kEndUseSub, jEndUse, 6 ) + gatherEndUseSubBEPS( kEndUseSub, jEndUse, 8 ) + gatherEndUseSubBEPS( kEndUseSub, jEndUse, 9 ) + gatherEndUseSubBEPS( kEndUseSub, jEndUse, 10 ) + gatherEndUseSubBEPS( kEndUseSub, jEndUse, 11 ) + gatherEndUseSubBEPS( kEndUseSub, jEndUse, 12 ) + gatherEndUseSubBEPS( kEndUseSub, jEndUse, 13 ) + gatherEndUseSubBEPS( kEndUseSub, jEndUse, 14 ); //additional fuel  <- gasoline | <- diesel | <- coal | <- fuel oil #1 | <- fuel oil #2 | <- propane | <- otherfuel1 | <- otherfuel2
					collapsedEndUseSub( kEndUseSub, jEndUse, 4 ) = gatherEndUseSubBEPS( kEndUseSub, jEndUse, 3 ); //district cooling <- purch cooling
					collapsedEndUseSub( kEndUseSub, jEndUse, 5 ) = gatherEndUseSubBEPS( kEndUseSub, jEndUse, 4 ) + gatherEndUseSubBEPS( kEndUseSub, jEndUse, 5 ); //district heating <- purch heating | <- steam
					collapsedEndUseSub( kEndUseSub, jEndUse, 6 ) = gatherEndUseSubBEPS( kEndUseSub, jEndUse, 7 ); //water
				}
			}

			// unit conversion - all values are used as divisors
			{ auto const SELECT_CASE_var( unitsStyle );
			if ( SELECT_CASE_var == unitsStyleJtoKWH ) {
				largeConversionFactor = 3600000.0;
				kConversionFactor = 1.0;
				waterConversionFactor = 1.0;
				areaConversionFactor = 1.0;
			} else if ( SELECT_CASE_var == unitsStyleInchPound ) {
				largeConversionFactor = getSpecificUnitDivider( "J", "kBtu" ); //1054351.84 J to kBtu
				kConversionFactor = 1.0;
				waterConversionFactor = getSpecificUnitDivider( "m3", "gal" ); //0.003785413 m3 to gal
				areaConversionFactor = getSpecificUnitDivider( "m2", "ft2" ); //0.092893973 m2 to ft2
			} else {
				largeConversionFactor = 1000000000.0;
				kConversionFactor = 1000.0;
				waterConversionFactor = 1.0;
				areaConversionFactor = 1.0;
			}}

			// convert floor areas
			convBldgGrossFloorArea = buildingGrossFloorArea / areaConversionFactor;
			convBldgCondFloorArea = buildingConditionedFloorArea / areaConversionFactor;

			//convert units into GJ (divide by 1,000,000,000) if J otherwise kWh
			for ( iResource = 1; iResource <= 5; ++iResource ) { //don't do water
				for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
					collapsedEndUse( iResource, jEndUse ) /= largeConversionFactor;
					for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
						collapsedEndUseSub( kEndUseSub, jEndUse, iResource ) /= largeConversionFactor;
					}
				}
				collapsedTotal( iResource ) /= largeConversionFactor;
			}
			//do water
			for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
				collapsedEndUse( 6, jEndUse ) /= waterConversionFactor;
				for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
					collapsedEndUseSub( kEndUseSub, jEndUse, 6 ) /= waterConversionFactor;
				}
			}

			collapsedTotal(6) = WaterConversionFunct(collapsedTotal(6), waterConversionFactor);

			// convert to GJ
			gatherPowerFuelFireGen /= largeConversionFactor;
			gatherPowerPV /= largeConversionFactor;
			gatherPowerWind /= largeConversionFactor;
			gatherPowerHTGeothermal /= largeConversionFactor;
			gatherPowerConversion /= largeConversionFactor;
			gatherElecProduced /= largeConversionFactor;
			gatherElecPurchased /= largeConversionFactor;
			gatherElecSurplusSold /= largeConversionFactor;

			// get change in overall state of charge for electrical storage devices.
			if ( facilityElectricServiceObj->numElecStorageDevices > 0 ) {
				// All flow in/out of storage is accounted for in gatherElecStorage, so separate calculation of change in state of charge is not necessary
				// OverallNetEnergyFromStorage = ( sum( ElecStorage.StartingEnergyStored() ) - sum( ElecStorage.ThisTimeStepStateOfCharge() ) ) + gatherElecStorage;
				OverallNetEnergyFromStorage = gatherElecStorage;
				OverallNetEnergyFromStorage /= largeConversionFactor;
			} else {
				OverallNetEnergyFromStorage = 0.0;
			}
			// determine which resource is the primary heating resourse
			resourcePrimaryHeating = 0;
			heatingMaximum = 0.0;
			for ( iResource = 1; iResource <= 5; ++iResource ) { //don't do water
				if ( collapsedEndUse( iResource, endUseHeating ) > heatingMaximum ) {
					heatingMaximum = collapsedEndUse( iResource, endUseHeating );
					resourcePrimaryHeating = iResource;
				}
			}

			//---- Source and Site Energy Sub-Table
			rowHead.allocate( 4 );
			columnHead.allocate( 3 );
			columnWidth.allocate( 3 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 3, 4 );

			{ auto const SELECT_CASE_var( unitsStyle );
			if ( SELECT_CASE_var == unitsStyleJtoKWH ) {
				columnHead( 1 ) = "Total Energy [kWh]";
				columnHead( 2 ) = "Energy Per Total Building Area [kWh/m2]";
				columnHead( 3 ) = "Energy Per Conditioned Building Area [kWh/m2]";
			} else if ( SELECT_CASE_var == unitsStyleInchPound ) {
				columnHead( 1 ) = "Total Energy [kBtu]";
				columnHead( 2 ) = "Energy Per Total Building Area [kBtu/ft2]";
				columnHead( 3 ) = "Energy Per Conditioned Building Area [kBtu/ft2]";
			} else {
				columnHead( 1 ) = "Total Energy [GJ]";
				columnHead( 2 ) = "Energy Per Total Building Area [MJ/m2]";
				columnHead( 3 ) = "Energy Per Conditioned Building Area [MJ/m2]";
			}}

			rowHead( 1 ) = "Total Site Energy";
			rowHead( 2 ) = "Net Site Energy";
			rowHead( 3 ) = "Total Source Energy";
			rowHead( 4 ) = "Net Source Energy";

			tableBody = "";

			// compute the net amount of electricity received from the utility which
			// is the amount purchased less the amount sold to the utility. Note that
			// previously these variables were converted into GJ so now we don't need
			// to do any conversion
			// water is not included   gatherTotalsBEPS(7)    !water
			totalSiteEnergyUse = ( gatherTotalsBEPS( 1 ) + gatherTotalsBEPS( 2 ) + gatherTotalsBEPS( 3 ) + gatherTotalsBEPS( 4 ) + gatherTotalsBEPS( 5 ) + gatherTotalsBEPS( 6 ) + gatherTotalsBEPS( 8 ) + gatherTotalsBEPS( 9 ) + gatherTotalsBEPS( 10 ) + gatherTotalsBEPS( 11 ) + gatherTotalsBEPS( 12 ) + gatherTotalsBEPS( 13 ) + gatherTotalsBEPS( 14 ) ) / largeConversionFactor; //electricity | natural gas | district cooling | district heating | steam | gasoline | diesel | coal | fuel oil #1 | fuel oil #2 | propane | otherfuel1 | otherfuel2

			netElecPurchasedSold = gatherElecPurchased - gatherElecSurplusSold;

			// water is not included   gatherTotalsBEPS(7)    !water
			netSiteEnergyUse = netElecPurchasedSold + ( gatherTotalsBEPS( 2 ) + gatherTotalsBEPS( 3 ) + gatherTotalsBEPS( 4 ) + gatherTotalsBEPS( 5 ) + gatherTotalsBEPS( 6 ) + gatherTotalsBEPS( 8 ) + gatherTotalsBEPS( 9 ) + gatherTotalsBEPS( 10 ) + gatherTotalsBEPS( 11 ) + gatherTotalsBEPS( 12 ) + gatherTotalsBEPS( 13 ) + gatherTotalsBEPS( 14 ) ) / largeConversionFactor; //electricity (already in GJ) | natural gas | district cooling | district heating | steam | gasoline | diesel | coal | fuel oil #1 | fuel oil #2 | propane | otherfuel1 | otherfuel2

			if ( efficiencyDistrictCooling == 0 ) efficiencyDistrictCooling = 1.0;
			if ( efficiencyDistrictHeating == 0 ) efficiencyDistrictHeating = 1.0;

			// source emissions already have the source factors included in the calcs.
			totalSourceEnergyUse = 0.0;
			//  electricity
			if ( fuelfactorsused( 1 ) ) {
				totalSourceEnergyUse += gatherTotalsSource( 1 );
			} else {
				totalSourceEnergyUse += gatherTotalsBEPS( 1 ) * sourceFactorElectric;
			}
			//  natural gas
			if ( fuelfactorsused( 2 ) ) {
				totalSourceEnergyUse += gatherTotalsSource( 2 );
			} else {
				totalSourceEnergyUse += gatherTotalsBEPS( 2 ) * sourceFactorNaturalGas;
			}
			// gasoline
			if ( fuelfactorsused( 3 ) ) {
				totalSourceEnergyUse += gatherTotalsSource( 3 );
			} else {
				totalSourceEnergyUse += gatherTotalsBEPS( 6 ) * sourceFactorGasoline;
			}
			// diesel
			if ( fuelfactorsused( 4 ) ) {
				totalSourceEnergyUse += gatherTotalsSource( 4 );
			} else {
				totalSourceEnergyUse += gatherTotalsBEPS( 8 ) * sourceFactorDiesel;
			}
			// coal
			if ( fuelfactorsused( 5 ) ) {
				totalSourceEnergyUse += gatherTotalsSource( 5 );
			} else {
				totalSourceEnergyUse += gatherTotalsBEPS( 9 ) * sourceFactorCoal;
			}
			// fuel oil #1
			if ( fuelfactorsused( 6 ) ) {
				totalSourceEnergyUse += gatherTotalsSource( 6 );
			} else {
				totalSourceEnergyUse += gatherTotalsBEPS( 10 ) * sourceFactorFuelOil1;
			}
			// fuel oil #2
			if ( fuelfactorsused( 7 ) ) {
				totalSourceEnergyUse += gatherTotalsSource( 7 );
			} else {
				totalSourceEnergyUse += gatherTotalsBEPS( 11 ) * sourceFactorFuelOil2;
			}
			// propane
			if ( fuelfactorsused( 8 ) ) {
				totalSourceEnergyUse += gatherTotalsSource( 8 );
			} else {
				totalSourceEnergyUse += gatherTotalsBEPS( 12 ) * sourceFactorPropane;
			}
			//otherfuel1
			if ( fuelfactorsused( 11 ) ) {
				totalSourceEnergyUse += gatherTotalsSource( 11 );
			} else {
				totalSourceEnergyUse += gatherTotalsBEPS( 13 ) * sourceFactorOtherFuel1;
			}
			//otherfuel2
			if ( fuelfactorsused( 12 ) ) {
				totalSourceEnergyUse += gatherTotalsSource( 12 );
			} else {
				totalSourceEnergyUse += gatherTotalsBEPS( 14 ) * sourceFactorOtherFuel2;
			}

			totalSourceEnergyUse = ( totalSourceEnergyUse + gatherTotalsBEPS( 3 ) * sourceFactorElectric / efficiencyDistrictCooling + gatherTotalsBEPS( 4 ) * sourceFactorNaturalGas / efficiencyDistrictHeating + gatherTotalsBEPS( 5 ) * sourceFactorSteam ) / largeConversionFactor; //district cooling | district heating | steam

			// now determine "net" source from purchased and surplus sold (still in J)

			if ( fuelfactorsused( 1 ) ) {
				netSourceElecPurchasedSold = gatherTotalsSource( 9 ) - gatherTotalsSource( 10 );
			} else {
				netSourceElecPurchasedSold = netElecPurchasedSold * sourceFactorElectric * largeConversionFactor; // back to J
			}

			netSourceEnergyUse = 0.0;
			//  natural gas
			if ( fuelfactorsused( 2 ) ) {
				netSourceEnergyUse += gatherTotalsSource( 2 );
			} else {
				netSourceEnergyUse += gatherTotalsBEPS( 2 ) * sourceFactorNaturalGas;
			}
			// gasoline
			if ( fuelfactorsused( 3 ) ) {
				netSourceEnergyUse += gatherTotalsSource( 3 );
			} else {
				netSourceEnergyUse += gatherTotalsBEPS( 6 ) * sourceFactorGasoline;
			}
			// diesel
			if ( fuelfactorsused( 4 ) ) {
				netSourceEnergyUse += gatherTotalsSource( 4 );
			} else {
				netSourceEnergyUse += gatherTotalsBEPS( 8 ) * sourceFactorDiesel;
			}
			// coal
			if ( fuelfactorsused( 5 ) ) {
				netSourceEnergyUse += gatherTotalsSource( 5 );
			} else {
				netSourceEnergyUse += gatherTotalsBEPS( 9 ) * sourceFactorCoal;
			}
			// fuel oil #1
			if ( fuelfactorsused( 6 ) ) {
				netSourceEnergyUse += gatherTotalsSource( 6 );
			} else {
				netSourceEnergyUse += gatherTotalsBEPS( 10 ) * sourceFactorFuelOil1;
			}
			// fuel oil #2
			if ( fuelfactorsused( 7 ) ) {
				netSourceEnergyUse += gatherTotalsSource( 7 );
			} else {
				netSourceEnergyUse += gatherTotalsBEPS( 11 ) * sourceFactorFuelOil2;
			}
			// propane
			if ( fuelfactorsused( 8 ) ) {
				netSourceEnergyUse += gatherTotalsSource( 8 );
			} else {
				netSourceEnergyUse += gatherTotalsBEPS( 12 ) * sourceFactorPropane;
			}
			// otherfuel1
			if ( fuelfactorsused( 11 ) ) {
				netSourceEnergyUse += gatherTotalsSource( 11 );
			} else {
				netSourceEnergyUse += gatherTotalsBEPS( 13 ) * sourceFactorOtherFuel1;
			}
			// otherfuel2
			if ( fuelfactorsused( 12 ) ) {
				netSourceEnergyUse += gatherTotalsSource( 12 );
			} else {
				netSourceEnergyUse += gatherTotalsBEPS( 14 ) * sourceFactorOtherFuel2;
			}

			netSourceEnergyUse = ( netSourceEnergyUse + netSourceElecPurchasedSold + gatherTotalsBEPS( 3 ) * sourceFactorElectric / efficiencyDistrictCooling + gatherTotalsBEPS( 4 ) * sourceFactorNaturalGas / efficiencyDistrictHeating + gatherTotalsBEPS( 5 ) * sourceFactorSteam ) / largeConversionFactor; // from other fuels | net source from electricity | district cooling | district heating | steam

			// show annual values
			tableBody( 1, 1 ) = RealToStr( totalSiteEnergyUse, 2 );
			tableBody( 1, 2 ) = RealToStr( netSiteEnergyUse, 2 );
			tableBody( 1, 3 ) = RealToStr( totalSourceEnergyUse, 2 );
			tableBody( 1, 4 ) = RealToStr( netSourceEnergyUse, 2 );
			// show  per building area
			if ( convBldgGrossFloorArea > 0 ) {
				tableBody( 2, 1 ) = RealToStr( totalSiteEnergyUse * kConversionFactor / convBldgGrossFloorArea, 2 );
				tableBody( 2, 2 ) = RealToStr( netSiteEnergyUse * kConversionFactor / convBldgGrossFloorArea, 2 );
				tableBody( 2, 3 ) = RealToStr( totalSourceEnergyUse * kConversionFactor / convBldgGrossFloorArea, 2 );
				tableBody( 2, 4 ) = RealToStr( netSourceEnergyUse * kConversionFactor / convBldgGrossFloorArea, 2 );
			}
			// show  per conditioned building area
			if ( convBldgCondFloorArea > 0 ) {
				tableBody( 3, 1 ) = RealToStr( totalSiteEnergyUse * kConversionFactor / convBldgCondFloorArea, 2 );
				tableBody( 3, 2 ) = RealToStr( netSiteEnergyUse * kConversionFactor / convBldgCondFloorArea, 2 );
				tableBody( 3, 3 ) = RealToStr( totalSourceEnergyUse * kConversionFactor / convBldgCondFloorArea, 2 );
				tableBody( 3, 4 ) = RealToStr( netSourceEnergyUse * kConversionFactor / convBldgCondFloorArea, 2 );
			}

			// heading for the entire sub-table
			if ( displayTabularBEPS ) {
				WriteSubtitle( "Site and Source Energy" );
				WriteTable( tableBody, rowHead, columnHead, columnWidth );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "AnnualBuildingUtilityPerformanceSummary", "Entire Facility", "Site and Source Energy" );
				}
			}

			//---- Source and Site Energy Sub-Table
			rowHead.allocate( 13 );
			columnHead.allocate( 1 );
			columnWidth.allocate( 1 );
			columnWidth = 50; //array assignment
			tableBody.allocate( 1, 13 );

			columnHead( 1 ) = "Site=>Source Conversion Factor";

			rowHead( 1 ) = "Electricity";
			rowHead( 2 ) = "Natural Gas";
			rowHead( 3 ) = "District Cooling";
			rowHead( 4 ) = "District Heating";
			rowHead( 5 ) = "Steam";
			rowHead( 6 ) = "Gasoline";
			rowHead( 7 ) = "Diesel";
			rowHead( 8 ) = "Coal";
			rowHead( 9 ) = "Fuel Oil #1";
			rowHead( 10 ) = "Fuel Oil #2";
			rowHead( 11 ) = "Propane";
			rowHead( 12 ) = "Other Fuel 1";
			rowHead( 13 ) = "Other Fuel 2";

			tableBody = "";

			// set columns to conversion factors
			// show values
			//  tableBody(1,1)  = TRIM(RealToStr(sourceFactorElectric,3))
			//  tableBody(2,1)  = TRIM(RealToStr(sourceFactorNaturalGas, 3))
			//  tableBody(3,1)  = TRIM(RealToStr(sourceFactorElectric/ efficiencyDistrictCooling,3))
			//  tableBody(4,1)  = TRIM(RealToStr(sourceFactorNaturalGas/ efficiencyDistrictHeating ,3))
			//  tableBody(5,1)  = TRIM(RealToStr(sourceFactorSteam ,3))
			//  tableBody(6,1)  = TRIM(RealToStr(sourceFactorGasoline ,3))
			//  tableBody(7,1)  = TRIM(RealToStr(sourceFactorDiesel ,3))
			//  tableBody(8,1)  = TRIM(RealToStr(sourceFactorCoal ,3))
			//  tableBody(9,1)  = TRIM(RealToStr(sourceFactorFuelOil1 ,3))
			//  tableBody(10,1) = TRIM(RealToStr(sourceFactorFuelOil2 ,3))
			//  tableBody(11,1) = TRIM(RealToStr(sourceFactorPropane ,3))

			if ( ! ffSchedUsed( 1 ) ) {
				tableBody( 1, 1 ) = RealToStr( sourceFactorElectric, 3 );
			} else if ( gatherTotalsBEPS( 1 ) > SmallValue ) {
				tableBody( 1, 1 ) = "Effective Factor = " + RealToStr( gatherTotalsBySourceBEPS( 1 ) / gatherTotalsBEPS( 1 ), 3 ) + " (calculated using schedule \"" + GetScheduleName( ffSchedIndex( 1 ) ) + "\")";
			} else {
				tableBody( 1, 1 ) = "N/A";
			}

			if ( ! ffSchedUsed( 2 ) ) {
				tableBody( 1, 2 ) = RealToStr( sourceFactorNaturalGas, 3 );
			} else if ( gatherTotalsBEPS( 2 ) > SmallValue ) {
				tableBody( 1, 2 ) = "Effective Factor = " + RealToStr( gatherTotalsBySourceBEPS( 2 ) / gatherTotalsBEPS( 2 ), 3 ) + " (calculated using schedule \"" + GetScheduleName( ffSchedIndex( 2 ) ) + "\")";
			} else {
				tableBody( 1, 2 ) = "N/A";
			}

			tableBody( 1, 3 ) = RealToStr( sourceFactorElectric / efficiencyDistrictCooling, 3 ); // District Cooling

			tableBody( 1, 4 ) = RealToStr( sourceFactorNaturalGas / efficiencyDistrictHeating, 3 ); // Disctrict Heating

			tableBody( 1, 5 ) = RealToStr( sourceFactorSteam, 3 ); // Steam

			if ( ! ffSchedUsed( 6 ) ) {
				tableBody( 1, 6 ) = RealToStr( sourceFactorGasoline, 3 );
			} else if ( gatherTotalsBEPS( 6 ) > SmallValue ) {
				tableBody( 1, 6 ) = "Effective Factor = " + RealToStr( gatherTotalsBySourceBEPS( 6 ) / gatherTotalsBEPS( 6 ), 3 ) + " (calculated using schedule \"" + GetScheduleName( ffSchedIndex( 6 ) ) + "\")";
			} else {
				tableBody( 1, 6 ) = "N/A";
			}

			if ( ! ffSchedUsed( 8 ) ) {
				tableBody( 1, 7 ) = RealToStr( sourceFactorDiesel, 3 );
			} else if ( gatherTotalsBEPS( 8 ) > SmallValue ) {
				tableBody( 1, 7 ) = "Effective Factor = " + RealToStr( gatherTotalsBySourceBEPS( 8 ) / gatherTotalsBEPS( 8 ), 3 ) + " (calculated using schedule \"" + GetScheduleName( ffSchedIndex( 8 ) ) + "\")";
			} else {
				tableBody( 1, 7 ) = "N/A";
			}

			if ( ! ffSchedUsed( 9 ) ) {
				tableBody( 1, 8 ) = RealToStr( sourceFactorCoal, 3 );
			} else if ( gatherTotalsBEPS( 9 ) > SmallValue ) {
				tableBody( 1, 8 ) = "Effective Factor = " + RealToStr( gatherTotalsBySourceBEPS( 9 ) / gatherTotalsBEPS( 9 ), 3 ) + " (calculated using schedule \"" + GetScheduleName( ffSchedIndex( 9 ) ) + "\")";
			} else {
				tableBody( 1, 8 ) = "N/A";
			}

			if ( ! ffSchedUsed( 10 ) ) {
				tableBody( 1, 9 ) = RealToStr( sourceFactorFuelOil1, 3 );
			} else if ( gatherTotalsBEPS( 10 ) > SmallValue ) {
				tableBody( 1, 9 ) = "Effective Factor = " + RealToStr( gatherTotalsBySourceBEPS( 10 ) / gatherTotalsBEPS( 10 ), 3 ) + " (calculated using schedule \"" + GetScheduleName( ffSchedIndex( 10 ) ) + "\")";
			} else {
				tableBody( 1, 9 ) = "N/A";
			}

			if ( ! ffSchedUsed( 11 ) ) {
				tableBody( 1, 10 ) = RealToStr( sourceFactorFuelOil2, 3 );
			} else if ( gatherTotalsBEPS( 11 ) > SmallValue ) {
				tableBody( 1, 10 ) = "Effective Factor = " + RealToStr( gatherTotalsBySourceBEPS( 11 ) / gatherTotalsBEPS( 11 ), 3 ) + " (calculated using schedule \"" + GetScheduleName( ffSchedIndex( 11 ) ) + "\")";
			} else {
				tableBody( 1, 10 ) = "N/A";
			}

			if ( ! ffSchedUsed( 12 ) ) {
				tableBody( 1, 11 ) = RealToStr( sourceFactorPropane, 3 );
			} else if ( gatherTotalsBEPS( 12 ) > SmallValue ) {
				tableBody( 1, 11 ) = "Effective Factor = " + RealToStr( gatherTotalsBySourceBEPS( 12 ) / gatherTotalsBEPS( 12 ), 3 ) + " (calculated using schedule \"" + GetScheduleName( ffSchedIndex( 12 ) ) + "\")";
			} else {
				tableBody( 1, 11 ) = "N/A";
			}

			if ( ! ffSchedUsed( 13 ) ) {
				tableBody( 1, 12 ) = RealToStr( sourceFactorOtherFuel1, 3 );
			} else if ( gatherTotalsBEPS( 13 ) > SmallValue ) {
				tableBody( 1, 12 ) = "Effective Factor = " + RealToStr( gatherTotalsBySourceBEPS( 13 ) / gatherTotalsBEPS( 13 ), 3 ) + " (calculated using schedule \"" + GetScheduleName( ffSchedIndex( 13 ) ) + "\")";
			} else {
				tableBody( 1, 12 ) = "N/A";
			}

			if ( ! ffSchedUsed( 14 ) ) {
				tableBody( 1, 13 ) = RealToStr( sourceFactorOtherFuel2, 3 );
			} else if ( gatherTotalsBEPS( 14 ) > SmallValue ) {
				tableBody( 1, 13 ) = "Effective Factor = " + RealToStr( gatherTotalsBySourceBEPS( 14 ) / gatherTotalsBEPS( 14 ), 3 ) + " (calculated using schedule \"" + GetScheduleName( ffSchedIndex( 14 ) ) + "\")";
			} else {
				tableBody( 1, 13 ) = "N/A";
			}

			// heading for the entire sub-table
			if ( displayTabularBEPS ) {
				WriteSubtitle( "Site to Source Energy Conversion Factors" );
				WriteTable( tableBody, rowHead, columnHead, columnWidth );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "AnnualBuildingUtilityPerformanceSummary", "Entire Facility", "Site to Source Energy Conversion Factors" );
				}
			}

			//---- Building Area Sub-Table
			rowHead.allocate( 3 );
			columnHead.allocate( 1 );
			columnWidth.allocate( 1 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 1, 3 );

			{ auto const SELECT_CASE_var( unitsStyle );
			if ( SELECT_CASE_var == unitsStyleJtoKWH ) {
				columnHead( 1 ) = "Area [m2]";
			} else if ( SELECT_CASE_var == unitsStyleInchPound ) {
				columnHead( 1 ) = "Area [ft2]";
			} else {
				columnHead( 1 ) = "Area [m2]";
			}}

			rowHead( 1 ) = "Total Building Area";
			rowHead( 2 ) = "Net Conditioned Building Area";
			rowHead( 3 ) = "Unconditioned Building Area";

			tableBody = "";
			tableBody( 1, 1 ) = RealToStr( convBldgGrossFloorArea, 2 );
			if ( unitsStyle == unitsStyleInchPound ) {
				PreDefTableEntry( pdchLeedGenData, "Total gross floor area [ft2]", RealToStr( convBldgGrossFloorArea, 2 ) );
			} else {
				PreDefTableEntry( pdchLeedGenData, "Total gross floor area [m2]", RealToStr( convBldgGrossFloorArea, 2 ) );
			}
			tableBody( 1, 2 ) = RealToStr( convBldgCondFloorArea, 2 );
			tableBody( 1, 3 ) = RealToStr( convBldgGrossFloorArea - convBldgCondFloorArea, 2 );

			// heading for the entire sub-table
			if ( displayTabularBEPS ) {
				WriteSubtitle( "Building Area" );
				WriteTable( tableBody, rowHead, columnHead, columnWidth );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "AnnualBuildingUtilityPerformanceSummary", "Entire Facility", "Building Area" );
				}
			}

			//---- End Use Sub-Table
			rowHead.allocate( 16 );
			columnHead.allocate( 6 );
			columnWidth.allocate( 6 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 6, 16 );
			for ( iResource = 1; iResource <= 6; ++iResource ) {
				useVal( iResource, 1 ) = collapsedEndUse( iResource, endUseHeating );
				useVal( iResource, 2 ) = collapsedEndUse( iResource, endUseCooling );
				useVal( iResource, 3 ) = collapsedEndUse( iResource, endUseInteriorLights );
				useVal( iResource, 4 ) = collapsedEndUse( iResource, endUseExteriorLights );
				useVal( iResource, 5 ) = collapsedEndUse( iResource, endUseInteriorEquipment );
				useVal( iResource, 6 ) = collapsedEndUse( iResource, endUseExteriorEquipment );
				useVal( iResource, 7 ) = collapsedEndUse( iResource, endUseFans );
				useVal( iResource, 8 ) = collapsedEndUse( iResource, endUsePumps );
				useVal( iResource, 9 ) = collapsedEndUse( iResource, endUseHeatRejection );
				useVal( iResource, 10 ) = collapsedEndUse( iResource, endUseHumidification );
				useVal( iResource, 11 ) = collapsedEndUse( iResource, endUseHeatRecovery );
				useVal( iResource, 12 ) = collapsedEndUse( iResource, endUseWaterSystem );
				useVal( iResource, 13 ) = collapsedEndUse( iResource, endUseRefrigeration );
				useVal( iResource, 14 ) = collapsedEndUse( iResource, endUseCogeneration );

				useVal( iResource, 15 ) = collapsedTotal( iResource ); // totals
			}

			rowHead( 1 ) = "Heating";
			rowHead( 2 ) = "Cooling";
			rowHead( 3 ) = "Interior Lighting";
			rowHead( 4 ) = "Exterior Lighting";
			rowHead( 5 ) = "Interior Equipment";
			rowHead( 6 ) = "Exterior Equipment";
			rowHead( 7 ) = "Fans";
			rowHead( 8 ) = "Pumps";
			rowHead( 9 ) = "Heat Rejection";
			rowHead( 10 ) = "Humidification";
			rowHead( 11 ) = "Heat Recovery";
			rowHead( 12 ) = "Water Systems";
			rowHead( 13 ) = "Refrigeration";
			rowHead( 14 ) = "Generators";
			rowHead( 15 ) = "";
			rowHead( 16 ) = "Total End Uses";

			{ auto const SELECT_CASE_var( unitsStyle );
			if ( SELECT_CASE_var == unitsStyleJtoKWH ) {
				columnHead( 1 ) = "Electricity [kWh]";
				columnHead( 2 ) = "Natural Gas [kWh]";
				columnHead( 3 ) = "Additional Fuel [kWh]";
				columnHead( 4 ) = "District Cooling [kWh]";
				columnHead( 5 ) = "District Heating [kWh]";
				columnHead( 6 ) = "Water [m3]";
			} else if ( SELECT_CASE_var == unitsStyleInchPound ) {
				columnHead( 1 ) = "Electricity [kBtu]";
				columnHead( 2 ) = "Natural Gas [kBtu]";
				columnHead( 3 ) = "Additional Fuel [kBtu]";
				columnHead( 4 ) = "District Cooling [kBtu]";
				columnHead( 5 ) = "District Heating [kBtu]";
				columnHead( 6 ) = "Water [gal]";
			} else {
				columnHead( 1 ) = "Electricity [GJ]";
				columnHead( 2 ) = "Natural Gas [GJ]";
				columnHead( 3 ) = "Additional Fuel [GJ]";
				columnHead( 4 ) = "District Cooling [GJ]";
				columnHead( 5 ) = "District Heating [GJ]";
				columnHead( 6 ) = "Water [m3]";
			}}

			tableBody = "";
			for ( iResource = 1; iResource <= 6; ++iResource ) {
				for ( jEndUse = 1; jEndUse <= 14; ++jEndUse ) {
					tableBody( iResource, jEndUse ) = RealToStr( useVal( iResource, jEndUse ), 2 );
				}
				tableBody( iResource, 16 ) = RealToStr( useVal( iResource, 15 ), 2 );
			}
			// add warning message if end use values do not add up to total
			for ( iResource = 1; iResource <= 6; ++iResource ) {
				Real64 curTotal = 0.0;
				for ( int jUse = 1; jUse <= 14; ++jUse ) {
					curTotal += useVal( iResource, jUse );
				}
				if ( abs( curTotal - collapsedTotal( iResource ) ) > ( collapsedTotal( iResource ) * 0.001 )) {
					ShowWarningError(ResourceWarningMessage(columnHead(iResource)));
				}
			}

			//complete the LEED end use table using the same values
			// for certain rows in the LEED table the subcategories are necessary so first compute those values
			leedFansParkFromFan = 0.0;
			leedFansParkFromExtFuelEquip = 0.0;
			leedIntLightProc = 0.0;
			leedCook = 0.0;
			leedIndProc = 0.0;
			leedElevEsc = 0.0;

			for ( iResource = 1; iResource <= 5; ++iResource ) { // don't bother with water
				for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
					if ( EndUseCategory( jEndUse ).NumSubcategories > 0 ) {
						for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
							subCatName = EndUseCategory( jEndUse ).SubcategoryName( kEndUseSub );
							if ( SameString( subCatName, "Fans - Parking Garage" ) ) {
								if ( jEndUse == 7 ) { //fans
									leedFansParkFromFan( iResource ) += collapsedEndUseSub( kEndUseSub, jEndUse, iResource );
								} else {
									leedFansParkFromExtFuelEquip( iResource ) += collapsedEndUseSub( kEndUseSub, jEndUse, iResource );
								}
							} else if ( SameString( subCatName, "Interior Lighting - Process" ) ) {
								leedIntLightProc( iResource ) += collapsedEndUseSub( kEndUseSub, jEndUse, iResource );
							} else if ( SameString( subCatName, "Cooking" ) ) {
								leedCook( iResource ) += collapsedEndUseSub( kEndUseSub, jEndUse, iResource );
							} else if ( SameString( subCatName, "Industrial Process" ) ) {
								leedIndProc( iResource ) += collapsedEndUseSub( kEndUseSub, jEndUse, iResource );
							} else if ( SameString( subCatName, "Elevators and Escalators" ) ) {
								leedElevEsc( iResource ) += collapsedEndUseSub( kEndUseSub, jEndUse, iResource );
							}
						}
					}
				}
			}

			unconvert = largeConversionFactor / 1000000000.0; //to avoid double converting, the values for the LEED report should be in GJ
			PreDefTableEntry( pdchLeedPerfElEneUse, "Interior Lighting", unconvert * ( useVal( colElectricity, 3 ) - leedIntLightProc( colElectricity ) ), 2 );
			PreDefTableEntry( pdchLeedPerfElEneUse, "Exterior Lighting", unconvert * useVal( colElectricity, 4 ), 2 );
			PreDefTableEntry( pdchLeedPerfElEneUse, "Space Heating", unconvert * useVal( colElectricity, 1 ), 2 );
			PreDefTableEntry( pdchLeedPerfElEneUse, "Space Cooling", unconvert * useVal( colElectricity, 2 ), 2 );
			PreDefTableEntry( pdchLeedPerfElEneUse, "Pumps", unconvert * useVal( colElectricity, 8 ), 2 );
			PreDefTableEntry( pdchLeedPerfElEneUse, "Heat Rejection", unconvert * useVal( colElectricity, 9 ), 2 );
			PreDefTableEntry( pdchLeedPerfElEneUse, "Fans-Interior", unconvert * ( useVal( colElectricity, 7 ) - leedFansParkFromFan( colElectricity ) ), 2 );
			PreDefTableEntry( pdchLeedPerfElEneUse, "Fans-Parking Garage", unconvert * ( leedFansParkFromFan( colElectricity ) + leedFansParkFromExtFuelEquip( colElectricity ) ), 2 );
			PreDefTableEntry( pdchLeedPerfElEneUse, "Service Water Heating", unconvert * useVal( colElectricity, 12 ), 2 );
			PreDefTableEntry( pdchLeedPerfElEneUse, "Receptacle Equipment", unconvert * useVal( colElectricity, 5 ), 2 );
			PreDefTableEntry( pdchLeedPerfElEneUse, "Interior Lighting (process)", unconvert * leedIntLightProc( colElectricity ), 2 );
			PreDefTableEntry( pdchLeedPerfElEneUse, "Refrigeration Equipment", unconvert * useVal( colElectricity, 13 ), 2 );
			PreDefTableEntry( pdchLeedPerfElEneUse, "Cooking", unconvert * leedCook( colElectricity ), 2 );
			PreDefTableEntry( pdchLeedPerfElEneUse, "Industrial Process", unconvert * leedIndProc( colElectricity ), 2 );
			PreDefTableEntry( pdchLeedPerfElEneUse, "Elevators and Escalators", unconvert * leedElevEsc( colElectricity ), 2 );
			PreDefTableEntry( pdchLeedPerfElEneUse, "Total Line", unconvert * useVal( colElectricity, 15 ), 2 );
			//  Energy Use Intensities
			if ( buildingGrossFloorArea > 0 ) {
				PreDefTableEntry( pdchLeedEuiElec, "Interior Lighting", unconvert * 1000 * ( useVal( colElectricity, 3 ) - leedIntLightProc( colElectricity ) ) / buildingGrossFloorArea, 2 );
				PreDefTableEntry( pdchLeedEuiElec, "Space Heating", unconvert * 1000 * useVal( colElectricity, 1 ) / buildingGrossFloorArea, 2 );
				PreDefTableEntry( pdchLeedEuiElec, "Space Cooling", unconvert * 1000 * useVal( colElectricity, 2 ) / buildingGrossFloorArea, 2 );
				PreDefTableEntry( pdchLeedEuiElec, "Fans-Interior", unconvert * 1000 * ( useVal( colElectricity, 7 ) - leedFansParkFromFan( colElectricity ) ) / buildingGrossFloorArea, 2 );
				PreDefTableEntry( pdchLeedEuiElec, "Service Water Heating", unconvert * 1000 * useVal( colElectricity, 12 ) / buildingGrossFloorArea, 2 );
				PreDefTableEntry( pdchLeedEuiElec, "Receptacle Equipment", unconvert * 1000 * useVal( colElectricity, 5 ) / buildingGrossFloorArea, 2 );
				nonMisc = useVal( colElectricity, 3 ) - leedIntLightProc( colElectricity ) + useVal( colElectricity, 1 ) + useVal( colElectricity, 2 ) + useVal( colElectricity, 7 ) - leedFansParkFromFan( colElectricity ) + useVal( colElectricity, 12 ) + useVal( colElectricity, 5 );
				PreDefTableEntry( pdchLeedEuiElec, "Miscellaneous", unconvert * 1000 * ( useVal( colElectricity, 15 ) - nonMisc ) / buildingGrossFloorArea, 2 );
				PreDefTableEntry( pdchLeedEuiElec, "Subtotal", unconvert * 1000 * useVal( colElectricity, 15 ) / buildingGrossFloorArea, 2 );
			}

			PreDefTableEntry( pdchLeedEusTotal, "Electricity", unconvert * useVal( colElectricity, 15 ), 2 );
			PreDefTableEntry( pdchLeedEusProc, "Electricity", unconvert * ( useVal( colElectricity, 5 ) + useVal( colElectricity, 13 ) ), 2 );
			if ( useVal( colElectricity, 15 ) != 0 ) {
				processFraction = ( useVal( colElectricity, 5 ) + useVal( colElectricity, 13 ) ) / useVal( colElectricity, 15 );
				processElecCost = LEEDelecCostTotal * processFraction;
			} else {
				processElecCost = 0.0;
			}
			PreDefTableEntry( pdchLeedEcsProc, "Electricity", processElecCost, 2 );
			addFootNoteSubTable( pdstLeedEneCostSum, "Process energy cost based on ratio of process to total energy." );

			PreDefTableEntry( pdchLeedPerfGasEneUse, "Interior Lighting", unconvert * ( useVal( colGas, 3 ) - leedIntLightProc( colGas ) ), 2 );
			PreDefTableEntry( pdchLeedPerfGasEneUse, "Exterior Lighting", unconvert * useVal( colGas, 4 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasEneUse, "Space Heating", unconvert * useVal( colGas, 1 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasEneUse, "Space Cooling", unconvert * useVal( colGas, 2 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasEneUse, "Pumps", unconvert * useVal( colGas, 8 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasEneUse, "Heat Rejection", unconvert * useVal( colGas, 9 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasEneUse, "Fans-Interior", unconvert * ( useVal( colGas, 7 ) - leedFansParkFromFan( colGas ) ), 2 );
			PreDefTableEntry( pdchLeedPerfGasEneUse, "Fans-Parking Garage", unconvert * ( leedFansParkFromFan( colGas ) + leedFansParkFromExtFuelEquip( colGas ) ), 2 );
			PreDefTableEntry( pdchLeedPerfGasEneUse, "Service Water Heating", unconvert * useVal( colGas, 12 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasEneUse, "Receptacle Equipment", unconvert * useVal( colGas, 5 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasEneUse, "Interior Lighting (process)", unconvert * leedIntLightProc( colGas ), 2 );
			PreDefTableEntry( pdchLeedPerfGasEneUse, "Refrigeration Equipment", unconvert * useVal( colGas, 13 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasEneUse, "Cooking", unconvert * leedCook( colGas ), 2 );
			PreDefTableEntry( pdchLeedPerfGasEneUse, "Industrial Process", unconvert * leedIndProc( colGas ), 2 );
			PreDefTableEntry( pdchLeedPerfGasEneUse, "Elevators and Escalators", unconvert * leedElevEsc( colGas ), 2 );
			PreDefTableEntry( pdchLeedPerfGasEneUse, "Total Line", unconvert * useVal( colGas, 15 ), 2 );
			//  Energy Use Intensities
			if ( buildingGrossFloorArea > 0 ) {
				PreDefTableEntry( pdchLeedEuiNatG, "Space Heating", unconvert * 1000 * useVal( colGas, 1 ) / buildingGrossFloorArea, 2 );
				PreDefTableEntry( pdchLeedEuiNatG, "Service Water Heating", unconvert * 1000 * useVal( colGas, 12 ) / buildingGrossFloorArea, 2 );
				nonMisc = useVal( colGas, 1 ) + useVal( colGas, 12 );
				PreDefTableEntry( pdchLeedEuiNatG, "Miscellaneous", unconvert * 1000 * ( useVal( colGas, 15 ) - nonMisc ) / buildingGrossFloorArea, 2 );
				PreDefTableEntry( pdchLeedEuiNatG, "Subtotal", unconvert * 1000 * useVal( colGas, 15 ) / buildingGrossFloorArea, 2 );
			}
			PreDefTableEntry( pdchLeedEusTotal, "Natural Gas", unconvert * useVal( colGas, 15 ), 2 );
			PreDefTableEntry( pdchLeedEusProc, "Natural Gas", unconvert * ( useVal( colGas, 5 ) + useVal( colGas, 13 ) ), 2 );
			if ( useVal( colGas, 15 ) != 0 ) {
				processFraction = ( useVal( colGas, 5 ) + useVal( colGas, 13 ) ) / useVal( colGas, 15 );
				processGasCost = LEEDgasCostTotal * processFraction;
			} else {
				processGasCost = 0.0;
			}
			PreDefTableEntry( pdchLeedEcsProc, "Natural Gas", processGasCost, 2 );

			PreDefTableEntry( pdchLeedPerfOthEneUse, "Interior Lighting", unconvert * ( useVal( colAdditionalFuel, 3 ) + useVal( colPurchCool, 3 ) + useVal( colPurchHeat, 3 ) - ( leedIntLightProc( colAdditionalFuel ) + leedIntLightProc( colPurchCool ) + leedIntLightProc( colPurchHeat ) ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthEneUse, "Exterior Lighting", unconvert * ( useVal( colAdditionalFuel, 4 ) + useVal( colPurchCool, 4 ) + useVal( colPurchHeat, 4 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthEneUse, "Space Heating", unconvert * ( useVal( colAdditionalFuel, 1 ) + useVal( colPurchCool, 1 ) + useVal( colPurchHeat, 1 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthEneUse, "Space Cooling", unconvert * ( useVal( colAdditionalFuel, 2 ) + useVal( colPurchCool, 2 ) + useVal( colPurchHeat, 2 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthEneUse, "Pumps", unconvert * ( useVal( colAdditionalFuel, 8 ) + useVal( colPurchCool, 8 ) + useVal( colPurchHeat, 8 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthEneUse, "Heat Rejection", unconvert * ( useVal( colAdditionalFuel, 9 ) + useVal( colPurchCool, 9 ) + useVal( colPurchHeat, 9 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthEneUse, "Fans-Interior", unconvert * ( useVal( colAdditionalFuel, 7 ) + useVal( colPurchCool, 7 ) + useVal( colPurchHeat, 7 ) - ( leedFansParkFromFan( colAdditionalFuel ) + leedFansParkFromFan( colPurchCool ) + leedFansParkFromFan( colPurchHeat ) ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthEneUse, "Fans-Parking Garage", unconvert * ( leedFansParkFromFan( colAdditionalFuel ) + leedFansParkFromFan( colPurchCool ) + leedFansParkFromFan( colPurchHeat ) + leedFansParkFromExtFuelEquip( colAdditionalFuel ) + leedFansParkFromExtFuelEquip( colPurchCool ) + leedFansParkFromExtFuelEquip( colPurchHeat ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthEneUse, "Service Water Heating", unconvert * ( useVal( colAdditionalFuel, 12 ) + useVal( colPurchCool, 12 ) + useVal( colPurchHeat, 12 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthEneUse, "Receptacle Equipment", unconvert * ( useVal( colAdditionalFuel, 5 ) + useVal( colPurchCool, 5 ) + useVal( colPurchHeat, 5 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthEneUse, "Interior Lighting (process)", unconvert * ( leedIntLightProc( colAdditionalFuel ) + leedIntLightProc( colPurchCool ) + leedIntLightProc( colPurchHeat ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthEneUse, "Refrigeration Equipment", unconvert * ( useVal( colAdditionalFuel, 13 ) + useVal( colPurchCool, 13 ) + useVal( colPurchHeat, 13 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthEneUse, "Cooking", unconvert * ( leedCook( colAdditionalFuel ) + leedCook( colPurchCool ) + leedCook( colPurchHeat ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthEneUse, "Industrial Process", unconvert * ( leedIndProc( colAdditionalFuel ) + leedIndProc( colPurchCool ) + leedIndProc( colPurchHeat ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthEneUse, "Elevators and Escalators", unconvert * ( leedElevEsc( colAdditionalFuel ) + leedElevEsc( colPurchCool ) + leedElevEsc( colPurchHeat ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthEneUse, "Total Line", unconvert * ( useVal( colAdditionalFuel, 15 ) + useVal( colPurchCool, 15 ) + useVal( colPurchHeat, 15 ) ), 2 );
			//  Energy Use Intensities
			if ( buildingGrossFloorArea > 0 ) {
				PreDefTableEntry( pdchLeedEuiOthr, "Miscellaneous", unconvert * 1000 * useVal( colAdditionalFuel, 15 ) / buildingGrossFloorArea, 2 );
				PreDefTableEntry( pdchLeedEuiOthr, "Subtotal", unconvert * 1000 * useVal( colAdditionalFuel, 15 ) / buildingGrossFloorArea, 2 );
			}
			PreDefTableEntry( pdchLeedEusTotal, "Additional", unconvert * ( useVal( colAdditionalFuel, 15 ) + useVal( colPurchCool, 15 ) + useVal( colPurchHeat, 15 ) ), 2 );
			PreDefTableEntry( pdchLeedEusProc, "Additional", unconvert * ( useVal( colAdditionalFuel, 5 ) + useVal( colAdditionalFuel, 13 ) + useVal( colPurchCool, 5 ) + useVal( colPurchCool, 13 ) + useVal( colPurchHeat, 5 ) + useVal( colPurchHeat, 13 ) ), 2 );
			if ( ( useVal( colAdditionalFuel, 15 ) + useVal( colPurchCool, 15 ) + useVal( colPurchHeat, 15 ) ) > 0.001 ) {
				processFraction = ( useVal( colAdditionalFuel, 5 ) + useVal( colAdditionalFuel, 13 ) + useVal( colPurchCool, 5 ) + useVal( colPurchCool, 13 ) + useVal( colPurchHeat, 5 ) + useVal( colPurchHeat, 13 ) ) / ( useVal( colAdditionalFuel, 15 ) + useVal( colPurchCool, 15 ) + useVal( colPurchHeat, 15 ) );
			} else {
				processFraction = 0.0;
			}
			processOthrCost = LEEDothrCostTotal * processFraction;
			PreDefTableEntry( pdchLeedEcsProc, "Additional", processOthrCost, 2 );
			PreDefTableEntry( pdchLeedEcsProc, "Total", processElecCost + processGasCost + processOthrCost, 2 );
			// accumulate for percentage table
			leedSiteIntLite = 0.0;
			leedSiteSpHeat = 0.0;
			leedSiteSpCool = 0.0;
			leedSiteFanInt = 0.0;
			leedSiteSrvWatr = 0.0;
			leedSiteRecept = 0.0;
			leedSiteTotal = 0.0;
			for ( iResource = 1; iResource <= 5; ++iResource ) { // don't bother with water
				leedSiteIntLite += useVal( iResource, 3 ) - leedIntLightProc( iResource );
				leedSiteSpHeat += useVal( iResource, 1 );
				leedSiteSpCool += useVal( iResource, 2 );
				leedSiteFanInt += useVal( iResource, 7 ) - leedFansParkFromFan( iResource );
				leedSiteSrvWatr += useVal( iResource, 12 );
				leedSiteRecept += useVal( iResource, 5 );
				leedSiteTotal += useVal( iResource, 15 );
			}
			if ( leedSiteTotal != 0 ) {
				PreDefTableEntry( pdchLeedEupPerc, "Interior Lighting", 100 * leedSiteIntLite / leedSiteTotal, 2 );
				PreDefTableEntry( pdchLeedEupPerc, "Space Heating", 100 * leedSiteSpHeat / leedSiteTotal, 2 );
				PreDefTableEntry( pdchLeedEupPerc, "Space Cooling", 100 * leedSiteSpCool / leedSiteTotal, 2 );
				PreDefTableEntry( pdchLeedEupPerc, "Fans-Interior", 100 * leedSiteFanInt / leedSiteTotal, 2 );
				PreDefTableEntry( pdchLeedEupPerc, "Service Water Heating", 100 * leedSiteSrvWatr / leedSiteTotal, 2 );
				PreDefTableEntry( pdchLeedEupPerc, "Receptacle Equipment", 100 * leedSiteRecept / leedSiteTotal, 2 );
				PreDefTableEntry( pdchLeedEupPerc, "Miscellaneous", 100 * ( leedSiteTotal - ( leedSiteIntLite + leedSiteSpHeat + leedSiteSpCool + leedSiteFanInt + leedSiteSrvWatr + leedSiteRecept ) ) / leedSiteTotal, 2 );
			}
			//totals across energy source
			PreDefTableEntry( pdchLeedEusTotal, "Total", unconvert * ( useVal( colAdditionalFuel, 15 ) + useVal( colPurchCool, 15 ) + useVal( colPurchHeat, 15 ) + useVal( colElectricity, 15 ) + useVal( colGas, 15 ) ), 2 );
			PreDefTableEntry( pdchLeedEusProc, "Total", unconvert * ( useVal( colAdditionalFuel, 5 ) + useVal( colAdditionalFuel, 13 ) + useVal( colPurchCool, 5 ) + useVal( colPurchCool, 13 ) + useVal( colPurchHeat, 5 ) + useVal( colPurchHeat, 13 ) + useVal( colElectricity, 5 ) + useVal( colElectricity, 13 ) + useVal( colGas, 5 ) + useVal( colGas, 13 ) ), 2 );

			footnote = "";
			{ auto const SELECT_CASE_var( resourcePrimaryHeating );
			if ( SELECT_CASE_var == colElectricity ) {
				footnote = "Note: Electricity appears to be the principal heating source based on energy usage.";
				PreDefTableEntry( pdchLeedGenData, "Principal Heating Source", "Electricity" );
			} else if ( SELECT_CASE_var == colGas ) {
				footnote = "Note: Natural gas appears to be the principal heating source based on energy usage.";
				PreDefTableEntry( pdchLeedGenData, "Principal Heating Source", "Natural Gas" );
			} else if ( SELECT_CASE_var == colAdditionalFuel ) {
				footnote = "Note: Additional fuel appears to be the principal heating source based on energy usage.";
				PreDefTableEntry( pdchLeedGenData, "Principal Heating Source", "Additional Fuel" );
			} else if ( SELECT_CASE_var == colPurchHeat ) {
				footnote = "Note: District heat appears to be the principal heating source based on energy usage.";
				PreDefTableEntry( pdchLeedGenData, "Principal Heating Source", "District Heat" );
			}}
			// heading for the entire sub-table
			if ( displayTabularBEPS ) {
				WriteSubtitle( "End Uses" );
				WriteTable( tableBody, rowHead, columnHead, columnWidth, false, footnote );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "AnnualBuildingUtilityPerformanceSummary", "Entire Facility", "End Uses" );
				}
			}

			//---- End Uses By Subcategory Sub-Table

			//determine if subcategories add up to the total and
			//if not, determine the difference for the 'other' row
			needOtherRow = false; //set array to all false assuming no other rows are needed
			for ( iResource = 1; iResource <= 6; ++iResource ) {
				for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
					if ( EndUseCategory( jEndUse ).NumSubcategories > 0 ) {
						//set the value to the total for the end use
						endUseSubOther( iResource, jEndUse ) = collapsedEndUse( iResource, jEndUse );
						// subtract off each sub end use category value
						for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
							endUseSubOther( iResource, jEndUse ) -= collapsedEndUseSub( kEndUseSub, jEndUse, iResource );
						}
						//if just a small value remains set it to zero
						if ( std::abs( endUseSubOther( iResource, jEndUse ) ) > 0.01 ) {
							needOtherRow( jEndUse ) = true;
						} else {
							endUseSubOther( iResource, jEndUse ) = 0.0;
						}
					} else {
						endUseSubOther( iResource, jEndUse ) = 0.0;
					}
				}
			}

			//determine the number of rows needed for sub-table
			numRows = 0;
			for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
				if ( EndUseCategory( jEndUse ).NumSubcategories > 0 ) {
					for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
						++numRows;
					}
					// check if an 'other' row is needed
					if ( needOtherRow( jEndUse ) ) {
						++numRows;
					}
				} else {
					++numRows;
				}
			}

			rowHead.allocate( numRows );
			columnHead.allocate( 7 );
			columnWidth.allocate( 7 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 7, numRows );

			rowHead = "";
			tableBody = "";

			// Build row head and subcategories columns
			i = 1;
			for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
				rowHead( i ) = EndUseCategory( jEndUse ).DisplayName;
				if ( EndUseCategory( jEndUse ).NumSubcategories > 0 ) {
					for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
						tableBody( 1, i ) = EndUseCategory( jEndUse ).SubcategoryName( kEndUseSub );
						++i;
					}
					// check if an 'other' row is needed
					if ( needOtherRow( jEndUse ) ) {
						tableBody( 1, i ) = "Other";
						++i;
					}
				} else {
					tableBody( 1, i ) = "General";
					++i;
				}
			}

			columnHead( 1 ) = "Subcategory";

			{ auto const SELECT_CASE_var( unitsStyle );
			if ( SELECT_CASE_var == unitsStyleJtoKWH ) {
				columnHead( 2 ) = "Electricity [kWh]";
				columnHead( 3 ) = "Natural Gas [kWh]";
				columnHead( 4 ) = "Additional Fuel [kWh]";
				columnHead( 5 ) = "District Cooling [kWh]";
				columnHead( 6 ) = "District Heating [kWh]";
				columnHead( 7 ) = "Water [m3]";
			} else if ( SELECT_CASE_var == unitsStyleInchPound ) {
				columnHead( 2 ) = "Electricity [kBtu]";
				columnHead( 3 ) = "Natural Gas [kBtu]";
				columnHead( 4 ) = "Additional Fuel [kBtu]";
				columnHead( 5 ) = "District Cooling [kBtu]";
				columnHead( 6 ) = "District Heating [kBtu]";
				columnHead( 7 ) = "Water [gal]";
			} else {
				columnHead( 2 ) = "Electricity [GJ]";
				columnHead( 3 ) = "Natural Gas [GJ]";
				columnHead( 4 ) = "Additional Fuel [GJ]";
				columnHead( 5 ) = "District Cooling [GJ]";
				columnHead( 6 ) = "District Heating [GJ]";
				columnHead( 7 ) = "Water [m3]";
			}}

			for ( iResource = 1; iResource <= 6; ++iResource ) {
				i = 1;
				for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
					if ( EndUseCategory( jEndUse ).NumSubcategories > 0 ) {
						for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
							tableBody( iResource + 1, i ) = RealToStr( collapsedEndUseSub( kEndUseSub, jEndUse, iResource ), 2 );
							++i;
						}
						//put other
						if ( needOtherRow( jEndUse ) ) {
							tableBody( iResource + 1, i ) = RealToStr( endUseSubOther( iResource, jEndUse ), 2 );
							++i;
						}
					} else {
						tableBody( iResource + 1, i ) = RealToStr( collapsedEndUse( iResource, jEndUse ), 2 );
						++i;
					}
				}
			}

			// heading for the entire sub-table
			if ( displayTabularBEPS ) {
				WriteSubtitle( "End Uses By Subcategory" );
				WriteTable( tableBody, rowHead, columnHead, columnWidth );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "AnnualBuildingUtilityPerformanceSummary", "Entire Facility", "End Uses By Subcategory" );
				}
			}
			//---- Normalized by Conditioned Area Sub-Table
			// Calculations for both normalized tables are first
			rowHead.allocate( 4 );
			columnHead.allocate( 6 );
			columnWidth.allocate( 6 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 6, 4 );
			for ( iResource = 1; iResource <= 6; ++iResource ) {
				normalVal( iResource, 1 ) = collapsedEndUse( iResource, endUseInteriorLights ) + collapsedEndUse( iResource, endUseExteriorLights ); //Lights     <- InteriorLights | <- ExteriorLights

				normalVal( iResource, 2 ) = collapsedEndUse( iResource, endUseFans ) + collapsedEndUse( iResource, endUsePumps ) + collapsedEndUse( iResource, endUseHeating ) + collapsedEndUse( iResource, endUseCooling ) + collapsedEndUse( iResource, endUseHeatRejection ) + collapsedEndUse( iResource, endUseHumidification ) + collapsedEndUse( iResource, endUseWaterSystem ); //HVAC       <- fans | <- pumps | <- heating | <- cooling | <- heat rejection | <- humidification | <- water system domestic hot water

				normalVal( iResource, 3 ) = collapsedEndUse( iResource, endUseInteriorEquipment ) + collapsedEndUse( iResource, endUseExteriorEquipment ) + collapsedEndUse( iResource, endUseCogeneration ) + collapsedEndUse( iResource, endUseHeatRecovery ) + collapsedEndUse( iResource, endUseRefrigeration ); //Other      <- InteriorEquipment | <- ExteriorEquipment | <- generator fuel | <- Heat Recovery (parasitics) | <- Refrigeration

				normalVal( iResource, 4 ) = collapsedTotal( iResource ); // totals
			}
			// convert the normalized end use values to MJ from GJ if using J
			for ( iResource = 1; iResource <= 5; ++iResource ) { //not including resource=6 water
				for ( jEndUse = 1; jEndUse <= 4; ++jEndUse ) {
					normalVal( iResource, jEndUse ) *= kConversionFactor;
				}
			}

			rowHead( 1 ) = "Lighting"; //typo fixed 5-17-04 BTG
			rowHead( 2 ) = "HVAC";
			rowHead( 3 ) = "Other";
			rowHead( 4 ) = "Total";

			{ auto const SELECT_CASE_var( unitsStyle );
			if ( SELECT_CASE_var == unitsStyleJtoKWH ) {
				columnHead( 1 ) = "Electricity Intensity [kWh/m2]";
				columnHead( 2 ) = "Natural Gas Intensity [kWh/m2]";
				columnHead( 3 ) = "Additional Fuel Intensity [kWh/m2]";
				columnHead( 4 ) = "District Cooling Intensity [kWh/m2]";
				columnHead( 5 ) = "District Heating Intensity [kWh/m2]";
				columnHead( 6 ) = "Water Intensity [m3/m2]";
			} else if ( SELECT_CASE_var == unitsStyleInchPound ) {
				columnHead( 1 ) = "Electricity Intensity [kBtu/ft2]";
				columnHead( 2 ) = "Natural Gas Intensity [kBtu/ft2]";
				columnHead( 3 ) = "Additional Fuel Intensity [kBtu/ft2]";
				columnHead( 4 ) = "District Cooling Intensity [kBtu/ft2]";
				columnHead( 5 ) = "District Heating Intensity [kBtu/ft2]";
				columnHead( 6 ) = "Water Intensity [gal/ft2]";
			} else {
				columnHead( 1 ) = "Electricity Intensity [MJ/m2]";
				columnHead( 2 ) = "Natural Gas Intensity [MJ/m2]";
				columnHead( 3 ) = "Additional Fuel Intensity [MJ/m2]";
				columnHead( 4 ) = "District Cooling Intensity [MJ/m2]";
				columnHead( 5 ) = "District Heating Intensity [MJ/m2]";
				columnHead( 6 ) = "Water Intensity [m3/m2]";
			}}

			WriteTextLine( "Normalized Metrics", true );

			// write the conditioned area based table
			tableBody = "";
			if ( convBldgCondFloorArea > 0 ) {
				for ( iResource = 1; iResource <= 6; ++iResource ) {
					for ( jEndUse = 1; jEndUse <= 4; ++jEndUse ) {
						tableBody( iResource, jEndUse ) = RealToStr( normalVal( iResource, jEndUse ) / convBldgCondFloorArea, 2 );
					}
				}
			}
			// heading for the entire sub-table
			if ( displayTabularBEPS ) {
				WriteSubtitle( "Utility Use Per Conditioned Floor Area" );
				WriteTable( tableBody, rowHead, columnHead, columnWidth );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "AnnualBuildingUtilityPerformanceSummary", "Entire Facility", "Utility Use Per Conditioned Floor Area" );
				}
			}
			//---- Normalized by Total Area Sub-Table
			tableBody = "";
			if ( convBldgGrossFloorArea > 0 ) {
				for ( iResource = 1; iResource <= 6; ++iResource ) {
					for ( jEndUse = 1; jEndUse <= 4; ++jEndUse ) {
						tableBody( iResource, jEndUse ) = RealToStr( normalVal( iResource, jEndUse ) / convBldgGrossFloorArea, 2 );
					}
				}
			}
			// heading for the entire sub-table
			if ( displayTabularBEPS ) {
				WriteSubtitle( "Utility Use Per Total Floor Area" );
				WriteTable( tableBody, rowHead, columnHead, columnWidth );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "AnnualBuildingUtilityPerformanceSummary", "Entire Facility", "Utility Use Per Total Floor Area" );
				}
			}

			//---- Electric Loads Satisfied Sub-Table
			rowHead.allocate( 14 );
			columnHead.allocate( 2 );
			columnWidth.allocate( 2 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 2, 14 );

			{ auto const SELECT_CASE_var( unitsStyle );
			if ( SELECT_CASE_var == unitsStyleJtoKWH ) {
				columnHead( 1 ) = "Electricity [kWh]";
			} else if ( SELECT_CASE_var == unitsStyleInchPound ) {
				columnHead( 1 ) = "Electricity [kBtu]";
			} else {
				columnHead( 1 ) = "Electricity [GJ]";
			}}
			columnHead( 2 ) = "Percent Electricity [%]";

			rowHead( 1 ) = "Fuel-Fired Power Generation";
			rowHead( 2 ) = "High Temperature Geothermal*";
			rowHead( 3 ) = "Photovoltaic Power";
			rowHead( 4 ) = "Wind Power";
			rowHead( 5 ) = "Power Conversion";
			rowHead( 6 ) = "Net Decrease in On-Site Storage";
			rowHead( 7 ) = "Total On-Site Electric Sources";
			rowHead( 8 ) = "";
			rowHead( 9 ) = "Electricity Coming From Utility";
			rowHead( 10 ) = "Surplus Electricity Going To Utility";
			rowHead( 11 ) = "Net Electricity From Utility";
			rowHead( 12 ) = "";
			rowHead( 13 ) = "Total On-Site and Utility Electric Sources";
			rowHead( 14 ) = "Total Electricity End Uses";

			tableBody = "";

			// show annual values
			unconvert = largeConversionFactor / 1000000000.0; //to avoid double converting, the values for the LEED report should be in GJ

			tableBody( 1, 1 ) = RealToStr( gatherPowerFuelFireGen, 3 );
			tableBody( 1, 2 ) = RealToStr( gatherPowerHTGeothermal, 3 );
			tableBody( 1, 3 ) = RealToStr( gatherPowerPV, 3 );
			PreDefTableEntry( pdchLeedRenAnGen, "Photovoltaic", unconvert * gatherPowerPV, 2 );
			tableBody( 1, 4 ) = RealToStr( gatherPowerWind, 3 );
			PreDefTableEntry( pdchLeedRenAnGen, "Wind", unconvert * gatherPowerWind, 2 );
			tableBody( 1, 5 ) = RealToStr( gatherPowerConversion, 3 );
			tableBody( 1, 6 ) = RealToStr( OverallNetEnergyFromStorage, 3 );
			tableBody( 1, 7 ) = RealToStr( gatherElecProduced, 3 );
			tableBody( 1, 9 ) = RealToStr( gatherElecPurchased, 3 );
			tableBody( 1, 10 ) = RealToStr( gatherElecSurplusSold, 3 );
			tableBody( 1, 11 ) = RealToStr( gatherElecPurchased - gatherElecSurplusSold, 3 );
			tableBody( 1, 13 ) = RealToStr( gatherElecProduced + ( gatherElecPurchased - gatherElecSurplusSold ), 3 );
			tableBody( 1, 14 ) = RealToStr( collapsedTotal( 1 ), 3 );

			// show annual percentages
			if ( collapsedTotal( 1 ) > 0 ) {
				tableBody( 2, 1 ) = RealToStr( 100.0 * gatherPowerFuelFireGen / collapsedTotal( 1 ), 2 );
				tableBody( 2, 2 ) = RealToStr( 100.0 * gatherPowerHTGeothermal / collapsedTotal( 1 ), 2 );
				tableBody( 2, 3 ) = RealToStr( 100.0 * gatherPowerPV / collapsedTotal( 1 ), 2 );
				tableBody( 2, 4 ) = RealToStr( 100.0 * gatherPowerWind / collapsedTotal( 1 ), 2 );
				tableBody( 2, 5 ) = RealToStr( 100.0 * gatherPowerConversion / collapsedTotal( 1 ), 2 );
				tableBody( 2, 6 ) = RealToStr( 100.0 * OverallNetEnergyFromStorage / collapsedTotal( 1 ), 2 );
				tableBody( 2, 7 ) = RealToStr( 100.0 * gatherElecProduced / collapsedTotal( 1 ), 2 );
				tableBody( 2, 9 ) = RealToStr( 100.0 * gatherElecPurchased / collapsedTotal( 1 ), 2 );
				tableBody( 2, 10 ) = RealToStr( 100.0 * gatherElecSurplusSold / collapsedTotal( 1 ), 2 );
				tableBody( 2, 11 ) = RealToStr( 100.0 * ( gatherElecPurchased - gatherElecSurplusSold ) / collapsedTotal( 1 ), 2 );
				tableBody( 2, 13 ) = RealToStr( 100.0 * ( gatherElecProduced + ( gatherElecPurchased - gatherElecSurplusSold ) ) / collapsedTotal( 1 ), 2 );
				tableBody( 2, 14 ) = RealToStr( 100.0, 2 );
			}

			// heading for the entire sub-table
			if ( displayTabularBEPS ) {
				WriteSubtitle( "Electric Loads Satisfied" );
				WriteTable( tableBody, rowHead, columnHead, columnWidth );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "AnnualBuildingUtilityPerformanceSummary", "Entire Facility", "Electric Loads Satisfied" );
				}
			}

			//---- On-Site Thermal Sources Sub-Table
			rowHead.allocate( 7 );
			columnHead.allocate( 2 );
			columnWidth.allocate( 2 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 2, 7 );

			{ auto const SELECT_CASE_var( unitsStyle );
			if ( SELECT_CASE_var == unitsStyleJtoKWH ) {
				columnHead( 1 ) = "Heat [kWh]";
			} else if ( SELECT_CASE_var == unitsStyleInchPound ) {
				columnHead( 1 ) = "Heat [kBtu]";
			} else {
				columnHead( 1 ) = "Heat [GJ]";
			}}

			columnHead( 2 ) = "Percent Heat [%]";

			rowHead( 1 ) = "Water-Side Heat Recovery";
			rowHead( 2 ) = "Air to Air Heat Recovery for Cooling";
			rowHead( 3 ) = "Air to Air Heat Recovery for Heating";
			rowHead( 4 ) = "High-Temperature Geothermal*";
			rowHead( 5 ) = "Solar Water Thermal";
			rowHead( 6 ) = "Solar Air Thermal";
			rowHead( 7 ) = "Total On-Site Thermal Sources";

			tableBody = "";

			// convert to GJ

			gatherWaterHeatRecovery /= largeConversionFactor;
			gatherAirHeatRecoveryCool /= largeConversionFactor;
			gatherAirHeatRecoveryHeat /= largeConversionFactor;
			gatherHeatHTGeothermal /= largeConversionFactor;
			gatherHeatSolarWater /= largeConversionFactor;
			gatherHeatSolarAir /= largeConversionFactor;

			// determine total on site heat
			totalOnsiteHeat = gatherWaterHeatRecovery + gatherAirHeatRecoveryCool + gatherAirHeatRecoveryHeat + gatherHeatHTGeothermal + gatherHeatSolarWater + gatherHeatSolarAir;

			// show annual values
			tableBody( 1, 1 ) = RealToStr( gatherWaterHeatRecovery, 2 );
			tableBody( 1, 2 ) = RealToStr( gatherAirHeatRecoveryCool, 2 );
			tableBody( 1, 3 ) = RealToStr( gatherAirHeatRecoveryHeat, 2 );
			tableBody( 1, 4 ) = RealToStr( gatherHeatHTGeothermal, 2 );
			tableBody( 1, 5 ) = RealToStr( gatherHeatSolarWater, 2 );
			tableBody( 1, 6 ) = RealToStr( gatherHeatSolarAir, 2 );
			tableBody( 1, 7 ) = RealToStr( totalOnsiteHeat, 2 );

			if ( totalOnsiteHeat > 0 ) {
				tableBody( 2, 1 ) = RealToStr( 100.0 * gatherWaterHeatRecovery / totalOnsiteHeat, 2 );
				tableBody( 2, 2 ) = RealToStr( 100.0 * gatherAirHeatRecoveryCool / totalOnsiteHeat, 2 );
				tableBody( 2, 3 ) = RealToStr( 100.0 * gatherAirHeatRecoveryHeat / totalOnsiteHeat, 2 );
				tableBody( 2, 4 ) = RealToStr( 100.0 * gatherHeatHTGeothermal / totalOnsiteHeat, 2 );
				tableBody( 2, 5 ) = RealToStr( 100.0 * gatherHeatSolarWater / totalOnsiteHeat, 2 );
				tableBody( 2, 6 ) = RealToStr( 100.0 * gatherHeatSolarAir / totalOnsiteHeat, 2 );
				tableBody( 2, 7 ) = RealToStr( 100.0, 2 );
			}

			// heading for the entire sub-table
			if ( displayTabularBEPS ) {
				WriteSubtitle( "On-Site Thermal Sources" );
				WriteTable( tableBody, rowHead, columnHead, columnWidth );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "AnnualBuildingUtilityPerformanceSummary", "Entire Facility", "On-Site Thermal Sources" );
				}
			}

			//---- Water Loads Sub-Table
			// As of 12/8/2003 decided to not include this sub-table to wait
			// until water use is implemented in EnergyPlus before displaying
			// the table. Implementing water end-uses makes sense for EnergyPlus
			// but since they are not really implemented as of December 2003 the
			// table would be all zeros.  Recommendation to exclude this table
			// for now made by Glazer and Crawley.
			//Aug 2006, adding table in with implementation of water system, BGriffith
			rowHead.allocate( 13 );
			columnHead.allocate( 2 );
			columnWidth.allocate( 2 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 2, 13 );
			{ auto const SELECT_CASE_var( unitsStyle );
			if ( SELECT_CASE_var == unitsStyleJtoKWH ) {
				columnHead( 1 ) = "Water [m3]";
			} else if ( SELECT_CASE_var == unitsStyleInchPound ) {
				columnHead( 1 ) = "Water [gal]";
			} else {
				columnHead( 1 ) = "Water [m3]";
			}}
			columnHead( 2 ) = "Percent Water [%]";
			rowHead( 1 ) = "Rainwater Collection";
			rowHead( 2 ) = "Condensate Collection";
			rowHead( 3 ) = "Groundwater Well";
			rowHead( 4 ) = "Total On Site Water Sources";
			rowHead( 5 ) = "-";
			rowHead( 6 ) = "Initial Storage";
			rowHead( 7 ) = "Final Storage";
			rowHead( 8 ) = "Change in Storage";
			rowHead( 9 ) = "-";
			rowHead( 10 ) = "Water Supplied by Utility";
			rowHead( 11 ) = "-";
			rowHead( 12 ) = "Total On Site, Change in Storage, and Utility Water Sources";
			rowHead( 13 ) = "Total Water End Uses";
			tableBody = "-";

			totalOnsiteWater = gatherRainWater + gatherCondensate + gatherWellwater;

			//  ! show annual values
			tableBody( 1, 1 ) = RealToStr( gatherRainWater / waterConversionFactor, 2 );
			tableBody( 1, 2 ) = RealToStr( gatherCondensate / waterConversionFactor, 2 );
			tableBody( 1, 3 ) = RealToStr( gatherWellwater / waterConversionFactor, 2 );
			tableBody( 1, 4 ) = RealToStr( totalOnsiteWater / waterConversionFactor, 2 );

			if ( allocated( WaterStorage ) ) {
				initialStorage = sum( WaterStorage, &StorageTankDataStruct::InitialVolume );
				finalStorage = sum( WaterStorage, &StorageTankDataStruct::ThisTimeStepVolume );
				StorageChange = initialStorage - finalStorage;
			} else {
				initialStorage = 0.0;
				finalStorage = 0.0;
				StorageChange = 0.0;
			}
			tableBody( 1, 6 ) = RealToStr( initialStorage / waterConversionFactor, 2 );
			tableBody( 1, 7 ) = RealToStr( finalStorage / waterConversionFactor, 2 );
			tableBody( 1, 8 ) = RealToStr( StorageChange / waterConversionFactor, 2 );

			totalWater = totalOnsiteWater + gatherMains + StorageChange;

			tableBody( 1, 10 ) = RealToStr( gatherMains / waterConversionFactor, 2 );
			tableBody( 1, 12 ) = RealToStr( totalWater / waterConversionFactor, 2 );
			tableBody( 1, 13 ) = RealToStr( gatherWaterEndUseTotal / waterConversionFactor, 2 );

			if ( gatherWaterEndUseTotal > 0 ) {
				tableBody( 2, 1 ) = RealToStr( 100.0 * gatherRainWater / gatherWaterEndUseTotal, 2 );
				tableBody( 2, 2 ) = RealToStr( 100.0 * gatherCondensate / gatherWaterEndUseTotal, 2 );
				tableBody( 2, 3 ) = RealToStr( 100.0 * gatherWellwater / gatherWaterEndUseTotal, 2 );
				tableBody( 2, 4 ) = RealToStr( 100.0 * totalOnsiteWater / gatherWaterEndUseTotal, 2 );
				tableBody( 2, 6 ) = RealToStr( 100.0 * initialStorage / gatherWaterEndUseTotal, 2 );
				tableBody( 2, 7 ) = RealToStr( 100.0 * finalStorage / gatherWaterEndUseTotal, 2 );
				tableBody( 2, 8 ) = RealToStr( 100.0 * StorageChange / gatherWaterEndUseTotal, 2 );

				tableBody( 2, 10 ) = RealToStr( 100.0 * gatherMains / gatherWaterEndUseTotal, 2 );

				tableBody( 2, 12 ) = RealToStr( 100.0 * totalWater / gatherWaterEndUseTotal, 2 );
				tableBody( 2, 13 ) = RealToStr( 100.0, 2 );
			}

			//  ! heading for the entire sub-table
			if ( displayTabularBEPS ) {
				WriteSubtitle( "Water Source Summary" );
				WriteTable( tableBody, rowHead, columnHead, columnWidth );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "AnnualBuildingUtilityPerformanceSummary", "Entire Facility", "Water Source Summary" );
				}
			}

			//---- Comfort and Setpoint Not Met Sub-Table
			if ( displayTabularBEPS ) {
				rowHead.allocate( 2 );
				columnHead.allocate( 1 );
				columnWidth.allocate( 1 );
				columnWidth = 14; //array assignment - same for all columns
				tableBody.allocate( 1, 2 );

				WriteSubtitle( "Setpoint Not Met Criteria" );

				curNameWithSIUnits = "Degrees [deltaC]";
				curNameAndUnits = curNameWithSIUnits;
				if ( unitsStyle == unitsStyleInchPound ) {
					LookupSItoIP( curNameWithSIUnits, indexUnitConv, curNameAndUnits );
				}
				columnHead( 1 ) = curNameAndUnits;

				rowHead( 1 ) = "Tolerance for Zone Heating Setpoint Not Met Time";
				rowHead( 2 ) = "Tolerance for Zone Cooling Setpoint Not Met Time";

				if ( unitsStyle != unitsStyleInchPound ) {
					tableBody( 1, 1 ) = RealToStr( std::abs( deviationFromSetPtThresholdHtg ), 2 );
					tableBody( 1, 2 ) = RealToStr( deviationFromSetPtThresholdClg, 2 );
				} else {
					tableBody( 1, 1 ) = RealToStr( ConvertIPdelta( indexUnitConv, std::abs( deviationFromSetPtThresholdHtg ) ), 2 );
					tableBody( 1, 2 ) = RealToStr( ConvertIPdelta( indexUnitConv, deviationFromSetPtThresholdClg ), 2 );
				}

				WriteTable( tableBody, rowHead, columnHead, columnWidth );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "AnnualBuildingUtilityPerformanceSummary", "Entire Facility", "Setpoint Not Met Criteria" );
				}
			}

			rowHead.allocate( 3 );
			columnHead.allocate( 1 );
			columnWidth.allocate( 1 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 1, 3 );

			if ( displayTabularBEPS ) {
				WriteSubtitle( "Comfort and Setpoint Not Met Summary" );
			}

			columnHead( 1 ) = "Facility [Hours]";

			rowHead( 1 ) = "Time Setpoint Not Met During Occupied Heating";
			rowHead( 2 ) = "Time Setpoint Not Met During Occupied Cooling";
			rowHead( 3 ) = "Time Not Comfortable Based on Simple ASHRAE 55-2004";

			tableBody( 1, 1 ) = RealToStr( TotalNotMetHeatingOccupiedForABUPS, 2 );
			tableBody( 1, 2 ) = RealToStr( TotalNotMetCoolingOccupiedForABUPS, 2 );
			PreDefTableEntry( pdchLeedAmData, "Number of hours heating loads not met", RealToStr( TotalNotMetHeatingOccupiedForABUPS, 2 ) );
			PreDefTableEntry( pdchLeedAmData, "Number of hours cooling loads not met", RealToStr( TotalNotMetCoolingOccupiedForABUPS, 2 ) );
			PreDefTableEntry( pdchLeedAmData, "Number of hours not met", RealToStr( TotalNotMetOccupiedForABUPS, 2 ) );
			tableBody( 1, 3 ) = RealToStr( TotalTimeNotSimpleASH55EitherForABUPS, 2 );

			if ( displayTabularBEPS ) {
				WriteTable( tableBody, rowHead, columnHead, columnWidth );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "AnnualBuildingUtilityPerformanceSummary", "Entire Facility", "Comfort and Setpoint Not Met Summary" );
				}
			}

			//---- Control Summary Sub-Table

			//---- End Notes
			if ( displayTabularBEPS ) {
				WriteTextLine( "Note 1: An asterisk (*) indicates that the feature is not yet implemented." );
			}
			//CALL WriteTextLine('Note 2: The source energy conversion factors used are: ')
			//CALL WriteTextLine('        1.05 for all fuels, 1 for district, and 3 for electricity.')
		}
	}

	std::string
	ResourceWarningMessage(std::string resource)
	{
		return "In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: " + resource;
	}

	Real64
	WaterConversionFunct(Real64 WaterTotal, Real64 ConversionFactor)
	{
		return WaterTotal / ConversionFactor;
	}

	void
	WriteSourceEnergyEndUseSummary()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Mangesh Basarkar
		//       DATE WRITTEN   September 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Take the gathered total and end use source energy meter data and structure
		//   the results into a tabular report for output.

		// METHODOLOGY EMPLOYED:
		//   Create arrays for the call to WriteTable and then call it.

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputProcessor::MaxNumSubcategories;
		using OutputProcessor::EndUseCategory;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// all arrays are in the format: (row, column)
		Array1D_string columnHead;
		Array1D_int columnWidth;
		Array1D_string rowHead;
		Array2D_string tableBody;

		// all arrays are in the format: (row, columnm)
		Array2D< Real64 > useVal( 6, 15 );
		Array1D< Real64 > collapsedTotal( 6 );
		Array2D< Real64 > collapsedEndUse( 6, NumEndUses );
		Array3D< Real64 > collapsedEndUseSub( MaxNumSubcategories, NumEndUses, 6 );
		int iResource;
		int jEndUse;
		Real64 largeConversionFactor;
		Real64 areaConversionFactor;
		Real64 convBldgCondFloorArea;

		if ( displaySourceEnergyEndUseSummary ) {
			// show the headers of the report
			WriteReportHeaders( "Source Energy End Use Components Summary", "Entire Facility", isAverage );
			// show the number of hours that the table applies to
			WriteTextLine( "Values gathered over " + RealToStr( gatherElapsedTimeBEPS, 2 ) + " hours", true );
			if ( gatherElapsedTimeBEPS < 8759.0 ) { // might not add up to 8760 exactly but can't be more than 1 hour diff.
				WriteTextLine( "WARNING: THE REPORT DOES NOT REPRESENT A FULL ANNUAL SIMULATION.", true );
			}
			WriteTextLine( "", true );
			// determine building floor areas
			DetermineBuildingFloorArea();
			// collapse the gatherEndUseBEPS array to the resource groups displayed
			for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
				collapsedEndUse( 1, jEndUse ) = gatherEndUseBySourceBEPS( 1, jEndUse ); //electricity
				collapsedEndUse( 2, jEndUse ) = gatherEndUseBySourceBEPS( 2, jEndUse ); //natural gas
				collapsedEndUse( 3, jEndUse ) = gatherEndUseBySourceBEPS( 6, jEndUse ) + gatherEndUseBySourceBEPS( 8, jEndUse ) + gatherEndUseBySourceBEPS( 9, jEndUse ) + gatherEndUseBySourceBEPS( 10, jEndUse ) + gatherEndUseBySourceBEPS( 11, jEndUse ) + gatherEndUseBySourceBEPS( 12, jEndUse ) + gatherEndUseBySourceBEPS( 13, jEndUse ) + gatherEndUseBySourceBEPS( 14, jEndUse ); //Additional fuel  <- gasoline | <- diesel | <- coal | <- fuel oil #1 | <- fuel oil #2 | <- propane | <- otherfuel1 | <- otherfuel2
				collapsedEndUse( 4, jEndUse ) = gatherEndUseBySourceBEPS( 3, jEndUse ); //district cooling <- purchased cooling
				collapsedEndUse( 5, jEndUse ) = gatherEndUseBySourceBEPS( 4, jEndUse ) + gatherEndUseBySourceBEPS( 5, jEndUse ); //district heating <- purchased heating | <- steam
				collapsedEndUse( 6, jEndUse ) = gatherEndUseBySourceBEPS( 7, jEndUse ); //water
			}
			// repeat with totals
			collapsedTotal( 1 ) = gatherTotalsBySourceBEPS( 1 ); //electricity
			collapsedTotal( 2 ) = gatherTotalsBySourceBEPS( 2 ); //natural gas
			collapsedTotal( 3 ) = gatherTotalsBySourceBEPS( 6 ) + gatherTotalsBySourceBEPS( 8 ) + gatherTotalsBySourceBEPS( 9 ) + gatherTotalsBySourceBEPS( 10 ) + gatherTotalsBySourceBEPS( 11 ) + gatherTotalsBySourceBEPS( 12 ) + gatherTotalsBySourceBEPS( 13 ) + gatherTotalsBySourceBEPS( 14 ); //Additional fuel  <- gasoline | <- diesel | <- coal | <- fuel oil #1 | <- fuel oil #2 | <- propane | <- otherfuel1 | <- otherfuel2
			collapsedTotal( 4 ) = gatherTotalsBySourceBEPS( 3 ); //district cooling <- purchased cooling
			collapsedTotal( 5 ) = gatherTotalsBySourceBEPS( 4 ) + gatherTotalsBySourceBEPS( 5 ); //district heating <- purchased heating | <- steam
			collapsedTotal( 6 ) = gatherTotalsBySourceBEPS( 7 ); //water

			// unit conversion - all values are used as divisors

			{ auto const SELECT_CASE_var( unitsStyle );
			if ( SELECT_CASE_var == unitsStyleJtoKWH ) {
				largeConversionFactor = 3600000.0;
				areaConversionFactor = 1.0;
			} else if ( SELECT_CASE_var == unitsStyleInchPound ) {
				largeConversionFactor = getSpecificUnitDivider( "J", "kBtu" ); //1054351.84 J to kBtu
				areaConversionFactor = getSpecificUnitDivider( "m2", "ft2" ); //0.092893973 m2 to ft2
			} else {
				largeConversionFactor = 1000000.0; // to MJ
				areaConversionFactor = 1.0;
			}}

			// convert floor areas
			convBldgCondFloorArea = buildingConditionedFloorArea / areaConversionFactor;

			//convert units into MJ (divide by 1,000,000) if J otherwise kWh
			for ( iResource = 1; iResource <= 5; ++iResource ) { //don't do water
				for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
					collapsedEndUse( iResource, jEndUse ) /= largeConversionFactor;
				}
				collapsedTotal( iResource ) /= largeConversionFactor;
			}

			rowHead.allocate( 16 );
			columnHead.allocate( 5 );
			columnWidth.allocate( 5 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 5, 16 );
			for ( iResource = 1; iResource <= 6; ++iResource ) {
				useVal( iResource, 1 ) = collapsedEndUse( iResource, endUseHeating );
				useVal( iResource, 2 ) = collapsedEndUse( iResource, endUseCooling );
				useVal( iResource, 3 ) = collapsedEndUse( iResource, endUseInteriorLights );
				useVal( iResource, 4 ) = collapsedEndUse( iResource, endUseExteriorLights );
				useVal( iResource, 5 ) = collapsedEndUse( iResource, endUseInteriorEquipment );
				useVal( iResource, 6 ) = collapsedEndUse( iResource, endUseExteriorEquipment );
				useVal( iResource, 7 ) = collapsedEndUse( iResource, endUseFans );
				useVal( iResource, 8 ) = collapsedEndUse( iResource, endUsePumps );
				useVal( iResource, 9 ) = collapsedEndUse( iResource, endUseHeatRejection );
				useVal( iResource, 10 ) = collapsedEndUse( iResource, endUseHumidification );
				useVal( iResource, 11 ) = collapsedEndUse( iResource, endUseHeatRecovery );
				useVal( iResource, 12 ) = collapsedEndUse( iResource, endUseWaterSystem );
				useVal( iResource, 13 ) = collapsedEndUse( iResource, endUseRefrigeration );
				useVal( iResource, 14 ) = collapsedEndUse( iResource, endUseCogeneration );

				useVal( iResource, 15 ) = collapsedTotal( iResource ); // totals
			}

			rowHead( 1 ) = "Heating";
			rowHead( 2 ) = "Cooling";
			rowHead( 3 ) = "Interior Lighting";
			rowHead( 4 ) = "Exterior Lighting";
			rowHead( 5 ) = "Interior Equipment";
			rowHead( 6 ) = "Exterior Equipment";
			rowHead( 7 ) = "Fans";
			rowHead( 8 ) = "Pumps";
			rowHead( 9 ) = "Heat Rejection";
			rowHead( 10 ) = "Humidification";
			rowHead( 11 ) = "Heat Recovery";
			rowHead( 12 ) = "Water Systems";
			rowHead( 13 ) = "Refrigeration";
			rowHead( 14 ) = "Generators";
			rowHead( 15 ) = "";
			rowHead( 16 ) = "Total Source Energy End Use Components";

			largeConversionFactor = 1.0;

			{ auto const SELECT_CASE_var( unitsStyle );
			if ( SELECT_CASE_var == unitsStyleJtoKWH ) {
				columnHead( 1 ) = "Source Electricity [kWh]";
				columnHead( 2 ) = "Source Natural Gas [kWh]";
				columnHead( 3 ) = "Source Additional Fuel [kWh]";
				columnHead( 4 ) = "Source District Cooling [kWh]";
				columnHead( 5 ) = "Source District Heating [kWh]";
			} else if ( SELECT_CASE_var == unitsStyleInchPound ) {
				columnHead( 1 ) = "Source Electricity [kBtu]";
				columnHead( 2 ) = "Source Natural Gas [kBtu]";
				columnHead( 3 ) = "Source Additional Fuel [kBtu]";
				columnHead( 4 ) = "Source District Cooling [kBtu]";
				columnHead( 5 ) = "Source District Heating [kBtu]";
			} else {
				columnHead( 1 ) = "Source Electricity [GJ]";
				columnHead( 2 ) = "Source Natural Gas [GJ]";
				columnHead( 3 ) = "Source Additional Fuel [GJ]";
				columnHead( 4 ) = "Source District Cooling [GJ]";
				columnHead( 5 ) = "Source District Heating [GJ]";
				largeConversionFactor = 1000.0; // for converting MJ to GJ
			}}

			//---- End Uses by Source Energy Sub-Table

			tableBody = "";
			for ( iResource = 1; iResource <= 5; ++iResource ) {
				for ( jEndUse = 1; jEndUse <= 14; ++jEndUse ) {
					tableBody( iResource, jEndUse ) = RealToStr( useVal( iResource, jEndUse ) / largeConversionFactor, 2 );
				}
				tableBody( iResource, 16 ) = RealToStr( useVal( iResource, 15 ) / largeConversionFactor, 2 );
			}

			// heading for the entire sub-table
			WriteSubtitle( "Source Energy End Use Components Summary" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "SourceEnergyEndUseComponentsSummary", "Entire Facility", "Source Energy End Use Components Summary" );
			}

			//---- Normalized by Conditioned Area Sub-Table

			{ auto const SELECT_CASE_var( unitsStyle );
			if ( SELECT_CASE_var == unitsStyleJtoKWH ) {
				columnHead( 1 ) = "Source Electricity [kWh/m2]";
				columnHead( 2 ) = "Source Natural Gas [kWh/m2]";
				columnHead( 3 ) = "Source Additional Fuel [kWh/m2]";
				columnHead( 4 ) = "Source District Cooling [kWh/m2]";
				columnHead( 5 ) = "Source District Heating [kWh/m2]";
			} else if ( SELECT_CASE_var == unitsStyleInchPound ) {
				columnHead( 1 ) = "Source Electricity [kBtu/ft2]";
				columnHead( 2 ) = "Source Natural Gas [kBtu/ft2]";
				columnHead( 3 ) = "Source Additional Fuel [kBtu/ft2]";
				columnHead( 4 ) = "Source District Cooling [kBtu/ft2]";
				columnHead( 5 ) = "Source District Heating [kBtu/ft2]";
			} else {
				columnHead( 1 ) = "Source Electricity [MJ/m2]";
				columnHead( 2 ) = "Source Natural Gas [MJ/m2]";
				columnHead( 3 ) = "Source Additional Fuel [MJ/m2]";
				columnHead( 4 ) = "Source District Cooling [MJ/m2]";
				columnHead( 5 ) = "Source District Heating [MJ/m2]";
			}}

			tableBody = "";
			if ( convBldgCondFloorArea > 0 ) {
				for ( iResource = 1; iResource <= 5; ++iResource ) {
					for ( jEndUse = 1; jEndUse <= 14; ++jEndUse ) {
						tableBody( iResource, jEndUse ) = RealToStr( useVal( iResource, jEndUse ) / convBldgCondFloorArea, 2 );
					}
					tableBody( iResource, 16 ) = RealToStr( useVal( iResource, 15 ) / convBldgCondFloorArea, 2 );
				}
			}

			WriteTextLine( "Normalized Metrics", true );

			// heading for the entire sub-table
			WriteSubtitle( "Source Energy End Use Components Per Conditioned Floor Area" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "SourceEnergyEndUseComponentsSummary", "Entire Facility", "Source Energy End Use Component Per Conditioned Floor Area" );
			}

			//---- Normalized by Total Area Sub-Table
			tableBody = "";
			if ( convBldgCondFloorArea > 0 ) {
				for ( iResource = 1; iResource <= 5; ++iResource ) {
					for ( jEndUse = 1; jEndUse <= 14; ++jEndUse ) {
						tableBody( iResource, jEndUse ) = RealToStr( useVal( iResource, jEndUse ) / convBldgCondFloorArea, 2 );
					}
					tableBody( iResource, 16 ) = RealToStr( useVal( iResource, 15 ) / convBldgCondFloorArea, 2 );
				}
			}

			// heading for the entire sub-table
			WriteSubtitle( "Source Energy End Use Components Per Total Floor Area" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "SourceEnergyEndUseComponentsSummary", "Entire Facility", "Source Energy End Use Components Per Total Floor Area" );
			}

		}
	}

	void
	WriteDemandEndUseSummary()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   January 2009
		//       MODIFIED       January 2010, Kyle Benne
		//                      Added SQLite output
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Take the gathered total and enduse meter data and structure
		//   the results into a tabular report for output.

		// METHODOLOGY EMPLOYED:
		//   Create arrays for the call to WriteTable and then call it.
		//   This report actually consists of many sub-tables each with
		//   its own call to WriteTable.

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputProcessor::MaxNumSubcategories;
		using OutputProcessor::EndUseCategory;
		using DataWater::WaterStorage;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const colElectricity( 1 );
		int const colGas( 2 );
		int const colAdditionalFuel( 3 );
		int const colPurchCool( 4 );
		int const colPurchHeat( 5 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// all arrays are in the format: (row, column)
		Array1D_string columnHead;
		Array1D_int columnWidth;
		Array1D_string rowHead;
		Array2D_string tableBody;

		// all arrays are in the format: (row, columnm)
		Array2D< Real64 > useVal( 6, 15 );
		Array1D< Real64 > collapsedTotal( 6 );
		Array2D< Real64 > collapsedEndUse( 6, NumEndUses );
		Array1D_int collapsedTimeStep( 6 );
		Array3D< Real64 > collapsedEndUseSub( MaxNumSubcategories, NumEndUses, 6 );
		int iResource;
		int jEndUse;
		int kEndUseSub;
		int i;
		int numRows;
		static std::string footnote;
		Real64 additionalFuelMax;
		int additionalFuelSelected;
		int additionalFuelNonZeroCount;
		int distrHeatSelected;
		bool bothDistrHeatNonZero;
		Real64 powerConversion;
		Real64 flowConversion;

		Array1D< Real64 > leedFansParkFromFan( 6 );
		Array1D< Real64 > leedFansParkFromExtFuelEquip( 6 );
		Array1D< Real64 > leedIntLightProc( 6 );
		Array1D< Real64 > leedCook( 6 );
		Array1D< Real64 > leedIndProc( 6 );
		Array1D< Real64 > leedElevEsc( 6 );
		Real64 unconvert;
		std::string subCatName;

		if ( displayDemandEndUse ) {
			// show the headers of the report
			WriteReportHeaders( "Demand End Use Components Summary", "Entire Facility", isAverage );
			// totals - select which additional fuel to display and which other district heating
			collapsedTotal = 0.0;
			collapsedTotal( 1 ) = gatherDemandTotal( 1 ); //electricity
			collapsedTimeStep( 1 ) = gatherDemandTimeStamp( 1 );
			collapsedTotal( 2 ) = gatherDemandTotal( 2 ); //natural gas
			collapsedTimeStep( 2 ) = gatherDemandTimeStamp( 2 );
			collapsedTotal( 4 ) = gatherDemandTotal( 3 ); //district cooling <- purchased cooling
			collapsedTimeStep( 4 ) = gatherDemandTimeStamp( 3 );
			collapsedTotal( 6 ) = gatherDemandTotal( 7 ); //water
			collapsedTimeStep( 6 ) = gatherDemandTimeStamp( 7 );
			// select which of the additional fuels should be displayed based on which has the highest
			// demand. This is usually likely to be the only additional fuel that is actually being used.
			// If an additional fuel is non-zero, a footnote to the table is added.
			// First step is to see if any additional fuels are non-zero
			additionalFuelNonZeroCount = 0;
			if ( gatherDemandTotal( 6 ) > 0.0 ) ++additionalFuelNonZeroCount;
			if ( gatherDemandTotal( 8 ) > 0.0 ) ++additionalFuelNonZeroCount;
			if ( gatherDemandTotal( 9 ) > 0.0 ) ++additionalFuelNonZeroCount;
			if ( gatherDemandTotal( 10 ) > 0.0 ) ++additionalFuelNonZeroCount;
			if ( gatherDemandTotal( 11 ) > 0.0 ) ++additionalFuelNonZeroCount;
			if ( gatherDemandTotal( 12 ) > 0.0 ) ++additionalFuelNonZeroCount;
			if ( gatherDemandTotal( 13 ) > 0.0 ) ++additionalFuelNonZeroCount;
			if ( gatherDemandTotal( 14 ) > 0.0 ) ++additionalFuelNonZeroCount;
			if ( additionalFuelNonZeroCount > 1 ) {
				footnote = "Additional fuels have non-zero demand but are not shown on this report.";
			}
			//assuming that at least one of these is non-zero
			additionalFuelSelected = 12; //default is propane if no other given
			additionalFuelMax = gatherDemandTotal( 12 );
			if ( additionalFuelNonZeroCount > 0 ) {
				if ( gatherDemandTotal( 6 ) > additionalFuelMax ) { // gasoline
					additionalFuelSelected = 6;
					additionalFuelMax = gatherDemandTotal( 6 );
				}
				if ( gatherDemandTotal( 8 ) > additionalFuelMax ) { // diesel
					additionalFuelSelected = 8;
					additionalFuelMax = gatherDemandTotal( 8 );
				}
				if ( gatherDemandTotal( 9 ) > additionalFuelMax ) { // coal
					additionalFuelSelected = 9;
					additionalFuelMax = gatherDemandTotal( 9 );
				}
				if ( gatherDemandTotal( 10 ) > additionalFuelMax ) { // fuel oil #1
					additionalFuelSelected = 10;
					additionalFuelMax = gatherDemandTotal( 10 );
				}
				if ( gatherDemandTotal( 11 ) > additionalFuelMax ) { // fuel oil #2
					additionalFuelSelected = 11;
					additionalFuelMax = gatherDemandTotal( 11 );
				}
				if ( gatherDemandTotal( 12 ) > additionalFuelMax ) { // propane
					additionalFuelSelected = 12;
					additionalFuelMax = gatherDemandTotal( 12 );
				}
				if ( gatherDemandTotal( 13 ) > additionalFuelMax ) { // otherfuel1
					additionalFuelSelected = 13;
					additionalFuelMax = gatherDemandTotal( 13 );
				}
				if ( gatherDemandTotal( 14 ) > additionalFuelMax ) { // otherfuel2
					additionalFuelSelected = 14;
					additionalFuelMax = gatherDemandTotal( 14 );
				}
			}
			//set the time of peak demand and total demand for the additinoal fuel selected
			collapsedTimeStep( 3 ) = gatherDemandTimeStamp( additionalFuelSelected );
			collapsedTotal( 3 ) = gatherDemandTotal( additionalFuelSelected );
			//set flag if both puchased heating and steam both have positive demand
			bothDistrHeatNonZero = ( gatherDemandTotal( 4 ) > 0.0 ) && ( gatherDemandTotal( 5 ) > 0.0 );
			//select the district heating source that has a larger demand
			if ( gatherDemandTotal( 4 ) > gatherDemandTotal( 5 ) ) {
				distrHeatSelected = 4; // purchased heating
				if ( bothDistrHeatNonZero ) {
					footnote += " Steam has non-zero demand but is not shown on this report.";
				}
			} else {
				distrHeatSelected = 5; // steam
				if ( bothDistrHeatNonZero ) {
					footnote += " District heating has non-zero demand but is not shown on this report.";
				}
			}
			//set the time of peak demand and total demand for the purchased heating/steam
			collapsedTimeStep( 5 ) = gatherDemandTimeStamp( distrHeatSelected );
			collapsedTotal( 5 ) = gatherDemandTotal( distrHeatSelected );

			//establish unit conversion factors
			if ( unitsStyle == unitsStyleInchPound ) {
				powerConversion = getSpecificUnitMultiplier( "W", "kBtuh" );
				flowConversion = getSpecificUnitMultiplier( "m3/s", "gal/min" );
			} else {
				powerConversion = 1.0;
				flowConversion = 1.0;
			}

			// collapse the gatherEndUseBEPS array to the resource groups displayed
			collapsedEndUse = 0.0;
			for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
				collapsedEndUse( 1, jEndUse ) = gatherDemandEndUse( 1, jEndUse ) * powerConversion; //electricity
				collapsedEndUse( 2, jEndUse ) = gatherDemandEndUse( 2, jEndUse ) * powerConversion; //natural gas
				collapsedEndUse( 3, jEndUse ) = gatherDemandEndUse( additionalFuelSelected, jEndUse ) * powerConversion; //additional fuel
				collapsedEndUse( 4, jEndUse ) = gatherDemandEndUse( 3, jEndUse ) * powerConversion; // purchased cooling
				collapsedEndUse( 5, jEndUse ) = gatherDemandEndUse( distrHeatSelected, jEndUse ) * powerConversion; //district heating
				collapsedEndUse( 6, jEndUse ) = gatherDemandEndUse( 7, jEndUse ) * flowConversion; //water
			}
			for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
				for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
					collapsedEndUseSub( kEndUseSub, jEndUse, 1 ) = gatherDemandEndUseSub( kEndUseSub, jEndUse, 1 ) * powerConversion; //electricity
					collapsedEndUseSub( kEndUseSub, jEndUse, 2 ) = gatherDemandEndUseSub( kEndUseSub, jEndUse, 2 ) * powerConversion; //natural gas
					collapsedEndUseSub( kEndUseSub, jEndUse, 3 ) = gatherDemandEndUseSub( kEndUseSub, jEndUse, additionalFuelSelected ) * powerConversion; //additional fuel
					collapsedEndUseSub( kEndUseSub, jEndUse, 4 ) = gatherDemandEndUseSub( kEndUseSub, jEndUse, 3 ) * powerConversion; //purch cooling
					collapsedEndUseSub( kEndUseSub, jEndUse, 5 ) = gatherDemandEndUseSub( kEndUseSub, jEndUse, distrHeatSelected ) * powerConversion; //district heating
					collapsedEndUseSub( kEndUseSub, jEndUse, 6 ) = gatherDemandEndUseSub( kEndUseSub, jEndUse, 7 ) * flowConversion; //water
				}
			}
			//convert totals
			collapsedTotal( 1 ) *= powerConversion; //electricity
			collapsedTotal( 2 ) *= powerConversion; //natural gas
			collapsedTotal( 3 ) *= powerConversion; //additional fuel
			collapsedTotal( 4 ) *= powerConversion; //purchased cooling
			collapsedTotal( 5 ) *= powerConversion; //district heating
			collapsedTotal( 6 ) *= flowConversion; //water
			//---- End Use Sub-Table
			rowHead.allocate( 17 );
			columnHead.allocate( 6 );
			columnWidth.allocate( 6 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 6, 17 );
			for ( iResource = 1; iResource <= 6; ++iResource ) {
				useVal( iResource, 1 ) = collapsedEndUse( iResource, endUseHeating );
				useVal( iResource, 2 ) = collapsedEndUse( iResource, endUseCooling );
				useVal( iResource, 3 ) = collapsedEndUse( iResource, endUseInteriorLights );
				useVal( iResource, 4 ) = collapsedEndUse( iResource, endUseExteriorLights );
				useVal( iResource, 5 ) = collapsedEndUse( iResource, endUseInteriorEquipment );
				useVal( iResource, 6 ) = collapsedEndUse( iResource, endUseExteriorEquipment );
				useVal( iResource, 7 ) = collapsedEndUse( iResource, endUseFans );
				useVal( iResource, 8 ) = collapsedEndUse( iResource, endUsePumps );
				useVal( iResource, 9 ) = collapsedEndUse( iResource, endUseHeatRejection );
				useVal( iResource, 10 ) = collapsedEndUse( iResource, endUseHumidification );
				useVal( iResource, 11 ) = collapsedEndUse( iResource, endUseHeatRecovery );
				useVal( iResource, 12 ) = collapsedEndUse( iResource, endUseWaterSystem );
				useVal( iResource, 13 ) = collapsedEndUse( iResource, endUseRefrigeration );
				useVal( iResource, 14 ) = collapsedEndUse( iResource, endUseCogeneration );
				useVal( iResource, 15 ) = collapsedTotal( iResource ); // totals
			}

			rowHead( 1 ) = "Time of Peak";
			rowHead( 2 ) = "Heating";
			rowHead( 3 ) = "Cooling";
			rowHead( 4 ) = "Interior Lighting";
			rowHead( 5 ) = "Exterior Lighting";
			rowHead( 6 ) = "Interior Equipment";
			rowHead( 7 ) = "Exterior Equipment";
			rowHead( 8 ) = "Fans";
			rowHead( 9 ) = "Pumps";
			rowHead( 10 ) = "Heat Rejection";
			rowHead( 11 ) = "Humidification";
			rowHead( 12 ) = "Heat Recovery";
			rowHead( 13 ) = "Water Systems";
			rowHead( 14 ) = "Refrigeration";
			rowHead( 15 ) = "Generators";
			rowHead( 16 ) = "";
			rowHead( 17 ) = "Total End Uses";

			if ( unitsStyle == unitsStyleInchPound ) {
				columnHead( 1 ) = "Electricity [kBtuh]";
				columnHead( 2 ) = "Natural Gas [kBtuh]";
				{ auto const SELECT_CASE_var( additionalFuelSelected );
				if ( SELECT_CASE_var == 6 ) { // gasoline
					columnHead( 3 ) = "Gasoline [kBtuh]";
				} else if ( SELECT_CASE_var == 8 ) { // Diesel
					columnHead( 3 ) = "Diesel [kBtuh]";
				} else if ( SELECT_CASE_var == 9 ) { // Coal
					columnHead( 3 ) = "Coal [kBtuh]";
				} else if ( SELECT_CASE_var == 10 ) { // Fuel Oil #1
					columnHead( 3 ) = "Fuel Oil #1 [kBtuh]";
				} else if ( SELECT_CASE_var == 11 ) { // Fuel Oil #2
					columnHead( 3 ) = "Fuel Oil #2 [kBtuh]";
				} else if ( SELECT_CASE_var == 12 ) { // Propane
					columnHead( 3 ) = "Propane [kBtuh]";
				} else if ( SELECT_CASE_var == 13 ) { // OtherFuel1
					columnHead( 3 ) = "Other Fuel 1 [kBtuh]";
				} else if ( SELECT_CASE_var == 14 ) { // OtherFuel2
					columnHead( 3 ) = "Other Fuel 2 [kBtuh]";
				}}
				columnHead( 4 ) = "District Cooling [kBtuh]";
				{ auto const SELECT_CASE_var( distrHeatSelected );
				if ( SELECT_CASE_var == 4 ) {
					columnHead( 5 ) = "District Heating [kBtuh]";
				} else if ( SELECT_CASE_var == 5 ) {
					columnHead( 5 ) = "Steam [kBtuh]";
				}}
				columnHead( 6 ) = "Water [gal/min]";
			} else {
				columnHead( 1 ) = "Electricity [W]";
				columnHead( 2 ) = "Natural Gas [W]";
				{ auto const SELECT_CASE_var( additionalFuelSelected );
				if ( SELECT_CASE_var == 6 ) { // gasoline
					columnHead( 3 ) = "Gasoline [W]";
				} else if ( SELECT_CASE_var == 8 ) { // Diesel
					columnHead( 3 ) = "Diesel [W]";
				} else if ( SELECT_CASE_var == 9 ) { // Coal
					columnHead( 3 ) = "Coal [W]";
				} else if ( SELECT_CASE_var == 10 ) { // Fuel Oil #1
					columnHead( 3 ) = "Fuel Oil #1 [W]";
				} else if ( SELECT_CASE_var == 11 ) { // Fuel Oil #2
					columnHead( 3 ) = "Fuel Oil #2 [W]";
				} else if ( SELECT_CASE_var == 12 ) { // Propane
					columnHead( 3 ) = "Propane [W]";
				} else if ( SELECT_CASE_var == 13 ) { // OtherFuel1
					columnHead( 3 ) = "Other Fuel 1 [W]";
				} else if ( SELECT_CASE_var == 14 ) { // OtherFuel2
					columnHead( 3 ) = "Other Fuel 2 [W]";
				}}
				columnHead( 4 ) = "District Cooling [W]";
				{ auto const SELECT_CASE_var( distrHeatSelected );
				if ( SELECT_CASE_var == 4 ) {
					columnHead( 5 ) = "District Heating [W]";
				} else if ( SELECT_CASE_var == 5 ) {
					columnHead( 5 ) = "Steam [W]";
				}}
				columnHead( 6 ) = "Water [m3/s]";
			}

			tableBody = "";
			for ( iResource = 1; iResource <= 6; ++iResource ) {
				for ( jEndUse = 1; jEndUse <= 14; ++jEndUse ) {
					tableBody( iResource, 1 + jEndUse ) = RealToStr( useVal( iResource, jEndUse ), 2 );
				}
				tableBody( iResource, 1 ) = DateToString( collapsedTimeStep( iResource ) );
				tableBody( iResource, 17 ) = RealToStr( collapsedTotal( iResource ), 2 );
			}

			//complete the LEED end use table using the same values
			// for certain rows in the LEED table the subcategories are necessary so first compute those values
			leedFansParkFromFan = 0.0;
			leedFansParkFromExtFuelEquip = 0.0;
			leedIntLightProc = 0.0;
			leedCook = 0.0;
			leedIndProc = 0.0;
			leedElevEsc = 0.0;
			for ( iResource = 1; iResource <= 5; ++iResource ) { // don't bother with water
				for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
					if ( EndUseCategory( jEndUse ).NumSubcategories > 0 ) {
						for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
							subCatName = EndUseCategory( jEndUse ).SubcategoryName( kEndUseSub );
							if ( SameString( subCatName, "Fans - Parking Garage" ) ) {
								if ( jEndUse == 7 ) { //fans
									leedFansParkFromFan( iResource ) += collapsedEndUseSub( kEndUseSub, jEndUse, iResource );
								} else {
									leedFansParkFromExtFuelEquip( iResource ) += collapsedEndUseSub( kEndUseSub, jEndUse, iResource );
								}
							} else if ( SameString( subCatName, "Interior Lighting - Process" ) ) {
								leedIntLightProc( iResource ) += collapsedEndUseSub( kEndUseSub, jEndUse, iResource );
							} else if ( SameString( subCatName, "Cooking" ) ) {
								leedCook( iResource ) += collapsedEndUseSub( kEndUseSub, jEndUse, iResource );
							} else if ( SameString( subCatName, "Industrial Process" ) ) {
								leedIndProc( iResource ) += collapsedEndUseSub( kEndUseSub, jEndUse, iResource );
							} else if ( SameString( subCatName, "Elevators and Escalators" ) ) {
								leedElevEsc( iResource ) += collapsedEndUseSub( kEndUseSub, jEndUse, iResource );
							}
						}
					}
				}
			}

			//complete the LEED end use table using the same values
			unconvert = 1 / powerConversion;
			PreDefTableEntry( pdchLeedPerfElDem, "Interior Lighting", unconvert * ( useVal( colElectricity, 3 ) - leedIntLightProc( colElectricity ) ), 2 );
			PreDefTableEntry( pdchLeedPerfElDem, "Exterior Lighting", unconvert * useVal( colElectricity, 4 ), 2 );
			PreDefTableEntry( pdchLeedPerfElDem, "Space Heating", unconvert * useVal( colElectricity, 1 ), 2 );
			PreDefTableEntry( pdchLeedPerfElDem, "Space Cooling", unconvert * useVal( colElectricity, 2 ), 2 );
			PreDefTableEntry( pdchLeedPerfElDem, "Pumps", unconvert * useVal( colElectricity, 8 ), 2 );
			PreDefTableEntry( pdchLeedPerfElDem, "Heat Rejection", unconvert * useVal( colElectricity, 9 ), 2 );
			PreDefTableEntry( pdchLeedPerfElDem, "Fans-Interior", unconvert * ( useVal( colElectricity, 7 ) - leedFansParkFromFan( colElectricity ) ), 2 );
			PreDefTableEntry( pdchLeedPerfElDem, "Fans-Parking Garage", unconvert * ( leedFansParkFromFan( colElectricity ) + leedFansParkFromExtFuelEquip( colElectricity ) ), 2 );
			PreDefTableEntry( pdchLeedPerfElDem, "Service Water Heating", unconvert * useVal( colElectricity, 12 ), 2 );
			PreDefTableEntry( pdchLeedPerfElDem, "Receptacle Equipment", unconvert * useVal( colElectricity, 5 ), 2 );
			PreDefTableEntry( pdchLeedPerfElDem, "Interior Lighting (process)", unconvert * leedIntLightProc( colElectricity ), 2 );
			PreDefTableEntry( pdchLeedPerfElDem, "Refrigeration Equipment", unconvert * useVal( colElectricity, 13 ), 2 );
			PreDefTableEntry( pdchLeedPerfElDem, "Cooking", unconvert * leedCook( colElectricity ), 2 );
			PreDefTableEntry( pdchLeedPerfElDem, "Industrial Process", unconvert * leedIndProc( colElectricity ), 2 );
			PreDefTableEntry( pdchLeedPerfElDem, "Elevators and Escalators", unconvert * leedElevEsc( colElectricity ), 2 );
			//CALL PreDefTableEntry(pdchLeedPerfElDem,'Total',useVal(15,colElectricity),2)

			PreDefTableEntry( pdchLeedPerfGasDem, "Interior Lighting", unconvert * ( useVal( colGas, 3 ) - leedIntLightProc( colGas ) ), 2 );
			PreDefTableEntry( pdchLeedPerfGasDem, "Exterior Lighting", unconvert * useVal( colGas, 4 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasDem, "Space Heating", unconvert * useVal( colGas, 1 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasDem, "Space Cooling", unconvert * useVal( colGas, 2 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasDem, "Pumps", unconvert * useVal( colGas, 8 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasDem, "Heat Rejection", unconvert * useVal( colGas, 9 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasDem, "Fans-Interior", unconvert * ( useVal( colGas, 7 ) - leedFansParkFromFan( colGas ) ), 2 );
			PreDefTableEntry( pdchLeedPerfGasDem, "Fans-Parking Garage", unconvert * ( leedFansParkFromFan( colGas ) + leedFansParkFromExtFuelEquip( colGas ) ), 2 );
			PreDefTableEntry( pdchLeedPerfGasDem, "Service Water Heating", unconvert * useVal( colGas, 12 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasDem, "Receptacle Equipment", unconvert * useVal( colGas, 5 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasDem, "Interior Lighting (process)", unconvert * leedIntLightProc( colGas ), 2 );
			PreDefTableEntry( pdchLeedPerfGasDem, "Refrigeration Equipment", unconvert * useVal( colGas, 13 ), 2 );
			PreDefTableEntry( pdchLeedPerfGasDem, "Cooking", unconvert * leedCook( colGas ), 2 );
			PreDefTableEntry( pdchLeedPerfGasDem, "Industrial Process", unconvert * leedIndProc( colGas ), 2 );
			PreDefTableEntry( pdchLeedPerfGasDem, "Elevators and Escalators", unconvert * leedElevEsc( colGas ), 2 );
			//CALL PreDefTableEntry(pdchLeedPerfGasDem,'Total',useVal(15,colGas),2)

			PreDefTableEntry( pdchLeedPerfOthDem, "Interior Lighting", unconvert * ( useVal( colAdditionalFuel, 3 ) + useVal( colPurchCool, 3 ) + useVal( colPurchHeat, 3 ) - ( leedIntLightProc( colAdditionalFuel ) + leedIntLightProc( colPurchCool ) + leedIntLightProc( colPurchHeat ) ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthDem, "Exterior Lighting", unconvert * ( useVal( colAdditionalFuel, 4 ) + useVal( colPurchCool, 4 ) + useVal( colPurchHeat, 4 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthDem, "Space Heating", unconvert * ( useVal( colAdditionalFuel, 1 ) + useVal( colPurchCool, 1 ) + useVal( colPurchHeat, 1 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthDem, "Space Cooling", unconvert * ( useVal( colAdditionalFuel, 2 ) + useVal( colPurchCool, 2 ) + useVal( colPurchHeat, 2 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthDem, "Pumps", unconvert * ( useVal( colAdditionalFuel, 8 ) + useVal( colPurchCool, 8 ) + useVal( colPurchHeat, 8 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthDem, "Heat Rejection", unconvert * ( useVal( colAdditionalFuel, 9 ) + useVal( colPurchCool, 9 ) + useVal( colPurchHeat, 9 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthDem, "Fans-Interior", unconvert * ( useVal( colAdditionalFuel, 7 ) + useVal( colPurchCool, 7 ) + useVal( colPurchHeat, 7 ) - ( leedFansParkFromFan( colAdditionalFuel ) + leedFansParkFromFan( colPurchCool ) + leedFansParkFromFan( colPurchHeat ) ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthDem, "Fans-Parking Garage", unconvert * ( leedFansParkFromFan( colAdditionalFuel ) + leedFansParkFromFan( colPurchCool ) + leedFansParkFromFan( colPurchHeat ) + leedFansParkFromExtFuelEquip( colAdditionalFuel ) + leedFansParkFromExtFuelEquip( colPurchCool ) + leedFansParkFromExtFuelEquip( colPurchHeat ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthDem, "Service Water Heating", unconvert * ( useVal( colAdditionalFuel, 12 ) + useVal( colPurchCool, 12 ) + useVal( colPurchHeat, 12 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthDem, "Receptacle Equipment", unconvert * ( useVal( colAdditionalFuel, 5 ) + useVal( colPurchCool, 5 ) + useVal( colPurchHeat, 5 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthDem, "Interior Lighting (process)", unconvert * ( leedIntLightProc( colAdditionalFuel ) + leedIntLightProc( colPurchCool ) + leedIntLightProc( colPurchHeat ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthDem, "Refrigeration Equipment", unconvert * ( useVal( colAdditionalFuel, 13 ) + useVal( colPurchCool, 13 ) + useVal( colPurchHeat, 13 ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthDem, "Cooking", unconvert * ( leedCook( colAdditionalFuel ) + leedCook( colPurchCool ) + leedCook( colPurchHeat ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthDem, "Industrial Process", unconvert * ( leedIndProc( colAdditionalFuel ) + leedIndProc( colPurchCool ) + leedIndProc( colPurchHeat ) ), 2 );
			PreDefTableEntry( pdchLeedPerfOthDem, "Elevators and Escalators", unconvert * ( leedElevEsc( colAdditionalFuel ) + leedElevEsc( colPurchCool ) + leedElevEsc( colPurchHeat ) ), 2 );
			//CALL PreDefTableEntry(pdchLeedPerfOthDem,'Total',useVal(15,colAdditionalFuel) + useVal(15,colPurchCool) + useVal(15,colPurchHeat),2)

			WriteSubtitle( "End Uses" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth, false, footnote );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "DemandEndUseComponentsSummary", "Entire Facility", "End Uses" );
			}

			//---- End Uses By Subcategory Sub-Table
			numRows = 0;
			for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
				if ( EndUseCategory( jEndUse ).NumSubcategories > 0 ) {
					for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
						++numRows;
					}
				} else {
					++numRows;
				}
			}

			rowHead.allocate( numRows );
			columnHead.allocate( 7 );
			columnWidth.allocate( 7 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 7, numRows );

			rowHead = "";
			tableBody = "";

			// Build row head and subcategories columns
			i = 1;
			for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
				rowHead( i ) = EndUseCategory( jEndUse ).DisplayName;
				if ( EndUseCategory( jEndUse ).NumSubcategories > 0 ) {
					for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
						tableBody( 1, i ) = EndUseCategory( jEndUse ).SubcategoryName( kEndUseSub );
						++i;
					}
				} else {
					tableBody( 1, i ) = "General";
					++i;
				}
			}

			if ( unitsStyle == unitsStyleInchPound ) {
				columnHead( 1 ) = "Subcategory";
				columnHead( 2 ) = "Electricity [kBtuh]";
				columnHead( 3 ) = "Natural Gas [kBtuh]";
				{ auto const SELECT_CASE_var( additionalFuelSelected );
				if ( SELECT_CASE_var == 6 ) { // gasoline
					columnHead( 4 ) = "Gasoline [kBtuh]";
				} else if ( SELECT_CASE_var == 8 ) { // Diesel
					columnHead( 4 ) = "Diesel [kBtuh]";
				} else if ( SELECT_CASE_var == 9 ) { // Coal
					columnHead( 4 ) = "Coal [kBtuh]";
				} else if ( SELECT_CASE_var == 10 ) { // Fuel Oil #1
					columnHead( 4 ) = "Fuel Oil #1 [kBtuh]";
				} else if ( SELECT_CASE_var == 11 ) { // Fuel Oil #2
					columnHead( 4 ) = "Fuel Oil #2 [kBtuh]";
				} else if ( SELECT_CASE_var == 12 ) { // Propane
					columnHead( 4 ) = "Propane [kBtuh]";
				} else if ( SELECT_CASE_var == 13 ) { // OtherFuel1
					columnHead( 4 ) = "Other Fuel 1 [kBtuh]";
				} else if ( SELECT_CASE_var == 14 ) { // OtherFuel2
					columnHead( 4 ) = "Other Fuel 2 [kBtuh]";
				}}
				columnHead( 5 ) = "District Cooling [kBtuh]";
				{ auto const SELECT_CASE_var( distrHeatSelected );
				if ( SELECT_CASE_var == 4 ) {
					columnHead( 6 ) = "District Heating [kBtuh]";
				} else if ( SELECT_CASE_var == 5 ) {
					columnHead( 6 ) = "Steam [kBtuh]";
				}}
				columnHead( 7 ) = "Water [gal/min]";
			} else {
				columnHead( 1 ) = "Subcategory";
				columnHead( 2 ) = "Electricity [W]";
				columnHead( 3 ) = "Natural Gas [W]";
				{ auto const SELECT_CASE_var( additionalFuelSelected );
				if ( SELECT_CASE_var == 6 ) { // gasoline
					columnHead( 4 ) = "Gasoline [W]";
				} else if ( SELECT_CASE_var == 8 ) { // Diesel
					columnHead( 4 ) = "Diesel [W]";
				} else if ( SELECT_CASE_var == 9 ) { // Coal
					columnHead( 4 ) = "Coal [W]";
				} else if ( SELECT_CASE_var == 10 ) { // Fuel Oil #1
					columnHead( 4 ) = "Fuel Oil #1 [W]";
				} else if ( SELECT_CASE_var == 11 ) { // Fuel Oil #2
					columnHead( 4 ) = "Fuel Oil #2 [W]";
				} else if ( SELECT_CASE_var == 12 ) { // Propane
					columnHead( 4 ) = "Propane [W]";
				} else if ( SELECT_CASE_var == 13 ) { // OtherFuel1
					columnHead( 4 ) = "Other Fuel 1 [W]";
				} else if ( SELECT_CASE_var == 14 ) { // OtherFuel2
					columnHead( 4 ) = "Other Fuel 2 [W]";
				}}
				columnHead( 5 ) = "District Cooling [W]";
				{ auto const SELECT_CASE_var( distrHeatSelected );
				if ( SELECT_CASE_var == 4 ) {
					columnHead( 6 ) = "District Heating [W]";
				} else if ( SELECT_CASE_var == 5 ) {
					columnHead( 6 ) = "Steam [W]";
				}}
				columnHead( 7 ) = "Water [m3/s]";
			}

			for ( iResource = 1; iResource <= 6; ++iResource ) {
				i = 1;
				for ( jEndUse = 1; jEndUse <= NumEndUses; ++jEndUse ) {
					if ( EndUseCategory( jEndUse ).NumSubcategories > 0 ) {
						for ( kEndUseSub = 1; kEndUseSub <= EndUseCategory( jEndUse ).NumSubcategories; ++kEndUseSub ) {
							tableBody( iResource + 1, i ) = RealToStr( collapsedEndUseSub( kEndUseSub, jEndUse, iResource ), 2 );
							++i;
						}
					} else {
						tableBody( iResource + 1, i ) = RealToStr( collapsedEndUse( iResource, jEndUse ), 2 );
						++i;
					}
				}
			}

			// heading for the entire sub-table
			WriteSubtitle( "End Uses By Subcategory" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth, false, footnote );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "DemandEndUseComponentsSummary", "Entire Facility", "End Uses By Subcategory" );
			}
		}
	}

	void
	WriteCompCostTable()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         BGriffith
		//       DATE WRITTEN   April/May 2004
		//       MODIFIED       January 2010, Kyle Benne
		//                      Added SQLite output
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// produce a results table from Cost Estimate Calculations

		// METHODOLOGY EMPLOYED:
		// USE data from CostEstimateManager, call JGlazer's subroutines

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataCostEstimate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array2D< Real64 > TableBodyData( 3, 10 );
		Real64 RefBldgConstCost; // holds interim value for construction component costs: reference bldg.
		Real64 CurntBldgConstCost; // holds interim value for construction component costs: current bldg.
		Array1D_string columnHead;
		Array1D_int columnWidth;
		Array1D_string rowHead;
		Array2D_string tableBody;
		int item; // do-loop counter for line items
		int NumRows; // number of rows in report table excluding table header
		int NumCols; // number of columns in report table
		static std::string SIunit;
		static std::string m2_unitName;
		static Real64 m2_unitConv( 0.0 );
		static int unitConvIndex( 0 );
		static std::string IPunitName;
		Real64 IPqty;
		Real64 IPsingleValue;
		Real64 IPvaluePer;

		if ( ! DoCostEstimate ) return;

		WriteReportHeaders( "Component Cost Economics Summary", "Entire Facility", isAverage );

		// compute floor area if no ABUPS
		if ( buildingConditionedFloorArea == 0.0 ) {
			DetermineBuildingFloorArea();
		}

		// 1st sub-table with total Costs and normalized with area
		rowHead.allocate( 10 );
		columnHead.allocate( 3 );
		columnWidth.allocate( 3 );
		columnWidth = 14; //array assignment - same for all columns
		tableBody.allocate( 3, 10 );

		columnHead( 1 ) = "Reference Bldg.";
		columnHead( 2 ) = "Current Bldg. Model";
		columnHead( 3 ) = "Difference";

		rowHead( 1 ) = "Line Item SubTotal (~~$~~)";
		rowHead( 2 ) = "Misc. Costs (~~$~~)";
		rowHead( 3 ) = "Regional Adjustment (~~$~~)";
		rowHead( 4 ) = "Design Fee (~~$~~)";
		rowHead( 5 ) = "Contractor Fee (~~$~~)";
		rowHead( 6 ) = "Contingency (~~$~~)";
		rowHead( 7 ) = "Permits, Bonds, Insurance (~~$~~)";
		rowHead( 8 ) = "Commissioning (~~$~~)";
		rowHead( 9 ) = "Cost Estimate Total (~~$~~)";
		if ( unitsStyle == unitsStyleInchPound ) {
			SIunit = "[m2]";
			LookupSItoIP( SIunit, unitConvIndex, m2_unitName );
			m2_unitConv = ConvertIP( unitConvIndex, 1.0 );
			rowHead( 10 ) = "Cost Per Conditioned Building Area (~~$~~/ft2)";
		} else {
			rowHead( 10 ) = "Cost Per Conditioned Building Area (~~$~~/m2)";
			m2_unitConv = 1.0;
		}
		TableBodyData = 0.0;
		tableBody = "";

		TableBodyData( 1, 1 ) = RefrncBldg.LineItemTot;
		tableBody( 1, 1 ) = RealToStr( TableBodyData( 1, 1 ), 2 );
		TableBodyData( 1, 2 ) = RefrncBldg.MiscCostperSqMeter * buildingConditionedFloorArea;
		tableBody( 1, 2 ) = RealToStr( TableBodyData( 1, 2 ), 2 );

		if ( RefrncBldg.RegionalModifier != 1.0 ) {
			TableBodyData( 1, 3 ) = ( RefrncBldg.LineItemTot + RefrncBldg.MiscCostperSqMeter * buildingConditionedFloorArea ) * ( RefrncBldg.RegionalModifier - 1.0 );
		} else {
			TableBodyData( 1, 3 ) = 0.0;
		}

		RefBldgConstCost = sum( TableBodyData( 1, {1,3} ) );

		tableBody( 1, 3 ) = RealToStr( TableBodyData( 1, 3 ), 2 );
		TableBodyData( 1, 4 ) = RefBldgConstCost * RefrncBldg.DesignFeeFrac;
		tableBody( 1, 4 ) = RealToStr( TableBodyData( 1, 4 ), 2 );
		TableBodyData( 1, 5 ) = RefBldgConstCost * RefrncBldg.ContractorFeeFrac;
		tableBody( 1, 5 ) = RealToStr( TableBodyData( 1, 5 ), 2 );
		TableBodyData( 1, 6 ) = RefBldgConstCost * RefrncBldg.ContingencyFrac;
		tableBody( 1, 6 ) = RealToStr( TableBodyData( 1, 6 ), 2 );
		TableBodyData( 1, 7 ) = RefBldgConstCost * RefrncBldg.BondCostFrac;
		tableBody( 1, 7 ) = RealToStr( TableBodyData( 1, 7 ), 2 );
		TableBodyData( 1, 8 ) = RefBldgConstCost * RefrncBldg.CommissioningFrac;
		tableBody( 1, 8 ) = RealToStr( TableBodyData( 1, 8 ), 2 );
		RefrncBldg.GrandTotal = sum( TableBodyData( 1, {1,8} ) );
		TableBodyData( 1, 9 ) = RefrncBldg.GrandTotal;
		tableBody( 1, 9 ) = RealToStr( TableBodyData( 1, 9 ), 2 );
		if ( buildingConditionedFloorArea > 0.0 ) {
			TableBodyData( 1, 10 ) = TableBodyData( 1, 9 ) / ( buildingConditionedFloorArea * m2_unitConv );
		}
		tableBody( 1, 10 ) = RealToStr( TableBodyData( 1, 10 ), 2 );

		TableBodyData( 2, 1 ) = CurntBldg.LineItemTot;
		tableBody( 2, 1 ) = RealToStr( TableBodyData( 2, 1 ), 2 );
		TableBodyData( 2, 2 ) = CurntBldg.MiscCostperSqMeter * buildingConditionedFloorArea;
		tableBody( 2, 2 ) = RealToStr( TableBodyData( 2, 2 ), 2 );
		if ( CurntBldg.RegionalModifier != 1.0 ) {
			TableBodyData( 2, 3 ) = ( CurntBldg.LineItemTot + CurntBldg.MiscCostperSqMeter * buildingConditionedFloorArea ) * ( CurntBldg.RegionalModifier - 1.0 );
		} else {
			TableBodyData( 2, 3 ) = 0.0;
		}
		tableBody( 2, 3 ) = RealToStr( TableBodyData( 2, 3 ), 2 );

		CurntBldgConstCost = sum( TableBodyData( 2, {1,3} ) );

		TableBodyData( 2, 4 ) = CurntBldgConstCost * CurntBldg.DesignFeeFrac;
		tableBody( 2, 4 ) = RealToStr( TableBodyData( 2, 4 ), 2 );

		TableBodyData( 2, 5 ) = CurntBldgConstCost * CurntBldg.ContractorFeeFrac;
		tableBody( 2, 5 ) = RealToStr( TableBodyData( 2, 5 ), 2 );
		TableBodyData( 2, 6 ) = CurntBldgConstCost * CurntBldg.ContingencyFrac;
		tableBody( 2, 6 ) = RealToStr( TableBodyData( 2, 6 ), 2 );
		TableBodyData( 2, 7 ) = CurntBldgConstCost * CurntBldg.BondCostFrac;
		tableBody( 2, 7 ) = RealToStr( TableBodyData( 2, 7 ), 2 );
		TableBodyData( 2, 8 ) = CurntBldgConstCost * CurntBldg.CommissioningFrac;
		tableBody( 2, 8 ) = RealToStr( TableBodyData( 2, 8 ), 2 );

		CurntBldg.GrandTotal = sum( TableBodyData( 2, {1,8} ) );
		TableBodyData( 2, 9 ) = CurntBldg.GrandTotal;
		tableBody( 2, 9 ) = RealToStr( TableBodyData( 2, 9 ), 2 );
		if ( buildingConditionedFloorArea > 0 ) {
			TableBodyData( 2, 10 ) = TableBodyData( 2, 9 ) / ( buildingConditionedFloorArea * m2_unitConv );
		}
		tableBody( 2, 10 ) = RealToStr( TableBodyData( 2, 10 ), 2 );

		TableBodyData( 3, {1,10} ) = TableBodyData( 2, {1,10} ) - TableBodyData( 1, {1,10} );
		tableBody( 3, 1 ) = RealToStr( TableBodyData( 3, 1 ), 2 );
		tableBody( 3, 2 ) = RealToStr( TableBodyData( 3, 2 ), 2 );
		tableBody( 3, 3 ) = RealToStr( TableBodyData( 3, 3 ), 2 );
		tableBody( 3, 4 ) = RealToStr( TableBodyData( 3, 4 ), 2 );
		tableBody( 3, 5 ) = RealToStr( TableBodyData( 3, 5 ), 2 );
		tableBody( 3, 6 ) = RealToStr( TableBodyData( 3, 6 ), 2 );
		tableBody( 3, 7 ) = RealToStr( TableBodyData( 3, 7 ), 2 );
		tableBody( 3, 8 ) = RealToStr( TableBodyData( 3, 8 ), 2 );
		tableBody( 3, 9 ) = RealToStr( TableBodyData( 3, 9 ), 2 );
		tableBody( 3, 10 ) = RealToStr( TableBodyData( 3, 10 ), 2 );

		WriteSubtitle( "Construction Cost Estimate Summary" );
		WriteTable( tableBody, rowHead, columnHead, columnWidth );
		if ( sqlite ) {
			sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Construction Cost Estimate Summary", "Entire Facility", "Construction Cost Estimate Summary" );
		}

		NumRows = NumLineItems + 1; //body will have the total and line items
		NumCols = 6; // Line no., Line name, Qty, Units, ValperQty, Subtotal
		rowHead.allocate( NumRows );
		columnHead.allocate( NumCols );
		columnWidth.dimension( NumCols, 14 ); //array assignment - same for all columns
		tableBody.allocate( NumCols, NumRows );
		tableBody = "--"; // array init
		rowHead = "--"; // array init
		rowHead( NumRows ) = "Line Item SubTotal"; //last line in table will be a total
		// setup up column headers
		columnHead( 1 ) = "Line No.";
		columnHead( 2 ) = "Item Name";
		columnHead( 3 ) = "Quantity.";
		columnHead( 4 ) = "Units";
		columnHead( 5 ) = "~~$~~ per Qty.";
		columnHead( 6 ) = "SubTotal ~~$~~";

		columnWidth = { 7, 30, 16, 10, 16, 16 }; //array assignment - for all columns

		for ( item = 1; item <= NumLineItems; ++item ) {
			tableBody( 1, item ) = IntToStr( CostLineItem( item ).LineNumber );
			tableBody( 2, item ) = CostLineItem( item ).LineName;
			if ( unitsStyle == unitsStyleInchPound ) {
				LookupSItoIP( CostLineItem( item ).Units, unitConvIndex, IPunitName );
				if ( unitConvIndex != 0 ) {
					IPqty = ConvertIP( unitConvIndex, CostLineItem( item ).Qty );
					tableBody( 3, item ) = RealToStr( IPqty, 2 );
					tableBody( 4, item ) = IPunitName;
					IPsingleValue = ConvertIP( unitConvIndex, 1.0 );
					if ( IPsingleValue != 0.0 ) {
						IPvaluePer = CostLineItem( item ).ValuePer / IPsingleValue;
						tableBody( 5, item ) = RealToStr( IPvaluePer, 2 );
					}
				} else {
					tableBody( 3, item ) = RealToStr( CostLineItem( item ).Qty, 2 );
					tableBody( 4, item ) = CostLineItem( item ).Units;
					tableBody( 5, item ) = RealToStr( CostLineItem( item ).ValuePer, 2 );
				}
			} else {
				tableBody( 3, item ) = RealToStr( CostLineItem( item ).Qty, 2 );
				tableBody( 4, item ) = CostLineItem( item ).Units;
				tableBody( 5, item ) = RealToStr( CostLineItem( item ).ValuePer, 2 );
			}
			tableBody( 6, item ) = RealToStr( CostLineItem( item ).LineSubTotal, 2 );
		}
		tableBody( 6, NumRows ) = RealToStr( CurntBldg.LineItemTot, 2 );
		WriteSubtitle( "Cost Line Item Details" ); //: '//TRIM(RealToStr(CostEstimateTotal, 2)))
		WriteTable( tableBody, rowHead, columnHead, columnWidth );
		if ( sqlite ) {
			sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Construction Cost Estimate Summary", "Entire Facility", "Cost Line Item Details" );
		}

	}

	void
	WriteVeriSumTable()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   June 2006
		//       MODIFIED       January 2010, Kyle Benne
		//                      Added SQLite output
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Summarize inputs and results for use with code and beyond-code
		//   compliance into a tabular report for output.

		// METHODOLOGY EMPLOYED:
		//   Create arrays for the call to WriteTable and then call it.
		//   This report actually consists of many sub-tables each with
		//   its own call to WriteTable.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataStringGlobals::VerString;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::Latitude;
		using DataEnvironment::Longitude;
		using DataEnvironment::Elevation;
		using DataEnvironment::TimeZoneNumber;
		using DataEnvironment::RunPeriodStartDayOfWeek;
		using DataEnvironment::WeatherFileLocationTitle;
		using DataHeatBalance::Zone;
		using DataHeatBalance::ZoneData;
		using DataHeatBalance::BuildingAzimuth;
		using DataHeatBalance::TotLights;
		using DataHeatBalance::Lights;
		using DataHeatBalance::People;
		using DataHeatBalance::TotPeople;
		using DataHeatBalance::ZoneElectric;
		using DataHeatBalance::TotElecEquip;
		using DataHeatBalance::ZoneGas;
		using DataHeatBalance::TotGasEquip;
		using DataHeatBalance::ZoneOtherEq;
		using DataHeatBalance::TotOthEquip;
		using DataHeatBalance::ZoneHWEq;
		using DataHeatBalance::TotHWEquip;
		using DataHeatBalance::BuildingRotationAppendixG;
		using DataSurfaces::Surface;
		using DataSurfaces::TotSurfaces;
		using DataSurfaces::SurfaceClass_Wall;
		using DataSurfaces::SurfaceClass_Floor;
		using DataSurfaces::SurfaceClass_Roof;
		using DataSurfaces::SurfaceClass_Window;
		using DataSurfaces::SurfaceClass_TDD_Dome;
		using DataSurfaces::FrameDivider;
		using DataSurfaces::ExternalEnvironment;
		using DataSurfaces::Ground;
		using DataSurfaces::OtherSideCondModeledExt;
		using DataSurfaces::GroundFCfactorMethod;
		using ScheduleManager::ScheduleAverageHoursPerWeek;
		using ScheduleManager::GetScheduleName;
		using ExteriorEnergyUse::ExteriorLights;
		using ExteriorEnergyUse::NumExteriorLights;
		using General::SafeDivide;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const wwrcTotal( 1 );
		int const wwrcNorth( 2 );
		int const wwrcEast( 3 );
		int const wwrcSouth( 4 );
		int const wwrcWest( 5 );
		int const wwrrWall( 1 );
		int const wwrrAbvGndWall( 2 );
		int const wwrrWindow( 3 );
		int const wwrrWWR( 4 );
		int const wwrrAbvGndWWR( 5 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// all arrays are in the format: (row, column)
		Array1D_string columnHead;
		Array1D_int columnWidth;
		Array1D_string rowHead;
		Array2D_string tableBody;

		int iSurf;
		int kOpaque;
		int zonePt;
		int iLight;
		int iZone;
		int iPeople;
		int iPlugProc;
		Real64 mult;
		Real64 curAzimuth;
		Real64 curArea;
		Real64 wallAreaN;
		Real64 wallAreaS;
		Real64 wallAreaE;
		Real64 wallAreaW;
		Real64 aboveGroundWallAreaN;
		Real64 aboveGroundWallAreaS;
		Real64 aboveGroundWallAreaE;
		Real64 aboveGroundWallAreaW;
		Real64 windowAreaN;
		Real64 windowAreaS;
		Real64 windowAreaE;
		Real64 windowAreaW;
		//wall and window areas attached to conditioned zones
		Real64 wallAreaNcond;
		Real64 wallAreaScond;
		Real64 wallAreaEcond;
		Real64 wallAreaWcond;
		Real64 aboveGroundWallAreaNcond;
		Real64 aboveGroundWallAreaScond;
		Real64 aboveGroundWallAreaEcond;
		Real64 aboveGroundWallAreaWcond;
		Real64 windowAreaNcond;
		Real64 windowAreaScond;
		Real64 windowAreaEcond;
		Real64 windowAreaWcond;
		bool isConditioned;
		bool isAboveGround;

		Real64 roofArea;
		Real64 skylightArea;
		Real64 totLightPower;
		Real64 totNumPeople;
		Real64 totPlugProcess;
		Real64 frameWidth;
		Real64 frameArea;

		bool zoneIsCond;
		bool usezoneFloorArea;

		static int grandTotal( 1 );
		static int condTotal( 2 );
		static int uncondTotal( 3 );
		static int notpartTotal( 4 );
		int iTotal;
		static std::string SIunit;
		static int unitConvIndex( 0 );
		static Real64 m_unitConv( 0.0 );
		static Real64 m2_unitConv( 0.0 );
		static Real64 m3_unitConv( 0.0 );
		static Real64 Wm2_unitConv( 0.0 );
		static std::string m_unitName;
		static std::string m2_unitName;
		static std::string m3_unitName;
		static std::string Wm2_unitName;

		//zone summary total
		static Array1D< Real64 > zstArea( 4 );
		static Array1D< Real64 > zstVolume( 4 );
		static Array1D< Real64 > zstWallArea( 4 );
		static Array1D< Real64 > zstWindowArea( 4 );
		static Array1D< Real64 > zstLight( 4 );
		static Array1D< Real64 > zstPeople( 4 );
		static Array1D< Real64 > zstPlug( 4 );

		zstArea = 0.0;
		zstVolume = 0.0;
		zstWallArea = 0.0;
		zstWindowArea = 0.0;
		zstLight = 0.0;
		zstPeople  = 0.0;
		zstPlug = 0.0;

		// misc
		Real64 pdiff;
		bool DetailedWWR;
		Real64 TotalWallArea;
		Real64 TotalWindowArea;
		Real64 TotalAboveGroundWallArea;

		// all arrays are in the format: (row, columnm)
		if ( displayTabularVeriSum ) {
			// show the headers of the report
			WriteReportHeaders( "Input Verification and Results Summary", "Entire Facility", isAverage );

			// do unit conversions if necessary
			if ( unitsStyle == unitsStyleInchPound ) {
				SIunit = "[m]";
				LookupSItoIP( SIunit, unitConvIndex, m_unitName );
				m_unitConv = ConvertIP( unitConvIndex, 1.0 );
				SIunit = "[m2]";
				LookupSItoIP( SIunit, unitConvIndex, m2_unitName );
				m2_unitConv = ConvertIP( unitConvIndex, 1.0 );
				SIunit = "[m3]";
				LookupSItoIP( SIunit, unitConvIndex, m3_unitName );
				m3_unitConv = ConvertIP( unitConvIndex, 1.0 );
				SIunit = "[W/m2]";
				LookupSItoIP( SIunit, unitConvIndex, Wm2_unitName );
				Wm2_unitConv = ConvertIP( unitConvIndex, 1.0 );
			} else {
				m_unitName = "[m]";
				m_unitConv = 1.0;
				m2_unitName = "[m2]";
				m2_unitConv = 1.0;
				m3_unitName = "[m3]";
				m3_unitConv = 1.0;
				Wm2_unitName = "[W/m2]";
				Wm2_unitConv = 1.0;
			}
			//---- General Sub-Table

			// since a variable number of design days is possible, first read them before sizing the arrays
			rowHead.allocate( 10 );
			columnHead.allocate( 1 );
			columnWidth.allocate( 1 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 1, 10 );

			columnHead( 1 ) = "Value";
			rowHead( 1 ) = "Program Version and Build";
			rowHead( 2 ) = "RunPeriod";
			rowHead( 3 ) = "Weather File";
			rowHead( 4 ) = "Latitude [deg]";
			rowHead( 5 ) = "Longitude [deg]";

			rowHead( 6 ) = "Elevation " + m_unitName;
			rowHead( 7 ) = "Time Zone";
			rowHead( 8 ) = "North Axis Angle [deg]";
			rowHead( 9 ) = "Rotation for Appendix G [deg]";
			rowHead( 10 ) = "Hours Simulated [hrs]";
			//  rowHead(9)  = 'Num Table Entries' !used for debugging

			tableBody = "";

			tableBody( 1, 1 ) = VerString; //program
			tableBody( 1, 2 ) = EnvironmentName; //runperiod name
			tableBody( 1, 3 ) = WeatherFileLocationTitle; //weather
			tableBody( 1, 4 ) = RealToStr( Latitude, 2 ); //latitude
			tableBody( 1, 5 ) = RealToStr( Longitude, 2 ); //longitude
			tableBody( 1, 6 ) = RealToStr( Elevation * m_unitConv, 2 ); //Elevation
			tableBody( 1, 7 ) = RealToStr( TimeZoneNumber, 2 ); //Time Zone
			tableBody( 1, 8 ) = RealToStr( BuildingAzimuth, 2 ); //north axis angle
			tableBody( 1, 9 ) = RealToStr( BuildingRotationAppendixG, 2 ); //Rotation for Appendix G
			tableBody( 1, 10 ) = RealToStr( gatherElapsedTimeBEPS, 2 ); //hours simulated
			//  tableBody(9,1) = TRIM(IntToStr(numTableEntry)) !number of table entries for predefined tables

			WriteSubtitle( "General" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "InputVerificationandResultsSummary", "Entire Facility", "General" );
			}

			//---- Window Wall Ratio Sub-Table
			WriteTextLine( "ENVELOPE", true );

			rowHead.allocate( 5 );
			columnHead.allocate( 5 );
			columnWidth.allocate( 5 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 5, 5 );

			columnHead( wwrcTotal ) = "Total";
			columnHead( wwrcNorth ) = "North (315 to 45 deg)";
			columnHead( wwrcEast ) = "East (45 to 135 deg)";
			columnHead( wwrcSouth ) = "South (135 to 225 deg)";
			columnHead( wwrcWest ) = "West (225 to 315 deg)";

			rowHead( wwrrWall ) = "Gross Wall Area " + m2_unitName;
			rowHead( wwrrAbvGndWall ) = "Above Ground Wall Area " + m2_unitName;
			rowHead( wwrrWindow ) = "Window Opening Area " + m2_unitName;
			rowHead( wwrrWWR ) = "Gross Window-Wall Ratio [%]";
			rowHead( wwrrAbvGndWWR ) = "Above Ground Window-Wall Ratio [%]";

			wallAreaN = 0.0;
			wallAreaS = 0.0;
			wallAreaE = 0.0;
			wallAreaW = 0.0;
			aboveGroundWallAreaN = 0.0;
			aboveGroundWallAreaS = 0.0;
			aboveGroundWallAreaE = 0.0;
			aboveGroundWallAreaW = 0.0;
			windowAreaN = 0.0;
			windowAreaS = 0.0;
			windowAreaE = 0.0;
			windowAreaW = 0.0;
			wallAreaNcond = 0.0;
			wallAreaScond = 0.0;
			wallAreaEcond = 0.0;
			wallAreaWcond = 0.0;
			aboveGroundWallAreaNcond = 0.0;
			aboveGroundWallAreaScond = 0.0;
			aboveGroundWallAreaEcond = 0.0;
			aboveGroundWallAreaWcond = 0.0;
			windowAreaNcond = 0.0;
			windowAreaScond = 0.0;
			windowAreaEcond = 0.0;
			windowAreaWcond = 0.0;
			roofArea = 0.0;
			skylightArea = 0.0;
			totLightPower = 0.0;
			totNumPeople = 0.0;
			totPlugProcess = 0.0;
			kOpaque = 0;

			DetailedWWR = ( GetNumSectionsFound( "DETAILEDWWR_DEBUG" ) > 0 );

			if ( DetailedWWR ) {
				gio::write( OutputFileDebug, fmtA ) << "======90.1 Classification [>=60 & <=120] tilt = wall==================";
				gio::write( OutputFileDebug, fmtA ) << "SurfName,Class,Area,Tilt";
			}

			for ( iSurf = 1; iSurf <= TotSurfaces; ++iSurf ) {
				//only exterior surfaces including underground
				if ( ! Surface( iSurf ).HeatTransSurf ) continue;
				isAboveGround = ( Surface( iSurf ).ExtBoundCond == ExternalEnvironment ) || ( Surface( iSurf ).ExtBoundCond == OtherSideCondModeledExt );
				if ( isAboveGround || ( Surface( iSurf ).ExtBoundCond == Ground ) || ( Surface( iSurf ).ExtBoundCond == GroundFCfactorMethod ) ) {
					curAzimuth = Surface( iSurf ).Azimuth;
					curArea = Surface( iSurf ).GrossArea;
					if ( Surface( iSurf ).FrameDivider != 0 ) {
						frameWidth = FrameDivider( Surface( iSurf ).FrameDivider ).FrameWidth;
						frameArea = ( Surface( iSurf ).Height + 2.0 * frameWidth ) * ( Surface( iSurf ).Width + 2.0 * frameWidth ) - ( Surface( iSurf ).Height * Surface( iSurf ).Width );
						curArea += frameArea;
					}
					zonePt = Surface( iSurf ).Zone;
					isConditioned = false;
					if ( zonePt > 0 ) {
						if ( Zone( zonePt ).SystemZoneNodeNumber > 0 ) {
							isConditioned = true;
						}
					}
					if ( ( Surface( iSurf ).Tilt >= 60.0 ) && ( Surface( iSurf ).Tilt <= 120.0 ) ) {
						//vertical walls and windows
						{ auto const SELECT_CASE_var( Surface( iSurf ).Class );
						if ( ( SELECT_CASE_var == SurfaceClass_Wall ) || ( SELECT_CASE_var == SurfaceClass_Floor ) || ( SELECT_CASE_var == SurfaceClass_Roof ) ) {
							mult = Zone( zonePt ).Multiplier * Zone( zonePt ).ListMultiplier;
							if ( ( curAzimuth >= 315.0 ) || ( curAzimuth < 45.0 ) ) {
								wallAreaN += curArea * mult;
								if ( isConditioned ) wallAreaNcond += curArea * mult;
								if ( isAboveGround ) {
									aboveGroundWallAreaN += curArea * mult;
									if ( isConditioned ) aboveGroundWallAreaNcond += curArea * mult;
								}
							} else if ( ( curAzimuth >= 45.0 ) && ( curAzimuth < 135.0 ) ) {
								wallAreaE += curArea * mult;
								if ( isConditioned ) wallAreaEcond += curArea * mult;
								if ( isAboveGround ) {
									aboveGroundWallAreaE += curArea * mult;
									if ( isConditioned ) aboveGroundWallAreaEcond += curArea * mult;
								}
							} else if ( ( curAzimuth >= 135.0 ) && ( curAzimuth < 225.0 ) ) {
								wallAreaS += curArea * mult;
								if ( isConditioned ) wallAreaScond += curArea * mult;
								if ( isAboveGround ) {
									aboveGroundWallAreaS += curArea * mult;
									if ( isConditioned ) aboveGroundWallAreaScond += curArea * mult;
								}
							} else if ( ( curAzimuth >= 225.0 ) && ( curAzimuth < 315.0 ) ) {
								wallAreaW += curArea * mult;
								if ( isConditioned ) wallAreaWcond += curArea * mult;
								if ( isAboveGround ) {
									aboveGroundWallAreaW += curArea * mult;
									if ( isConditioned ) aboveGroundWallAreaWcond += curArea * mult;
								}
							}
							if ( DetailedWWR ) {
								gio::write( OutputFileDebug, fmtA ) << Surface( iSurf ).Name + ",Wall," + RoundSigDigits( curArea * mult, 1 ) + ',' + RoundSigDigits( Surface( iSurf ).Tilt, 1 );
							}
						} else if ( ( SELECT_CASE_var == SurfaceClass_Window ) || ( SELECT_CASE_var == SurfaceClass_TDD_Dome ) ) {
							mult = Zone( zonePt ).Multiplier * Zone( zonePt ).ListMultiplier * Surface( iSurf ).Multiplier;
							if ( ( curAzimuth >= 315.0 ) || ( curAzimuth < 45.0 ) ) {
								windowAreaN += curArea * mult;
								if ( isConditioned ) windowAreaNcond += curArea * mult;
							} else if ( ( curAzimuth >= 45.0 ) && ( curAzimuth < 135.0 ) ) {
								windowAreaE += curArea * mult;
								if ( isConditioned ) windowAreaEcond += curArea * mult;
							} else if ( ( curAzimuth >= 135.0 ) && ( curAzimuth < 225.0 ) ) {
								windowAreaS += curArea * mult;
								if ( isConditioned ) windowAreaScond += curArea * mult;
							} else if ( ( curAzimuth >= 225.0 ) && ( curAzimuth < 315.0 ) ) {
								windowAreaW += curArea * mult;
								if ( isConditioned ) windowAreaWcond += curArea * mult;
							}
							if ( DetailedWWR ) {
								gio::write( OutputFileDebug, fmtA ) << Surface( iSurf ).Name + ",Window," + RoundSigDigits( curArea * mult, 1 ) + ',' + RoundSigDigits( Surface( iSurf ).Tilt, 1 );
							}
						}}
					} else if ( Surface( iSurf ).Tilt < 60.0 ) { //roof and skylights
						{ auto const SELECT_CASE_var( Surface( iSurf ).Class );
						if ( ( SELECT_CASE_var == SurfaceClass_Wall ) || ( SELECT_CASE_var == SurfaceClass_Floor ) || ( SELECT_CASE_var == SurfaceClass_Roof ) ) {
							mult = Zone( zonePt ).Multiplier * Zone( zonePt ).ListMultiplier;
							roofArea += curArea * mult;
							if ( DetailedWWR ) {
								gio::write( OutputFileDebug, fmtA ) << Surface( iSurf ).Name + ",Roof," + RoundSigDigits( curArea * mult, 1 ) + ',' + RoundSigDigits( Surface( iSurf ).Tilt, 1 );
							}
						} else if ( ( SELECT_CASE_var == SurfaceClass_Window ) || ( SELECT_CASE_var == SurfaceClass_TDD_Dome ) ) {
							mult = Zone( zonePt ).Multiplier * Zone( zonePt ).ListMultiplier * Surface( iSurf ).Multiplier;
							skylightArea += curArea * mult;
							if ( DetailedWWR ) {
								gio::write( OutputFileDebug, fmtA ) << Surface( iSurf ).Name + ",Skylight," + RoundSigDigits( curArea * mult, 1 ) + ',' + RoundSigDigits( Surface( iSurf ).Tilt, 1 );
							}
						}}
					} else { //floors
						//ignored
					}
				}
			}

			TotalWallArea = wallAreaN + wallAreaS + wallAreaE + wallAreaW;
			TotalAboveGroundWallArea = aboveGroundWallAreaN + aboveGroundWallAreaS + aboveGroundWallAreaE + aboveGroundWallAreaW;
			TotalWindowArea = windowAreaN + windowAreaS + windowAreaE + windowAreaW;
			if ( DetailedWWR ) {
				gio::write( OutputFileDebug, fmtA ) << "========================";
				gio::write( OutputFileDebug, fmtA ) << "TotalWallArea,WallAreaN,WallAreaS,WallAreaE,WallAreaW";
				gio::write( OutputFileDebug, fmtA ) << "TotalWindowArea,WindowAreaN,WindowAreaS,WindowAreaE,WindowAreaW";
				gio::write( OutputFileDebug, fmtA ) << RoundSigDigits( TotalWallArea, 2 ) + ',' + RoundSigDigits( wallAreaN, 2 ) + ',' + RoundSigDigits( wallAreaS, 2 ) + ',' + RoundSigDigits( wallAreaE, 2 ) + ',' + RoundSigDigits( wallAreaW, 2 );
				gio::write( OutputFileDebug, fmtA ) << RoundSigDigits( TotalWindowArea, 2 ) + ',' + RoundSigDigits( windowAreaN, 2 ) + ',' + RoundSigDigits( windowAreaS, 2 ) + ',' + RoundSigDigits( windowAreaE, 2 ) + ',' + RoundSigDigits( windowAreaW, 2 );
			}

			tableBody = "";

			tableBody( wwrcNorth, wwrrWall ) = RealToStr( wallAreaN * m2_unitConv, 2 );
			tableBody( wwrcSouth, wwrrWall ) = RealToStr( wallAreaS * m2_unitConv, 2 );
			tableBody( wwrcEast, wwrrWall ) = RealToStr( wallAreaE * m2_unitConv, 2 );
			tableBody( wwrcWest, wwrrWall ) = RealToStr( wallAreaW * m2_unitConv, 2 );
			tableBody( wwrcTotal, wwrrWall ) = RealToStr( TotalWallArea * m2_unitConv, 2 );

			tableBody( wwrcNorth, wwrrAbvGndWall ) = RealToStr( aboveGroundWallAreaN * m2_unitConv, 2 );
			tableBody( wwrcSouth, wwrrAbvGndWall ) = RealToStr( aboveGroundWallAreaS * m2_unitConv, 2 );
			tableBody( wwrcEast, wwrrAbvGndWall ) = RealToStr( aboveGroundWallAreaE * m2_unitConv, 2 );
			tableBody( wwrcWest, wwrrAbvGndWall ) = RealToStr( aboveGroundWallAreaW * m2_unitConv, 2 );
			tableBody( wwrcTotal, wwrrAbvGndWall ) = RealToStr( TotalAboveGroundWallArea * m2_unitConv, 2 );

			tableBody( wwrcNorth, wwrrWindow ) = RealToStr( windowAreaN * m2_unitConv, 2 );
			tableBody( wwrcSouth, wwrrWindow ) = RealToStr( windowAreaS * m2_unitConv, 2 );
			tableBody( wwrcEast, wwrrWindow ) = RealToStr( windowAreaE * m2_unitConv, 2 );
			tableBody( wwrcWest, wwrrWindow ) = RealToStr( windowAreaW * m2_unitConv, 2 );
			tableBody( wwrcTotal, wwrrWindow ) = RealToStr( TotalWindowArea * m2_unitConv, 2 );

			tableBody( wwrcNorth, wwrrWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaN, wallAreaN ), 2 );
			tableBody( wwrcSouth, wwrrWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaS, wallAreaS ), 2 );
			tableBody( wwrcEast, wwrrWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaE, wallAreaE ), 2 );
			tableBody( wwrcWest, wwrrWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaW, wallAreaW ), 2 );
			tableBody( wwrcTotal, wwrrWWR ) = RealToStr( 100.0 * SafeDivide( TotalWindowArea, TotalWallArea ), 2 );

			tableBody( wwrcNorth, wwrrAbvGndWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaN, aboveGroundWallAreaN ), 2 );
			tableBody( wwrcSouth, wwrrAbvGndWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaS, aboveGroundWallAreaS ), 2 );
			tableBody( wwrcEast, wwrrAbvGndWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaE, aboveGroundWallAreaE ), 2 );
			tableBody( wwrcWest, wwrrAbvGndWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaW, aboveGroundWallAreaW ), 2 );
			tableBody( wwrcTotal, wwrrAbvGndWWR ) = RealToStr( 100.0 * SafeDivide( TotalWindowArea, TotalAboveGroundWallArea ), 2 );

			WriteSubtitle( "Window-Wall Ratio" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "InputVerificationandResultsSummary", "Entire Facility", "Window-Wall Ratio" );
			}

			//---- Conditioned Window Wall Ratio Sub-Table
			rowHead.allocate( 5 );
			columnHead.allocate( 5 );
			columnWidth.allocate( 5 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 5, 5 );

			columnHead( wwrcTotal ) = "Total";
			columnHead( wwrcNorth ) = "North (315 to 45 deg)";
			columnHead( wwrcEast ) = "East (45 to 135 deg)";
			columnHead( wwrcSouth ) = "South (135 to 225 deg)";
			columnHead( wwrcWest ) = "West (225 to 315 deg)";

			rowHead( wwrrWall ) = "Gross Wall Area " + m2_unitName;
			rowHead( wwrrAbvGndWall ) = "Above Ground Wall Area " + m2_unitName;
			rowHead( wwrrWindow ) = "Window Opening Area " + m2_unitName;
			rowHead( wwrrWWR ) = "Gross Window-Wall Ratio [%]";
			rowHead( wwrrAbvGndWWR ) = "Above Ground Window-Wall Ratio [%]";

			//calculations appear in last block with normal window-wall ratio table

			TotalWallArea = wallAreaNcond + wallAreaScond + wallAreaEcond + wallAreaWcond;
			TotalAboveGroundWallArea = aboveGroundWallAreaNcond + aboveGroundWallAreaScond + aboveGroundWallAreaEcond + aboveGroundWallAreaWcond;
			TotalWindowArea = windowAreaNcond + windowAreaScond + windowAreaEcond + windowAreaWcond;

			tableBody = "";

			tableBody( wwrcNorth, wwrrWall ) = RealToStr( wallAreaNcond * m2_unitConv, 2 );
			tableBody( wwrcSouth, wwrrWall ) = RealToStr( wallAreaScond * m2_unitConv, 2 );
			tableBody( wwrcEast, wwrrWall ) = RealToStr( wallAreaEcond * m2_unitConv, 2 );
			tableBody( wwrcWest, wwrrWall ) = RealToStr( wallAreaWcond * m2_unitConv, 2 );
			tableBody( wwrcTotal, wwrrWall ) = RealToStr( TotalWallArea * m2_unitConv, 2 );

			tableBody( wwrcNorth, wwrrAbvGndWall ) = RealToStr( aboveGroundWallAreaNcond * m2_unitConv, 2 );
			tableBody( wwrcSouth, wwrrAbvGndWall ) = RealToStr( aboveGroundWallAreaScond * m2_unitConv, 2 );
			tableBody( wwrcEast, wwrrAbvGndWall ) = RealToStr( aboveGroundWallAreaEcond * m2_unitConv, 2 );
			tableBody( wwrcWest, wwrrAbvGndWall ) = RealToStr( aboveGroundWallAreaWcond * m2_unitConv, 2 );
			tableBody( wwrcTotal, wwrrAbvGndWall ) = RealToStr( TotalAboveGroundWallArea * m2_unitConv, 2 );

			tableBody( wwrcNorth, wwrrWindow ) = RealToStr( windowAreaNcond * m2_unitConv, 2 );
			tableBody( wwrcSouth, wwrrWindow ) = RealToStr( windowAreaScond * m2_unitConv, 2 );
			tableBody( wwrcEast, wwrrWindow ) = RealToStr( windowAreaEcond * m2_unitConv, 2 );
			tableBody( wwrcWest, wwrrWindow ) = RealToStr( windowAreaWcond * m2_unitConv, 2 );
			tableBody( wwrcTotal, wwrrWindow ) = RealToStr( TotalWindowArea * m2_unitConv, 2 );

			tableBody( wwrcNorth, wwrrWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaNcond, wallAreaNcond ), 2 );
			tableBody( wwrcSouth, wwrrWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaScond, wallAreaScond ), 2 );
			tableBody( wwrcEast, wwrrWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaEcond, wallAreaEcond ), 2 );
			tableBody( wwrcWest, wwrrWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaWcond, wallAreaWcond ), 2 );
			tableBody( wwrcTotal, wwrrWWR ) = RealToStr( 100.0 * SafeDivide( TotalWindowArea, TotalWallArea ), 2 );

			tableBody( wwrcNorth, wwrrAbvGndWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaNcond, aboveGroundWallAreaNcond ), 2 );
			tableBody( wwrcSouth, wwrrAbvGndWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaScond, aboveGroundWallAreaScond ), 2 );
			tableBody( wwrcEast, wwrrAbvGndWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaEcond, aboveGroundWallAreaEcond ), 2 );
			tableBody( wwrcWest, wwrrAbvGndWWR ) = RealToStr( 100.0 * SafeDivide( windowAreaWcond, aboveGroundWallAreaWcond ), 2 );
			tableBody( wwrcTotal, wwrrAbvGndWWR ) = RealToStr( 100.0 * SafeDivide( TotalWindowArea, TotalAboveGroundWallArea ), 2 );

			WriteSubtitle( "Conditioned Window-Wall Ratio" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "InputVerificationandResultsSummary", "Entire Facility", "Conditioned Window-Wall Ratio" );
			}

			//---- Skylight Roof Ratio Sub-Table
			rowHead.allocate( 3 );
			columnHead.allocate( 1 );
			columnWidth.allocate( 1 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 1, 3 );

			columnHead( 1 ) = "Total";

			rowHead( 1 ) = "Gross Roof Area " + m2_unitName;
			rowHead( 2 ) = "Skylight Area " + m2_unitName;
			rowHead( 3 ) = "Skylight-Roof Ratio [%]";

			if ( DetailedWWR ) {
				gio::write( OutputFileDebug, fmtA ) << "========================";
				gio::write( OutputFileDebug, fmtA ) << "TotalRoofArea,SkylightArea";
				gio::write( OutputFileDebug, fmtA ) << RoundSigDigits( roofArea, 2 ) + ',' + RoundSigDigits( skylightArea, 2 );
			}

			tableBody( 1, 1 ) = RealToStr( roofArea * m2_unitConv, 2 );
			tableBody( 1, 2 ) = RealToStr( skylightArea * m2_unitConv, 2 );
			tableBody( 1, 3 ) = RealToStr( 100.0 * SafeDivide( skylightArea, roofArea ), 2 );

			WriteSubtitle( "Skylight-Roof Ratio" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "InputVerificationandResultsSummary", "Entire Facility", "Skylight-Roof Ratio" );
			}

			Real64 const totExtGrossWallArea_Multiplied( sum( Zone, &ZoneData::ExtGrossWallArea_Multiplied ) );
			Real64 const totExtGrossGroundWallArea_Multiplied( sum( Zone, &ZoneData::ExtGrossGroundWallArea_Multiplied ) );
			if ( totExtGrossWallArea_Multiplied > 0.0 || totExtGrossGroundWallArea_Multiplied > 0.0 ) {
				pdiff = std::abs( ( wallAreaN + wallAreaS + wallAreaE + wallAreaW ) - ( totExtGrossWallArea_Multiplied + totExtGrossGroundWallArea_Multiplied ) ) / ( totExtGrossWallArea_Multiplied + totExtGrossGroundWallArea_Multiplied );
				if ( pdiff > 0.019 ) {
					ShowWarningError( "WriteVeriSumTable: InputVerificationsAndResultsSummary: Wall area based on [>=60,<=120] degrees (tilt) as walls" );
					ShowContinueError( "differs ~" + RoundSigDigits( pdiff * 100.0, 1 ) + "% from user entered Wall class surfaces. Degree calculation based on ASHRAE 90.1 wall definitions." );
					//      CALL ShowContinueError('Calculated based on degrees=['//  &
					//         TRIM(ADJUSTL(RealToStr((wallAreaN + wallAreaS + wallAreaE + wallAreaW),3)))//  &
					//         '] m2, Calculated from user entered Wall class surfaces=['//  &
					//         TRIM(ADJUSTL(RealToStr(SUM(Zone(1:NumOfZones)%ExtGrossWallArea_Multiplied),3)))//' m2.')
					ShowContinueError( "Check classes of surfaces and tilts for discrepancies." );
					ShowContinueError( "Total wall area by ASHRAE 90.1 definition=" + stripped( RealToStr( ( wallAreaN + wallAreaS + wallAreaE + wallAreaW ), 3 ) ) + " m2." );
					ShowContinueError( "Total exterior wall area from user entered classes=" + stripped( RealToStr( totExtGrossWallArea_Multiplied, 3 ) ) + " m2." );
					ShowContinueError( "Total ground contact wall area from user entered classes=" + stripped( RealToStr( totExtGrossGroundWallArea_Multiplied, 3 ) ) + " m2." );
				}
			}
			//---- Space Summary Sub-Table
			WriteTextLine( "PERFORMANCE", true );

			rowHead.allocate( NumOfZones + 4 );
			columnHead.allocate( 10 );
			columnWidth.allocate( 10 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 10, NumOfZones + 4 );

			columnHead( 1 ) = "Area " + m2_unitName;
			columnHead( 2 ) = "Conditioned (Y/N)";
			columnHead( 3 ) = "Part of Total Floor Area (Y/N)";
			columnHead( 4 ) = "Volume " + m3_unitName;
			columnHead( 5 ) = "Multipliers";
			columnHead( 6 ) = "Gross Wall Area " + m2_unitName;
			columnHead( 7 ) = "Window Glass Area " + m2_unitName;
			columnHead( 8 ) = "Lighting " + Wm2_unitName;
			columnHead( 9 ) = "People " + m2_unitName.substr( 0, len( m2_unitName ) - 1 ) + " per person" + m2_unitName[ len( m2_unitName ) - 1 ];
			columnHead( 10 ) = "Plug and Process " + Wm2_unitName;

			rowHead = "";

			rowHead( NumOfZones + grandTotal ) = "Total";
			rowHead( NumOfZones + condTotal ) = "Conditioned Total";
			rowHead( NumOfZones + uncondTotal ) = "Unconditioned Total";
			rowHead( NumOfZones + notpartTotal ) = "Not Part of Total";

			tableBody = "";

			for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
				mult = Zone( iZone ).Multiplier * Zone( iZone ).ListMultiplier;
				rowHead( iZone ) = Zone( iZone ).Name;
				if ( Zone( iZone ).SystemZoneNodeNumber > 0 ) {
					tableBody( 2, iZone ) = "Yes";
					zoneIsCond = true;
				} else {
					tableBody( 2, iZone ) = "No";
					zoneIsCond = false;
				}
				if ( Zone( iZone ).isPartOfTotalArea ) {
					tableBody( 3, iZone ) = "Yes";
					usezoneFloorArea = true;
				} else {
					tableBody( 3, iZone ) = "No";
					usezoneFloorArea = false;
				}
				tableBody( 1, iZone ) = RealToStr( Zone( iZone ).FloorArea * m2_unitConv, 2 );
				tableBody( 4, iZone ) = RealToStr( Zone( iZone ).Volume * m3_unitConv, 2 );
				//no unit conversion necessary since done automatically
				PreDefTableEntry( pdchLeedSutSpArea, Zone( iZone ).Name, Zone( iZone ).FloorArea, 2 );
				if ( zoneIsCond ) {
					PreDefTableEntry( pdchLeedSutOcArea, Zone( iZone ).Name, Zone( iZone ).FloorArea, 2 );
					PreDefTableEntry( pdchLeedSutUnArea, Zone( iZone ).Name, "0.00" );
				} else {
					PreDefTableEntry( pdchLeedSutOcArea, Zone( iZone ).Name, "0.00" );
					PreDefTableEntry( pdchLeedSutUnArea, Zone( iZone ).Name, Zone( iZone ).FloorArea, 2 );
				}
				tableBody( 5, iZone ) = RealToStr( mult, 2 );
				tableBody( 6, iZone ) = RealToStr( Zone( iZone ).ExtGrossWallArea * m2_unitConv, 2 );
				tableBody( 7, iZone ) = RealToStr( Zone( iZone ).ExtWindowArea * m2_unitConv, 2 );
				// lighting density
				totLightPower = 0.0;
				for ( iLight = 1; iLight <= TotLights; ++iLight ) {
					if ( iZone == Lights( iLight ).ZonePtr ) {
						totLightPower += Lights( iLight ).DesignLevel;
					}
				}
				if ( Zone( iZone ).FloorArea > 0 && usezoneFloorArea ) {
					tableBody( 8, iZone ) = RealToStr( Wm2_unitConv * totLightPower / Zone( iZone ).FloorArea, 4 );
				}
				// people density
				totNumPeople = 0.0;
				for ( iPeople = 1; iPeople <= TotPeople; ++iPeople ) {
					if ( iZone == People( iPeople ).ZonePtr ) {
						totNumPeople += People( iPeople ).NumberOfPeople;
					}
				}
				if ( totNumPeople > 0 ) {
					tableBody( 9, iZone ) = RealToStr( Zone( iZone ).FloorArea * m2_unitConv / totNumPeople, 2 );
				}
				// plug and process density
				totPlugProcess = 0.0;
				for ( iPlugProc = 1; iPlugProc <= TotElecEquip; ++iPlugProc ) {
					if ( iZone == ZoneElectric( iPlugProc ).ZonePtr ) {
						totPlugProcess += ZoneElectric( iPlugProc ).DesignLevel;
					}
				}
				for ( iPlugProc = 1; iPlugProc <= TotGasEquip; ++iPlugProc ) {
					if ( iZone == ZoneGas( iPlugProc ).ZonePtr ) {
						totPlugProcess += ZoneGas( iPlugProc ).DesignLevel;
					}
				}
				for ( iPlugProc = 1; iPlugProc <= TotOthEquip; ++iPlugProc ) {
					if ( iZone == ZoneOtherEq( iPlugProc ).ZonePtr ) {
						totPlugProcess += ZoneOtherEq( iPlugProc ).DesignLevel;
					}
				}
				for ( iPlugProc = 1; iPlugProc <= TotHWEquip; ++iPlugProc ) {
					if ( iZone == ZoneHWEq( iPlugProc ).ZonePtr ) {
						totPlugProcess += ZoneHWEq( iPlugProc ).DesignLevel;
					}
				}
				if ( Zone( iZone ).FloorArea > 0 && usezoneFloorArea ) {
					tableBody( 10, iZone ) = RealToStr( totPlugProcess * Wm2_unitConv / Zone( iZone ).FloorArea, 4 );
				}
				//total rows for conditioned, unconditioned, and total
				if ( usezoneFloorArea ) {
					zstArea( grandTotal ) += mult * Zone( iZone ).FloorArea;
					zstVolume( grandTotal ) += mult * Zone( iZone ).Volume;
					zstWallArea( grandTotal ) += mult * Zone( iZone ).ExtGrossWallArea;
					zstWindowArea( grandTotal ) += mult * Zone( iZone ).ExtWindowArea;
					zstLight( grandTotal ) += mult * totLightPower;
					zstPeople( grandTotal ) += mult * totNumPeople;
					zstPlug( grandTotal ) += mult * totPlugProcess;
				} else {
					zstArea( notpartTotal ) += mult * Zone( iZone ).FloorArea;
					zstVolume( notpartTotal ) += mult * Zone( iZone ).Volume;
					zstWallArea( notpartTotal ) += mult * Zone( iZone ).ExtGrossWallArea;
					zstWindowArea( notpartTotal ) += mult * Zone( iZone ).ExtWindowArea;
					zstLight( notpartTotal ) += mult * totLightPower;
					zstPeople( notpartTotal ) += mult * totNumPeople;
					zstPlug( notpartTotal ) += mult * totPlugProcess;
				}
				if ( zoneIsCond && usezoneFloorArea ) {
					zstArea( condTotal ) += mult * Zone( iZone ).FloorArea;
					zstVolume( condTotal ) += mult * Zone( iZone ).Volume;
					zstWallArea( condTotal ) += mult * Zone( iZone ).ExtGrossWallArea;
					zstWindowArea( condTotal ) += mult * Zone( iZone ).ExtWindowArea;
					zstLight( condTotal ) += mult * totLightPower;
					zstPeople( condTotal ) += mult * totNumPeople;
					zstPlug( condTotal ) += mult * totPlugProcess;
				} else if ( ! zoneIsCond ) {
					zstArea( uncondTotal ) += mult * Zone( iZone ).FloorArea;
					zstVolume( uncondTotal ) += mult * Zone( iZone ).Volume;
					zstWallArea( uncondTotal ) += mult * Zone( iZone ).ExtGrossWallArea;
					zstWindowArea( uncondTotal ) += mult * Zone( iZone ).ExtWindowArea;
					zstLight( uncondTotal ) += mult * totLightPower;
					zstPeople( uncondTotal ) += mult * totNumPeople;
					zstPlug( uncondTotal ) += mult * totPlugProcess;
				} else {
					zstArea( notpartTotal ) += mult * Zone( iZone ).FloorArea;
					zstVolume( notpartTotal ) += mult * Zone( iZone ).Volume;
					zstWallArea( notpartTotal ) += mult * Zone( iZone ).ExtGrossWallArea;
					zstWindowArea( notpartTotal ) += mult * Zone( iZone ).ExtWindowArea;
					zstLight( notpartTotal ) += mult * totLightPower;
					zstPeople( notpartTotal ) += mult * totNumPeople;
					zstPlug( notpartTotal ) += mult * totPlugProcess;
				}
			}
			for ( iTotal = 1; iTotal <= 4; ++iTotal ) {
				tableBody( 1, NumOfZones + iTotal ) = RealToStr( zstArea( iTotal ) * m2_unitConv, 2 );
				tableBody( 4, NumOfZones + iTotal ) = RealToStr( zstVolume( iTotal ) * m3_unitConv, 2 );
				tableBody( 6, NumOfZones + iTotal ) = RealToStr( zstWallArea( iTotal ) * m2_unitConv, 2 );
				tableBody( 7, NumOfZones + iTotal ) = RealToStr( zstWindowArea( iTotal ) * m2_unitConv, 2 );
				if ( zstArea( iTotal ) != 0 ) {
					tableBody( 8, NumOfZones + iTotal ) = RealToStr( zstLight( iTotal ) * Wm2_unitConv / zstArea( iTotal ), 4 );
					tableBody( 10, NumOfZones + iTotal ) = RealToStr( zstPlug( iTotal ) * Wm2_unitConv / zstArea( iTotal ), 4 );
				}
				if ( zstPeople( iTotal ) != 0 ) {
					tableBody( 9, NumOfZones + iTotal ) = RealToStr( zstArea( iTotal ) * m2_unitConv / zstPeople( iTotal ), 2 );
				}
			}
			PreDefTableEntry( pdchLeedSutSpArea, "Totals", zstArea( grandTotal ), 2 );
			PreDefTableEntry( pdchLeedSutOcArea, "Totals", zstArea( condTotal ), 2 );
			PreDefTableEntry( pdchLeedSutUnArea, "Totals", zstArea( uncondTotal ), 2 );

			WriteSubtitle( "Zone Summary" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "InputVerificationandResultsSummary", "Entire Facility", "Zone Summary" );
			}
		}
	}

	void
	WriteAdaptiveComfortTable()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Tyler Hoyt
		//       DATE WRITTEN   August 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Writes summary table for adaptive comfort models. Tabulates
		// occupied hours not meeting comfort bounds for ASHRAE-55 and
		// CEN-15251 adaptive models.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using DataHeatBalance::People;
		using DataHeatBalance::TotPeople;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Array1D_string columnHead( 5 );
		Array1D_int columnWidth;
		Array1D_string rowHead;
		Array2D_string tableBody;
		static int numPeopleAdaptive( 0 );
		int i;
		Array1D_int peopleInd; // Index the relevant people

		// Should deallocate after writing table. - LKL

		if ( displayAdaptiveComfort && TotPeople > 0 ) {
			peopleInd.allocate( TotPeople );

			for ( i = 1; i <= TotPeople; ++i ) {
				if ( People( i ).AdaptiveASH55 || People( i ).AdaptiveCEN15251 ) {
					++numPeopleAdaptive;
					peopleInd( numPeopleAdaptive ) = i;
				}
			}

			rowHead.allocate( numPeopleAdaptive );
			tableBody.allocate( 5, numPeopleAdaptive );

			WriteReportHeaders( "Adaptive Comfort Summary", "Entire Facility", 0 );
			WriteSubtitle( "Time Not Meeting the Adaptive Comfort Models during Occupied Hours" );

			columnWidth.allocate( 5 );
			columnWidth = 10;
			columnHead( 1 ) = "ASHRAE55 90% Acceptability Limits [Hours]";
			columnHead( 2 ) = "ASHRAE55 80% Acceptability Limits  [Hours]";
			columnHead( 3 ) = "CEN15251 Category I Acceptability Limits [Hours]";
			columnHead( 4 ) = "CEN15251 Category II Acceptability Limits [Hours]";
			columnHead( 5 ) = "CEN15251 Category III Acceptability Limits [Hours]";

			tableBody = "";
			for ( i = 1; i <= numPeopleAdaptive; ++i ) {
				rowHead( i ) = People( i ).Name;
				if ( People( i ).AdaptiveASH55 ) {
					tableBody( 1, i ) = RealToStr( People( peopleInd( i ) ).TimeNotMetASH5590, 2 );
					tableBody( 2, i ) = RealToStr( People( peopleInd( i ) ).TimeNotMetASH5580, 2 );
				}
				if ( People( i ).AdaptiveCEN15251 ) {
					tableBody( 3, i ) = RealToStr( People( peopleInd( i ) ).TimeNotMetCEN15251CatI, 2 );
					tableBody( 4, i ) = RealToStr( People( peopleInd( i ) ).TimeNotMetCEN15251CatII, 2 );
					tableBody( 5, i ) = RealToStr( People( peopleInd( i ) ).TimeNotMetCEN15251CatIII, 2 );
				}
			}

			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "AdaptiveComfortReport", "Entire Facility", "People Summary" );
			}
		}

	}

	void
	WritePredefinedTables()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2006
		//       MODIFIED       January 2010, Kyle Benne; Added SQLite output
		//                      March 2010, Linda Lawrie; Modify SizingPeriod:DesignDay to convert column/humidity types
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Write out tables that have been predefined with data gathered
		//   throughout the program code.

		// METHODOLOGY EMPLOYED:
		//   Create arrays for the call to WriteTable and then call it.
		//   This is a generic routine to write a report with multiple
		//   subtables. The structure of the report are created in
		//   OutputReportPredefined which also includes a routine that
		//   builds up a tableEntry array which holds the data for the
		//   predefined reports.

		// REFERENCES:
		// na

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// all arrays are in the format: (row, column)
		Array1D_string columnHead;
		Array1D_int columnWidth;
		Array1D_string rowHead;
		Array2D_string tableBody;
		Array1D_int rowToUnqObjName;
		Array1D_int colHeadToColTag;
		int curNumColumns;
		int curNumRows;
		int curColumn;
		Array1D_string uniqueObjectName;
		Array1D_bool useUniqueObjectName;
		int numUnqObjName;
		std::string curObjectName;
		int countRow;
		int countColumn;
		int found;
		int curColTagIndex;
		int curRowUnqObjIndex;
		int colCurrent( 0 );
		int rowCurrent( 0 );
		int iReportName;
		int kColumnTag;
		int lTableEntry;
		int mUnqObjNames;
		int nColHead;
		int oRowHead;
		std::string colTagWithSI;
		std::string curColTag;
		Array1D_int colUnitConv;
		int indexUnitConv;
		int columnUnitConv;
		std::string repTableTag;
		Real64 IPvalue;

		// loop through the entries and associate them with the subtable and create
		// list of unique object names
		// Much of this code is to allow for integer compares instead of string
		// compares that are nested three levels in a loop.
		uniqueObjectName.allocate( numTableEntry );
		useUniqueObjectName.allocate( numTableEntry );
		numUnqObjName = 0;
		for ( lTableEntry = 1; lTableEntry <= numTableEntry; ++lTableEntry ) {
			//associate the subtable with each column
			curColumn = tableEntry( lTableEntry ).indexColumn;
			if ( ( curColumn >= 1 ) && ( curColumn <= numColumnTag ) ) {
				tableEntry( lTableEntry ).subTableIndex = columnTag( curColumn ).indexSubTable;
			}
			//make a list of unique object names
			curObjectName = tableEntry( lTableEntry ).objectName;
			found = 0;
			for ( mUnqObjNames = 1; mUnqObjNames <= numUnqObjName; ++mUnqObjNames ) {
				if ( SameString( curObjectName, uniqueObjectName( mUnqObjNames ) ) ) {
					found = mUnqObjNames;
				}
			}
			// if found then point to the unique object
			if ( found > 0 ) {
				tableEntry( lTableEntry ).uniqueObjName = found;
				// if not found add to the unique object list
			} else {
				++numUnqObjName;
				uniqueObjectName( numUnqObjName ) = curObjectName;
				tableEntry( lTableEntry ).uniqueObjName = numUnqObjName;
			}
		}
		// loop through all reports and include those that have been flagged as 'show'
		for ( iReportName = 1; iReportName <= numReportName; ++iReportName ) {
			if ( reportName( iReportName ).show ) {
				WriteReportHeaders( reportName( iReportName ).namewithspaces, "Entire Facility", isAverage );
				// loop through the subtables and include those that are associated with this report
				for ( int jSubTable = 1, jSubTable_end = numSubTable; jSubTable <= jSubTable_end; ++jSubTable ) {
					if ( subTable( jSubTable ).indexReportName == iReportName ) {
						//determine how many columns
						curNumColumns = 0;
						for ( kColumnTag = 1; kColumnTag <= numColumnTag; ++kColumnTag ) {
							if ( columnTag( kColumnTag ).indexSubTable == jSubTable ) {
								++curNumColumns;
							}
						}
						//determine how many rows by going through table entries and setting
						//flag in useUniqueObjectName to true, then count number of true's.
						useUniqueObjectName = false; //array assignment
						for ( lTableEntry = 1; lTableEntry <= numTableEntry; ++lTableEntry ) {
							if ( tableEntry( lTableEntry ).subTableIndex == jSubTable ) {
								useUniqueObjectName( tableEntry( lTableEntry ).uniqueObjName ) = true;
							}
						}
						curNumRows = 0;
						for ( mUnqObjNames = 1; mUnqObjNames <= numUnqObjName; ++mUnqObjNames ) {
							if ( useUniqueObjectName( mUnqObjNames ) ) {
								++curNumRows;
							}
						}
						if ( curNumRows == 0 ) curNumRows = 1;
						// now create the arrays that are filled with values
						rowHead.allocate( curNumRows );
						columnHead.allocate( curNumColumns );
						columnWidth.dimension( curNumColumns, 14 ); //array assignment - same for all columns
						tableBody.allocate( curNumColumns, curNumRows );
						rowHead = "";
						columnHead = "";
						tableBody = "";
						// this array stores the unique object name index for each row
						rowToUnqObjName.allocate( curNumRows );
						// this array stores the columnHead index for each column
						colHeadToColTag.allocate( curNumColumns );
						colUnitConv.allocate( curNumColumns );
						// set row headings
						countRow = 0;
						rowHead( 1 ) = "None";
						for ( mUnqObjNames = 1; mUnqObjNames <= numUnqObjName; ++mUnqObjNames ) {
							if ( useUniqueObjectName( mUnqObjNames ) ) {
								++countRow;
								rowHead( countRow ) = uniqueObjectName( mUnqObjNames );
								rowToUnqObjName( countRow ) = mUnqObjNames;
							}
						}
						// set column headings
						countColumn = 0;
						for ( kColumnTag = 1; kColumnTag <= numColumnTag; ++kColumnTag ) {
							if ( columnTag( kColumnTag ).indexSubTable == jSubTable ) {
								++countColumn;
								//do the unit conversions
								colTagWithSI = columnTag( kColumnTag ).heading;
								if ( unitsStyle == unitsStyleInchPound ) {
									LookupSItoIP( colTagWithSI, indexUnitConv, curColTag );
									colUnitConv( countColumn ) = indexUnitConv;
								} else {
									curColTag = colTagWithSI;
									colUnitConv( countColumn ) = 0;
								}
								columnHead( countColumn ) = curColTag;
								colHeadToColTag( countColumn ) = kColumnTag;
							}
						}
						// fill the body of the table from the entries
						// find the entries associated with the current subtable
						for ( lTableEntry = 1; lTableEntry <= numTableEntry; ++lTableEntry ) {
							if ( tableEntry( lTableEntry ).subTableIndex == jSubTable ) {
								//determine what column the current entry is in
								curColTagIndex = tableEntry( lTableEntry ).indexColumn;
								for ( nColHead = 1; nColHead <= curNumColumns; ++nColHead ) {
									if ( curColTagIndex == colHeadToColTag( nColHead ) ) {
										colCurrent = nColHead;
										break;
									}
								}
								//determine what row the current entry is in
								curRowUnqObjIndex = tableEntry( lTableEntry ).uniqueObjName;
								for ( oRowHead = 1; oRowHead <= curNumRows; ++oRowHead ) {
									if ( curRowUnqObjIndex == rowToUnqObjName( oRowHead ) ) {
										rowCurrent = oRowHead;
										break;
									}
								}
								//finally assign the entry to the place in the table body
								if ( unitsStyle == unitsStyleInchPound ) {
									columnUnitConv = colUnitConv( colCurrent );
									if ( SameString( subTable( jSubTable ).name, "SizingPeriod:DesignDay" ) ) {
										if ( SameString( columnHead( colCurrent ), "Humidity Value" ) ) {
											LookupSItoIP( tableEntry( lTableEntry + 1 ).charEntry, columnUnitConv, repTableTag );
											tableEntry( lTableEntry + 1 ).charEntry = repTableTag;
										}
									}
									if ( tableEntry( lTableEntry ).origEntryIsReal && ( columnUnitConv != 0 ) ) {
										IPvalue = ConvertIP( columnUnitConv, tableEntry( lTableEntry ).origRealEntry );
										tableBody( colCurrent, rowCurrent ) = RealToStr( IPvalue, tableEntry( lTableEntry ).significantDigits );
									} else {
										tableBody( colCurrent, rowCurrent ) = tableEntry( lTableEntry ).charEntry;
									}
								} else {
									tableBody( colCurrent, rowCurrent ) = tableEntry( lTableEntry ).charEntry;
								}
							}
						}
						//create the actual output table
						WriteSubtitle( subTable( jSubTable ).name );
						WriteTable( tableBody, rowHead, columnHead, columnWidth, false, subTable( jSubTable ).footnote );
						if ( sqlite ) {
							sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, reportName( iReportName ).name, "Entire Facility", subTable( jSubTable ).name );
						}
					}
				}
			}
		}
	}

	void
	WriteComponentSizing()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   July 2007
		//       MODIFIED       January 2010, Kyle Benne
		//                      Added SQLite output
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Write out tables based on component sizing data originally
		//   found in the EIO report.

		// METHODOLOGY EMPLOYED:
		//   Create arrays for the call to WriteTable and then call it.
		//   The tables created do not have known headers for rows or
		//   columns so those are determined based on what calls have
		//   been made to the ReportSizingOutput routine.  A table
		//   is created for each type of component. Columns are created
		//   for each description within that table. Rows are created
		//   for each named object.

		// REFERENCES:
		// na

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// all arrays are in the format: (row, column)
		Array1D_string columnHead;
		Array1D_int columnWidth;
		Array1D_int colUnitConv;
		Array1D_string rowHead;
		Array2D_string tableBody;
		Array1D_string uniqueDesc;
		int numUniqueDesc;
		Array1D_string uniqueObj;
		int numUniqueObj;
		std::string curDesc;
		std::string curObj;
		int foundEntry;
		int foundDesc;
		int foundObj;
		int loopLimit;
		int iTableEntry;
		int jUnique;
		static std::string curColHeadWithSI;
		static std::string curColHead;
		static int indexUnitConv( 0 );
		static Real64 curValueSI( 0.0 );
		static Real64 curValue( 0.0 );

		if ( displayComponentSizing ) {
			WriteReportHeaders( "Component Sizing Summary", "Entire Facility", isAverage );
			//The arrays that look for unique headers are dimensioned in the
			//running program since the size of the number of entries is
			//not previouslly known. Use the size of all entries since that
			//is the maximum possible.
			uniqueDesc.allocate( numCompSizeTableEntry );
			uniqueObj.allocate( numCompSizeTableEntry );
			//initially clear the written flags for entire array
			// The following line is not really necessary and it is possible that the array has
			// not been allocated when this is first called.
			//  CompSizeTableEntry%written = .FALSE.
			// repeat the following loop until everything in array has been
			// written into a table
			loopLimit = 0;
			while ( loopLimit <= 100 ) { //put a maximum count since complex loop that could run indefinitely if error
				foundEntry = 0;
				++loopLimit;
				for ( iTableEntry = 1; iTableEntry <= numCompSizeTableEntry; ++iTableEntry ) {
					if ( ! CompSizeTableEntry( iTableEntry ).written ) {
						foundEntry = iTableEntry;
						break;
					}
				}
				if ( foundEntry == 0 ) break; //leave main loop - all items put into tables
				//clear active items
				for ( auto & e : CompSizeTableEntry ) e.active = false;
				//make an unwritten item that is of the same type active - these will be the
				//entries for the particular subtable.
				for ( iTableEntry = 1; iTableEntry <= numCompSizeTableEntry; ++iTableEntry ) {
					if ( ! CompSizeTableEntry( iTableEntry ).written ) {
						if ( SameString( CompSizeTableEntry( iTableEntry ).typeField, CompSizeTableEntry( foundEntry ).typeField ) ) {
							CompSizeTableEntry( iTableEntry ).active = true;
						}
					}
				}
				//identify unique descriptions and objects (columns and rows) in order
				//to size the table arrays properly.
				//reset the counters for the arrays looking for unique rows and columns
				numUniqueDesc = 0;
				numUniqueObj = 0;
				for ( iTableEntry = 1; iTableEntry <= numCompSizeTableEntry; ++iTableEntry ) {
					//search for descriptions
					foundDesc = 0;
					if ( CompSizeTableEntry( iTableEntry ).active ) {
						curDesc = CompSizeTableEntry( iTableEntry ).description;
						//look through the list of unique items to see if it matches
						for ( jUnique = 1; jUnique <= numUniqueDesc; ++jUnique ) {
							if ( SameString( curDesc, uniqueDesc( jUnique ) ) ) {
								foundDesc = jUnique;
								break;
							}
						}
						//if not found add to the list
						if ( foundDesc == 0 ) {
							++numUniqueDesc;
							uniqueDesc( numUniqueDesc ) = curDesc;
						}
						//search for objects
						foundObj = 0;
						curObj = CompSizeTableEntry( iTableEntry ).nameField;
						for ( jUnique = 1; jUnique <= numUniqueObj; ++jUnique ) {
							if ( SameString( curObj, uniqueObj( jUnique ) ) ) {
								foundObj = jUnique;
								break;
							}
						}
						//if not found add to the list
						if ( foundObj == 0 ) {
							++numUniqueObj;
							uniqueObj( numUniqueObj ) = curObj;
						}
					}
				}
				//make sure the table has at least one row and columns
				if ( numUniqueDesc == 0 ) numUniqueDesc = 1;
				if ( numUniqueObj == 0 ) numUniqueObj = 1;
				//now that the unique row and column headers are known the array
				//sizes can be set for the table arrays
				rowHead.allocate( numUniqueObj );
				columnHead.allocate( numUniqueDesc );
				columnWidth.dimension( numUniqueDesc, 14 ); //array assignment - same for all columns
				colUnitConv.allocate( numUniqueDesc );
				tableBody.allocate( numUniqueDesc, numUniqueObj );
				// initialize table body to blanks (in case entries are incomplete)
				tableBody = "";
				//transfer the row and column headings first
				for ( jUnique = 1; jUnique <= numUniqueDesc; ++jUnique ) {
					//do the unit conversions
					curColHeadWithSI = uniqueDesc( jUnique );
					if ( unitsStyle == unitsStyleInchPound ) {
						LookupSItoIP( curColHeadWithSI, indexUnitConv, curColHead );
						colUnitConv( jUnique ) = indexUnitConv;
					} else {
						curColHead = curColHeadWithSI;
						colUnitConv( jUnique ) = 0;
					}
					columnHead( jUnique ) = curColHead;
				}
				for ( jUnique = 1; jUnique <= numUniqueObj; ++jUnique ) {
					rowHead( jUnique ) = uniqueObj( jUnique );
				}
				//fill the table
				for ( iTableEntry = 1; iTableEntry <= numCompSizeTableEntry; ++iTableEntry ) {
					//find the row and column for the specific entry
					if ( CompSizeTableEntry( iTableEntry ).active ) {
						curDesc = CompSizeTableEntry( iTableEntry ).description;
						foundDesc = 0;
						for ( jUnique = 1; jUnique <= numUniqueDesc; ++jUnique ) {
							if ( SameString( uniqueDesc( jUnique ), curDesc ) ) {
								foundDesc = jUnique;
								break;
							}
						}
						curObj = CompSizeTableEntry( iTableEntry ).nameField;
						foundObj = 0;
						for ( jUnique = 1; jUnique <= numUniqueObj; ++jUnique ) {
							if ( SameString( rowHead( jUnique ), curObj ) ) {
								foundObj = jUnique;
								break;
							}
						}
						if ( ( foundDesc >= 1 ) && ( foundObj >= 1 ) ) {
							curValueSI = CompSizeTableEntry( iTableEntry ).valField;
							if ( unitsStyle == unitsStyleInchPound ) {
								if ( colUnitConv( foundDesc ) != 0 ) {
									curValue = ConvertIP( colUnitConv( foundDesc ), curValueSI );
								} else {
									curValue = curValueSI;
								}
							} else {
								curValue = curValueSI;
							}
							if ( std::abs( curValue ) >= 1.0 ) {
								tableBody( foundDesc, foundObj ) = RealToStr( curValue, 2 );
							} else {
								tableBody( foundDesc, foundObj ) = RealToStr( curValue, 6 );
							}
							CompSizeTableEntry( iTableEntry ).written = true;
						}
					}
				}
				//write the table
				WriteSubtitle( CompSizeTableEntry( foundEntry ).typeField );
				WriteTable( tableBody, rowHead, columnHead, columnWidth, false, "User-Specified values were used. Design Size values were used if no User-Specified values were provided." );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "ComponentSizingSummary", "Entire Facility", CompSizeTableEntry( foundEntry ).typeField );
				}
			}
		}
	}

	void
	WriteSurfaceShadowing()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   July 2007
		//       MODIFIED       January 2010, Kyle Benne
		//                      Added SQLite output
		//       RE-ENGINEERED  June 2014, Stuart Mentzer, Performance tuning

		// PURPOSE OF THIS SUBROUTINE:
		//   Write out tables based on which surfaces shade subsurfaces.

		// METHODOLOGY EMPLOYED:
		//   Create arrays for the call to WriteTable and then call it.
		//   Use <br> tag to put multiple rows into a single cell.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSurfaces::Surface;
		using DataSurfaces::TotSurfaces;
		using namespace DataShadowingCombinations;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// all arrays are in the format: (row, column)
		Array1D_string columnHead( 1 );
		Array1D_int columnWidth( 1 );
		Array1D_string rowHead;
		Array2D_string tableBody;
		//CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: unique
		Array1D_int unique;
		int numUnique;
		//CHARACTER(len=MaxNameLength)                               :: curRecSurf
		int curRecSurf;
		std::string listOfSurf;
		int iShadRel;
		int jUnique;
		int iKindRec;
		int numreceivingfields;
		int HTS;
		int NGSS;

		//displaySurfaceShadowing = false  for debugging
		if ( displaySurfaceShadowing ) {
			numreceivingfields = 0;
			for ( HTS = 1; HTS <= TotSurfaces; ++HTS ) {
				numreceivingfields += ShadowComb( HTS ).NumGenSurf;
				numreceivingfields += ShadowComb( HTS ).NumSubSurf;
			}

			ShadowRelate.allocate( numreceivingfields );
			numShadowRelate = 0;
			for ( HTS = 1; HTS <= TotSurfaces; ++HTS ) {
				for ( NGSS = 1; NGSS <= ShadowComb( HTS ).NumGenSurf; ++NGSS ) {
					++numShadowRelate;
					ShadowRelate( numShadowRelate ).castSurf = ShadowComb( HTS ).GenSurf( NGSS );
					ShadowRelate( numShadowRelate ).recSurf = HTS;
					ShadowRelate( numShadowRelate ).recKind = recKindSurface;
				}
				for ( NGSS = 1; NGSS <= ShadowComb( HTS ).NumSubSurf; ++NGSS ) {
					++numShadowRelate;
					ShadowRelate( numShadowRelate ).castSurf = ShadowComb( HTS ).SubSurf( NGSS );
					ShadowRelate( numShadowRelate ).recSurf = HTS;
					ShadowRelate( numShadowRelate ).recKind = recKindSubsurface;
				}
			}
			assert( numreceivingfields == numShadowRelate );

			WriteReportHeaders( "Surface Shadowing Summary", "Entire Facility", isAverage );
			unique.allocate( numShadowRelate );
			// do entire process twice, once with surfaces receiving, once with subsurfaces receiving
			for ( iKindRec = recKindSurface; iKindRec <= recKindSubsurface; ++iKindRec ) {

				// Build map from receiving surface to container of names
				typedef  std::map< int, std::pair< int, std::vector< std::string const * > > >  ShadowMap;
				ShadowMap shadow_map;
				for ( iShadRel = 1; iShadRel <= numShadowRelate; ++iShadRel ) {
					if ( ShadowRelate( iShadRel ).recKind == iKindRec ) {
						curRecSurf = ShadowRelate( iShadRel ).recSurf;
						std::string const & name( Surface( ShadowRelate( iShadRel ).castSurf ).Name );
						auto & elem( shadow_map[ curRecSurf ] ); // Creates the entry if not present (and zero-initializes the int in the pair)
						elem.first += static_cast< int >( name.length() ); // Accumulate total of name lengths
						elem.second.push_back( &name ); // Add this name
					}
				}
				numUnique = static_cast< int >( shadow_map.size() );
				if ( numUnique == 0 ) {
					columnHead( 1 ) = "None";
				} else {
					columnHead( 1 ) = "Possible Shadow Receivers";
				}
				columnWidth = 14; // array assignment - same for all columns
				rowHead.allocate( numUnique );
				tableBody.allocate( 1, numUnique );
				jUnique = 0;
				for ( auto const & elem : shadow_map ) {
					++jUnique;
					curRecSurf = elem.first;
					rowHead( jUnique ) = Surface( curRecSurf ).Name;
					listOfSurf.clear();
					listOfSurf.reserve( elem.second.first + ( 3 * numUnique ) ); // To avoid string allocations during appends
					for ( auto const * p : elem.second.second ) {
						listOfSurf += *p;
						listOfSurf += " | "; //'<br>' // Separate append to avoid string temporary
					}
					tableBody( 1, jUnique ) = listOfSurf;
				}

				// write the table
				if ( iKindRec == recKindSurface ) {
					WriteSubtitle( "Surfaces (Walls, Roofs, etc) that may be Shadowed by Other Surfaces" );
					if ( sqlite ) {
						sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "SurfaceShadowingSummary", "Entire Facility", "Surfaces (Walls, Roofs, etc) that may be Shadowed by Other Surfaces" );
					}
				} else if ( iKindRec == recKindSubsurface ) {
					WriteSubtitle( "Subsurfaces (Windows and Doors) that may be Shadowed by Surfaces" );
					if ( sqlite ) {
						sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "SurfaceShadowingSummary", "Entire Facility", "Subsurfaces (Windows and Doors) that may be Shadowed by Surfaces" );
					}
				}
				WriteTable( tableBody, rowHead, columnHead, columnWidth );
			}
		}
	}

	void
	AddTOCZoneLoadComponentTable()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Add the table of contents entries for the Zone heat transfer
		//   summary report.

		// METHODOLOGY EMPLOYED:
		//   Call the AddTOCEntry routine for each zone.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Using/Aliasing
		using DataHeatBalance::Zone;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataGlobals::CompLoadReportIsReq;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int iZone;

		if ( displayZoneComponentLoadSummary && CompLoadReportIsReq ) {
			for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
				if ( ! ZoneEquipConfig( iZone ).IsControlled ) continue;
				AddTOCEntry( "Zone Component Load Summary", Zone( iZone ).Name );
			}
		}
	}

	void
	AllocateLoadComponentArrays()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   April 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Allocate the arrays related to the load component report

		// METHODOLOGY EMPLOYED:
		//   Use the ALLOCATE command

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using DataSurfaces::TotSurfaces;
		using DataEnvironment::TotDesDays;
		using DataEnvironment::TotRunDesPersDays;
		using DataGlobals::NumOfTimeStepInHour;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:


		if ( AllocateLoadComponentArraysDoAllocate ) {
			//For many of the following arrays the last dimension is the number of environments and is same as sizing arrays
			radiantPulseUsed.allocate( {0,TotDesDays + TotRunDesPersDays}, NumOfZones );
			radiantPulseUsed = 0.0;
			radiantPulseTimestep.allocate( {0,TotDesDays + TotRunDesPersDays}, NumOfZones );
			radiantPulseTimestep = 0;
			radiantPulseReceived.allocate( {0,TotDesDays + TotRunDesPersDays}, TotSurfaces );
			radiantPulseReceived = 0.0;
			loadConvectedNormal.allocate( TotDesDays + TotRunDesPersDays, {0,NumOfTimeStepInHour * 24}, TotSurfaces );
			loadConvectedNormal = 0.0;
			loadConvectedWithPulse.allocate( TotDesDays + TotRunDesPersDays, {0,NumOfTimeStepInHour * 24}, TotSurfaces );
			loadConvectedWithPulse = 0.0;
			netSurfRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
			netSurfRadSeq = 0.0;
			decayCurveCool.allocate( NumOfTimeStepInHour * 24, TotSurfaces );
			decayCurveCool = 0.0;
			decayCurveHeat.allocate( NumOfTimeStepInHour * 24, TotSurfaces );
			decayCurveHeat = 0.0;
			ITABSFseq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
			ITABSFseq = 0.0;
			TMULTseq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			TMULTseq = 0.0;
			peopleInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			peopleInstantSeq = 0.0;
			peopleLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			peopleLatentSeq = 0.0;
			peopleRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			peopleRadSeq = 0.0;
			lightInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			lightInstantSeq = 0.0;
			lightRetAirSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			lightRetAirSeq = 0.0;
			lightLWRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			lightLWRadSeq = 0.0;
			lightSWRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
			lightSWRadSeq = 0.0;
			equipInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			equipInstantSeq = 0.0;
			equipLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			equipLatentSeq = 0.0;
			equipRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			equipRadSeq = 0.0;
			refrigInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			refrigInstantSeq = 0.0;
			refrigRetAirSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			refrigRetAirSeq = 0.0;
			refrigLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			refrigLatentSeq = 0.0;
			waterUseInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			waterUseInstantSeq = 0.0;
			waterUseLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			waterUseLatentSeq = 0.0;
			hvacLossInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			hvacLossInstantSeq = 0.0;
			hvacLossRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			hvacLossRadSeq = 0.0;
			powerGenInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			powerGenInstantSeq = 0.0;
			powerGenRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			powerGenRadSeq = 0.0;
			infilInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			infilInstantSeq = 0.0;
			infilLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			infilLatentSeq = 0.0;
			zoneVentInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			zoneVentInstantSeq = 0.0;
			zoneVentLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			zoneVentLatentSeq = 0.0;
			interZoneMixInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			interZoneMixInstantSeq = 0.0;
			interZoneMixLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			interZoneMixLatentSeq = 0.0;
			feneCondInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
			feneCondInstantSeq = 0.0;
			//  ALLOCATE(feneSolarInstantSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
			//  feneSolarInstantSeq = 0.0d0
			feneSolarRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
			feneSolarRadSeq = 0.0;
			AllocateLoadComponentArraysDoAllocate = false;
		}
	}

	void
	DeallocateLoadComponentArrays()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Deallocate the arrays related to the load component report that will not
		//   be needed in the reporting.

		// METHODOLOGY EMPLOYED:
		//   Use the DEALLOCATE command

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using DataSurfaces::TotSurfaces;
		using DataEnvironment::TotDesDays;
		using DataEnvironment::TotRunDesPersDays;
		using DataGlobals::NumOfTimeStepInHour;

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		radiantPulseUsed.deallocate();
		radiantPulseTimestep.deallocate();
		radiantPulseReceived.deallocate();
		//need for reporting  DEALLOCATE(loadConvectedNormal)
		loadConvectedWithPulse.deallocate();
		//need for reporting  DEALLOCATE(decayCurveCool)
		//need for reporting  DEALLOCATE(decayCurveHeat)
	}

	void
	ComputeLoadComponentDecayCurve()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Determines the load component decay curve based on normal and pulse results from zone sizing.

		// METHODOLOGY EMPLOYED:
		// Decay curve is the fraction of the heat convected from a surface over the initial radiant heat
		// absorbed by the surface.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSizing::CalcFinalZoneSizing;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataSurfaces::Surface;
		using DataSurfaces::TotSurfaces;
		using DataGlobals::NumOfTimeStepInHour;

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
		static int ZoneNum( 0 );
		static int SurfNum( 0 );
		static int TimeStep( 0 );
		static int TimeOfPulse( 0 );
		static int CoolDesSelected( 0 ); // design day selected for cooling
		static int HeatDesSelected( 0 ); // design day selected for heating
		int i;
		Real64 diff;

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			ZoneNum = Surface( SurfNum ).Zone;
			if ( ZoneNum == 0 ) continue;
			if ( ! ZoneEquipConfig( ZoneNum ).IsControlled ) continue;
			CoolDesSelected = CalcFinalZoneSizing( ZoneNum ).CoolDDNum;
			//loop over timesteps after pulse occured
			if ( CoolDesSelected != 0 ) {
				TimeOfPulse = radiantPulseTimestep( CoolDesSelected, ZoneNum );
				// if the CoolDesSelected time is on a different day than
				// when the pulse occurred, need to scan back and find when
				// the pulse occurred.
				if ( TimeOfPulse == 0 ) {
					for ( i = CoolDesSelected; i >= 1; --i ) {
						TimeOfPulse = radiantPulseTimestep( i, ZoneNum );
						if ( TimeOfPulse != 0 ) break;
					}
				}
				if ( TimeOfPulse == 0 ) TimeOfPulse = 1;
				for ( TimeStep = TimeOfPulse; TimeStep <= NumOfTimeStepInHour * 24; ++TimeStep ) {
					if ( radiantPulseReceived( CoolDesSelected, SurfNum ) != 0.0 ) {
						diff = loadConvectedWithPulse( CoolDesSelected, TimeStep, SurfNum ) - loadConvectedNormal( CoolDesSelected, TimeStep, SurfNum );
						decayCurveCool( TimeStep - TimeOfPulse + 1, SurfNum ) = -diff / radiantPulseReceived( CoolDesSelected, SurfNum );
					} else {
						decayCurveCool( TimeStep - TimeOfPulse + 1, SurfNum ) = 0.0;
					}
				}
			}
			HeatDesSelected = CalcFinalZoneSizing( ZoneNum ).HeatDDNum;
			if ( HeatDesSelected != 0 ) {
				TimeOfPulse = radiantPulseTimestep( HeatDesSelected, ZoneNum );
				// scan back to the day that the heating pulse occurs, if necessary
				if ( TimeOfPulse == 0 ) {
					for ( i = HeatDesSelected; i >= 1; --i ) {
						TimeOfPulse = radiantPulseTimestep( i, ZoneNum );
						if ( TimeOfPulse != 0 ) break;
					}
				}
				if ( TimeOfPulse == 0 ) TimeOfPulse = 1;
				for ( TimeStep = TimeOfPulse; TimeStep <= NumOfTimeStepInHour * 24; ++TimeStep ) {
					if ( radiantPulseReceived( HeatDesSelected, SurfNum ) != 0.0 ) {
						diff = loadConvectedWithPulse( HeatDesSelected, TimeStep, SurfNum ) - loadConvectedNormal( HeatDesSelected, TimeStep, SurfNum );
						decayCurveHeat( TimeStep - TimeOfPulse + 1, SurfNum ) = -diff / radiantPulseReceived( HeatDesSelected, SurfNum );
					} else {
						decayCurveHeat( TimeStep - TimeOfPulse + 1, SurfNum ) = 0.0;
					}
				}
			}
		}
	}

	void
	GatherComponentLoadsSurface()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   September 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Gather values during sizing used for loads component report.

		// METHODOLOGY EMPLOYED:
		//   Save sequence of values for report during sizing.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using DataGlobals::NumOfTimeStepInHour;
		using DataGlobals::CompLoadReportIsReq;
		using DataGlobals::isPulseZoneSizing;
		using DataSizing::CurOverallSimDay;
		using DataSurfaces::Surface;
		using DataSurfaces::TotSurfaces;
		using DataSurfaces::WinGainConvGlazToZoneRep;
		using DataSurfaces::WinGainConvGlazShadGapToZoneRep;
		using DataSurfaces::WinGainConvShadeToZoneRep;
		using DataSurfaces::WinGainFrameDividerToZoneRep;
		using DataSurfaces::SurfaceClass_Window;
		using DataZoneEquipment::ZoneEquipConfig;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int iSurf( 0 );
		static int ZoneNum( 0 );
		static int TimeStepInDay( 0 );

		if ( CompLoadReportIsReq && ! isPulseZoneSizing ) {
			TimeStepInDay = ( HourOfDay - 1 ) * NumOfTimeStepInHour + TimeStep;
			feneCondInstantSeq( CurOverallSimDay, TimeStepInDay, _ ) = 0.0;
			for ( iSurf = 1; iSurf <= TotSurfaces; ++iSurf ) {
				ZoneNum = Surface( iSurf ).Zone;
				if ( ZoneNum == 0 ) continue;
				if ( Surface( iSurf ).Class != SurfaceClass_Window ) continue;
				// IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE
				feneCondInstantSeq( CurOverallSimDay, TimeStepInDay, ZoneNum ) += WinGainConvGlazToZoneRep( iSurf ) + WinGainConvGlazShadGapToZoneRep( iSurf ) + WinGainConvShadeToZoneRep( iSurf ) + WinGainFrameDividerToZoneRep( iSurf );
				// for now assume zero instant solar - may change related
				// to how blinds and shades absorb solar radiation and
				// convect that heat that timestep.
				//feneSolarInstantSeq(ZoneNum,TimeStepInDay,CurOverallSimDay) = 0
			}
		}
	}

	void
	GatherComponentLoadsHVAC()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   September 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Gather values during sizing used for loads component report.

		// METHODOLOGY EMPLOYED:
		//   Save sequence of values for report during sizing.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using DataGlobals::NumOfTimeStepInHour;
		using DataGlobals::CompLoadReportIsReq;
		using DataGlobals::isPulseZoneSizing;
		using DataSizing::CurOverallSimDay;
		using DataHeatBalance::ZnAirRpt;
		using DataHVACGlobals::TimeStepSys;
		using DataAirflowNetwork::SimulateAirflowNetwork;
		using DataAirflowNetwork::AirflowNetworkControlSimple;
		using DataAirflowNetwork::AirflowNetworkReportData;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int iZone( 0 );
		static int TimeStepInDay( 0 );

		if ( CompLoadReportIsReq && ! isPulseZoneSizing ) {
			TimeStepInDay = ( HourOfDay - 1 ) * NumOfTimeStepInHour + TimeStep;
			for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
				infilInstantSeq( CurOverallSimDay, TimeStepInDay, iZone ) = ( ( ZnAirRpt( iZone ).InfilHeatGain - ZnAirRpt( iZone ).InfilHeatLoss ) / ( TimeStepSys * SecInHour ) ); //zone infiltration
				if ( SimulateAirflowNetwork > AirflowNetworkControlSimple ) {
					infilInstantSeq( CurOverallSimDay, TimeStepInDay, iZone ) += ( AirflowNetworkReportData( iZone ).MultiZoneInfiSenGainW - AirflowNetworkReportData( iZone ).MultiZoneInfiSenLossW ); //air flow network
				}
				infilLatentSeq( CurOverallSimDay, TimeStepInDay, iZone ) = ( ( ZnAirRpt( iZone ).InfilLatentGain - ZnAirRpt( iZone ).InfilLatentLoss ) / ( TimeStepSys * SecInHour ) ); //zone infiltration
				if ( SimulateAirflowNetwork > AirflowNetworkControlSimple ) {
					infilLatentSeq( CurOverallSimDay, TimeStepInDay, iZone ) += ( AirflowNetworkReportData( iZone ).MultiZoneInfiLatGainW - AirflowNetworkReportData( iZone ).MultiZoneInfiLatLossW ); //air flow network
				}

				zoneVentInstantSeq( CurOverallSimDay, TimeStepInDay, iZone ) = ( ( ZnAirRpt( iZone ).VentilHeatGain - ZnAirRpt( iZone ).VentilHeatLoss ) / ( TimeStepSys * SecInHour ) ); //zone ventilation
				zoneVentLatentSeq( CurOverallSimDay, TimeStepInDay, iZone ) = ( ( ZnAirRpt( iZone ).VentilLatentGain - ZnAirRpt( iZone ).VentilLatentLoss ) / ( TimeStepSys * SecInHour ) ); //zone ventilation

				interZoneMixInstantSeq( CurOverallSimDay, TimeStepInDay, iZone ) = ( ( ZnAirRpt( iZone ).MixHeatGain - ZnAirRpt( iZone ).MixHeatLoss ) / ( TimeStepSys * SecInHour ) ); //zone mixing
				if ( SimulateAirflowNetwork > AirflowNetworkControlSimple ) {
					interZoneMixInstantSeq( CurOverallSimDay, TimeStepInDay, iZone ) += ( AirflowNetworkReportData( iZone ).MultiZoneMixSenGainW - AirflowNetworkReportData( iZone ).MultiZoneMixSenLossW ); //air flow network
				}
				interZoneMixLatentSeq( CurOverallSimDay, TimeStepInDay, iZone ) = ( ( ZnAirRpt( iZone ).MixLatentGain - ZnAirRpt( iZone ).MixLatentLoss ) / ( TimeStepSys * SecInHour ) ); //zone mixing
				if ( SimulateAirflowNetwork > AirflowNetworkControlSimple ) {
					interZoneMixLatentSeq( CurOverallSimDay, TimeStepInDay, iZone ) += ( AirflowNetworkReportData( iZone ).MultiZoneMixLatGainW - AirflowNetworkReportData( iZone ).MultiZoneMixLatLossW ); //air flow network
				}
			}
		}
	}


	void
	WriteZoneLoadComponentTable()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  Amir Roth, Feb 2016 (unified with ComputeDelayedComponents to simplify code and debugging)

		// PURPOSE OF THIS SUBROUTINE:
		//   Write the tables for the ZoneLoadComponentSummary and
		//   ZoneLoadComponentDetail reports which summarize the major
		//   load components for each zone in the building.

		// METHODOLOGY EMPLOYED:
		//   Create arrays for the call to WriteTable and then call it.
		//   This report actually consists of many sub-tables each with
		//   its own call to WriteTable.
		// The overall methodology is explained below:
		// Determine decay curve - Pulse of radiant heat which is about 5% of lighting and
		// equipment input [radiantPulseUsed(iZone)] for a single timestep a few hours after
		// cooling or heat is scheduled on for each zone [radiantPulseTimestep(iZone)].
		// The radiant heat received on each wall is stored [radiantPulseReceived(jSurface)].
		// The load convected in the normal case [loadConvectedNormal(jSurface, kTime, mode)]
		// and in the case with the pulse [loadConvectedWithPulse(jSurface, kTime, mode)].
		// The difference divided by the pulse received by each surface
		// [radiantPulseReceived(jSurface)] is stored in [decayCurve(jSurface,kTime,mode)].
		// Determine delayed loads - From the last timestep of the peak load on the zone
		// working backwards any radiant heat that was absorbed by the wall from an internal gain
		// or solar gain is multiplied by the appropriate timesteps in the decay curve
		// [decayCurve(jSurface,kTime,mode)] for timesteps that make up
		// the number of averaged timesteps are used to determine the peak load
		// [NumTimeStepsInAvg]. The sum for all surfaces in the zone are added together to
		// determine the delayed load.
		// Determine instant loads - Average the convective portion of the internal gains
		// for the timesteps made up of the peak load period. Average those across the peak
		// load period.
		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Using/Aliasing
		using DataHeatBalance::Zone;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataSurfaces::Surface;
		using DataSurfaces::TotSurfaces;
		using DataSurfaces::ExternalEnvironment;
		using DataSurfaces::Ground;
		using DataSurfaces::GroundFCfactorMethod;
		using DataSurfaces::OtherSideCoefNoCalcExt;
		using DataSurfaces::OtherSideCoefCalcExt;
		using DataSurfaces::OtherSideCondModeledExt;
		using DataSurfaces::SurfaceClass_Wall;
		using DataSurfaces::SurfaceClass_Floor;
		using DataSurfaces::SurfaceClass_Roof;
		using DataSurfaces::SurfaceClass_Door;
		using DataSurfaces::SurfaceClass_Window;
		using DataSurfaces::OSC;
		using DataSizing::CalcFinalZoneSizing;
		using DataSizing::NumTimeStepsInAvg;
		using DataSizing::CoolPeakDateHrMin;
		using DataSizing::HeatPeakDateHrMin;
		using DataSizing::CalcZoneSizing;
		using DataGlobals::NumOfTimeStepInHour;
		using DataGlobals::CompLoadReportIsReq;
		using DataGlobals::ShowDecayCurvesInEIO;
		using General::MovingAvg;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using Psychrometrics::PsyRhFnTdbWPb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// These correspond to the columns in the load component table
		int const cSensInst( 1 );
		int const cSensDelay( 2 );
		int const cSensRA( 3 );
		int const cLatent( 4 );
		int const cTotal( 5 );
		int const cPerc( 6 );

		//internal gains
		int const rPeople( 1 );
		int const rLights( 2 );
		int const rEquip( 3 );
		int const rRefrig( 4 );
		int const rWaterUse( 5 );
		int const rHvacLoss( 6 );
		int const rPowerGen( 7 );
		//misc
		int const rDOAS( 8 );
		int const rInfil( 9 );
		int const rZoneVent( 10 );
		int const rIntZonMix( 11 );
		//opaque surfaces
		int const rRoof( 12 );
		int const rIntZonCeil( 13 );
		int const rOtherRoof( 14 );
		int const rExtWall( 15 );
		int const rIntZonWall( 16 );
		int const rGrdWall( 17 );
		int const rOtherWall( 18 );
		int const rExtFlr( 19 );
		int const rIntZonFlr( 20 );
		int const rGrdFlr( 21 );
		int const rOtherFlr( 22 );
		//subsurfaces
		int const rFeneCond( 23 );
		int const rFeneSolr( 24 );
		int const rOpqDoor( 25 );
		//total
		int const rGrdTot( 26 );

		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		if ( !(displayZoneComponentLoadSummary && CompLoadReportIsReq) )
			return;

		Array1D< Real64 > seqData; // raw data sequence that has not been averaged yet
		Array1D< Real64 > AvgData; // sequence data after averaging
		int NumOfTimeStepInDay;
		Array1D< Real64 > delayOpaque; // hold values for report for delayed opaque
		Real64 singleSurfDelay;
		Array1D< Real64 > totalColumn;
		Array1D< Real64 > percentColumn;
		Array1D< Real64 > grandTotalRow;
		Real64 totalGrandTotal;
		Real64 powerConversion;
		int tempConvIndx; // temperature conversion index
		int curExtBoundCond;
		Real64 mult; // zone multiplier

		// all arrays are in the format: (row, column)
		Array1D_string columnHead;
		Array1D_int columnWidth;
		Array1D_string rowHead;
		Array2D_string tableBody;

		// Delayed components are moved into this function so that we can calculate them one zone at a time
		// with Array1D
		Array1D< Real64 > peopleDelaySeqHeat;
		Array1D< Real64 > peopleDelaySeqCool;
		Array1D< Real64 > lightDelaySeqHeat;
		Array1D< Real64 > lightDelaySeqCool;
		Array1D< Real64 > equipDelaySeqHeat;
		Array1D< Real64 > equipDelaySeqCool;
		Array1D< Real64 > hvacLossDelaySeqHeat;
		Array1D< Real64 > hvacLossDelaySeqCool;
		Array1D< Real64 > powerGenDelaySeqHeat;
		Array1D< Real64 > powerGenDelaySeqCool;
		Array1D< Real64 > feneSolarDelaySeqHeat;
		Array1D< Real64 > feneSolarDelaySeqCool;
		Array2D< Real64 > surfDelaySeqHeat;
		Array2D< Real64 > surfDelaySeqCool;

		peopleDelaySeqHeat.dimension( NumOfTimeStepInHour * 24, 0.0 );
		peopleDelaySeqHeat = 0.0;
		peopleDelaySeqCool.allocate( NumOfTimeStepInHour * 24 );
		peopleDelaySeqCool = 0.0;
		lightDelaySeqHeat.allocate( NumOfTimeStepInHour * 24 );
		lightDelaySeqHeat = 0.0;
		lightDelaySeqCool.allocate( NumOfTimeStepInHour * 24 );
		lightDelaySeqCool = 0.0;
		equipDelaySeqHeat.allocate( NumOfTimeStepInHour * 24 );
		equipDelaySeqHeat = 0.0;
		equipDelaySeqCool.allocate( NumOfTimeStepInHour * 24 );
		equipDelaySeqCool = 0.0;
		hvacLossDelaySeqHeat.allocate( NumOfTimeStepInHour * 24 );
		hvacLossDelaySeqHeat = 0.0;
		hvacLossDelaySeqCool.allocate( NumOfTimeStepInHour * 24 );
		hvacLossDelaySeqCool = 0.0;
		powerGenDelaySeqHeat.allocate( NumOfTimeStepInHour * 24 );
		powerGenDelaySeqHeat = 0.0;
		powerGenDelaySeqCool.allocate( NumOfTimeStepInHour * 24 );
		powerGenDelaySeqCool = 0.0;
		feneSolarDelaySeqHeat.allocate( NumOfTimeStepInHour * 24 );
		feneSolarDelaySeqHeat = 0.0;
		feneSolarDelaySeqCool.allocate( NumOfTimeStepInHour * 24 );
		feneSolarDelaySeqCool = 0.0;
		surfDelaySeqHeat.allocate( NumOfTimeStepInHour * 24, TotSurfaces );
		surfDelaySeqHeat = 0.0;
		surfDelaySeqCool.allocate( NumOfTimeStepInHour * 24, TotSurfaces );
		surfDelaySeqCool = 0.0;

		Array1D< Real64 > peopleRadIntoSurf;
		Array1D< Real64 > equipRadIntoSurf;
		Array1D< Real64 > hvacLossRadIntoSurf;
		Array1D< Real64 > powerGenRadIntoSurf;
		Array1D< Real64 > lightLWRadIntoSurf;

		peopleRadIntoSurf.allocate( NumOfTimeStepInHour * 24 );
		equipRadIntoSurf.allocate( NumOfTimeStepInHour * 24 );
		hvacLossRadIntoSurf.allocate( NumOfTimeStepInHour * 24 );
		powerGenRadIntoSurf.allocate( NumOfTimeStepInHour * 24 );
		lightLWRadIntoSurf.allocate( NumOfTimeStepInHour * 24 );

		NumOfTimeStepInDay = NumOfTimeStepInHour * 24;
		seqData.allocate( NumOfTimeStepInDay );
		AvgData.allocate( NumOfTimeStepInDay );
		delayOpaque.allocate( rGrdTot );
		totalColumn.allocate( rGrdTot );
		percentColumn.allocate( rGrdTot );
		grandTotalRow.allocate( cPerc );

		//establish unit conversion factors
		if ( unitsStyle == unitsStyleInchPound ) {
			powerConversion = getSpecificUnitMultiplier( "W", "Btu/h" ); //or kBtuh?
			tempConvIndx = getSpecificUnitIndex( "C", "F" );
		} else {
			powerConversion = 1.0;
			tempConvIndx = 0; //when zero is used with ConvertIP the value is returned unconverted
		}

		// show the line definition for the decay curves
		if ( ShowDecayCurvesInEIO ) {
			gio::write( OutputFileInits, fmtA ) << "! <Radiant to Convective Decay Curves for Cooling>,Zone Name, Surface Name, Time 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36";
			gio::write( OutputFileInits, fmtA ) << "! <Radiant to Convective Decay Curves for Heating>,Zone Name, Surface Name, Time 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36";
		}

		for ( int iZone = 1; iZone <= NumOfZones; ++iZone ) {
			if ( ! ZoneEquipConfig( iZone ).IsControlled ) continue;
			mult = Zone( iZone ).Multiplier * Zone( iZone ).ListMultiplier;
			if ( mult == 0.0 ) mult = 1.0;

			ZoneData &zd( Zone( iZone ) );
			if ( allocated( CalcFinalZoneSizing ) ) {

				// Heating design days
				int desSelected = CalcFinalZoneSizing( iZone ).HeatDDNum;
				if ( desSelected != 0 ) {

					for ( int kTimeStep = 1; kTimeStep <= NumOfTimeStepInHour * 24; ++kTimeStep ) {
						Real64 peopleConvIntoZone = 0.0;
						Real64 equipConvIntoZone = 0.0;
						Real64 hvacLossConvIntoZone = 0.0;
						Real64 powerGenConvIntoZone = 0.0;
						Real64 lightLWConvIntoZone = 0.0;
						Real64 lightSWConvIntoZone = 0.0;
						Real64 feneSolarConvIntoZone = 0.0;
						Real64 adjFeneSurfNetRadSeq = 0.0;

						for ( int jSurf = zd.SurfaceFirst; jSurf <= zd.SurfaceLast; ++jSurf ) {
							if ( ! Surface( jSurf ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

							//determine for each timestep the amount of radiant heat for each end use absorbed in each surface
							Real64 QRadThermInAbsMult = TMULTseq( desSelected, kTimeStep, iZone ) * ITABSFseq( desSelected, kTimeStep, jSurf ) * Surface( jSurf ).Area;
							peopleRadIntoSurf( kTimeStep ) = peopleRadSeq( desSelected, kTimeStep, iZone ) * QRadThermInAbsMult;
							equipRadIntoSurf( kTimeStep ) = equipRadSeq( desSelected, kTimeStep, iZone ) * QRadThermInAbsMult;
							hvacLossRadIntoSurf( kTimeStep ) = hvacLossRadSeq( desSelected, kTimeStep, iZone ) * QRadThermInAbsMult;
							powerGenRadIntoSurf( kTimeStep ) = powerGenRadSeq( desSelected, kTimeStep, iZone ) * QRadThermInAbsMult;
							lightLWRadIntoSurf( kTimeStep ) = lightLWRadSeq( desSelected, kTimeStep, iZone ) * QRadThermInAbsMult;
							//for each time step, step back through time and apply decay curve
							Real64 peopleConvFromSurf = 0.0;
							Real64 equipConvFromSurf = 0.0;
							Real64 hvacLossConvFromSurf = 0.0;
							Real64 powerGenConvFromSurf = 0.0;
							Real64 lightLWConvFromSurf = 0.0;
							Real64 lightSWConvFromSurf = 0.0;
							Real64 feneSolarConvFromSurf = 0.0;
							for ( int mStepBack = 1; mStepBack <= kTimeStep; ++mStepBack ) {
								peopleConvFromSurf += peopleRadIntoSurf( kTimeStep - mStepBack + 1 ) * decayCurveHeat( mStepBack, jSurf );
								equipConvFromSurf += equipRadIntoSurf( kTimeStep - mStepBack + 1 ) * decayCurveHeat( mStepBack, jSurf );
								hvacLossConvFromSurf += hvacLossRadIntoSurf( kTimeStep - mStepBack + 1 ) * decayCurveHeat( mStepBack, jSurf );
								powerGenConvFromSurf += powerGenRadIntoSurf( kTimeStep - mStepBack + 1 ) * decayCurveHeat( mStepBack, jSurf );
								lightLWConvFromSurf += lightLWRadIntoSurf( kTimeStep - mStepBack + 1 ) * decayCurveHeat( mStepBack, jSurf );
								// short wave is already accumulated by surface
								lightSWConvFromSurf += lightSWRadSeq( desSelected, kTimeStep - mStepBack + 1, jSurf ) * decayCurveHeat( mStepBack, jSurf );
								feneSolarConvFromSurf += feneSolarRadSeq( desSelected, kTimeStep - mStepBack + 1, jSurf ) * decayCurveHeat( mStepBack, jSurf );
							} // for mStepBack
							peopleConvIntoZone += peopleConvFromSurf;
							equipConvIntoZone += equipConvFromSurf;
							hvacLossConvIntoZone += hvacLossConvFromSurf;
							powerGenConvIntoZone += powerGenConvFromSurf;
							lightLWConvIntoZone += lightLWConvFromSurf;
							lightSWConvIntoZone += lightSWConvFromSurf;
							feneSolarConvIntoZone += feneSolarConvFromSurf;
							// determine the remaining convective heat from the surfaces that are not based
							// on any of these other loads
							//negative because heat from surface should be positive
							surfDelaySeqHeat( kTimeStep, jSurf ) = -loadConvectedNormal( desSelected, kTimeStep, jSurf ) - netSurfRadSeq( desSelected, kTimeStep, jSurf ) - ( peopleConvFromSurf + equipConvFromSurf + hvacLossConvFromSurf + powerGenConvFromSurf + lightLWConvFromSurf + lightSWConvFromSurf + feneSolarConvFromSurf ); //remove net radiant for the surface
							// also remove the net radiant component on the instanteous conduction for fenestration
							if ( Surface( jSurf ).Class == SurfaceClass_Window ) {
								adjFeneSurfNetRadSeq += netSurfRadSeq( desSelected, kTimeStep, jSurf );
							}
						} // for jSurf
						peopleDelaySeqHeat( kTimeStep ) = peopleConvIntoZone;
						equipDelaySeqHeat( kTimeStep ) = equipConvIntoZone;
						hvacLossDelaySeqHeat( kTimeStep ) = hvacLossConvIntoZone;
						powerGenDelaySeqHeat( kTimeStep ) = powerGenConvIntoZone;
						//combine short wave (visible) and long wave (thermal) impacts
						lightDelaySeqHeat( kTimeStep ) = lightLWConvIntoZone + lightSWConvIntoZone;
						feneSolarDelaySeqHeat( kTimeStep ) = feneSolarConvIntoZone;
						// also remove the net radiant component on the instanteous conduction for fenestration
						feneCondInstantSeq( desSelected, kTimeStep, iZone ) -= adjFeneSurfNetRadSeq;
					} // for kTimeStep
				} // if desSelected != 0

				// Cooling design days
				desSelected = CalcFinalZoneSizing( iZone ).CoolDDNum;
				if ( desSelected != 0 ) {

					for ( int kTimeStep = 1; kTimeStep <= NumOfTimeStepInHour * 24; ++kTimeStep ) {
						Real64 peopleConvIntoZone = 0.0;
						Real64 equipConvIntoZone = 0.0;
						Real64 hvacLossConvIntoZone = 0.0;
						Real64 powerGenConvIntoZone = 0.0;
						Real64 lightLWConvIntoZone = 0.0;
						Real64 lightSWConvIntoZone = 0.0;
						Real64 feneSolarConvIntoZone = 0.0;
						Real64 adjFeneSurfNetRadSeq = 0.0;

						for ( int jSurf = zd.SurfaceFirst; jSurf <= zd.SurfaceLast; ++jSurf ) {
							if ( ! Surface( jSurf ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

							//determine for each timestep the amount of radiant heat for each end use absorbed in each surface
							Real64 QRadThermInAbsMult = TMULTseq( desSelected, kTimeStep, iZone ) * ITABSFseq( desSelected, kTimeStep, jSurf ) * Surface( jSurf ).Area;
							peopleRadIntoSurf( kTimeStep ) = peopleRadSeq( desSelected, kTimeStep, iZone ) * QRadThermInAbsMult;
							equipRadIntoSurf( kTimeStep ) = equipRadSeq( desSelected, kTimeStep, iZone ) * QRadThermInAbsMult;
							hvacLossRadIntoSurf( kTimeStep ) = hvacLossRadSeq( desSelected, kTimeStep, iZone ) * QRadThermInAbsMult;
							powerGenRadIntoSurf( kTimeStep ) = powerGenRadSeq( desSelected, kTimeStep, iZone ) * QRadThermInAbsMult;
							lightLWRadIntoSurf( kTimeStep ) = lightLWRadSeq( desSelected, kTimeStep, iZone ) * QRadThermInAbsMult;
							//for each time step, step back through time and apply decay curve
							Real64 peopleConvFromSurf = 0.0;
							Real64 equipConvFromSurf = 0.0;
							Real64 hvacLossConvFromSurf = 0.0;
							Real64 powerGenConvFromSurf = 0.0;
							Real64 lightLWConvFromSurf = 0.0;
							Real64 lightSWConvFromSurf = 0.0;
							Real64 feneSolarConvFromSurf = 0.0;
							for ( int mStepBack = 1; mStepBack <= kTimeStep; ++mStepBack ) {
								peopleConvFromSurf += peopleRadIntoSurf( kTimeStep - mStepBack + 1 ) * decayCurveCool( mStepBack, jSurf );
								equipConvFromSurf += equipRadIntoSurf( kTimeStep - mStepBack + 1 ) * decayCurveCool( mStepBack, jSurf );
								hvacLossConvFromSurf += hvacLossRadIntoSurf( kTimeStep - mStepBack + 1 ) * decayCurveCool( mStepBack, jSurf );
								powerGenConvFromSurf += powerGenRadIntoSurf( kTimeStep - mStepBack + 1 ) * decayCurveCool( mStepBack, jSurf );
								lightLWConvFromSurf += lightLWRadIntoSurf( kTimeStep - mStepBack + 1 ) * decayCurveCool( mStepBack, jSurf );
								// short wave is already accumulated by surface
								lightSWConvFromSurf += lightSWRadSeq( desSelected, kTimeStep - mStepBack + 1, jSurf ) * decayCurveCool( mStepBack, jSurf );
								feneSolarConvFromSurf += feneSolarRadSeq( desSelected, kTimeStep - mStepBack + 1, jSurf ) * decayCurveCool( mStepBack, jSurf );
							} // for mStepBack
							peopleConvIntoZone += peopleConvFromSurf;
							equipConvIntoZone += equipConvFromSurf;
							hvacLossConvIntoZone += hvacLossConvFromSurf;
							powerGenConvIntoZone += powerGenConvFromSurf;
							lightLWConvIntoZone += lightLWConvFromSurf;
							lightSWConvIntoZone += lightSWConvFromSurf;
							feneSolarConvIntoZone += feneSolarConvFromSurf;
							// determine the remaining convective heat from the surfaces that are not based
							// on any of these other loads
							//negative because heat from surface should be positive
							surfDelaySeqCool( kTimeStep, jSurf ) = -loadConvectedNormal( desSelected, kTimeStep, jSurf ) - netSurfRadSeq( desSelected, kTimeStep, jSurf ) - ( peopleConvFromSurf + equipConvFromSurf + hvacLossConvFromSurf + powerGenConvFromSurf + lightLWConvFromSurf + lightSWConvFromSurf + feneSolarConvFromSurf ); //remove net radiant for the surface
							// also remove the net radiant component on the instanteous conduction for fenestration
							if ( Surface( jSurf ).Class == SurfaceClass_Window ) {
								adjFeneSurfNetRadSeq += netSurfRadSeq( desSelected, kTimeStep, jSurf );
							}
						} // for jSurf
						peopleDelaySeqCool( kTimeStep ) = peopleConvIntoZone;
						equipDelaySeqCool( kTimeStep ) = equipConvIntoZone;
						hvacLossDelaySeqCool( kTimeStep ) = hvacLossConvIntoZone;
						powerGenDelaySeqCool( kTimeStep ) = powerGenConvIntoZone;
						//combine short wave (visible) and long wave (thermal) impacts
						lightDelaySeqCool( kTimeStep ) = lightLWConvIntoZone + lightSWConvIntoZone;
						feneSolarDelaySeqCool( kTimeStep ) = feneSolarConvIntoZone;
						// also remove the net radiant component on the instanteous conduction for fenestration
						feneCondInstantSeq( desSelected, kTimeStep, iZone ) -= adjFeneSurfNetRadSeq;
					} // for kTimeStep
				} // if desSelected != 0

			} // if allocated( CalcFinalZoneSizing )

			//---- Cooling Peak Load Components Sub-Table
			WriteReportHeaders( "Zone Component Load Summary", Zone( iZone ).Name, isAverage );

			rowHead.allocate( rGrdTot );
			columnHead.allocate( cPerc );
			columnWidth.dimension( cPerc, 14 ); //array assignment - same for all columns
			tableBody.allocate( cPerc, rGrdTot );

			if ( unitsStyle != unitsStyleInchPound ) {
				columnHead( cSensInst ) = "Sensible - Instant [W]";
				columnHead( cSensDelay ) = "Sensible - Delayed [W]";
				columnHead( cSensRA ) = "Sensible - Return Air [W]";
				columnHead( cLatent ) = "Latent [W]";
				columnHead( cTotal ) = "Total [W]";
				columnHead( cPerc ) = "%Grand Total";
			} else {
				columnHead( cSensInst ) = "Sensible - Instant [Btu/h]";
				columnHead( cSensDelay ) = "Sensible - Delayed [Btu/h]";
				columnHead( cSensRA ) = "Sensible - Return Air [Btu/h]";
				columnHead( cLatent ) = "Latent [Btu/h]";
				columnHead( cTotal ) = "Total [Btu/h]";
				columnHead( cPerc ) = "%Grand Total";
			}

			//internal gains
			rowHead( rPeople ) = "People";
			rowHead( rLights ) = "Lights";
			rowHead( rEquip ) = "Equipment";
			rowHead( rRefrig ) = "Refrigeration Equipment";
			rowHead( rWaterUse ) = "Water Use Equipment";
			rowHead( rPowerGen ) = "Power Generation Equipment";
			rowHead( rHvacLoss ) = "HVAC Equipment Losses";
			rowHead( rRefrig ) = "Refrigeration";
			//misc
			rowHead( rDOAS ) = "DOAS Direct to Zone";
			rowHead( rInfil ) = "Infiltration";
			rowHead( rZoneVent ) = "Zone Ventilation";
			rowHead( rIntZonMix ) = "Interzone Mixing";
			//opaque surfaces
			rowHead( rRoof ) = "Roof";
			rowHead( rIntZonCeil ) = "Interzone Ceiling";
			rowHead( rOtherRoof ) = "Other Roof";
			rowHead( rExtWall ) = "Exterior Wall";
			rowHead( rIntZonWall ) = "Interzone Wall";
			rowHead( rGrdWall ) = "Ground Contact Wall";
			rowHead( rOtherWall ) = "Other Wall";
			rowHead( rExtFlr ) = "Exterior Floor";
			rowHead( rIntZonFlr ) = "Interzone Floor";
			rowHead( rGrdFlr ) = "Ground Contact Floor";
			rowHead( rOtherFlr ) = "Other Floor";
			//subsurfaces
			rowHead( rFeneCond ) = "Fenestration Conduction";
			rowHead( rFeneSolr ) = "Fenestration Solar";
			rowHead( rOpqDoor ) = "Opaque Door";
			rowHead( rGrdTot ) = "Grand Total";

			tableBody = "";
			totalColumn = 0.0;
			percentColumn = 0.0;
			grandTotalRow = 0.0;

			int CoolDesSelected = CalcFinalZoneSizing( iZone ).CoolDDNum;
			int timeCoolMax = CalcFinalZoneSizing( iZone ).TimeStepNumAtCoolMax;
			if ( CoolDesSelected != 0 && timeCoolMax != 0 ) {

				//PEOPLE
				seqData = peopleInstantSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rPeople ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rPeople ) += AvgData( timeCoolMax );
				grandTotalRow( cSensInst ) += AvgData( timeCoolMax );

				seqData = peopleLatentSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cLatent, rPeople ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rPeople ) += AvgData( timeCoolMax );
				grandTotalRow( cLatent ) += AvgData( timeCoolMax );

				seqData = peopleDelaySeqCool( _ ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensDelay, rPeople ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rPeople ) += AvgData( timeCoolMax );
				grandTotalRow( cSensDelay ) += AvgData( timeCoolMax );

				//LIGHTS
				seqData = lightInstantSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rLights ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rLights ) += AvgData( timeCoolMax );
				grandTotalRow( cSensInst ) += AvgData( timeCoolMax );

				seqData = lightRetAirSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensRA, rLights ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rLights ) += AvgData( timeCoolMax );
				grandTotalRow( cSensRA ) += AvgData( timeCoolMax );

				seqData = lightDelaySeqCool( _ ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensDelay, rLights ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rLights ) += AvgData( timeCoolMax );
				grandTotalRow( cSensDelay ) += AvgData( timeCoolMax );

				//EQUIPMENT
				seqData = equipInstantSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rEquip ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rEquip ) += AvgData( timeCoolMax );
				grandTotalRow( cSensInst ) += AvgData( timeCoolMax );

				seqData = equipLatentSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cLatent, rEquip ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rEquip ) += AvgData( timeCoolMax );
				grandTotalRow( cLatent ) += AvgData( timeCoolMax );

				seqData = equipDelaySeqCool( _ ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensDelay, rEquip ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rEquip ) += AvgData( timeCoolMax );
				grandTotalRow( cSensDelay ) += AvgData( timeCoolMax );

				//REFRIGERATION EQUIPMENT
				seqData = refrigInstantSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rRefrig ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rRefrig ) += AvgData( timeCoolMax );
				grandTotalRow( cSensInst ) += AvgData( timeCoolMax );

				seqData = refrigRetAirSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensRA, rRefrig ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rRefrig ) += AvgData( timeCoolMax );
				grandTotalRow( cSensRA ) += AvgData( timeCoolMax );

				seqData = refrigLatentSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cLatent, rRefrig ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rRefrig ) += AvgData( timeCoolMax );
				grandTotalRow( cLatent ) += AvgData( timeCoolMax );

				//WATER USE EQUIPMENT
				seqData = waterUseInstantSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rWaterUse ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rWaterUse ) += AvgData( timeCoolMax );
				grandTotalRow( cSensInst ) += AvgData( timeCoolMax );

				seqData = waterUseLatentSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cLatent, rWaterUse ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rWaterUse ) += AvgData( timeCoolMax );
				grandTotalRow( cLatent ) += AvgData( timeCoolMax );

				//HVAC EQUIPMENT LOSSES
				seqData = hvacLossInstantSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rHvacLoss ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rHvacLoss ) += AvgData( timeCoolMax );
				grandTotalRow( cSensInst ) += AvgData( timeCoolMax );

				seqData = hvacLossDelaySeqCool( _ ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensDelay, rHvacLoss ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rHvacLoss ) += AvgData( timeCoolMax );
				grandTotalRow( cSensDelay ) += AvgData( timeCoolMax );

				//POWER GENERATION EQUIPMENT
				seqData = powerGenInstantSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rPowerGen ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rPowerGen ) += AvgData( timeCoolMax );
				grandTotalRow( cSensInst ) += AvgData( timeCoolMax );

				seqData = powerGenDelaySeqCool( _ ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensDelay, rPowerGen ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rPowerGen ) += AvgData( timeCoolMax );
				grandTotalRow( cSensDelay ) += AvgData( timeCoolMax );

				//DOAS
				tableBody( cSensInst, rDOAS ) = RealToStr( CalcZoneSizing( CoolDesSelected, iZone ).DOASHeatAddSeq( timeCoolMax ), 2 );
				totalColumn( rDOAS ) += CalcZoneSizing( CoolDesSelected, iZone ).DOASHeatAddSeq( timeCoolMax );
				grandTotalRow( cSensInst ) += CalcZoneSizing( CoolDesSelected, iZone ).DOASHeatAddSeq( timeCoolMax );

				tableBody( cLatent, rDOAS ) = RealToStr( CalcZoneSizing( CoolDesSelected, iZone ).DOASLatAddSeq( timeCoolMax ), 2 );
				totalColumn( rDOAS ) += CalcZoneSizing( CoolDesSelected, iZone ).DOASLatAddSeq( timeCoolMax );
				grandTotalRow( cLatent ) += CalcZoneSizing( CoolDesSelected, iZone ).DOASLatAddSeq( timeCoolMax );

				//INFILTRATION
				seqData = infilInstantSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rInfil ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rInfil ) += AvgData( timeCoolMax );
				grandTotalRow( cSensInst ) += AvgData( timeCoolMax );

				seqData = infilLatentSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cLatent, rInfil ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rInfil ) += AvgData( timeCoolMax );
				grandTotalRow( cLatent ) += AvgData( timeCoolMax );

				//ZONE VENTILATION
				seqData = zoneVentInstantSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rZoneVent ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rZoneVent ) += AvgData( timeCoolMax );
				grandTotalRow( cSensInst ) += AvgData( timeCoolMax );

				seqData = zoneVentLatentSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cLatent, rZoneVent ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rZoneVent ) += AvgData( timeCoolMax );
				grandTotalRow( cLatent ) += AvgData( timeCoolMax );

				//INTERZONE MIXING
				seqData = interZoneMixInstantSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rIntZonMix ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rIntZonMix ) += AvgData( timeCoolMax );
				grandTotalRow( cSensInst ) += AvgData( timeCoolMax );

				seqData = interZoneMixLatentSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cLatent, rIntZonMix ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rIntZonMix ) += AvgData( timeCoolMax );
				grandTotalRow( cLatent ) += AvgData( timeCoolMax );

				//FENESTRATION CONDUCTION
				seqData = feneCondInstantSeq( CoolDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rFeneCond ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rFeneCond ) += AvgData( timeCoolMax );
				grandTotalRow( cSensInst ) += AvgData( timeCoolMax );

				//FENESTRATION SOLAR
				//      seqData = feneSolarInstantSeq(iZone,:,CoolDesSelected) * powerConversion
				//      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
				//      tableBody(rFeneSolr,cSensInst)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
				//      totalColumn(rFeneSolr) = totalColumn(rFeneSolr) + AvgData(timeCoolMax)
				//      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeCoolMax)

				seqData = feneSolarDelaySeqCool( _ ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensDelay, rFeneSolr ) = RealToStr( AvgData( timeCoolMax ), 2 );
				totalColumn( rFeneSolr ) += AvgData( timeCoolMax );
				grandTotalRow( cSensDelay ) += AvgData( timeCoolMax );

				//opaque surfaces - must combine individual surfaces by class and other side conditions
				delayOpaque = 0.0;
				for ( int kSurf = zd.SurfaceFirst; kSurf <= zd.SurfaceLast; ++kSurf ) {
					if ( ! Surface( kSurf ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

					curExtBoundCond = Surface( kSurf ).ExtBoundCond;
					//if exterior is other side coefficients using ground preprocessor terms then
					//set it to ground instead of other side coefficients
					if ( curExtBoundCond == OtherSideCoefNoCalcExt || curExtBoundCond == OtherSideCoefCalcExt ) {
						if ( has_prefixi( OSC( Surface( kSurf ).OSCPtr ).Name, "surfPropOthSdCoef" ) ) {
							curExtBoundCond = Ground;
						}
					}
					seqData = surfDelaySeqCool( _, kSurf );
					MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
					singleSurfDelay = AvgData( timeCoolMax ) * powerConversion;
					{ auto const SELECT_CASE_var( Surface( kSurf ).Class );
						if ( SELECT_CASE_var == SurfaceClass_Wall ) {
							{ auto const SELECT_CASE_var1( curExtBoundCond );
								if ( SELECT_CASE_var1 == ExternalEnvironment ) {
									delayOpaque( rExtWall ) += singleSurfDelay;
								} else if ( ( SELECT_CASE_var1 == Ground ) || ( SELECT_CASE_var1 == GroundFCfactorMethod ) ) {
									delayOpaque( rGrdWall ) += singleSurfDelay;
								} else if ( ( SELECT_CASE_var1 == OtherSideCoefNoCalcExt ) || ( SELECT_CASE_var1 == OtherSideCoefCalcExt ) || ( SELECT_CASE_var1 == OtherSideCondModeledExt ) ) {
									delayOpaque( rOtherWall ) += singleSurfDelay;
								} else { //interzone
									delayOpaque( rIntZonWall ) += singleSurfDelay;
								}}
						} else if ( SELECT_CASE_var == SurfaceClass_Floor ) {
							{ auto const SELECT_CASE_var1( curExtBoundCond );
								if ( SELECT_CASE_var1 == ExternalEnvironment ) {
									delayOpaque( rExtFlr ) += singleSurfDelay;
								} else if ( ( SELECT_CASE_var1 == Ground ) || ( SELECT_CASE_var1 == GroundFCfactorMethod ) ) {
									delayOpaque( rGrdFlr ) += singleSurfDelay;
								} else if ( ( SELECT_CASE_var1 == OtherSideCoefNoCalcExt ) || ( SELECT_CASE_var1 == OtherSideCoefCalcExt ) || ( SELECT_CASE_var1 == OtherSideCondModeledExt ) ) {
									delayOpaque( rOtherFlr ) += singleSurfDelay;
								} else { //interzone
									delayOpaque( rIntZonFlr ) += singleSurfDelay;
								}}
						} else if ( SELECT_CASE_var == SurfaceClass_Roof ) {
							{ auto const SELECT_CASE_var1( curExtBoundCond );
								if ( SELECT_CASE_var1 == ExternalEnvironment ) {
									delayOpaque( rRoof ) += singleSurfDelay;
								} else if ( ( SELECT_CASE_var1 == Ground ) || ( SELECT_CASE_var1 == GroundFCfactorMethod ) || ( SELECT_CASE_var1 == OtherSideCoefNoCalcExt ) || ( SELECT_CASE_var1 == OtherSideCoefCalcExt ) || ( SELECT_CASE_var1 == OtherSideCondModeledExt ) ) {
									delayOpaque( rOtherRoof ) += singleSurfDelay;
								} else { //interzone
									delayOpaque( rIntZonCeil ) += singleSurfDelay;
								}}
						} else if ( SELECT_CASE_var == SurfaceClass_Door ) {
							delayOpaque( rOpqDoor ) += singleSurfDelay;
						}}
				}
			}
			for ( int k = rRoof; k <= rOtherFlr; ++k ) {
				tableBody( cSensDelay, k ) = RealToStr( delayOpaque( k ), 2 );
				totalColumn( k ) += delayOpaque( k );
				grandTotalRow( cSensDelay ) += delayOpaque( k );
			}
			tableBody( cSensDelay, rOpqDoor ) = RealToStr( delayOpaque( rOpqDoor ), 2 );
			totalColumn( rOpqDoor ) += delayOpaque( rOpqDoor );
			grandTotalRow( cSensDelay ) += delayOpaque( rOpqDoor );

			//GRAND TOTAL ROW
			totalGrandTotal = 0.0;
			for ( int k = 1; k <= cLatent; ++k ) {
				tableBody( k, rGrdTot ) = RealToStr( grandTotalRow( k ), 2 );
				totalGrandTotal += grandTotalRow( k );
			}
			tableBody( cTotal, rGrdTot ) = RealToStr( totalGrandTotal, 2 );

			//TOTAL COLUMN AND PERCENT COLUMN
			for ( int k = 1; k <= rOpqDoor; ++k ) { //to last row before total
				tableBody( cTotal, k ) = RealToStr( totalColumn( k ), 2 );
				if ( totalGrandTotal != 0.0 ) {
					tableBody( cPerc, k ) = RealToStr( 100 * totalColumn( k ) / totalGrandTotal, 2 );
				}
			}

			WriteSubtitle( "Estimated Cooling Peak Load Components" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "ZoneComponentLoadSummary", Zone( iZone ).Name, "Estimated Cooling Peak Load Components" );
			}

			//---- Cooling Peak Conditions

			rowHead.allocate( 10 );
			columnHead.allocate( 1 );
			columnWidth.allocate( 1 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 1, 10 );

			columnHead( 1 ) = "Value";
			if ( unitsStyle != unitsStyleInchPound ) {
				rowHead( 1 ) = "Time of Peak Load";
				rowHead( 2 ) = "Outside  Dry Bulb Temperature [C]";
				rowHead( 3 ) = "Outside  Wet Bulb Temperature [C]";
				rowHead( 4 ) = "Outside Humidity Ratio at Peak [kgWater/kgAir]";
				rowHead( 5 ) = "Zone Dry Bulb Temperature [C]";
				rowHead( 6 ) = "Zone Relative Humdity [%]";
				rowHead( 7 ) = "Zone Humidity Ratio at Peak [kgWater/kgAir]";
				rowHead( 8 ) = "Peak Design Sensible Load [W]";
				rowHead( 9 ) = "Estimated Instant + Delayed Sensible Load [W]";
				rowHead( 10 ) = "Difference [W]";
			} else {
				rowHead( 1 ) = "Time of Peak Load";
				rowHead( 2 ) = "Outside  Dry Bulb Temperature [F]";
				rowHead( 3 ) = "Outside  Wet Bulb Temperature [F]";
				rowHead( 4 ) = "Outside Humidity Ratio at Peak [lbWater/lbAir]";
				rowHead( 5 ) = "Zone Dry Bulb Temperature [F]";
				rowHead( 6 ) = "Zone Relative Humdity [%]";
				rowHead( 7 ) = "Zone Humidity Ratio at Peak [lbWater/lbAir]";
				rowHead( 8 ) = "Peak Design Sensible Load [Btu/h]";
				rowHead( 9 ) = "Estimated Instant + Delayed Sensible Load [Btu/h]";
				rowHead( 10 ) = "Difference [Btu/h]";
			}

			tableBody = "";

			if ( timeCoolMax != 0 ) {

				//Time of Peak Load
				tableBody( 1, 1 ) = CoolPeakDateHrMin( iZone );

				//Outside  Dry Bulb Temperature
				tableBody( 1, 2 ) = RealToStr( ConvertIP( tempConvIndx, CalcFinalZoneSizing( iZone ).CoolOutTempSeq( timeCoolMax ) ), 2 );

				//Outside  Wet Bulb Temperature
				//use standard sea level air pressure because air pressure is not tracked with sizing data
				if ( CalcFinalZoneSizing( iZone ).CoolOutHumRatSeq( timeCoolMax ) < 1.0 && CalcFinalZoneSizing( iZone ).CoolOutHumRatSeq( timeCoolMax ) > 0.0 ) {
					tableBody( 1, 3 ) = RealToStr( ConvertIP( tempConvIndx, PsyTwbFnTdbWPb( CalcFinalZoneSizing( iZone ).CoolOutTempSeq( timeCoolMax ), CalcFinalZoneSizing( iZone ).CoolOutHumRatSeq( timeCoolMax ), 101325.0 ) ), 2 );
				}

				//Outside Humidity Ratio at Peak
				tableBody( 1, 4 ) = RealToStr( CalcFinalZoneSizing( iZone ).CoolOutHumRatSeq( timeCoolMax ), 5 );

				//Zone Dry Bulb Temperature
				tableBody( 1, 5 ) = RealToStr( ConvertIP( tempConvIndx, CalcFinalZoneSizing( iZone ).CoolZoneTempSeq( timeCoolMax ) ), 2 );

				//Zone Relative Humdity
				//use standard sea level air pressure because air pressure is not tracked with sizing data
				tableBody( 1, 6 ) = RealToStr( 100 * PsyRhFnTdbWPb( CalcFinalZoneSizing( iZone ).CoolZoneTempSeq( timeCoolMax ), CalcFinalZoneSizing( iZone ).CoolZoneHumRatSeq( timeCoolMax ), 101325.0 ), 2 );

				//Zone Humidity Ratio at Peak
				tableBody( 1, 7 ) = RealToStr( CalcFinalZoneSizing( iZone ).CoolZoneHumRatSeq( timeCoolMax ), 5 );

			}

			//Peak Design Sensible Load
			tableBody( 1, 8 ) = RealToStr( ( CalcFinalZoneSizing( iZone ).DesCoolLoad / mult ) * powerConversion, 2 );

			//Estimated Instant + Delayed Sensible Load
			tableBody( 1, 9 ) = RealToStr( grandTotalRow( cSensInst ) + grandTotalRow( cSensDelay ), 2 );

			//Difference
			tableBody( 1, 10 ) = RealToStr( ( CalcFinalZoneSizing( iZone ).DesCoolLoad / mult ) * powerConversion - ( grandTotalRow( cSensInst ) + grandTotalRow( cSensDelay ) ), 2 );

			WriteSubtitle( "Cooling Peak Conditions" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "ZoneComponentLoadSummary", Zone( iZone ).Name, "Cooling Peak Conditions" );
			}

			//    !
			//    !---- Radiant to Convective Decay Curves for Cooling
			//    !
			//    numObj = 0
			//    !determine the number of surfaces to include
			//    DO kSurf = 1, TotSurfaces
			//      ZoneNum = Surface(kSurf)%Zone
			//      IF (ZoneNum .NE. iZone) CYCLE
			//      IF (ZoneNum .EQ. 0) CYCLE
			//      IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE
			//      numObj = numObj + 1
			//    END DO
			//    ALLOCATE(rowHead(numObj))
			//    ALLOCATE(columnHead(16))
			//    ALLOCATE(columnWidth(16))
			//    columnWidth = 14 !array assignment - same for all columns
			//    ALLOCATE(tableBody(numObj,16))
			//    columnHead(1) = 'Time 1'
			//    columnHead(2) = 'Time 2'
			//    columnHead(3) = 'Time 3'
			//    columnHead(4) = 'Time 4'
			//    columnHead(5) = 'Time 5'
			//    columnHead(6) = 'Time 6'
			//    columnHead(7) = 'Time 7'
			//    columnHead(8) = 'Time 8'
			//    columnHead(9) = 'Time 9'
			//    columnHead(10) = 'Time 10'
			//    columnHead(11) = 'Time 11'
			//    columnHead(12) = 'Time 12'
			//    columnHead(13) = 'Time 13'
			//    columnHead(14) = 'Time 14'
			//    columnHead(15) = 'Time 15'
			//    columnHead(16) = 'Time 16'
			//    tableBody = ''
			//    objCount = 0
			//    DO kSurf = 1, TotSurfaces
			//      ZoneNum = Surface(kSurf)%Zone
			//      IF (ZoneNum .NE. iZone) CYCLE
			//      IF (ZoneNum .EQ. 0) CYCLE
			//      IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE
			//      objCount = objCount + 1
			//      rowHead(objCount) = TRIM(Surface(kSurf)%Name)
			//      DO jTime = 1, 16
			//        tableBody(objCount,jTime) = TRIM(RealToStr(decayCurveCool(kSurf,jTime),3))
			//      END DO
			//    END DO
			//    CALL WriteSubtitle('Radiant to Convective Decay Curves for Cooling')
			//    CALL WriteTable(tableBody,rowHead,columnHead,columnWidth)
			//    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
			//                                        'ZoneComponentLoadDetail',&
			//                                        TRIM(Zone(iZone)%Name),&
			//                                        'Radiant to Convective Decay Curves for Cooling')
			//    DEALLOCATE(columnHead)
			//    DEALLOCATE(rowHead)
			//    DEALLOCATE(columnWidth)
			//    DEALLOCATE(tableBody)

			// Put the decay curve into the EIO file
			if ( ShowDecayCurvesInEIO ) {
				for ( int kSurf = zd.SurfaceFirst; kSurf <= zd.SurfaceLast; ++kSurf ) {
					{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, "(4A)", flags ) << "Radiant to Convective Decay Curves for Cooling," << Zone( iZone ).Name << ',' << Surface( kSurf ).Name; }
					for ( int jTime = 1; jTime <= min( NumOfTimeStepInHour * 24, 36 ); ++jTime ) {
						{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, "(A,F6.3)", flags ) << ',' << decayCurveCool( jTime, kSurf ); }
					}
					{ IOFlags flags; flags.ADVANCE( "YES" ); gio::write( OutputFileInits, "()", flags ); } //put a line feed at the end of the line
				}
			}

			//---- Heating Peak Load Components Sub-Table
			rowHead.allocate( rGrdTot );
			columnHead.allocate( cPerc );
			columnWidth.dimension( cPerc, 14 ); //array assignment - same for all columns
			tableBody.allocate( cPerc, rGrdTot );

			if ( unitsStyle != unitsStyleInchPound ) {
				columnHead( cSensInst ) = "Sensible - Instant [W]";
				columnHead( cSensDelay ) = "Sensible - Delayed [W]";
				columnHead( cSensRA ) = "Sensible - Return Air [W]";
				columnHead( cLatent ) = "Latent [W]";
				columnHead( cTotal ) = "Total [W]";
				columnHead( cPerc ) = "%Grand Total";
			} else {
				columnHead( cSensInst ) = "Sensible - Instant [Btu/h]";
				columnHead( cSensDelay ) = "Sensible - Delayed [Btu/h]";
				columnHead( cSensRA ) = "Sensible - Return Air [Btu/h]";
				columnHead( cLatent ) = "Latent [Btu/h]";
				columnHead( cTotal ) = "Total [Btu/h]";
				columnHead( cPerc ) = "%Grand Total";
			}

			//internal gains
			rowHead( rPeople ) = "People";
			rowHead( rLights ) = "Lights";
			rowHead( rEquip ) = "Equipment";
			rowHead( rRefrig ) = "Refrigeration Equipment";
			rowHead( rWaterUse ) = "Water Use Equipment";
			rowHead( rPowerGen ) = "Power Generation Equipment";
			rowHead( rHvacLoss ) = "HVAC Equipment Losses";
			rowHead( rRefrig ) = "Refrigeration";
			//misc
			rowHead( rDOAS ) = "DOAS";
			rowHead( rInfil ) = "Infiltration";
			rowHead( rZoneVent ) = "Zone Ventilation";
			rowHead( rIntZonMix ) = "Interzone Mixing";
			//opaque surfaces
			rowHead( rRoof ) = "Roof";
			rowHead( rIntZonCeil ) = "Interzone Ceiling";
			rowHead( rOtherRoof ) = "Other Roof";
			rowHead( rExtWall ) = "Exterior Wall";
			rowHead( rIntZonWall ) = "Interzone Wall";
			rowHead( rGrdWall ) = "Ground Contact Wall";
			rowHead( rOtherWall ) = "Other Wall";
			rowHead( rExtFlr ) = "Exterior Floor";
			rowHead( rIntZonFlr ) = "Interzone Floor";
			rowHead( rGrdFlr ) = "Ground Contact Floor";
			rowHead( rOtherFlr ) = "Other Floor";
			//subsurfaces
			rowHead( rFeneCond ) = "Fenestration Conduction";
			rowHead( rFeneSolr ) = "Fenestration Solar";
			rowHead( rOpqDoor ) = "Opaque Door";
			rowHead( rGrdTot ) = "Grand Total";

			tableBody = "";
			totalColumn = 0.0;
			percentColumn = 0.0;
			grandTotalRow = 0.0;

			int HeatDesSelected = CalcFinalZoneSizing( iZone ).HeatDDNum;
			int timeHeatMax = CalcFinalZoneSizing( iZone ).TimeStepNumAtHeatMax;
			if ( HeatDesSelected != 0 && timeHeatMax != 0 ) {

				//PEOPLE
				seqData = peopleInstantSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rPeople ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rPeople ) += AvgData( timeHeatMax );
				grandTotalRow( cSensInst ) += AvgData( timeHeatMax );

				seqData = peopleLatentSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cLatent, rPeople ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rPeople ) += AvgData( timeHeatMax );
				grandTotalRow( cLatent ) += AvgData( timeHeatMax );

				seqData = peopleDelaySeqHeat( _ ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensDelay, rPeople ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rPeople ) += AvgData( timeHeatMax );
				grandTotalRow( cSensDelay ) += AvgData( timeHeatMax );

				//LIGHTS
				seqData = lightInstantSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rLights ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rLights ) += AvgData( timeHeatMax );
				grandTotalRow( cSensInst ) += AvgData( timeHeatMax );

				seqData = lightRetAirSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensRA, rLights ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rLights ) += AvgData( timeHeatMax );
				grandTotalRow( cSensRA ) += AvgData( timeHeatMax );

				seqData = lightDelaySeqHeat( _ ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensDelay, rLights ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rLights ) += AvgData( timeHeatMax );
				grandTotalRow( cSensDelay ) += AvgData( timeHeatMax );

				//EQUIPMENT
				seqData = equipInstantSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rEquip ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rEquip ) += AvgData( timeHeatMax );
				grandTotalRow( cSensInst ) += AvgData( timeHeatMax );

				seqData = equipLatentSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cLatent, rEquip ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rEquip ) += AvgData( timeHeatMax );
				grandTotalRow( cLatent ) += AvgData( timeHeatMax );

				seqData = equipDelaySeqHeat( _ ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensDelay, rEquip ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rEquip ) += AvgData( timeHeatMax );
				grandTotalRow( cSensDelay ) += AvgData( timeHeatMax );

				//REFRIGERATION EQUIPMENT
				seqData = refrigInstantSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rRefrig ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rRefrig ) += AvgData( timeHeatMax );
				grandTotalRow( cSensInst ) += AvgData( timeHeatMax );

				seqData = refrigRetAirSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensRA, rRefrig ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rRefrig ) += AvgData( timeHeatMax );
				grandTotalRow( cSensRA ) += AvgData( timeHeatMax );

				seqData = refrigLatentSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cLatent, rRefrig ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rRefrig ) += AvgData( timeHeatMax );
				grandTotalRow( cLatent ) += AvgData( timeHeatMax );

				//WATER USE EQUIPMENT
				seqData = waterUseInstantSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rWaterUse ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rWaterUse ) += AvgData( timeHeatMax );
				grandTotalRow( cSensInst ) += AvgData( timeHeatMax );

				seqData = waterUseLatentSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cLatent, rWaterUse ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rWaterUse ) += AvgData( timeHeatMax );
				grandTotalRow( cLatent ) += AvgData( timeHeatMax );

				//HVAC EQUIPMENT LOSSES
				seqData = hvacLossInstantSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rHvacLoss ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rHvacLoss ) += AvgData( timeHeatMax );
				grandTotalRow( cSensInst ) += AvgData( timeHeatMax );

				seqData = hvacLossDelaySeqHeat( _ ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensDelay, rHvacLoss ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rHvacLoss ) += AvgData( timeHeatMax );
				grandTotalRow( cSensDelay ) += AvgData( timeHeatMax );

				//POWER GENERATION EQUIPMENT
				seqData = powerGenInstantSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rPowerGen ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rPowerGen ) += AvgData( timeHeatMax );
				grandTotalRow( cSensInst ) += AvgData( timeHeatMax );

				seqData = powerGenDelaySeqHeat( _ ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensDelay, rPowerGen ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rPowerGen ) += AvgData( timeHeatMax );
				grandTotalRow( cSensDelay ) += AvgData( timeHeatMax );

				//DOAS
				tableBody( cSensInst, rDOAS ) = RealToStr( CalcZoneSizing( HeatDesSelected, iZone ).DOASHeatAddSeq( timeHeatMax ), 2 );
				totalColumn( rDOAS ) += CalcZoneSizing( HeatDesSelected, iZone ).DOASHeatAddSeq( timeHeatMax );
				grandTotalRow( cSensDelay ) += CalcZoneSizing( HeatDesSelected, iZone ).DOASHeatAddSeq( timeHeatMax );

				tableBody( cLatent, rDOAS ) = RealToStr( CalcZoneSizing( HeatDesSelected, iZone ).DOASLatAddSeq( timeHeatMax ), 2 );
				totalColumn( rDOAS ) += CalcZoneSizing( HeatDesSelected, iZone ).DOASLatAddSeq( timeHeatMax );
				grandTotalRow( cLatent ) += CalcZoneSizing( HeatDesSelected, iZone ).DOASLatAddSeq( timeHeatMax );

				//INFILTRATION
				seqData = infilInstantSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rInfil ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rInfil ) += AvgData( timeHeatMax );
				grandTotalRow( cSensInst ) += AvgData( timeHeatMax );

				seqData = infilLatentSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cLatent, rInfil ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rInfil ) += AvgData( timeHeatMax );
				grandTotalRow( cLatent ) += AvgData( timeHeatMax );

				//ZONE VENTILATION
				seqData = zoneVentInstantSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rZoneVent ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rZoneVent ) += AvgData( timeHeatMax );
				grandTotalRow( cSensInst ) += AvgData( timeHeatMax );

				seqData = zoneVentLatentSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cLatent, rZoneVent ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rZoneVent ) += AvgData( timeHeatMax );
				grandTotalRow( cLatent ) += AvgData( timeHeatMax );

				//INTERZONE MIXING
				seqData = interZoneMixInstantSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rIntZonMix ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rIntZonMix ) += AvgData( timeHeatMax );
				grandTotalRow( cSensInst ) += AvgData( timeHeatMax );

				seqData = interZoneMixLatentSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cLatent, rIntZonMix ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rIntZonMix ) += AvgData( timeHeatMax );
				grandTotalRow( cLatent ) += AvgData( timeHeatMax );

				//FENESTRATION CONDUCTION
				seqData = feneCondInstantSeq( HeatDesSelected, _, iZone ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensInst, rFeneCond ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rFeneCond ) += AvgData( timeHeatMax );
				grandTotalRow( cSensInst ) += AvgData( timeHeatMax );

				//FENESTRATION SOLAR
				//      seqData = feneSolarInstantSeq(iZone,:,HeatDesSelected) * powerConversion
				//      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
				//      tableBody(rFeneSolr,cSensInst)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
				//      totalColumn(rFeneSolr) = totalColumn(rFeneSolr) + AvgData(timeHeatMax)
				//      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeHeatMax)

				seqData = feneSolarDelaySeqHeat( _ ) * powerConversion;
				MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				tableBody( cSensDelay, rFeneSolr ) = RealToStr( AvgData( timeHeatMax ), 2 );
				totalColumn( rFeneSolr ) += AvgData( timeHeatMax );
				grandTotalRow( cSensDelay ) += AvgData( timeHeatMax );

				//opaque surfaces - must combine individual surfaces by class and other side conditions
				delayOpaque = 0.0;
				for ( int kSurf = zd.SurfaceFirst; kSurf <= zd.SurfaceLast; ++kSurf ) {
					if ( ! Surface( kSurf ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

					curExtBoundCond = Surface( kSurf ).ExtBoundCond;
					//if exterior is other side coefficients using ground preprocessor terms then
					//set it to ground instead of other side coefficients
					if ( curExtBoundCond == OtherSideCoefNoCalcExt || curExtBoundCond == OtherSideCoefCalcExt ) {
						if ( has_prefixi( OSC( Surface( kSurf ).OSCPtr ).Name, "surfPropOthSdCoef" ) ) {
							curExtBoundCond = Ground;
						}
					}
					seqData = surfDelaySeqHeat( _, kSurf );
					MovingAvg( seqData, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
					singleSurfDelay = AvgData( timeHeatMax ) * powerConversion;
					{ auto const SELECT_CASE_var( Surface( kSurf ).Class );
						if ( SELECT_CASE_var == SurfaceClass_Wall ) {
							{ auto const SELECT_CASE_var1( curExtBoundCond );
								if ( SELECT_CASE_var1 == ExternalEnvironment ) {
									delayOpaque( rExtWall ) += singleSurfDelay;
								} else if ( ( SELECT_CASE_var1 == Ground ) || ( SELECT_CASE_var1 == GroundFCfactorMethod ) ) {
									delayOpaque( rGrdWall ) += singleSurfDelay;
								} else if ( ( SELECT_CASE_var1 == OtherSideCoefNoCalcExt ) || ( SELECT_CASE_var1 == OtherSideCoefCalcExt ) || ( SELECT_CASE_var1 == OtherSideCondModeledExt ) ) {
									delayOpaque( rOtherWall ) += singleSurfDelay;
								} else { //interzone
									delayOpaque( rIntZonWall ) += singleSurfDelay;
								}}
						} else if ( SELECT_CASE_var == SurfaceClass_Floor ) {
							{ auto const SELECT_CASE_var1( curExtBoundCond );
								if ( SELECT_CASE_var1 == ExternalEnvironment ) {
									delayOpaque( rExtFlr ) += singleSurfDelay;
								} else if ( ( SELECT_CASE_var1 == Ground ) || ( SELECT_CASE_var1 == GroundFCfactorMethod ) ) {
									delayOpaque( rGrdFlr ) += singleSurfDelay;
								} else if ( ( SELECT_CASE_var1 == OtherSideCoefNoCalcExt ) || ( SELECT_CASE_var1 == OtherSideCoefCalcExt ) || ( SELECT_CASE_var1 == OtherSideCondModeledExt ) ) {
									delayOpaque( rOtherFlr ) += singleSurfDelay;
								} else { //interzone
									delayOpaque( rIntZonFlr ) += singleSurfDelay;
								}}
						} else if ( SELECT_CASE_var == SurfaceClass_Roof ) {
							{ auto const SELECT_CASE_var1( curExtBoundCond );
								if ( SELECT_CASE_var1 == ExternalEnvironment ) {
									delayOpaque( rRoof ) += singleSurfDelay;
								} else if ( ( SELECT_CASE_var1 == Ground ) || ( SELECT_CASE_var1 == GroundFCfactorMethod ) || ( SELECT_CASE_var1 == OtherSideCoefNoCalcExt ) || ( SELECT_CASE_var1 == OtherSideCoefCalcExt ) || ( SELECT_CASE_var1 == OtherSideCondModeledExt ) ) {
									delayOpaque( rOtherRoof ) += singleSurfDelay;
								} else { //interzone
									delayOpaque( rIntZonCeil ) += singleSurfDelay;
								}}
						} else if ( SELECT_CASE_var == SurfaceClass_Door ) {
							delayOpaque( rOpqDoor ) += singleSurfDelay;
						}}
				}
			}
			for ( int k = rRoof; k <= rOtherFlr; ++k ) {
				tableBody( cSensDelay, k ) = RealToStr( delayOpaque( k ), 2 );
				totalColumn( k ) += delayOpaque( k );
				grandTotalRow( cSensDelay ) += delayOpaque( k );
			}
			tableBody( cSensDelay, rOpqDoor ) = RealToStr( delayOpaque( rOpqDoor ), 2 );
			totalColumn( rOpqDoor ) += delayOpaque( rOpqDoor );
			grandTotalRow( cSensDelay ) += delayOpaque( rOpqDoor );

			//GRAND TOTAL ROW
			totalGrandTotal = 0.0;
			for ( int k = 1; k <= cLatent; ++k ) {
				tableBody( k, rGrdTot ) = RealToStr( grandTotalRow( k ), 2 );
				totalGrandTotal += grandTotalRow( k );
			}
			tableBody( cTotal, rGrdTot ) = RealToStr( totalGrandTotal, 2 );

			//TOTAL COLUMN AND PERCENT COLUMN
			for ( int k = 1; k <= rOpqDoor; ++k ) { //to last row before total
				tableBody( cTotal, k ) = RealToStr( totalColumn( k ), 2 );
				if ( totalGrandTotal != 0.0 ) {
					tableBody( cPerc, k ) = RealToStr( 100 * totalColumn( k ) / totalGrandTotal, 2 );
				}
			}

			WriteSubtitle( "Estimated Heating Peak Load Components" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "ZoneComponentLoadSummary", Zone( iZone ).Name, "Estimated Heating Peak Load Components" );
			}

			//---- Heating Peak Conditions Sub-Table

			rowHead.allocate( 10 );
			columnHead.allocate( 1 );
			columnWidth.allocate( 1 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 1, 10 );

			columnHead( 1 ) = "Value";
			if ( unitsStyle != unitsStyleInchPound ) {
				rowHead( 1 ) = "Time of Peak Load";
				rowHead( 2 ) = "Outside  Dry Bulb Temperature [C]";
				rowHead( 3 ) = "Outside  Wet Bulb Temperature [C]";
				rowHead( 4 ) = "Outside Humidity Ratio at Peak [kgWater/kgAir]";
				rowHead( 5 ) = "Zone Dry Bulb Temperature [C]";
				rowHead( 6 ) = "Zone Relative Humdity [%]";
				rowHead( 7 ) = "Zone Humidity Ratio at Peak [kgWater/kgAir]";
				rowHead( 8 ) = "Peak Design Sensible Load [W]";
				rowHead( 9 ) = "Estimated Instant + Delayed Sensible Load [W]";
				rowHead( 10 ) = "Difference [W]";
			} else {
				rowHead( 1 ) = "Time of Peak Load";
				rowHead( 2 ) = "Outside  Dry Bulb Temperature [F]";
				rowHead( 3 ) = "Outside  Wet Bulb Temperature [F]";
				rowHead( 4 ) = "Outside Humidity Ratio at Peak [lbWater/lbAir]";
				rowHead( 5 ) = "Zone Dry Bulb Temperature [F]";
				rowHead( 6 ) = "Zone Relative Humdity [%]";
				rowHead( 7 ) = "Zone Humidity Ratio at Peak [lbWater/lbAir]";
				rowHead( 8 ) = "Peak Design Sensible Load [Btu/h]";
				rowHead( 9 ) = "Estimated Instant + Delayed Sensible Load [Btu/h]";
				rowHead( 10 ) = "Difference [Btu/h]";
			}

			tableBody = "";

			if ( timeHeatMax != 0 ) {
				//Time of Peak Load
				tableBody( 1, 1 ) = HeatPeakDateHrMin( iZone );

				//Outside  Dry Bulb Temperature
				tableBody( 1, 2 ) = RealToStr( ConvertIP( tempConvIndx, CalcFinalZoneSizing( iZone ).HeatOutTempSeq( timeHeatMax ) ), 2 );

				//Outside  Wet Bulb Temperature
				//use standard sea level air pressure because air pressure is not tracked with sizing data
				if ( CalcFinalZoneSizing( iZone ).HeatOutHumRatSeq( timeHeatMax ) < 1.0 && CalcFinalZoneSizing( iZone ).HeatOutHumRatSeq( timeHeatMax ) > 0.0 ) {
					tableBody( 1, 3 ) = RealToStr( ConvertIP( tempConvIndx, PsyTwbFnTdbWPb( CalcFinalZoneSizing( iZone ).HeatOutTempSeq( timeHeatMax ), CalcFinalZoneSizing( iZone ).HeatOutHumRatSeq( timeHeatMax ), 101325.0 ) ), 2 );
				}

				//Humidity Ratio at Peak
				tableBody( 1, 4 ) = RealToStr( CalcFinalZoneSizing( iZone ).HeatOutHumRatSeq( timeHeatMax ), 5 );

				//Zone Dry Bulb Temperature
				tableBody( 1, 5 ) = RealToStr( ConvertIP( tempConvIndx, CalcFinalZoneSizing( iZone ).HeatZoneTempSeq( timeHeatMax ) ), 2 );

				//Zone Relative Temperature
				//use standard sea level air pressure because air pressure is not tracked with sizing data
				tableBody( 1, 6 ) = RealToStr( 100 * PsyRhFnTdbWPb( CalcFinalZoneSizing( iZone ).HeatZoneTempSeq( timeHeatMax ), CalcFinalZoneSizing( iZone ).HeatZoneHumRatSeq( timeHeatMax ), 101325.0 ), 2 );

				//Zone Relative Humdity
				tableBody( 1, 7 ) = RealToStr( CalcFinalZoneSizing( iZone ).HeatZoneHumRatSeq( timeHeatMax ), 5 );

			}

			//Peak Design Sensible Load
			tableBody( 1, 8 ) = RealToStr( ( -CalcFinalZoneSizing( iZone ).DesHeatLoad / mult ) * powerConversion, 2 ); //change sign

			//Estimated Instant + Delayed Sensible Load
			tableBody( 1, 9 ) = RealToStr( grandTotalRow( cSensInst ) + grandTotalRow( cSensDelay ), 2 );

			//Difference
			tableBody( 1, 10 ) = RealToStr( ( -CalcFinalZoneSizing( iZone ).DesHeatLoad / mult ) * powerConversion - ( grandTotalRow( cSensInst ) + grandTotalRow( cSensDelay ) ), 2 );

			WriteSubtitle( "Heating Peak Conditions" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "ZoneComponentLoadSummary", Zone( iZone ).Name, "Heating Peak Conditions" );
			}

			//    !
			//    !---- Radiant to Convective Decay Curves for Heating
			//    !
			//    numObj = 0
			//    !determine the number of surfaces to include
			//    DO kSurf = 1, TotSurfaces
			//      ZoneNum = Surface(kSurf)%Zone
			//      IF (ZoneNum .NE. iZone) CYCLE
			//      IF (ZoneNum .EQ. 0) CYCLE
			//      IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE
			//      numObj = numObj + 1
			//    END DO
			//    ALLOCATE(rowHead(numObj))
			//    ALLOCATE(columnHead(16))
			//    ALLOCATE(columnWidth(16))
			//    columnWidth = 14 !array assignment - same for all columns
			//    ALLOCATE(tableBody(numObj,16))
			//    columnHead(1) = 'Time 1'
			//    columnHead(2) = 'Time 2'
			//    columnHead(3) = 'Time 3'
			//    columnHead(4) = 'Time 4'
			//    columnHead(5) = 'Time 5'
			//    columnHead(6) = 'Time 6'
			//    columnHead(7) = 'Time 7'
			//    columnHead(8) = 'Time 8'
			//    columnHead(9) = 'Time 9'
			//    columnHead(10) = 'Time 10'
			//    columnHead(11) = 'Time 11'
			//    columnHead(12) = 'Time 12'
			//    columnHead(13) = 'Time 13'
			//    columnHead(14) = 'Time 14'
			//    columnHead(15) = 'Time 15'
			//    columnHead(16) = 'Time 16'
			//    tableBody = ''
			//    objCount = 0
			//    DO kSurf = 1, TotSurfaces
			//      ZoneNum = Surface(kSurf)%Zone
			//      IF (ZoneNum .NE. iZone) CYCLE
			//      IF (ZoneNum .EQ. 0) CYCLE
			//      IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE
			//      objCount = objCount + 1
			//      rowHead(objCount) = TRIM(Surface(kSurf)%Name)
			//      DO jTime = 1, 16
			//        tableBody(objCount,jTime) = TRIM(RealToStr(decayCurveHeat(kSurf,jTime),3))
			//      END DO
			//    END DO
			//    CALL WriteSubtitle('Radiant to Convective Decay Curves for Heating')
			//    CALL WriteTable(tableBody,rowHead,columnHead,columnWidth)
			//    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
			//                                        'ZoneComponentLoadDetail',&
			//                                        TRIM(Zone(iZone)%Name),&
			//                                        'Radiant to Convective Decay Curves for Heating')
			//    DEALLOCATE(columnHead)
			//    DEALLOCATE(rowHead)
			//    DEALLOCATE(columnWidth)
			//    DEALLOCATE(tableBody)

			// Put the decay curve into the EIO file
			if ( ShowDecayCurvesInEIO ) {
				for ( int kSurf = zd.SurfaceFirst; kSurf <= zd.SurfaceLast; ++kSurf ) {
					{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, "(4A)", flags ) << "Radiant to Convective Decay Curves for Heating," << Zone( iZone ).Name << ',' << Surface( kSurf ).Name; }
					for ( int jTime = 1; jTime <= min( NumOfTimeStepInHour * 24, 36 ); ++jTime ) {
						{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, "(A,F6.3)", flags ) << ',' << decayCurveHeat( jTime, kSurf ); }
					}
					{ IOFlags flags; flags.ADVANCE( "YES" ); gio::write( OutputFileInits, "()", flags ); } //put a line feed at the end of the line
				}
			} // if ShowDecayCurvesInEIO

		} // for iZone

		peopleRadIntoSurf.deallocate();
		equipRadIntoSurf.deallocate();
		hvacLossRadIntoSurf.deallocate();
		powerGenRadIntoSurf.deallocate();
		lightLWRadIntoSurf.deallocate();

		peopleDelaySeqHeat.deallocate();
		peopleDelaySeqCool.deallocate();
		lightDelaySeqHeat.deallocate();
		lightDelaySeqCool.deallocate();
		equipDelaySeqHeat.deallocate();
		equipDelaySeqCool.deallocate();
		hvacLossDelaySeqHeat.deallocate();
		hvacLossDelaySeqCool.deallocate();
		powerGenDelaySeqHeat.deallocate();
		powerGenDelaySeqCool.deallocate();
		feneSolarDelaySeqHeat.deallocate();
		feneSolarDelaySeqCool.deallocate();
		surfDelaySeqHeat.deallocate();
		surfDelaySeqCool.deallocate();

	} // WriteZoneLoadComponentTable()

	void
	WriteReportHeaders(
		std::string const & reportName,
		std::string const & objectName,
		int const averageOrSum
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Write the first few lines of each report with headers to the output
		//   file for tabular reports.
		// Using/Aliasing
		using DataStringGlobals::VerString;
		using DataHeatBalance::BuildingName;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		std::string const modifiedReportName( reportName + ( averageOrSum == isSum ? " per second" : "" ) );

		for ( int iStyle = 1; iStyle <= numStyles; ++iStyle ) {
			std::ostream & tbl_stream( *TabularOutputFile( iStyle ) );
			std::string const & curDel( del( iStyle ) );
			auto const style( TableStyle( iStyle ) );
			if ( ( style == tableStyleComma ) || ( style == tableStyleTab ) ) {
				tbl_stream << "----------------------------------------------------------------------------------------------------\n";
				tbl_stream << "REPORT:" << curDel << modifiedReportName << '\n';
				tbl_stream << "FOR:" << curDel << objectName << '\n';
			} else if ( style == tableStyleFixed ) {
				tbl_stream << "----------------------------------------------------------------------------------------------------\n";
				tbl_stream << "REPORT:      " << curDel << modifiedReportName << '\n';
				tbl_stream << "FOR:         " << curDel << objectName << '\n';
			} else if ( style == tableStyleHTML ) {
				tbl_stream << "<hr>\n";
				tbl_stream << "<p><a href=\"#toc\" style=\"float: right\">Table of Contents</a></p>\n";
				tbl_stream << "<a name=" << MakeAnchorName( reportName, objectName ) << "></a>\n";
				tbl_stream << "<p>Report:<b>" << curDel << modifiedReportName << "</b></p>\n";
				tbl_stream << "<p>For:<b>" << curDel << objectName << "</b></p>\n";
				tbl_stream << "<p>Timestamp: <b>" << std::setw( 4 ) << td( 1 ) << '-' << std::setfill( '0' ) << std::setw( 2 ) << td( 2 ) << '-' << std::setw( 2 ) << td( 3 ) << '\n';
				tbl_stream << "    " << std::setw( 2 ) << td( 5 ) << ':' << std::setw( 2 ) << td( 6 ) << ':' << std::setw( 2 ) << td( 7 ) << std::setfill( ' ' ) << "</b></p>\n";
			} else if ( style == tableStyleXML ) {
				if ( len( prevReportName ) != 0 ) {
					tbl_stream << "</" << prevReportName << ">\n"; //close the last element if it was used.
				}
				tbl_stream << "<" << ConvertToElementTag( modifiedReportName ) << ">\n";
				tbl_stream << "  <for>" << objectName << "</for>\n";
				prevReportName = ConvertToElementTag( modifiedReportName ); //save the name for next time
			}
		}
		//clear the active subtable name for the XML reporting
		activeSubTableName = "";
		//save the report name if the subtable name is not available during XML processing
		activeReportName = modifiedReportName;
		//save the "for" which is the object name in the report for HTML comment that contains the report, for, and subtable
		activeForName = objectName;
	}

	void
	WriteSubtitle( std::string const & subtitle )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   November 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Insert a subtitle into the current report

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iStyle;

		for ( iStyle = 1; iStyle <= numStyles; ++iStyle ) {
			auto const style( TableStyle( iStyle ) );
			if ( ( style == tableStyleComma ) || ( style == tableStyleTab ) || ( style == tableStyleFixed ) ) {
				std::ostream & tbl_stream( *TabularOutputFile( iStyle ) );
				tbl_stream << subtitle << "\n\n";
			} else if ( style == tableStyleHTML ) {
				std::ostream & tbl_stream( *TabularOutputFile( iStyle ) );
				tbl_stream << "<b>" << subtitle << "</b><br><br>\n";
				tbl_stream << "<!-- FullName:" << activeReportName << '_' << activeForName << '_' << subtitle << "-->\n";
			} else if ( style == tableStyleXML ) {
				// save the active subtable name for the XML reporting
				activeSubTableName = subtitle;
				// no other output is needed since WriteTable uses the subtable name for each record.
			}
		}
	}

	void
	WriteTextLine(
		std::string const & lineOfText,
		Optional_bool_const isBold
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   April 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Insert a subtitle into the current report

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iStyle;
		bool useBold;

		if ( present( isBold ) ) {
			useBold = isBold;
		} else {
			useBold = false;
		}

		for ( iStyle = 1; iStyle <= numStyles; ++iStyle ) {
			auto const style( TableStyle( iStyle ) );
			if ( ( style == tableStyleComma ) || ( style == tableStyleTab ) || ( style == tableStyleFixed ) ) {
				std::ostream & tbl_stream( *TabularOutputFile( iStyle ) );
				tbl_stream << lineOfText << '\n';
			} else if ( style == tableStyleHTML ) {
				std::ostream & tbl_stream( *TabularOutputFile( iStyle ) );
				if ( useBold ) {
					tbl_stream << "<b>" << lineOfText << "</b><br><br>\n";
				} else {
					tbl_stream << lineOfText << "<br>\n";
				}
			} else if ( style == tableStyleXML ) {
				std::ostream & tbl_stream( *TabularOutputFile( iStyle ) );
				if ( ! lineOfText.empty() ) {
					tbl_stream << "<note>" << lineOfText << "</note>\n";
				}
			}
		}
	}

	void
	WriteTable(
		Array2S_string const body, // row,column
		Array1S_string const rowLabels,
		Array1S_string const columnLabels,
		Array1S_int widthColumn,
		Optional_bool_const transposeXML,
		Optional_string_const footnoteText
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Output a table to the tabular output file in the selected
		//   style (comma, tab, space, html, xml).
		//   The widthColumn array is only used for fixed space formatted reports
		//   if columnLables contain a vertical bar '|', they are broken into multiple
		//   rows.  If they exceed the column width even after that and the format is
		//   fixed, they are further shortened.
		//   To include the currency symbol ($ by default but other symbols if the user
		//   has input it with Economics:CurrencyType) use the string ~~$~~ in the row
		//   headers, column headers, and body. For HTML files, the ASCII or UNICODE
		//   symbol for the currency will be included. For TXT files, the ASCII symbol
		//   will be used.

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const blank;

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array2D_string colLabelMulti;
		std::string workColumn;
		Array1D_string rowLabelTags;
		Array1D_string columnLabelTags;
		Array1D_string rowUnitStrings;
		Array1D_string columnUnitStrings;
		Array2D_string bodyEsc;

		int numColLabelRows;
		int maxNumColLabelRows;
		std::string::size_type widthRowLabel;
		std::string::size_type maxWidthRowLabel;

		int rowsBody;
		int colsBody;
		int colsColumnLabels;
		int colsWidthColumn;
		int rowsRowLabels;

		int iCol;
		int jRow;
		int colWidthLimit;
		std::string::size_type barLoc;

		std::string outputLine;
		std::string spaces;
		int iStyle;
		std::string curDel;
		std::string tagWithAttrib;
		std::string::size_type col1start;
		bool doTransposeXML;
		bool isTableBlank;
		bool isRecordBlank;

		if ( present( transposeXML ) ) {
			doTransposeXML = transposeXML;
		} else {
			doTransposeXML = false; //if not present assume that the XML table should not be transposed
		}
		// create blank string
		spaces = blank; // REPEAT(' ',1000)
		// get sizes of arrays
		rowsBody = isize( body, 2 );
		colsBody = isize( body, 1 );
		rowsRowLabels = isize( rowLabels );
		colsColumnLabels = isize( columnLabels );
		colsWidthColumn = isize( widthColumn );
		// check size of arrays for consistancy and if inconsistent use smaller value
		// and display warning
		if ( rowsBody != rowsRowLabels ) {
			ShowWarningError( "REPORT:TABLE Inconsistant number of rows." );
			rowsBody = min( rowsBody, rowsRowLabels );
			rowsRowLabels = rowsBody;
		}
		if ( ( colsBody != colsColumnLabels ) || ( colsBody != colsWidthColumn ) ) {
			ShowWarningError( "REPORT:TABLE Inconsistant number of columns." );
			colsBody = min( colsBody, min( colsColumnLabels, colsWidthColumn ) );
			colsWidthColumn = colsBody;
			colsColumnLabels = colsBody;
		}
		// create arrays to hold the XML tags
		rowLabelTags.allocate( rowsBody );
		columnLabelTags.allocate( colsBody );
		rowUnitStrings.allocate( rowsBody );
		columnUnitStrings.allocate( colsBody );
		bodyEsc.allocate( colsBody, rowsBody );
		// create new array to hold multiple line column lables
		colLabelMulti.allocate( colsColumnLabels, 50 );
		colLabelMulti = blank; //set array to blank
		numColLabelRows = 0; //default value
		maxNumColLabelRows = 0;
		for ( iStyle = 1; iStyle <= numStyles; ++iStyle ) {
			std::ostream & tbl_stream( *TabularOutputFile( iStyle ) );
			curDel = del( iStyle );
			// go through the columns and break them into multiple lines
			// if bar '|' is found in a row then break into two lines
			// if longer than the column width break into two lines for fixed style only
			for ( iCol = 1; iCol <= colsColumnLabels; ++iCol ) {
				numColLabelRows = 0;
				workColumn = columnLabels( iCol );
				widthColumn( iCol ) = max( widthColumn( iCol ), static_cast< int >( len( columnLabels( iCol ) ) ) );
				while ( true ) {
					barLoc = index( workColumn, '|' );
					if ( barLoc != std::string::npos ) {
						++numColLabelRows;
						colLabelMulti( iCol, numColLabelRows ) = workColumn.substr( 0, barLoc );
						workColumn.erase( 0, barLoc + 1 );
					} else {
						++numColLabelRows;
						colLabelMulti( iCol, numColLabelRows ) = workColumn;
						break; //inner do loop
					}
				}
				if ( numColLabelRows > maxNumColLabelRows ) {
					maxNumColLabelRows = numColLabelRows;
				}
			}
			// extra preprocessing for fixed style reports
			if ( TableStyle( iStyle ) == tableStyleFixed ) {
				// break column headings into multiple rows if long (for fixed) or contain two spaces in a row.
				for ( iCol = 1; iCol <= colsColumnLabels; ++iCol ) {
					colWidthLimit = widthColumn( iCol );
					for ( jRow = 1; jRow <= maxNumColLabelRows; ++jRow ) {
						pare( colLabelMulti( iCol, jRow ), colWidthLimit );
					}
				}
				maxWidthRowLabel = 0;
				for ( jRow = 1; jRow <= rowsRowLabels; ++jRow ) {
					widthRowLabel = len( rowLabels( jRow ) );
					if ( widthRowLabel > maxWidthRowLabel ) {
						maxWidthRowLabel = widthRowLabel;
					}
				}
			}
			// output depending on style of format
			auto const style( TableStyle( iStyle ) );
			if ( ( style == tableStyleComma ) || ( style == tableStyleTab ) ) {
				// column headers
				for ( jRow = 1; jRow <= maxNumColLabelRows; ++jRow ) {
					outputLine = curDel; // one leading delimiters on column header lines
					for ( iCol = 1; iCol <= colsColumnLabels; ++iCol ) {
						outputLine += curDel + stripped( colLabelMulti( iCol, jRow ) );
					}
					tbl_stream << InsertCurrencySymbol( outputLine, false ) << '\n';
				}
				// body with row headers
				for ( jRow = 1; jRow <= rowsBody; ++jRow ) {
					outputLine = curDel + rowLabels( jRow ); // one leading delimiters on table body lines
					for ( iCol = 1; iCol <= colsBody; ++iCol ) {
						outputLine += curDel + stripped( body( iCol, jRow ) );
					}
					tbl_stream << InsertCurrencySymbol( outputLine, false ) << '\n';
				}
				if ( present( footnoteText ) ) {
					if ( ! footnoteText().empty() ) {
						tbl_stream << footnoteText() << '\n';
					}
				}
				tbl_stream << "\n\n";

			} else if ( style == tableStyleFixed ) {
				// column headers
				for ( jRow = 1; jRow <= maxNumColLabelRows; ++jRow ) {
					outputLine = blank; // spaces(:maxWidthRowLabel+2)  // two extra spaces and leave blank area for row labels
					col1start = max( maxWidthRowLabel + 2u, static_cast< std::string::size_type >( 3u ) );
					for ( iCol = 1; iCol <= colsColumnLabels; ++iCol ) {
						if ( iCol != 1 ) {
							outputLine += "  " + rjustified( sized( colLabelMulti( iCol, jRow ), widthColumn( iCol ) ) );
						} else {
							outputLine = std::string( col1start - 1, ' ' ) + "  " + rjustified( sized( colLabelMulti( iCol, jRow ), widthColumn( iCol ) ) );
						}
					}
					tbl_stream << InsertCurrencySymbol( outputLine, false ) << '\n';
				}
				// body with row headers
				for ( jRow = 1; jRow <= rowsBody; ++jRow ) {
					outputLine = "  " + rjustified( sized( rowLabels( jRow ), maxWidthRowLabel ) ); // two blank spaces on table body lines
					//col1start = max( len( outputLine ) + 2u, maxWidthRowLabel + 2u );
					for ( iCol = 1; iCol <= colsBody; ++iCol ) {
						if ( iCol != 1 ) {
							outputLine += "  " + rjustified( sized( body( iCol, jRow ), widthColumn( iCol ) ) );
						} else {
							outputLine += "   " + rjustified( sized( body( iCol, jRow ), widthColumn( iCol ) ) );
						}
					}
					tbl_stream << InsertCurrencySymbol( outputLine, false ) << '\n';
				}
				if ( present( footnoteText ) ) {
					if ( ! footnoteText().empty() ) {
						tbl_stream << footnoteText() << '\n';
					}
				}
				tbl_stream << "\n\n";

			} else if ( style == tableStyleHTML ) {
				// set up it being a table
				tbl_stream << "<table border=\"1\" cellpadding=\"4\" cellspacing=\"0\">\n";
				// column headers
				tbl_stream << "  <tr><td></td>\n"; // start new row and leave empty cell
				for ( iCol = 1; iCol <= colsColumnLabels; ++iCol ) {
					outputLine = "    <td align=\"right\">";
					for ( jRow = 1; jRow <= maxNumColLabelRows; ++jRow ) {
						outputLine += colLabelMulti( iCol, jRow );
						if ( jRow < maxNumColLabelRows ) {
							outputLine += "<br>";
						}
					}
					tbl_stream << InsertCurrencySymbol( outputLine, true ) << "</td>\n";
				}
				tbl_stream << "  </tr>\n";
				// body with row headers
				for ( jRow = 1; jRow <= rowsBody; ++jRow ) {
					tbl_stream << "  <tr>\n";
					if ( rowLabels( jRow ) != "" ) {
						tbl_stream << "    <td align=\"right\">" << InsertCurrencySymbol( rowLabels( jRow ), true ) << "</td>\n";
					} else {
						tbl_stream << "    <td align=\"right\">&nbsp;</td>\n";
					}
					for ( iCol = 1; iCol <= colsBody; ++iCol ) {
						if ( body( iCol, jRow ) != "" ) {
							tbl_stream << "    <td align=\"right\">" << InsertCurrencySymbol( body( iCol, jRow ), true ) << "</td>\n";
						} else {
							tbl_stream << "    <td align=\"right\">&nbsp;</td>\n";
						}
					}
					tbl_stream << "  </tr>\n";
				}
				// end the table
				tbl_stream << "</table>\n";
				if ( present( footnoteText ) ) {
					if ( ! footnoteText().empty() ) {
						tbl_stream << "<i>" << footnoteText() << "</i>\n";
					}
				}
				tbl_stream << "<br><br>\n";
			} else if ( style == tableStyleXML ) {
				//check if entire table is blank and it if is skip generating anything
				isTableBlank = true;
				for ( jRow = 1; jRow <= rowsBody; ++jRow ) {
					for ( iCol = 1; iCol <= colsBody; ++iCol ) {
						if ( len( body( iCol, jRow ) ) > 0 ) {
							isTableBlank = false;
							break;
						}
					}
					if ( ! isTableBlank ) break;
				}
				// if non-blank cells in the table body were found create the table.
				if ( ! isTableBlank ) {
					//if report name and subtable name the same add "record" to the end
					activeSubTableName = ConvertToElementTag( activeSubTableName );
					activeReportNameNoSpace = ConvertToElementTag( activeReportName );
					if ( SameString( activeSubTableName, activeReportNameNoSpace ) ) {
						activeSubTableName += "Record";
					}
					//if no subtable name use the report name and add "record" to the end
					if ( len( activeSubTableName ) == 0 ) {
						activeSubTableName = activeReportNameNoSpace + "Record";
					}
					// if a single column table, transpose it automatically
					if ( ( colsBody == 1 ) && ( rowsBody > 1 ) ) {
						doTransposeXML = true;
					}
					// first convert all row and column headers into tags compatible with XML strings
					for ( jRow = 1; jRow <= rowsBody; ++jRow ) {
						rowLabelTags( jRow ) = ConvertToElementTag( rowLabels( jRow ) );
						if ( len( rowLabelTags( jRow ) ) == 0 ) {
							rowLabelTags( jRow ) = "none";
						}
						rowUnitStrings( jRow ) = GetUnitSubString( rowLabels( jRow ) );
						if ( SameString( rowUnitStrings( jRow ), "Invalid/Undefined" ) ) {
							rowUnitStrings( jRow ) = "";
						}
					}
					for ( iCol = 1; iCol <= colsBody; ++iCol ) {
						columnLabelTags( iCol ) = ConvertToElementTag( columnLabels( iCol ) );
						if ( len( columnLabelTags( iCol ) ) == 0 ) {
							columnLabelTags( iCol ) = "none";
						}
						columnUnitStrings( iCol ) = GetUnitSubString( columnLabels( iCol ) );
						if ( SameString( columnUnitStrings( iCol ), "Invalid/Undefined" ) ) {
							columnUnitStrings( iCol ) = "";
						}
					}
					// convert entire table body to one with escape characters (no " ' < > &)
					for ( jRow = 1; jRow <= rowsBody; ++jRow ) {
						for ( iCol = 1; iCol <= colsBody; ++iCol ) {
							bodyEsc( iCol, jRow ) = ConvertToEscaped( body( iCol, jRow ) );
						}
					}
					if ( ! doTransposeXML ) {
						// body with row headers
						for ( jRow = 1; jRow <= rowsBody; ++jRow ) {
							// check if record is blank and it if is skip generating anything
							isRecordBlank = true;
							for ( iCol = 1; iCol <= colsBody; ++iCol ) {
								if ( len( bodyEsc( iCol, jRow ) ) > 0 ) {
									isRecordBlank = false;
									break;
								}
							}
							if ( ! isRecordBlank ) {
								tbl_stream << "  <" << activeSubTableName << ">\n";
								if ( len( rowLabelTags( jRow ) ) > 0 ) {
									tbl_stream << "    <name>" << rowLabelTags( jRow ) << "</name>\n";
								}
								for ( iCol = 1; iCol <= colsBody; ++iCol ) {
									if ( len( stripped( bodyEsc( iCol, jRow ) ) ) > 0 ) { // skip blank cells
										tagWithAttrib = "<" + columnLabelTags( iCol );
										if ( len( columnUnitStrings( iCol ) ) > 0 ) {
											tagWithAttrib += std::string( " units=" ) + char( 34 ) + columnUnitStrings( iCol ) + char( 34 ) + '>'; // if units are present add them as an attribute
										} else {
											tagWithAttrib += ">";
										}
										tbl_stream << "    " << tagWithAttrib << stripped( bodyEsc( iCol, jRow ) ) << "</" << columnLabelTags( iCol ) << ">\n";
									}
								}
								tbl_stream << "  </" << activeSubTableName << ">\n";
							}
						}
					} else { // transpose XML table
						// body with row headers
						for ( iCol = 1; iCol <= colsBody; ++iCol ) {
							// check if record is blank and it if is skip generating anything
							isRecordBlank = true;
							for ( jRow = 1; jRow <= rowsBody; ++jRow ) {
								if ( len( bodyEsc( iCol, jRow ) ) > 0 ) {
									isRecordBlank = false;
									break;
								}
							}
							if ( ! isRecordBlank ) {
								tbl_stream << "  <" << activeSubTableName << ">\n";
								// if the column has units put them into the name tag
								if ( len( columnLabelTags( iCol ) ) > 0 ) {
									if ( len( columnUnitStrings( iCol ) ) > 0 ) {
										tbl_stream << "    <name units=" << char( 34 ) << columnUnitStrings( iCol ) << char( 34 ) << '>' << columnLabelTags( iCol ) << "</name>\n";
									} else {
										tbl_stream << "    <name>" << columnLabelTags( iCol ) << "</name>\n";
									}
								}
								for ( jRow = 1; jRow <= rowsBody; ++jRow ) {
									if ( len( bodyEsc( iCol, jRow ) ) > 0 ) { // skip blank cells
										tagWithAttrib = "<" + rowLabelTags( jRow );
										if ( len( rowUnitStrings( jRow ) ) > 0 ) {
											tagWithAttrib += std::string( " units=" ) + char( 34 ) + rowUnitStrings( jRow ) + char( 34 ) + ">\n"; // if units are present add them as an attribute
										} else {
											tagWithAttrib += ">";
										}
										tbl_stream << "    " << tagWithAttrib << stripped( bodyEsc( iCol, jRow ) ) << "</" << rowLabelTags( jRow ) << ">\n";
									}
								}
								tbl_stream << "  </" << activeSubTableName << ">\n";
							}
						}
					}
					if ( present( footnoteText ) ) {
						if ( ! footnoteText().empty() ) {
							tbl_stream << "  <footnote>" << footnoteText() << "</footnote>\n";
						}
					}
				}
			} else {

			}
		}
	}

	std::string
	MakeAnchorName(
		std::string const & reportString,
		std::string const & objectString
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   June 2005
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Use the name of the report and object be used to create and HTML anchor

		// METHODOLOGY EMPLOYED:
		//   Remove spaces and put double colon between names

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		std::string StringOut;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		for ( std::string::size_type i = 0, e = reportString.length(); i < e; ++i ) {
			if ( has( validChars, reportString[ i ] ) ) {
				StringOut += reportString[ i ];
			}
		}
		StringOut += "::";
		for ( std::string::size_type i = 0, e = objectString.length(); i < e; ++i ) {
			if ( has( validChars, objectString[ i ] ) ) {
				StringOut += objectString[ i ];
			}
		}
		return StringOut;
	}

	std::string
	InsertCurrencySymbol(
		std::string const & inString, // Input String
		bool const isHTML // True if an HTML string
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Looks for the ~~$~~

		// METHODOLOGY EMPLOYED:
		//   na
		// Using/Aliasing
		using namespace DataCostEstimate;

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

		std::string outSt( trimmed( inString ) ); // Result String
		std::string::size_type loc = index( outSt, "~~$~~" );
		while ( loc != std::string::npos ) {
			if ( isHTML ) {
				outSt = inString.substr( 0, loc ) + monetaryUnit( selectedMonetaryUnit ).html + outSt.substr( loc + 5 );
			} else {
				outSt = inString.substr( 0, loc ) + monetaryUnit( selectedMonetaryUnit ).txt + outSt.substr( loc + 5 );
			}
			loc = index( outSt, "~~$~~" );
		}
		return outSt;
	}

	std::string
	ConvertToElementTag( std::string const & inString ) // Input String
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Convert report column or row header into a tag string
		//   that just has A-Z, a-z, or 0-1 characters and is
		//   shown in camelCase.

		// METHODOLOGY EMPLOYED:
		//   na

		// Return value
		std::string outString; // Result String

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		bool foundOther = true; // flag if character found besides A-Z, a-z, 0-9
		for ( std::string::size_type iIn = 0, e = inString.length(); iIn < e; ++iIn ) {
			char const c( inString[ iIn ] );
			int const curCharVal = int( c );
			if ( ( curCharVal >= 65 ) && ( curCharVal <= 90 ) ) { // A-Z upper case
				if ( foundOther ) {
					outString += c; // keep as upper case after finding a space or another character
				} else {
					outString += char( curCharVal + 32 ); // convert to lower case
				}
				foundOther = false;
			} else if ( ( curCharVal >= 97 ) && ( curCharVal <= 122 ) ) { // A-Z lower case
				if ( foundOther ) {
					outString += char( curCharVal - 32 ); // convert to upper case
				} else {
					outString += c; // leave as lower case
				}
				foundOther = false;
			} else if ( ( curCharVal >= 48 ) && ( curCharVal <= 57 ) ) { // 0-9 numbers
				// if first character is a number then prepend with the letter "t"
				if ( outString.length() == 0 ) outString += 't';
				outString += c;
				foundOther = false;
			} else if ( curCharVal == 91 ) { // [ bracket
				break; // stop parsing because unit string was found
			} else {
				foundOther = true;
			}
		}
		return outString;
	}

	std::string
	ConvertUnicodeToUTF8( unsigned long const codepoint )
	{
		// Taken from http://stackoverflow.com/a/19968992/2358662 and http://stackoverflow.com/a/148766/2358662
		std::string s;
		if ( codepoint <= 0x7f ) {
			s.push_back( static_cast<char>( codepoint ) );
		} else if ( codepoint <= 0x7ff ) {
			s.push_back( static_cast<char>( 0xc0 | ( ( codepoint >> 6 ) & 0x1f ) ) );
			s.push_back( static_cast<char>( 0x80 | ( codepoint & 0x3f ) ) );
		} else if ( codepoint <= 0xffff ) {
			s.push_back( static_cast<char>( 0xe0 | ( ( codepoint >> 12 ) & 0x0f ) ) );
			s.push_back( static_cast<char>( 0x80 | ( ( codepoint >> 6 ) & 0x3f ) ) );
			s.push_back( static_cast<char>( 0x80 | ( codepoint & 0x3f ) ) );
		} else if ( codepoint <= 0x10ffff ) {
			s.push_back( static_cast<char>( 0xf0 | ( ( codepoint >> 18 ) & 0x07 ) ) );
			s.push_back( static_cast<char>( 0x80 | ( ( codepoint >> 12 ) & 0x3f ) ) );
			s.push_back( static_cast<char>( 0x80 | ( ( codepoint >> 6 ) & 0x3f ) ) );
			s.push_back( static_cast<char>( 0x80 | ( codepoint & 0x3f ) ) );
		}
		return s;
	}

	std::string
	ConvertToEscaped( std::string const & inString ) // Input String
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Convert to XML safe escaped character string
		//   so it excludes:
		//               " ' < > &

		if ( inString.empty() ) return "";

		std::string s;
		auto const inputSize = inString.size();
		s.reserve( inputSize );
		size_t index( 0 );
		char c;

		while ( true ) {
			if ( index == inputSize ) break;
			c = inString[ index++ ];
			if ( c == '\"' ) {
				s += "&quot;";
			} else if ( c == '&' ) {
				s += "&amp;";
			} else if ( c == '\'' ) {
				s += "&apos;";
			} else if ( c == '<' ) {
				s += "&lt;";
			} else if ( c == '>' ) {
				s += "&gt;";
			} else if ( c == char( 176 ) ) {
				s += "&deg;";
			} else if ( c == '\xC2' ) {
				if ( index == inputSize ) {
					s += '\xC2';
				} else {
					c = inString[ index++ ];
					if ( c == '\xB0' ) {
						s += "&deg;";
					} else {
						s += '\xC2';
						s += c;
					}
				}
			} else if ( c == '\xB0' ) {
				s += "&deg;";
			} else if ( c == '\\' ) {
				if ( index == inputSize ) break;
				c = inString[ index++ ];
				if ( c == '"' ) {
					s += "&quot;";
				} else if ( c == '\'' ) {
					s += "&apos;";
				} else if ( c == 'u' || c == 'x' ) {
					int remainingLen = inputSize - index;
					unsigned long codePoint( 0 );
					if ( c == 'u' && remainingLen > 3 ) {
						codePoint = std::stoul( inString.substr( index, 4 ), nullptr, 16 );
						index += 4;
					} else if ( c == 'x' && remainingLen > 1 ) {
						codePoint = std::stoul( inString.substr( index, 2 ), nullptr, 16 );
						index += 2;
					}
					auto const unicodeString = ConvertUnicodeToUTF8( codePoint );
					if ( unicodeString == "\xC2\xB0" ) { // only check for degree at this point
						s += "&deg;";
					} else {
						s += unicodeString;
					}
				} else {
					s += c;
				}
			} else {
				s += c;
			}
		}
		return s;
	}

	void
	DetermineBuildingFloorArea()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   November 2003
		//       MODIFIED       BTG added checks for plenums. Feb2004
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   To determine the total floor area of the building and the
		//   conditioned floor area of the building

		// METHODOLOGY EMPLOYED:
		//   Use the Zone array and sum the areas for all zones

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalance::Zone;
		using ZonePlenum::NumZoneReturnPlenums;
		using ZonePlenum::NumZoneSupplyPlenums;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 curZoneArea;
		int iZone;
		//INTEGER  :: found

		buildingGrossFloorArea = 0.0;
		buildingConditionedFloorArea = 0.0;
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			curZoneArea = Zone( iZone ).FloorArea * Zone( iZone ).Multiplier * Zone( iZone ).ListMultiplier;

			// OLD CHECK IF PLENUM SHOULD BE EXCLUDED AUTOMATICALLY
			//check if this zone is also a return plenum or a supply plenum
			//found = 0
			//if (NumZoneReturnPlenums > 0) THEN
			//  found = FindItemInList(Zone(iZone)%Name, ZoneRetPlenCond%ZoneName, NumZoneReturnPlenums)
			//endif
			//IF (found /= 0)  curZoneArea = 0.0d0
			//found = 0
			//if (NumZoneSupplyPlenums > 0) THEN
			//  found = FindItemInList(Zone(iZone)%Name, ZoneSupPlenCond%ZoneName, NumZoneSupplyPlenums)
			//endif
			//IF (found /= 0)  curZoneArea = 0.0d0

			if ( Zone( iZone ).isPartOfTotalArea ) {
				buildingGrossFloorArea += curZoneArea;
				// If a ZoneHVAC:EquipmentConnections is used for a zone then
				// it is considered conditioned. Also ZONE SUPPLY PLENUM and ZONE RETURN PLENUM are
				// also is considered conditioned.
				if ( Zone( iZone ).SystemZoneNodeNumber > 0 ) {
					buildingConditionedFloorArea += curZoneArea;
				}
			}
		}
	}

	//======================================================================================================================
	//======================================================================================================================

	//    ROUTINES TO RESET GATHERED VALUES TO ZERO

	//======================================================================================================================
	//======================================================================================================================

	void
	ResetTabularReports()
	{
		// Jason Glazer - October 2015
		// Reset all gathering arrays to zero for multi-year simulations
		// so that only last year is reported in tabular reports
		using ThermalComfort::ResetThermalComfortSimpleASH55;
		using ThermalComfort::ResetSetPointMet;
		using OutputProcessor::isFinalYear;

		gatherElapsedTimeBEPS = 0.0;
		ResetMonthlyGathering();
		OutputReportTabularAnnual::ResetAnnualGathering();
		ResetBinGathering();
		ResetBEPSGathering();
		ResetSourceEnergyEndUseGathering();
		ResetPeakDemandGathering();
		ResetHeatGainGathering();
		ResetRemainingPredefinedEntries();
		ResetThermalComfortSimpleASH55();
		ResetSetPointMet();
		ResetAdaptiveComfort();
		isFinalYear = true;
	}

	void
	ResetMonthlyGathering(){
		// Jason Glazer - October 2015
		// Reset all monthly gathering arrays to zero for multi-year simulations
		// so that only last year is reported in tabular reports
		int iInput;
		int jTable;
		int kColumn;
		int curTable;
		int curCol;
		static Real64 BigNum( 0.0 );

		for ( iInput = 1; iInput <= MonthlyInputCount; ++iInput ) {
			for ( jTable = 1; jTable <= MonthlyInput( iInput ).numTables; ++jTable ) {
				curTable = jTable + MonthlyInput( iInput ).firstTable - 1;
				for ( kColumn = 1; kColumn <= MonthlyTables( curTable ).numColumns; ++kColumn ) {
					curCol = kColumn + MonthlyTables( curTable ).firstColumn - 1;
					MonthlyColumns( curCol ).timeStamp = 0;
					MonthlyColumns( curCol ).duration = 0.0;
					if ( MonthlyColumns( curCol ).aggType == aggTypeMaximum || MonthlyColumns( curCol ).aggType == aggTypeMaximumDuringHoursShown ){
						MonthlyColumns( curCol ).reslt = -huge( BigNum );
					}
					else if ( MonthlyColumns( curCol ).aggType == aggTypeMinimum || MonthlyColumns( curCol ).aggType == aggTypeMinimumDuringHoursShown ){
						MonthlyColumns( curCol ).reslt = huge( BigNum );
					}
					else {
						MonthlyColumns( curCol ).reslt = 0.0;
					}
				}
			}
		}
	}

	void
	ResetBinGathering(){
		// Jason Glazer - October 2015
		// Reset all timebins gathering arrays to zero for multi-year simulations
		// so that only last year is reported in tabular reports
		Real64 const bigVal( 0.0 ); // used with HUGE: Value doesn't matter, only type: Initialize so compiler doesn't warn about use uninitialized

		// clear the binning arrays to zeros
		for ( auto & e : BinResults ) {
			e.mnth = 0.0;
			e.hrly = 0.0;
		}
		for ( auto & e : BinResultsBelow ) {
			e.mnth = 0.0;
			e.hrly = 0.0;
		}
		for ( auto & e : BinResultsAbove ) {
			e.mnth = 0.0;
			e.hrly = 0.0;
		}

		// re-initialize statistics counters
		for ( auto & e : BinStatistics ) {
			e.minimum = huge( bigVal );
			e.maximum = -huge( bigVal );
			e.n = 0;
			e.sum = 0.0;
			e.sum2 = 0.0;
		}
	}

	void
	ResetBEPSGathering(){
		// Jason Glazer - October 2015
		// Reset all ABUPS gathering arrays to zero for multi-year simulations
		// so that only last year is reported in tabular reports
		gatherTotalsBEPS = 0.0;
		gatherEndUseBEPS = 0.0;
		gatherEndUseSubBEPS = 0.0;
		gatherTotalsSource = 0.0;
		// reset the specific componenents being gathered
		gatherPowerFuelFireGen = 0.0;
		gatherPowerPV = 0.0;
		gatherPowerWind = 0.0;
		gatherPowerHTGeothermal = 0.0;
		gatherElecProduced = 0.0;
		gatherElecPurchased = 0.0;
		gatherElecSurplusSold = 0.0;
		gatherElecStorage = 0.0;
		gatherPowerConversion = 0.0;
		gatherWaterHeatRecovery = 0.0;
		gatherAirHeatRecoveryCool = 0.0;
		gatherAirHeatRecoveryHeat = 0.0;
		gatherHeatHTGeothermal = 0.0;
		gatherHeatSolarWater = 0.0;
		gatherHeatSolarAir = 0.0;
		gatherRainWater = 0.0;
		gatherCondensate = 0.0;
		gatherWellwater = 0.0;
		gatherMains = 0.0;
		gatherWaterEndUseTotal = 0.0;
	}

	void
	ResetSourceEnergyEndUseGathering(){
		// Jason Glazer - October 2015
		// Reset all source energy end use table gathering arrays to zero for multi-year simulations
		// so that only last year is reported in tabular reports
		gatherTotalsBySourceBEPS = 0.0;
		gatherEndUseBySourceBEPS = 0.0;
	}

	void
	ResetPeakDemandGathering(){
		// Jason Glazer - October 2015
		// Reset all demand end use components table gathering arrays to zero for multi-year simulations
		// so that only last year is reported in tabular reports
		gatherDemandTotal = 0.0;
		gatherDemandTimeStamp = 0;
		gatherDemandEndUse = 0.0;
		gatherDemandEndUseSub = 0.0;

	}

	void
	ResetHeatGainGathering(){
		// Jason Glazer - October 2015
		// Reset all sensible heat gas summary report gathering arrays to zero for multi-year simulations
		// so that only last year is reported in tabular reports
		using DataHeatBalance::ZonePreDefRep;
		using DataHeatBalance::BuildingPreDefRep;
		int iZone;
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			ZonePreDefRep( iZone ).SHGSAnPeoplAdd = 0.;
			ZonePreDefRep( iZone ).SHGSAnLiteAdd = 0.;
			ZonePreDefRep( iZone ).SHGSAnHvacHt = 0.;
			ZonePreDefRep( iZone ).SHGSAnHvacCl = 0.;
			ZonePreDefRep( iZone ).SHGSAnIzaAdd = 0.;
			ZonePreDefRep( iZone ).SHGSAnIzaRem = 0.;
			ZonePreDefRep( iZone ).SHGSAnWindAdd = 0.;
			ZonePreDefRep( iZone ).SHGSAnWindRem = 0.;
			ZonePreDefRep( iZone ).SHGSAnInfilAdd = 0.;
			ZonePreDefRep( iZone ).SHGSAnInfilRem = 0.;
			ZonePreDefRep( iZone ).SHGSAnEquipAdd = 0.;
			ZonePreDefRep( iZone ).SHGSAnEquipRem = 0.;
			ZonePreDefRep( iZone ).SHGSAnHvacATUHt = 0.;
			ZonePreDefRep( iZone ).SHGSAnHvacATUCl = 0.;
			ZonePreDefRep( iZone ).SHGSAnSurfHt = 0.;
			ZonePreDefRep( iZone ).SHGSAnSurfCl = 0.;
			ZonePreDefRep( iZone ).SHGSAnOtherAdd = 0.;
			ZonePreDefRep( iZone ).SHGSAnOtherRem = 0.;
			ZonePreDefRep( iZone ).htPeak = 0.;
			ZonePreDefRep( iZone ).htPtTimeStamp = 0;
			ZonePreDefRep( iZone ).SHGSHtHvacHt = 0.;
			ZonePreDefRep( iZone ).SHGSHtHvacCl = 0.;
			ZonePreDefRep( iZone ).SHGSHtSurfHt = 0.;
			ZonePreDefRep( iZone ).SHGSHtSurfCl = 0.;
			ZonePreDefRep( iZone ).SHGSHtHvacATUHt = 0.;
			ZonePreDefRep( iZone ).SHGSHtHvacATUCl = 0.;
			ZonePreDefRep( iZone ).SHGSHtPeoplAdd = 0.;
			ZonePreDefRep( iZone ).SHGSHtLiteAdd = 0.;
			ZonePreDefRep( iZone ).SHGSHtEquipAdd = 0.;
			ZonePreDefRep( iZone ).SHGSHtEquipRem = 0.;
			ZonePreDefRep( iZone ).SHGSHtWindAdd = 0.;
			ZonePreDefRep( iZone ).SHGSHtWindRem = 0.;
			ZonePreDefRep( iZone ).SHGSHtIzaAdd = 0.;
			ZonePreDefRep( iZone ).SHGSHtIzaRem = 0.;
			ZonePreDefRep( iZone ).SHGSHtInfilAdd = 0.;
			ZonePreDefRep( iZone ).SHGSHtInfilRem = 0.;
			ZonePreDefRep( iZone ).SHGSHtOtherAdd = 0.;
			ZonePreDefRep( iZone ).SHGSHtOtherRem = 0.;
			ZonePreDefRep( iZone ).clPeak = 0.;
			ZonePreDefRep( iZone ).clPtTimeStamp = 0;
			ZonePreDefRep( iZone ).SHGSClHvacHt = 0.;
			ZonePreDefRep( iZone ).SHGSClHvacCl = 0.;
			ZonePreDefRep( iZone ).SHGSClSurfHt = 0.;
			ZonePreDefRep( iZone ).SHGSClSurfCl = 0.;
			ZonePreDefRep( iZone ).SHGSClHvacATUHt = 0.;
			ZonePreDefRep( iZone ).SHGSClHvacATUCl = 0.;
			ZonePreDefRep( iZone ).SHGSClPeoplAdd = 0.;
			ZonePreDefRep( iZone ).SHGSClLiteAdd = 0.;
			ZonePreDefRep( iZone ).SHGSClEquipAdd = 0.;
			ZonePreDefRep( iZone ).SHGSClEquipRem = 0.;
			ZonePreDefRep( iZone ).SHGSClWindAdd = 0.;
			ZonePreDefRep( iZone ).SHGSClWindRem = 0.;
			ZonePreDefRep( iZone ).SHGSClIzaAdd = 0.;
			ZonePreDefRep( iZone ).SHGSClIzaRem = 0.;
			ZonePreDefRep( iZone ).SHGSClInfilAdd = 0.;
			ZonePreDefRep( iZone ).SHGSClInfilRem = 0.;
			ZonePreDefRep( iZone ).SHGSClOtherAdd = 0.;
			ZonePreDefRep( iZone ).SHGSClOtherRem = 0.;
		}

		BuildingPreDefRep.htPeak = 0.;
		BuildingPreDefRep.htPtTimeStamp = 0;
		BuildingPreDefRep.SHGSHtHvacHt = 0.0;
		BuildingPreDefRep.SHGSHtHvacCl = 0.0;
		BuildingPreDefRep.SHGSHtHvacATUHt = 0.0;
		BuildingPreDefRep.SHGSHtHvacATUCl = 0.0;
		BuildingPreDefRep.SHGSHtSurfHt = 0.0;
		BuildingPreDefRep.SHGSHtSurfCl = 0.0;
		BuildingPreDefRep.SHGSHtPeoplAdd = 0.0;
		BuildingPreDefRep.SHGSHtLiteAdd = 0.0;
		BuildingPreDefRep.SHGSHtEquipAdd = 0.0;
		BuildingPreDefRep.SHGSHtWindAdd = 0.0;
		BuildingPreDefRep.SHGSHtIzaAdd = 0.0;
		BuildingPreDefRep.SHGSHtInfilAdd = 0.0;
		BuildingPreDefRep.SHGSHtOtherAdd = 0.0;
		BuildingPreDefRep.SHGSHtEquipRem = 0.0;
		BuildingPreDefRep.SHGSHtWindRem = 0.0;
		BuildingPreDefRep.SHGSHtIzaRem = 0.0;
		BuildingPreDefRep.SHGSHtInfilRem = 0.0;
		BuildingPreDefRep.SHGSHtOtherRem = 0.0;

		BuildingPreDefRep.clPeak = 0.;
		BuildingPreDefRep.clPtTimeStamp = 0;
		BuildingPreDefRep.SHGSClHvacHt = 0.0;
		BuildingPreDefRep.SHGSClHvacCl = 0.0;
		BuildingPreDefRep.SHGSClSurfHt = 0.0;
		BuildingPreDefRep.SHGSClSurfCl = 0.0;
		BuildingPreDefRep.SHGSClHvacATUHt = 0.0;
		BuildingPreDefRep.SHGSClHvacATUCl = 0.0;
		BuildingPreDefRep.SHGSClPeoplAdd = 0.0;
		BuildingPreDefRep.SHGSClLiteAdd = 0.0;
		BuildingPreDefRep.SHGSClEquipAdd = 0.0;
		BuildingPreDefRep.SHGSClWindAdd = 0.0;
		BuildingPreDefRep.SHGSClIzaAdd = 0.0;
		BuildingPreDefRep.SHGSClInfilAdd = 0.0;
		BuildingPreDefRep.SHGSClOtherAdd = 0.0;
		BuildingPreDefRep.SHGSClEquipRem = 0.0;
		BuildingPreDefRep.SHGSClWindRem = 0.0;
		BuildingPreDefRep.SHGSClIzaRem = 0.0;
		BuildingPreDefRep.SHGSClInfilRem = 0.0;
		BuildingPreDefRep.SHGSClOtherRem = 0.0;

	}

	void
	ResetRemainingPredefinedEntries(){
		// Jason Glazer - October 2015
		// Reset all entries that are added to the predefined reports in the FillRemainingPredefinedEntries() function to zero for multi-year simulations
		// so that only last year is reported in tabular reports
		using DataHeatBalance::TotLights;
		using DataHeatBalance::Lights;
		using ExteriorEnergyUse::ExteriorLights;
		using ExteriorEnergyUse::NumExteriorLights;
		using DataHeatBalance::Zone;
		using DataHeatBalance::ZonePreDefRep;

		Real64 const bigVal( 0.0 ); // used with HUGE: Value doesn't matter, only type: Initialize so compiler doesn't warn about use uninitialized
		int iLight;
		int iZone;

		for ( iLight = 1; iLight <= TotLights; ++iLight ) {
			Lights( iLight ).SumTimeNotZeroCons = 0.;
			Lights( iLight ).SumConsumption = 0.;
		}
		for ( iLight = 1; iLight <= NumExteriorLights; ++iLight ) {
			ExteriorLights( iLight ).SumTimeNotZeroCons = 0.;
			ExteriorLights( iLight ).SumConsumption = 0.;
		}
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			if ( Zone( iZone ).SystemZoneNodeNumber >= 0 ) { //conditioned zones only
				if ( Zone( iZone ).isNominalOccupied ) {
					ZonePreDefRep( iZone ).MechVentVolTotal = 0.;
					ZonePreDefRep( iZone ).MechVentVolMin = huge( bigVal );
					ZonePreDefRep( iZone ).InfilVolTotal = 0.;
					ZonePreDefRep( iZone ).InfilVolMin = huge( bigVal );
					ZonePreDefRep( iZone ).AFNInfilVolTotal = 0.;
					ZonePreDefRep( iZone ).AFNInfilVolMin = huge( bigVal );
					ZonePreDefRep( iZone ).SimpVentVolTotal = 0.;
					ZonePreDefRep( iZone ).SimpVentVolMin = huge( bigVal );
					ZonePreDefRep( iZone ).TotTimeOcc = 0.;
				}
			}
		}
	}

	void
	ResetAdaptiveComfort()
	{
	// Jason Glazer - October 2015
	// Reset accumulation variable for adaptive comfort report to zero for multi-year simulations
	// so that only last year is reported in tabular reports
		using DataHeatBalance::People;
		using DataHeatBalance::TotPeople;
		int i;
		if ( displayAdaptiveComfort && TotPeople > 0 ) {
			for ( i = 1; i <= TotPeople; ++i ) {
				if ( People( i ).AdaptiveASH55 ) {
					People( i ).TimeNotMetASH5590 = 0.;
					People( i ).TimeNotMetASH5580 = 0.;
				}
				if ( People( i ).AdaptiveCEN15251 ) {
					People( i ).TimeNotMetCEN15251CatI = 0.;
					People( i ).TimeNotMetCEN15251CatII = 0.;
					People( i ).TimeNotMetCEN15251CatIII = 0.;
				}
			}
		}
	}


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
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   June 2005
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Determine if point q is in triangle defined by points a,b,c

		// METHODOLOGY EMPLOYED:
		//   The function used three times is positive the point is on the "right"
		//   side and negative if on "left" side. By checking to make sure the signs
		//   are always the same. it determines that the point is inside of the
		//   triangle.

		// REFERENCES:
		//   http://mcraefamily.com/MathHelp/GeometryPointAndTriangle2.htm

		// USE STATEMENTS:

		// Return value
		bool isInTriangle;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 fAB;
		Real64 fCA;
		Real64 fBC;

		fAB = ( qy - y1 ) * ( x2 - x1 ) - ( qx - x1 ) * ( y2 - y1 );
		fCA = ( qy - y3 ) * ( x1 - x3 ) - ( qx - x3 ) * ( y1 - y3 );
		fBC = ( qy - y2 ) * ( x3 - x2 ) - ( qx - x2 ) * ( y3 - y2 );
		if ( ( fAB * fBC ) >= 0.0 && ( fBC * fCA ) >= 0.0 ) {
			isInTriangle = true;
		} else {
			isInTriangle = false;
		}
		return isInTriangle;
	}

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
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   June 2005
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Determine if point q is in a quadrilateral defined by points a,b,c,d
		//   Those points should express a quadrilateral in order of the points going
		//   around the outside of the polygon. They should not describe an "hourglass"
		//   shape where the lines cross in the middle of the figure.

		// METHODOLOGY EMPLOYED:
		//   Check if the point is in triangle a,b,c or in triangle c,d,a

		// REFERENCES:
		//   http://mcraefamily.com/MathHelp/GeometryPointAndTriangle4.htm

		// USE STATEMENTS:

		// Return value
		bool isInQuadrilateral;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool inABC;
		bool inCDA;

		inABC = isInTriangle( qx, qy, ax, ay, bx, by, cx, cy );
		inCDA = isInTriangle( qx, qy, cx, cy, dx, dy, ax, ay );
		if ( inABC || inCDA ) {
			isInQuadrilateral = true;
		} else {
			isInQuadrilateral = false;
		}
		return isInQuadrilateral;
	}

	//======================================================================================================================
	//======================================================================================================================

	//    SUPPORT ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	std::string
	RealToStr(
		Real64 const RealIn,
		int const numDigits
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2003
		//       MODIFIED       November 2008; LKL - prevent errors
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//   Abstract away the internal write concept

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		std::string StringOut;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static Array1D< gio::Fmt > formDigits( {0,9}, { "(F12.0)", "(F12.1)", "(F12.2)", "(F12.3)", "(F12.4)", "(F12.5)", "(F12.6)", "(F12.7)", "(F12.8)", "(F12.9)" } ); // formDigits(0) | formDigits(1) | formDigits(2) | formDigits(3) | formDigits(4) | formDigits(5) | formDigits(6) | formDigits(7) | formDigits(8) | formDigits(9)
		static Array1D< Real64 > const maxvalDigits( {0,9}, { 9999999999.0, 999999999.0, 99999999.0, 9999999.0, 999999.0, 99999.0, 9999.0, 999.0, 99.0, 9.0 } ); // maxvalDigits(0) | maxvalDigits(1) | maxvalDigits(2) | maxvalDigits(3) | maxvalDigits(4) | maxvalDigits(5) | maxvalDigits(6) | maxvalDigits(7) | maxvalDigits(8) | maxvalDigits(9)
		static gio::Fmt fmtd( "(E12.6)" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int nDigits;

		nDigits = numDigits;
		if ( RealIn < 0.0 ) --nDigits;
		if ( nDigits > 9 ) nDigits = 9;
		if ( nDigits < 0 ) nDigits = 0;

		if ( std::abs( RealIn ) > maxvalDigits( nDigits ) ) {
			gio::write( StringOut, fmtd ) << RealIn;
		} else {
			gio::write( StringOut, formDigits( nDigits ) ) << RealIn;
		}
		//  WRITE(FMT=, UNIT=stringOut) RealIn
		// check if it did not fit
		//  IF (stringOut(1:1) .EQ. "*") THEN
		//    WRITE(FMT='(E12.6)', UNIT=stringOut) RealIn
		//  END IF

		//WRITE(FMT="(F10.4)", UNIT=stringOut, IOSTAT=status ) RealIn
		return StringOut;
	}

	std::string
	IntToStr( int const intIn )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Abstract away the internal write concept

		// Return value
		std::string StringOut;

		gio::write( StringOut, fmtLD ) << intIn;
		return StringOut;
	}

	Real64
	StrToReal( std::string const & stringIn )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Abstract away the internal read concept

		// Return value
		Real64 realValue;

		{ IOFlags flags; gio::read( stringIn, fmtLD, flags ) >> realValue; if ( flags.err() ) goto Label900; }
		return realValue;
Label900: ;
		realValue = -99999.0;
		return realValue;
	}

	std::string
	DateToString( int const codedDate ) // word containing encoded month, day, hour, minute
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Convert the coded date format into a usable
		//   string

		// Using/Aliasing
		using General::DecodeMonDayHrMin;

		// Return value
		std::string StringOut;

		// Locals
		// ((month*100 + day)*100 + hour)*100 + minute
		static gio::Fmt DateFmt( "(I2.2,'-',A3,'-',I2.2,':',I2.2)" );

		int Month; // month in integer format (1-12)
		int Day; // day in integer format (1-31)
		int Hour; // hour in integer format (1-24)
		int Minute; // minute in integer format (0:59)
		std::string monthName;

		if ( codedDate != 0 ) {
			DecodeMonDayHrMin( codedDate, Month, Day, Hour, Minute );
			--Hour;
			if ( Minute == 60 ) {
				++Hour;
				Minute = 0;
			}
			if ( Month == 1 ) {
				monthName = "JAN";
			} else if ( Month == 2 ) {
				monthName = "FEB";
			} else if ( Month == 3 ) {
				monthName = "MAR";
			} else if ( Month == 4 ) {
				monthName = "APR";
			} else if ( Month == 5 ) {
				monthName = "MAY";
			} else if ( Month == 6 ) {
				monthName = "JUN";
			} else if ( Month == 7 ) {
				monthName = "JUL";
			} else if ( Month == 8 ) {
				monthName = "AUG";
			} else if ( Month == 9 ) {
				monthName = "SEP";
			} else if ( Month == 10 ) {
				monthName = "OCT";
			} else if ( Month == 11 ) {
				monthName = "NOV";
			} else if ( Month == 12 ) {
				monthName = "DEC";
			} else {
				monthName = "***";
			}
			gio::write( StringOut, DateFmt ) << Day << monthName << Hour << Minute;
			if ( has( StringOut, "*" ) ) {
				StringOut = "-";
			}
		} else { // codeddate = 0
			StringOut = "-";
		}

		return StringOut;
	}

	void
	AddTOCEntry(
		std::string const & nameSection,
		std::string const & nameReport
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   September 2005
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Adds an entry for the TOC so that it can be created
		//   prior to the actual reports being generated. Note that
		//   the arguments must match what is used in
		//   "WriteReportHeaders" for the HTML anchors to work
		//   correctly.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		//    na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//    na

		// INTERFACE BLOCK SPECIFICATIONS:
		//    na

		// DERIVED TYPE DEFINITIONS:
		//    na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//    na

		if ( ! allocated( TOCEntries ) ) {
			TOCEntriesSize = 20;
			TOCEntries.allocate( TOCEntriesSize );
			TOCEntriesCount = 1;
		} else {
			++TOCEntriesCount;
			// if larger than current size grow the array
			if ( TOCEntriesCount > TOCEntriesSize ) {
				TOCEntries.redimension( TOCEntriesSize += 20 );
			}
		}
		TOCEntries( TOCEntriesCount ).reportName = nameReport;
		TOCEntries( TOCEntriesCount ).sectionName = nameSection;
	}

	void
	SetupUnitConversions()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   February 12, 2009
		//    MODIFIED       March 2010; Linda Lawrie; Add deltaC and KJ/KG
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Initialize the array that contains the unit conversion
		//   information. The code is based on code generated
		//   in a spreadsheet titled UnitConversion.xls

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		//    na

		// USE STATEMENTS:

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//    na

		// INTERFACE BLOCK SPECIFICATIONS:
		//    na

		// DERIVED TYPE DEFINITIONS:
		//    na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//    na
		UnitConvSize = 94;
		UnitConv.allocate( UnitConvSize );
		UnitConv( 1 ).siName = "%";
		UnitConv( 2 ).siName = "°C";
		UnitConv( 3 ).siName = "0=OFF 1=ON";
		UnitConv( 4 ).siName = "0-NO  1-YES";
		UnitConv( 5 ).siName = "1-YES 0-NO";
		UnitConv( 6 ).siName = "A";
		UnitConv( 7 ).siName = "ACH";
		UnitConv( 8 ).siName = "ACH";
		UnitConv( 9 ).siName = "BASE 10C";
		UnitConv( 10 ).siName = "BASE 18C";
		UnitConv( 11 ).siName = "C";
		UnitConv( 12 ).siName = "CD/M2";
		UnitConv( 13 ).siName = "DEG";
		UnitConv( 14 ).siName = "FRAC";
		UnitConv( 15 ).siName = "HOUR";
		UnitConv( 16 ).siName = "HOURS";
		UnitConv( 17 ).siName = "HR";
		UnitConv( 18 ).siName = "HRS";
		UnitConv( 19 ).siName = "J";
		UnitConv( 20 ).siName = "J";
		UnitConv( 21 ).siName = "J";
		UnitConv( 22 ).siName = "J";
		UnitConv( 23 ).siName = "J";
		UnitConv( 24 ).siName = "J";
		UnitConv( 25 ).siName = "J/KG";
		UnitConv( 26 ).siName = "J/KG H2O";
		UnitConv( 27 ).siName = "J/M2";
		UnitConv( 28 ).siName = "K/M";
		UnitConv( 29 ).siName = "KG";
		UnitConv( 30 ).siName = "KG/KG";
		UnitConv( 31 ).siName = "KG/M3";
		UnitConv( 32 ).siName = "KG/S";
		UnitConv( 33 ).siName = "KGWATER/KGAIR";
		UnitConv( 34 ).siName = "KGWATER/SEC";
		UnitConv( 35 ).siName = "KMOL/S";
		UnitConv( 36 ).siName = "KMOL/SEC";
		UnitConv( 37 ).siName = "KWH";
		UnitConv( 38 ).siName = "L";
		UnitConv( 39 ).siName = "L";
		UnitConv( 40 ).siName = "LUM/W";
		UnitConv( 41 ).siName = "LUX";
		UnitConv( 42 ).siName = "M";
		UnitConv( 43 ).siName = "M";
		UnitConv( 44 ).siName = "M/S";
		UnitConv( 45 ).siName = "M/S";
		UnitConv( 46 ).siName = "M2";
		UnitConv( 47 ).siName = "M2/PERSON";
		UnitConv( 48 ).siName = "M3";
		UnitConv( 49 ).siName = "M3";
		UnitConv( 50 ).siName = "M3/M2";
		UnitConv( 51 ).siName = "M3/S";
		UnitConv( 52 ).siName = "M3/S";
		UnitConv( 53 ).siName = "M3/S-M2";
		UnitConv( 54 ).siName = "M3/S-PERSON";
		UnitConv( 55 ).siName = "M3/S-PERSON";
		UnitConv( 56 ).siName = "PA";
		UnitConv( 57 ).siName = "PA";
		UnitConv( 58 ).siName = "PA";
		UnitConv( 59 ).siName = "PA";
		UnitConv( 60 ).siName = "PA";
		UnitConv( 61 ).siName = "PA";
		UnitConv( 62 ).siName = "PA";
		UnitConv( 63 ).siName = "PA";
		UnitConv( 64 ).siName = "S";
		UnitConv( 65 ).siName = "V";
		UnitConv( 66 ).siName = "W";
		UnitConv( 67 ).siName = "W";
		UnitConv( 68 ).siName = "W";
		UnitConv( 69 ).siName = "W";
		UnitConv( 70 ).siName = "W";
		UnitConv( 71 ).siName = "W/KG";
		UnitConv( 72 ).siName = "W/KG H2O";
		UnitConv( 73 ).siName = "W/K";
		UnitConv( 74 ).siName = "W/M2";
		UnitConv( 75 ).siName = "W/M2";
		UnitConv( 76 ).siName = "W/M2-C";
		UnitConv( 77 ).siName = "W/M2-K";
		UnitConv( 78 ).siName = "W/W";
		UnitConv( 79 ).siName = "deltaC";
		UnitConv( 80 ).siName = "KJ/KG";
		UnitConv( 81 ).siName = "W-S/M3";
		UnitConv( 82 ).siName = "W-S/M3";
		UnitConv( 83 ).siName = "~~$~~/m2";
		UnitConv( 84 ).siName = "GJ";
		UnitConv( 85 ).siName = "GJ";
		UnitConv( 86 ).siName = "GJ";
		UnitConv( 87 ).siName = "GJ";
		UnitConv( 88 ).siName = "GJ";
		UnitConv( 89 ).siName = "GJ";
		UnitConv( 90 ).siName = "GJ";
		UnitConv( 91 ).siName = "MJ/m2";
		UnitConv( 92 ).siName = "MJ/m2";
		UnitConv( 93 ).siName = "MJ/m2";
		UnitConv( 94 ).siName = "Invalid/Undefined";

		UnitConv( 1 ).ipName = "%";
		UnitConv( 2 ).ipName = "F";
		UnitConv( 3 ).ipName = "0=Off 1=On";
		UnitConv( 4 ).ipName = "0-No  1-Yes";
		UnitConv( 5 ).ipName = "1-Yes 0-No";
		UnitConv( 6 ).ipName = "A";
		UnitConv( 7 ).ipName = "ACH";
		UnitConv( 8 ).ipName = "ach";
		UnitConv( 9 ).ipName = "base 50F";
		UnitConv( 10 ).ipName = "base 65F";
		UnitConv( 11 ).ipName = "F";
		UnitConv( 12 ).ipName = "cd/in2";
		UnitConv( 13 ).ipName = "deg";
		UnitConv( 14 ).ipName = "Frac";
		UnitConv( 15 ).ipName = "Hour";
		UnitConv( 16 ).ipName = "Hours";
		UnitConv( 17 ).ipName = "hr";
		UnitConv( 18 ).ipName = "hrs";
		UnitConv( 19 ).ipName = "kBtu";
		UnitConv( 20 ).ipName = "kWh";
		UnitConv( 21 ).ipName = "therm";
		UnitConv( 22 ).ipName = "MMBtu";
		UnitConv( 23 ).ipName = "Wh";
		UnitConv( 24 ).ipName = "ton-hrs";
		UnitConv( 25 ).ipName = "Btu/lb";
		UnitConv( 26 ).ipName = "Btu/lbWater";
		UnitConv( 27 ).ipName = "kBtu/sqft";
		UnitConv( 28 ).ipName = "F/ft";
		UnitConv( 29 ).ipName = "lb";
		UnitConv( 30 ).ipName = "lb/lb";
		UnitConv( 31 ).ipName = "lb/ft3";
		UnitConv( 32 ).ipName = "lb/s";
		UnitConv( 33 ).ipName = "lbWater/lbAir";
		UnitConv( 34 ).ipName = "lbWater/s";
		UnitConv( 35 ).ipName = "kmol/s";
		UnitConv( 36 ).ipName = "kmol/sec";
		UnitConv( 37 ).ipName = "kWh";
		UnitConv( 38 ).ipName = "gal";
		UnitConv( 39 ).ipName = "ft3";
		UnitConv( 40 ).ipName = "lum/W";
		UnitConv( 41 ).ipName = "foot-candles";
		UnitConv( 42 ).ipName = "ft";
		UnitConv( 43 ).ipName = "in";
		UnitConv( 44 ).ipName = "ft/min";
		UnitConv( 45 ).ipName = "miles/hr";
		UnitConv( 46 ).ipName = "ft2";
		UnitConv( 47 ).ipName = "ft2/person";
		UnitConv( 48 ).ipName = "ft3";
		UnitConv( 49 ).ipName = "gal";
		UnitConv( 50 ).ipName = "f3/f2";
		UnitConv( 51 ).ipName = "ft3/min";
		UnitConv( 52 ).ipName = "gal/min";
		UnitConv( 53 ).ipName = "ft3/min-ft2";
		UnitConv( 54 ).ipName = "ft3/min-person";
		UnitConv( 55 ).ipName = "gal/min-person";
		UnitConv( 56 ).ipName = "psi";
		UnitConv( 57 ).ipName = "inHg";
		UnitConv( 58 ).ipName = "inH2O";
		UnitConv( 59 ).ipName = "ftH2O";
		UnitConv( 60 ).ipName = "psi";
		UnitConv( 61 ).ipName = "inHg";
		UnitConv( 62 ).ipName = "inH2O";
		UnitConv( 63 ).ipName = "ftH2O";
		UnitConv( 64 ).ipName = "s";
		UnitConv( 65 ).ipName = "V";
		UnitConv( 66 ).ipName = "Btu/h";
		UnitConv( 67 ).ipName = "W";
		UnitConv( 68 ).ipName = "kW";
		UnitConv( 69 ).ipName = "kBtuh";
		UnitConv( 70 ).ipName = "ton";
		UnitConv( 71 ).ipName = "kBtuh/lb";
		UnitConv( 72 ).ipName = "kBtuh/lb";
		UnitConv( 73 ).ipName = "Btu/h-F";
		UnitConv( 74 ).ipName = "Btu/h-ft2";
		UnitConv( 75 ).ipName = "kBtuh/ft2";
		UnitConv( 76 ).ipName = "Btu/h-ft2-F";
		UnitConv( 77 ).ipName = "Btu/h-ft2-F";
		UnitConv( 78 ).ipName = "Btuh/Btuh";
		UnitConv( 79 ).ipName = "deltaF";
		UnitConv( 80 ).ipName = "Btu/lb";
		UnitConv( 81 ).ipName = "W-min/ft3";
		UnitConv( 82 ).ipName = "W-min/gal";
		UnitConv( 83 ).ipName = "~~$~~/ft2";
		UnitConv( 84 ).ipName = "kBtu";
		UnitConv( 85 ).ipName = "kWh";
		UnitConv( 86 ).ipName = "kWh";
		UnitConv( 87 ).ipName = "therm";
		UnitConv( 88 ).ipName = "MMBtu";
		UnitConv( 89 ).ipName = "Wh";
		UnitConv( 90 ).ipName = "ton-hrs";
		UnitConv( 91 ).ipName = "kWh/ft2";
		UnitConv( 92 ).ipName = "kBtu/ft2";
		UnitConv( 93 ).ipName = "kBtu/ft2";
		UnitConv( 94 ).ipName = "Invalid/Undefined";

		UnitConv( 1 ).mult = 1.0;
		UnitConv( 2 ).mult = 1.8;
		UnitConv( 3 ).mult = 1.0;
		UnitConv( 4 ).mult = 1.0;
		UnitConv( 5 ).mult = 1.0;
		UnitConv( 6 ).mult = 1.0;
		UnitConv( 7 ).mult = 1.0;
		UnitConv( 8 ).mult = 1.0;
		UnitConv( 9 ).mult = 1.8;
		UnitConv( 10 ).mult = 1.8;
		UnitConv( 11 ).mult = 1.8;
		UnitConv( 12 ).mult = 0.000645160041625726;
		UnitConv( 13 ).mult = 1.0;
		UnitConv( 14 ).mult = 1.0;
		UnitConv( 15 ).mult = 1.0;
		UnitConv( 16 ).mult = 1.0;
		UnitConv( 17 ).mult = 1.0;
		UnitConv( 18 ).mult = 1.0;
		UnitConv( 19 ).mult = 0.00000094845;
		UnitConv( 20 ).mult = 0.000000277778;
		UnitConv( 21 ).mult = 0.0000000094845;
		UnitConv( 22 ).mult = 0.00000000094845;
		UnitConv( 23 ).mult = 0.000277777777777778;
		UnitConv( 24 ).mult = 0.0000000789847;
		UnitConv( 25 ).mult = 0.00042956;
		UnitConv( 26 ).mult = 0.0000004302105;
		UnitConv( 27 ).mult = 0.00000008811404;
		UnitConv( 28 ).mult = 0.54861322767449;
		UnitConv( 29 ).mult = 2.2046;
		UnitConv( 30 ).mult = 1.0;
		UnitConv( 31 ).mult = 0.062428;
		UnitConv( 32 ).mult = 2.2046;
		UnitConv( 33 ).mult = 1.0;
		UnitConv( 34 ).mult = 2.2046;
		UnitConv( 35 ).mult = 1.0;
		UnitConv( 36 ).mult = 1.0;
		UnitConv( 37 ).mult = 1.0;
		UnitConv( 38 ).mult = 0.264172037284185;
		UnitConv( 39 ).mult = 0.0353146624712848;
		UnitConv( 40 ).mult = 1.0;
		UnitConv( 41 ).mult = 0.092902267;
		UnitConv( 42 ).mult = 3.281;
		UnitConv( 43 ).mult = 39.37;
		UnitConv( 44 ).mult = 196.86;
		UnitConv( 45 ).mult = 2.2369;
		UnitConv( 46 ).mult = 10.764961;
		UnitConv( 47 ).mult = 10.764961;
		UnitConv( 48 ).mult = 35.319837041;
		UnitConv( 49 ).mult = 264.172;
		UnitConv( 50 ).mult = 3.281;
		UnitConv( 51 ).mult = 2118.6438;
		UnitConv( 52 ).mult = 15852.0;
		UnitConv( 53 ).mult = 196.85;
		UnitConv( 54 ).mult = 2118.6438;
		UnitConv( 55 ).mult = 15852.0;
		UnitConv( 56 ).mult = 0.0001450377;
		UnitConv( 57 ).mult = 0.00029613;
		UnitConv( 58 ).mult = 0.00401463;
		UnitConv( 59 ).mult = 0.00033455;
		UnitConv( 60 ).mult = 0.0001450377;
		UnitConv( 61 ).mult = 0.00029613;
		UnitConv( 62 ).mult = 0.00401463;
		UnitConv( 63 ).mult = 0.00033455;
		UnitConv( 64 ).mult = 1.0;
		UnitConv( 65 ).mult = 1.0;
		UnitConv( 66 ).mult = 3.412;
		UnitConv( 67 ).mult = 1.0;
		UnitConv( 68 ).mult = 0.001;
		UnitConv( 69 ).mult = 0.00341442;
		UnitConv( 70 ).mult = 0.0002843333;
		UnitConv( 71 ).mult = 0.001547673;
		UnitConv( 72 ).mult = 0.001547673;
		UnitConv( 73 ).mult = 1.8987;
		UnitConv( 74 ).mult = 0.316954237;
		UnitConv( 75 ).mult = 0.000316954237;
		UnitConv( 76 ).mult = 0.176085687;
		UnitConv( 77 ).mult = 0.176085687;
		UnitConv( 78 ).mult = 1.0;
		UnitConv( 79 ).mult = 1.8;
		UnitConv( 80 ).mult = 0.42956;
		UnitConv( 81 ).mult = 1.0 / 2118.6438;
		UnitConv( 82 ).mult = 1.0 / 15852;
		UnitConv( 83 ).mult = 1.0 / 10.764961;
		UnitConv( 84 ).mult = 0.00000094845 * 1000000000;
		UnitConv( 85 ).mult = 0.000000277778 * 1000000000;
		UnitConv( 86 ).mult = 0.000000277778 * 1000000000;
		UnitConv( 87 ).mult = 0.0000000094845 * 1000000000;
		UnitConv( 88 ).mult = 0.00000000094845 * 1000000000;
		UnitConv( 89 ).mult = 0.000277777777777778 * 1000000000;
		UnitConv( 90 ).mult = 0.0000000789847 * 1000000000;
		UnitConv( 91 ).mult = 0.277777777777778 / 10.764961;
		UnitConv( 92 ).mult = 0.94708628903179 / 10.764961;
		UnitConv( 93 ).mult = 0.94708628903179 / 10.764961;
		UnitConv( 94 ).mult = 1.0;

		UnitConv( 2 ).offset = 32.0;
		UnitConv( 11 ).offset = 32.0;
		UnitConv( 25 ).offset = 7.6736;
		UnitConv( 80 ).offset = 7.6736; // 80 is KJ/KG -- should this be multiplied by 1000?

		UnitConv( 20 ).hint = "ELEC";
		UnitConv( 21 ).hint = "GAS";
		UnitConv( 24 ).hint = "COOL";
		UnitConv( 38 ).hint = "WATER";
		UnitConv( 49 ).hint = "WATER";
		UnitConv( 52 ).hint = "WATER";
		UnitConv( 67 ).hint = "ELEC";
		UnitConv( 70 ).hint = "COOL";
		UnitConv( 82 ).hint = "WATER";
		UnitConv( 85 ).hint = "CONSUMP";
		UnitConv( 86 ).hint = "ELEC";
		UnitConv( 87 ).hint = "GAS";
		UnitConv( 90 ).hint = "COOL";
		UnitConv( 91 ).hint = "ELEC";
		UnitConv( 92 ).hint = "GAS";
		UnitConv( 92 ).hint = "ADDITIONAL";

		UnitConv( 19 ).several = true;
		UnitConv( 20 ).several = true;
		UnitConv( 21 ).several = true;
		UnitConv( 22 ).several = true;
		UnitConv( 23 ).several = true;
		UnitConv( 24 ).several = true;
		UnitConv( 38 ).several = true;
		UnitConv( 39 ).several = true;
		UnitConv( 42 ).several = true;
		UnitConv( 43 ).several = true;
		UnitConv( 44 ).several = true;
		UnitConv( 45 ).several = true;
		UnitConv( 48 ).several = true;
		UnitConv( 49 ).several = true;
		UnitConv( 51 ).several = true;
		UnitConv( 52 ).several = true;
		UnitConv( 54 ).several = true;
		UnitConv( 55 ).several = true;
		UnitConv( 56 ).several = true;
		UnitConv( 57 ).several = true;
		UnitConv( 58 ).several = true;
		UnitConv( 59 ).several = true;
		UnitConv( 60 ).several = true;
		UnitConv( 61 ).several = true;
		UnitConv( 62 ).several = true;
		UnitConv( 63 ).several = true;
		UnitConv( 66 ).several = true;
		UnitConv( 67 ).several = true;
		UnitConv( 68 ).several = true;
		UnitConv( 69 ).several = true;
		UnitConv( 70 ).several = true;
		UnitConv( 74 ).several = true;
		UnitConv( 75 ).several = true;
		UnitConv( 81 ).several = true;
		UnitConv( 82 ).several = true;
		UnitConv( 84 ).several = true;
		UnitConv( 85 ).several = true;
		UnitConv( 86 ).several = true;
		UnitConv( 87 ).several = true;
		UnitConv( 88 ).several = true;
		UnitConv( 89 ).several = true;
		UnitConv( 90 ).several = true;
		UnitConv( 91 ).several = true;
		UnitConv( 92 ).several = true;
	}

	std::string
	GetUnitSubString( std::string const & inString ) // Input String
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   return the substring contained in brackets []
		//   that contains the units.

		// METHODOLOGY EMPLOYED:
		//   na

		// Return value
		std::string outUnit; // Result String

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		//check if string has brackets or parentheses
		std::string::size_type const posLBrac = index( inString, '[' ); // left bracket
		std::string::size_type const posRBrac = index( inString, ']' ); // right bracket
		//extract the substring with the units
		if ( ( posLBrac != std::string::npos ) && ( posRBrac != std::string::npos ) && ( posRBrac - posLBrac >= 2 ) ) {
			outUnit = inString.substr( posLBrac + 1, posRBrac - posLBrac - 1 );
		}
		return outUnit;
	}

	void
	LookupSItoIP(
		std::string const & stringInWithSI,
		int & unitConvIndex,
		std::string & stringOutWithIP
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   February 12, 2009
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   The input string to this subroutine can either contain
		//   a unit that should be looked up or it can contain
		//   but the unit and other text (such as the column heading)
		//   that includes a hint for when the unit may have multiple
		//   possible conversions. If the input string includes
		//   just the unit it does not have either brackets or
		//   parenthesis. If the string includes text with a possible
		//   hint the units themselves will be in either brackets
		//   or parentheses. The index to the unitConv array is returned
		//   which can be used with the convertIP function. Also the
		//   string with the IP units substituted is returned.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		//    na

		// USE STATEMENTS:
		//    na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//    na

		// INTERFACE BLOCK SPECIFICATIONS:
		//    na

		// DERIVED TYPE DEFINITIONS:
		//    na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//    na
		std::string unitSIOnly;
		int modeInString;
		int const misBrac( 1 );
		int const misParen( 2 );
		int const misNoHint( 3 );
		std::string const stringInUpper( MakeUPPERCase( stringInWithSI ) );

		stringOutWithIP = "";
		//check if string has brackets or parentheses
		std::string::size_type posLBrac = index( stringInUpper, '[' ); // left bracket
		std::string::size_type posRBrac = index( stringInUpper, ']' ); // right bracket
		std::string::size_type posLParen = index( stringInUpper, '(' ); // left parenthesis
		std::string::size_type posRParen = index( stringInUpper, ')' ); // right parenthesis
		//extract the substring with the units
		if ( ( posLBrac != std::string::npos ) && ( posRBrac != std::string::npos ) && ( posRBrac - posLBrac >= 2 ) ) {
			unitSIOnly = stringInUpper.substr( posLBrac + 1, posRBrac - posLBrac - 1 );
			modeInString = misBrac;
		} else if ( ( posLParen != std::string::npos ) && ( posRParen != std::string::npos ) && ( posRParen - posLParen >= 2 ) ) {
			unitSIOnly = stringInUpper.substr( posLParen + 1, posRParen - posLParen - 1 );
			modeInString = misParen;
		} else {
			unitSIOnly = stringInUpper;
			modeInString = misNoHint;
		}
		int defaultConv = 0;
		int foundConv = 0;
		int firstOfSeveral = 0;
		for ( int iUnit = 1; iUnit <= UnitConvSize; ++iUnit ) {
			if ( SameString( UnitConv( iUnit ).siName, unitSIOnly ) ) {
				if ( UnitConv( iUnit ).several ) {
					if ( firstOfSeveral == 0 ) firstOfSeveral = iUnit;
					if ( UnitConv( iUnit ).is_default ) defaultConv = iUnit;
					// look for the hint string
					if ( len( UnitConv( iUnit ).hint ) > 0 ) {
						if ( has( stringInUpper, UnitConv( iUnit ).hint ) ) {
							foundConv = iUnit;
							break;
						}
					}
				} else { //not several possibilities so don't bother with rest of array
					foundConv = iUnit;
					break;
				}
			}
		}
		// if it is found set the selected value to what was found. if not found,
		// directly set it to the default and if no default set it to the first item
		// in group.  Return zero if not found.
		int selectedConv( 0 );
		if ( foundConv > 0 ) {
			selectedConv = foundConv;
		} else {
			// not found - see if in a group it should be default or first.
			if ( firstOfSeveral == 0 ) {
				selectedConv = 0;
			} else {
				if ( defaultConv != 0 ) {
					selectedConv = defaultConv;
				} else {
					selectedConv = firstOfSeveral;
				}
			}
		}
		// if one was selected substitute the units into the output string
		if ( selectedConv > 0 ) {
			if ( modeInString == misBrac ) {
				stringOutWithIP = stringInWithSI.substr( 0, posLBrac + 1 ) + UnitConv( selectedConv ).ipName + stringInWithSI.substr( posRBrac );
			} else if ( modeInString == misParen ) {
				stringOutWithIP = stringInWithSI.substr( 0, posLParen + 1 ) + UnitConv( selectedConv ).ipName + stringInWithSI.substr( posRParen );
			} else if ( modeInString == misNoHint ) {
				stringOutWithIP = UnitConv( selectedConv ).ipName;
			}
		} else {
			// if no conversion just output the input string
			stringOutWithIP = stringInWithSI;
		}
		// For debugging only
		//CALL  ShowWarningError('LookupSItoIP in: ' // TRIM(stringInWithSI) // ' out: ' // TRIM(stringOutWithIP))
		//IF (foundConv .NE. 0) CALL  ShowWarningError('   Hint ' // TRIM(UnitConv(foundConv)%hint) // IntToStr(foundConv) )
		unitConvIndex = selectedConv;
	}

	Real64
	ConvertIP(
		int const unitConvIndex,
		Real64 const SIvalue
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   February 13, 2009
		//    MODIFIED       September 2012
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Apply the selected unit conversion to the input value
		//   expressed in SI units to result in IP units.
		//   If zero is provided as unit index, return the original
		//   value (no conversion)

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		//    na

		// USE STATEMENTS:

		// Return value
		Real64 ConvertIP;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//    na

		// INTERFACE BLOCK SPECIFICATIONS:
		//    na

		// DERIVED TYPE DEFINITIONS:
		//    na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//    na
		if ( unitConvIndex == 0 ) {
			ConvertIP = SIvalue;
		} else if ( ( unitConvIndex > 0 ) && ( unitConvIndex <= UnitConvSize ) ) {
			ConvertIP = ( SIvalue * UnitConv( unitConvIndex ).mult ) + UnitConv( unitConvIndex ).offset;
		} else {
			ConvertIP = 0.0;
		}
		return ConvertIP;
	}

	Real64
	ConvertIPdelta(
		int const unitConvIndex,
		Real64 const SIvalue
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   February 18, 2009
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Apply the selected unit conversion to the input value
		//   expressed in SI units to result in IP units. This routine
		//   only uses the mulitplier and NOT the offset and is appropriate
		//   when the number being converted is a difference or delta
		//   between values (such as a temperature difference).

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		//    na

		// USE STATEMENTS:

		// Return value
		Real64 ConvertIPdelta;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//    na

		// INTERFACE BLOCK SPECIFICATIONS:
		//    na

		// DERIVED TYPE DEFINITIONS:
		//    na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//    na

		if ( ( unitConvIndex > 0 ) && ( unitConvIndex <= UnitConvSize ) ) {
			ConvertIPdelta = SIvalue * UnitConv( unitConvIndex ).mult;
		} else {
			ConvertIPdelta = 0.0;
		}
		return ConvertIPdelta;
	}

	void
	GetUnitConversion(
		int const unitConvIndex,
		Real64 & multiplier,
		Real64 & offset,
		std::string & IPunit
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   February 13, 2009
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Return of the multiplier and adder for the given
		//   SI to IP unit conversion.
		//     SI = (IP * multipier) + offset
		//  This function could be replaced by referencing the
		//  array directly but does include some checking of the
		//  bounds of the array.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		//    na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//    na

		// INTERFACE BLOCK SPECIFICATIONS:
		//    na

		// DERIVED TYPE DEFINITIONS:
		//    na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//    na
		if ( ( unitConvIndex > 0 ) && ( unitConvIndex <= UnitConvSize ) ) {
			multiplier = UnitConv( unitConvIndex ).mult;
			offset = UnitConv( unitConvIndex ).offset;
			IPunit = UnitConv( unitConvIndex ).ipName;
		} else {
			multiplier = 0.0;
			offset = 0.0;
			IPunit = "";
		}
	}

	Real64
	getSpecificUnitMultiplier(
		std::string const & SIunit,
		std::string const & IPunit
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   February 13, 2009
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Return of the multiplier for a specific
		//   SI to IP unit conversion. No offset is provided so
		//   it cannot be used to convert units such as temperatures
		//   that include an offset.
		//     SI = (IP * multipier) + offset
		//   Unlike LookupSItoIP, this function does not expect more
		//   the units in the two input parameters. No hints or
		//   defaults are used since both the SI and IP units are
		//   input by the user.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		//    na

		// USE STATEMENTS:

		// Return value
		Real64 getSpecificUnitMultiplier;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//    na

		// INTERFACE BLOCK SPECIFICATIONS:
		//    na

		// DERIVED TYPE DEFINITIONS:
		//    na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int found( 0 );
		static int iUnit( 0 );

		for ( iUnit = 1; iUnit <= UnitConvSize; ++iUnit ) {
			if ( SameString( UnitConv( iUnit ).siName, SIunit ) ) {
				if ( SameString( UnitConv( iUnit ).ipName, IPunit ) ) {
					found = iUnit;
					break;
				}
			}
		}
		if ( found != 0 ) {
			getSpecificUnitMultiplier = UnitConv( found ).mult;
		} else {
			getSpecificUnitMultiplier = 0.0;
		}
		return getSpecificUnitMultiplier;
	}

	Real64
	getSpecificUnitDivider(
		std::string const & SIunit,
		std::string const & IPunit
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   February 13, 2009
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Returns the divider (1/multiplier) for a specific
		//   SI to IP unit conversion. No offset is provided so
		//   it cannot be used to convert units such as temperatures
		//   that include an offset.
		//     SI = (IP * multipier) + offset
		//   Unlike LookupSItoIP, this function does not expect more
		//   the units in the two input parameters. No hints or
		//   defaults are used since both the SI and IP units are
		//   input by the user.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		//    na

		// USE STATEMENTS:

		// Return value
		Real64 getSpecificUnitDivider;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//    na

		// INTERFACE BLOCK SPECIFICATIONS:
		//    na

		// DERIVED TYPE DEFINITIONS:
		//    na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 mult;

		mult = getSpecificUnitMultiplier( SIunit, IPunit );
		if ( mult != 0 ) {
			getSpecificUnitDivider = 1 / mult;
		} else {
			getSpecificUnitDivider = 0.0;
		}
		return getSpecificUnitDivider;
	}

	Real64
	getSpecificUnitIndex(
		std::string const & SIunit,
		std::string const & IPunit
	)
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   September 21, 2012
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Return of the unit conversion index for a specific
		//   SI to IP unit conversion.
		//   Unlike LookupSItoIP, this function does not expect more
		//   the units in the two input parameters. No hints or
		//   defaults are used since both the SI and IP units are
		//   input by the user.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		//    na

		// USE STATEMENTS:

		// Return value
		Real64 getSpecificUnitIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//    na

		// INTERFACE BLOCK SPECIFICATIONS:
		//    na

		// DERIVED TYPE DEFINITIONS:
		//    na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int found( 0 );
		static int iUnit( 0 );

		for ( iUnit = 1; iUnit <= UnitConvSize; ++iUnit ) {
			if ( SameString( UnitConv( iUnit ).siName, SIunit ) ) {
				if ( SameString( UnitConv( iUnit ).ipName, IPunit ) ) {
					found = iUnit;
					break;
				}
			}
		}
		if ( found != 0 ) {
			getSpecificUnitIndex = found;
		} else {
			getSpecificUnitIndex = 0.0;
		}
		return getSpecificUnitIndex;
	}

} // OutputReportTabular

} // EnergyPlus
