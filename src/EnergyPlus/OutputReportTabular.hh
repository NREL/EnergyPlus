// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/OutputProcessor.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace OutputReportTabular {

    // Using/Aliasing

    // Data
    // MODULE PARAMETER DEFINITIONS:

    enum class iAggType
    {
        Unassigned,
        SumOrAvg,
        Maximum,
        Minimum,
        ValueWhenMaxMin,
        HoursZero,
        HoursNonZero,
        HoursPositive,
        HoursNonPositive,
        HoursNegative,
        HoursNonNegative,
        SumOrAverageHoursShown,
        MaximumDuringHoursShown,
        MinimumDuringHoursShown,
    };

    enum class iTableStyle
    {
        Unassigned,
        Comma,
        Tab,
        Fixed,
        HTML,
        XML,
    };

    enum class iUnitsStyle
    {
        None,
        JtoKWH,
        JtoMJ,
        JtoGJ,
        InchPound,
        NotFound,
    };

    // These correspond to the columns in the load component table
    constexpr int cSensInst(1);
    constexpr int cSensDelay(2);
    constexpr int cSensRA(3);
    constexpr int cLatent(4);
    constexpr int cTotal(5);
    constexpr int cPerc(6);
    constexpr int cArea(7);
    constexpr int cPerArea(8);

    // internal gains
    constexpr int rPeople(1);
    constexpr int rLights(2);
    constexpr int rEquip(3);
    constexpr int rRefrig(4);
    constexpr int rWaterUse(5);
    constexpr int rHvacLoss(6);
    constexpr int rPowerGen(7);
    // misc
    constexpr int rDOAS(8);
    constexpr int rInfil(9);
    constexpr int rZoneVent(10);
    constexpr int rIntZonMix(11);
    // opaque surfaces
    constexpr int rRoof(12);
    constexpr int rIntZonCeil(13);
    constexpr int rOtherRoof(14);
    constexpr int rExtWall(15);
    constexpr int rIntZonWall(16);
    constexpr int rGrdWall(17);
    constexpr int rOtherWall(18);
    constexpr int rExtFlr(19);
    constexpr int rIntZonFlr(20);
    constexpr int rGrdFlr(21);
    constexpr int rOtherFlr(22);
    // subsurfaces
    constexpr int rFeneCond(23);
    constexpr int rFeneSolr(24);
    constexpr int rOpqDoor(25);
    // total
    constexpr int rGrdTot(26);

    // BEPS Report Related Variables
    // From Report:Table:Predefined - BEPS
    constexpr int numResourceTypes(14);
    constexpr int numSourceTypes(12);

    constexpr const char *validChars("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_:.");

    enum class iOutputType
    {
        zoneOutput,
        airLoopOutput,
        facilityOutput,
    };

    // MODULE VARIABLE DECLARATIONS:

    // The Binned table type is different and only references one variable and its structure is very
    // different from the others so it is has its own type.

    constexpr int numNamedMonthly(63);
    // These reports are detailed/named in routine InitializePredefinedMonthlyTitles

    // Allow up to five output files to be created
    constexpr int maxNumStyles(5);

    // LineTypes for reading the stat file
    enum class StatLineType
    {
        Initialized, // used as a dummy placeholder
        StatisticsLine,
        LocationLine,
        LatLongLine,
        ElevationLine,
        StdPressureLine,
        DataSourceLine,
        WMOStationLine,
        DesignConditionsLine,
        heatingConditionsLine,
        coolingConditionsLine,
        stdHDDLine,
        stdCDDLine,
        maxDryBulbLine,
        minDryBulbLine,
        maxDewPointLine,
        minDewPointLine,
        wthHDDLine,
        wthCDDLine,
        KoppenLine,
        KoppenDes1Line,
        KoppenDes2Line,
        AshStdLine,
        AshStdDes1Line,
        AshStdDes2Line,
        AshStdDes3Line,
    };

    // Types

    struct OutputTableBinnedType
    {
        // Members
        std::string keyValue;   // the key value (usually an asterisk to indicate all variables
        std::string varOrMeter; // the name of the variable or meter
        Real64 intervalStart;   // The lowest value for the intervals being binned into.
        Real64 intervalSize;    // The size of the bins starting with Interval start.
        int intervalCount;      // The number of bins used. The number of hours below the start of
        // the lowest bin and above the value of the last bin are also shown.
        int resIndex; // result index - pointer to BinResults array
        int numTables;
        OutputProcessor::VariableType typeOfVar;
        OutputProcessor::StoreType avgSum;      // Variable  is Averaged=1 or Summed=2
        OutputProcessor::TimeStepType stepType; // Variable time step is Zone=1 or HVAC=2
        OutputProcessor::Unit units;            // the units enumeration
        std::string ScheduleName;               // the name of the schedule
        int scheduleIndex;                      // index to the schedule specified - if no schedule use zero

        // Default Constructor
        OutputTableBinnedType()
            : intervalStart(0.0), intervalSize(0.0), intervalCount(0), resIndex(0), numTables(0), typeOfVar(OutputProcessor::VariableType::NotFound),
              avgSum(OutputProcessor::StoreType::Averaged), stepType(OutputProcessor::TimeStepType::TimeStepZone), scheduleIndex(0)
        {
        }
    };

    struct BinResultsType
    {
        // Members
        Array1D<Real64> mnth; // monthly bins
        Array1D<Real64> hrly; // hourly bins

        // Default Constructor
        BinResultsType() : mnth(12, 0.0), hrly(24, 0.0)
        {
        }
    };

    struct BinObjVarIDType
    {
        // Members
        std::string namesOfObj; // name of the object
        int varMeterNum;        // variable or meter number

        // Default Constructor
        BinObjVarIDType() : varMeterNum(0)
        {
        }
    };

    struct BinStatisticsType
    {
        // Members
        Real64 sum;     // sum of the variable
        Real64 sum2;    // sum of the variable squared
        int n;          // number of items in sum
        Real64 minimum; // minimum value
        Real64 maximum; // maximum value

        // Default Constructor
        BinStatisticsType() : sum(0.0), sum2(0.0), n(0), minimum(0.0), maximum(0.0)
        {
        }
    };

    struct NamedMonthlyType
    {
        // Members
        std::string title; // report title
        bool show;         // if report should be shown

        // Default Constructor
        NamedMonthlyType() : show(false)
        {
        }
    };

    struct MonthlyInputType
    {
        // Members
        std::string name;  // identifier
        int numFieldSet;   // number of monthly field sets
        int firstFieldSet; // pointer to the first field set
        int numTables;     // number of tables
        int firstTable;    // pointer to the first table
        int showDigits;    // the number of digits to be shown

        // Default Constructor
        MonthlyInputType() : numFieldSet(0), firstFieldSet(0), numTables(0), firstTable(0), showDigits(0)
        {
        }
    };

    struct MonthlyFieldSetInputType
    {
        // Members
        std::string variMeter;          // the name of the variable or meter
        std::string colHead;            // the column header to use instead of the variable name (only for predefined)
        iAggType aggregate;             // the type of aggregation for the variable (see aggType parameters)
        OutputProcessor::Unit varUnits; // Units enumeration
        std::string variMeterUpper;     // the name of the variable or meter uppercased
        OutputProcessor::VariableType typeOfVar;
        int keyCount;                              // noel
        OutputProcessor::StoreType varAvgSum;      // Variable  is Averaged=1 or Summed=2
        OutputProcessor::TimeStepType varStepType; // Variable time step is Zone=1 or HVAC=2
        Array1D_string NamesOfKeys;                // keyNames !noel
        Array1D_int IndexesForKeyVar;              // keyVarIndexes !noel

        // Default Constructor
        MonthlyFieldSetInputType()
            : aggregate(iAggType::Unassigned), varUnits(OutputProcessor::Unit::None), typeOfVar(OutputProcessor::VariableType::NotFound), keyCount(0),
              varAvgSum(OutputProcessor::StoreType::Averaged), varStepType(OutputProcessor::TimeStepType::TimeStepZone)
        {
        }
    };

    struct MonthlyTablesType
    {
        // Members
        std::string keyValue; // the key value - the object names that result in the variable
        int firstColumn;      // pointer to the monthly column array for the first item
        int numColumns;       // number of columns for the table

        // Default Constructor
        MonthlyTablesType() : firstColumn(0), numColumns(0)
        {
        }
    };

    struct MonthlyColumnsType
    {
        // Members
        std::string varName;                     // name of variable
        std::string colHead;                     // column header (not used for user defined monthly)
        int varNum;                              // variable or meter number
        OutputProcessor::VariableType typeOfVar; // 0=not found, 1=integer, 2=real, 3=meter
        OutputProcessor::StoreType avgSum;       // Variable  is Averaged=1 or Summed=2
        OutputProcessor::TimeStepType stepType;  // Variable time step is Zone=1 or HVAC=2
        OutputProcessor::Unit units;             // the units string, may be blank
        iAggType aggType;                        // index to the type of aggregation (see list of parameters)
        Array1D<Real64> reslt;                   // monthly results
        Array1D<Real64> duration;                // the time during which results are summed for use in averages
        Array1D_int timeStamp;                   // encoded timestamp of max or min
        Real64 aggForStep;                       // holds the aggregation for the HVAC time steps when smaller than
        // the zone timestep

        // Default Constructor
        MonthlyColumnsType()
            : varNum(0), typeOfVar(OutputProcessor::VariableType::NotFound), avgSum(OutputProcessor::StoreType::Averaged),
              stepType(OutputProcessor::TimeStepType::TimeStepZone), units(OutputProcessor::Unit::None), aggType(iAggType::Unassigned),
              reslt(12, 0.0), duration(12, 0.0), timeStamp(12, 0), aggForStep(0.0)
        {
        }
    };

    struct TOCEntriesType
    {
        // Members
        std::string reportName;  // the name of the individual report
        std::string sectionName; // the name of the section containing individual reports
        bool isWritten;          // flag if the entry has been written to TOC

        // Default Constructor
        TOCEntriesType() : isWritten(false)
        {
        }
    };

    struct UnitConvType
    {
        // Members
        std::string siName; // the name abbreviation or symbol of the SI units
        std::string ipName; // the name abbreviation or symbol of the IP units
        Real64 mult;        // the multiplier used to convert from SI to IP in IP = (SI * mult) + offset
        Real64 offset;      // the offset used to convert from SI to IP in IP = (SI * mult) + offset
        std::string hint;   // the string used when multiple SI units match
        bool several;       // several different options for the SI unit to be converted into IP
        bool is_default;    // if part of a set of "several" this should be used as default

        // Default Constructor
        UnitConvType() : mult(1.0), offset(0.0), several(false), is_default(false)
        {
        }
    };

    struct CompLoadTablesType
    {
        // members
        int desDayNum;             // design day number
        int timeStepMax;           // times step of the day that the maximum occurs
        Array2D<Real64> cells;     // main component table results (column, row)
        Array2D_bool cellUsed;     // flag if the cell is used for the table of results (column, row)
        std::string peakDateHrMin; // string containing peak timestamp
        Real64 outsideDryBulb;     // outside dry bulb temperature at peak
        Real64 outsideWetBulb;     // outside wet bulb temperature at peak
        Real64 outsideHumRatio;    // outside humidity ratio at peak
        Real64 zoneDryBulb;        // zone dry bulb temperature at peak
        Real64 zoneRelHum;         // zone relative humidity at peak
        Real64 zoneHumRatio;       // zone humidity ratio at peak

        Real64 supAirTemp;     // supply air temperature
        Real64 mixAirTemp;     // mixed air temperature
        Real64 mainFanAirFlow; // main fan air flow
        Real64 outsideAirFlow; // outside air flow
        Real64 designPeakLoad; // design peak load
        Real64 diffDesignPeak; // difference between Design and Peak Load

        Real64 peakDesSensLoad;    // peak design sensible load
        Real64 estInstDelSensLoad; // estimated instant plus delayed sensible load
        Real64 diffPeakEst;        // difference between the peak design sensible load and the estimated instant plus delayed sensible load
        Array1D_int zoneIndices;   // the zone numbers covered by the report

        Real64 outsideAirRatio;   // outside Air
        Real64 floorArea;         // floor area
        Real64 airflowPerFlrArea; // airflow per floor area
        Real64 airflowPerTotCap;  // airflow per total capacity
        Real64 areaPerTotCap;     // area per total capacity
        Real64 totCapPerArea;     // total capacity per area
        Real64 chlPumpPerFlow;    // chiller pump power per flow
        Real64 cndPumpPerFlow;    // condenser pump power per flow
        Real64 numPeople;         // number of people

        // default constructor
        CompLoadTablesType()
            : desDayNum(0), timeStepMax(0), outsideDryBulb(0.), outsideWetBulb(0.), outsideHumRatio(0.), zoneDryBulb(0.), zoneRelHum(0.),
              supAirTemp(0.), mixAirTemp(0.), mainFanAirFlow(0.), outsideAirFlow(0.), designPeakLoad(0.), diffDesignPeak(0.), peakDesSensLoad(0.),
              estInstDelSensLoad(0.), diffPeakEst(0.), outsideAirRatio(0.), floorArea(0.), airflowPerFlrArea(0.), airflowPerTotCap(0.),
              areaPerTotCap(0.), totCapPerArea(0.), chlPumpPerFlow(0.), cndPumpPerFlow(0.), numPeople(0.)

        {
        }
    };

    struct ZompComponentAreasType
    {
        // members
        Real64 floor;
        Real64 roof;
        Real64 ceiling;
        Real64 extWall;
        Real64 intZoneWall;
        Real64 grndCntWall;
        Real64 extFloor;
        Real64 intZoneFloor;
        Real64 grndCntFloor;
        Real64 fenestration;
        Real64 door;

        // default constructor
        ZompComponentAreasType()
            : floor(0.), roof(0.), ceiling(0.), extWall(0.), intZoneWall(0.), grndCntWall(0.), extFloor(0.), intZoneFloor(0.), grndCntFloor(0.),
              fenestration(0.), door(0.)
        {
        }
    };

    // Functions

    std::ofstream &open_tbl_stream(EnergyPlusData &state, int const iStyle, fs::path const &filePath, bool output_to_file = true);

    void UpdateTabularReports(EnergyPlusData &state, OutputProcessor::TimeStepType t_timeStepType); // What kind of data to update (Zone, HVAC)

    //======================================================================================================================
    //======================================================================================================================

    //    GET INPUT ROUTINES

    //======================================================================================================================
    //======================================================================================================================

    void GetInputTabularMonthly(EnergyPlusData &state);

    int AddMonthlyReport(EnergyPlusData &state, std::string const &inReportName, int const inNumDigitsShown);

    void AddMonthlyFieldSetInput(
        EnergyPlusData &state, int const inMonthReport, std::string const &inVariMeter, std::string const &inColHead, iAggType const inAggregate);

    void InitializeTabularMonthly(EnergyPlusData &state);

    bool isInvalidAggregationOrder(EnergyPlusData &state);

    void GetInputTabularTimeBins(EnergyPlusData &state);

    bool warningAboutKeyNotFound(EnergyPlusData &state, int foundIndex, int inObjIndex, std::string const &moduleName);

    void GetInputTabularStyle(EnergyPlusData &state);

    iUnitsStyle SetUnitsStyleFromString(std::string const &unitStringIn);

    void GetInputOutputTableSummaryReports(EnergyPlusData &state);

    bool isCompLoadRepReq(EnergyPlusData &state);

    bool hasSizingPeriodsDays(EnergyPlusData &state);

    void InitializePredefinedMonthlyTitles(EnergyPlusData &state);

    void CreatePredefinedMonthlyReports(EnergyPlusData &state);

    void GetInputFuelAndPollutionFactors(EnergyPlusData &state);

    //======================================================================================================================
    //======================================================================================================================

    //    OTHER INITIALIZATION ROUTINES

    //======================================================================================================================
    //======================================================================================================================

    void OpenOutputTabularFile(EnergyPlusData &state);

    void CloseOutputTabularFile(EnergyPlusData &state);

    void WriteTableOfContents(EnergyPlusData &state);

    //======================================================================================================================
    //======================================================================================================================

    //    GATHER DATA EACH TIME STEP ROUTINES

    //======================================================================================================================
    //======================================================================================================================

    void GatherBinResultsForTimestep(EnergyPlusData &state, OutputProcessor::TimeStepType t_timeStepType); // What kind of data to update (Zone, HVAC)

    void GatherMonthlyResultsForTimestep(EnergyPlusData &state,
                                         OutputProcessor::TimeStepType t_timeStepType); // What kind of data to update (Zone, HVAC)

    void GatherBEPSResultsForTimestep(EnergyPlusData &state,
                                      OutputProcessor::TimeStepType t_timeStepType); // What kind of data to update (Zone, HVAC)

    void GatherSourceEnergyEndUseResultsForTimestep(EnergyPlusData &state,
                                                    OutputProcessor::TimeStepType t_timeStepType); // What kind of data to update (Zone, HVAC)

    void GatherPeakDemandForTimestep(EnergyPlusData &state, OutputProcessor::TimeStepType t_timeStepType); // What kind of data to update (Zone, HVAC)

    void GatherHeatGainReport(EnergyPlusData &state, OutputProcessor::TimeStepType t_timeStepType); // What kind of data to update (Zone, HVAC)

    void GatherHeatEmissionReport(EnergyPlusData &state, OutputProcessor::TimeStepType t_timeStepType);

    void CalcHeatEmissionReport(EnergyPlusData &state);

    //======================================================================================================================
    //======================================================================================================================

    //    WRITE OUTPUT FILE ROUTINES

    //======================================================================================================================
    //======================================================================================================================

    void WriteTabularReports(EnergyPlusData &state);

    void parseStatLine(const std::string &lineIn,
                       StatLineType &lineType,
                       bool &desConditionlinepassed,
                       bool &heatingDesignlinepassed,
                       bool &coolingDesignlinepassed,
                       bool &isKoppen);

    void FillWeatherPredefinedEntries(EnergyPlusData &state);

    std::string GetColumnUsingTabs(std::string const &inString, // Input String
                                   int const colNum             // Column number
    );

    void FillRemainingPredefinedEntries(EnergyPlusData &state);

    void WriteMonthlyTables(EnergyPlusData &state);

    void WriteTimeBinTables(EnergyPlusData &state);

    void WriteBEPSTable(EnergyPlusData &state);

    std::string ResourceWarningMessage(std::string resource);

    Real64 WaterConversionFunct(Real64 WaterTotal, Real64 ConversionFactor);

    void WriteSourceEnergyEndUseSummary(EnergyPlusData &state);

    void WriteDemandEndUseSummary(EnergyPlusData &state);

    void WriteCompCostTable(EnergyPlusData &state);

    void WriteVeriSumTable(EnergyPlusData &state);

    void WriteAdaptiveComfortTable(EnergyPlusData &state);

    void WriteThermalResilienceTables(EnergyPlusData &state);

    void WriteCO2ResilienceTables(EnergyPlusData &state);

    void WriteVisualResilienceTables(EnergyPlusData &state);

    void WriteResilienceBinsTable(EnergyPlusData &state,
                                  int const columnNum,
                                  std::vector<int> const &columnHead,
                                  Array1D<std::vector<Real64>> const &ZoneBins);

    void WriteSETHoursTable(EnergyPlusData &state,
                            int const columnNum,
                            std::vector<std::string> const &columnHead,
                            Array1D<std::vector<Real64>> const &ZoneBins);

    void WriteHeatEmissionTable(EnergyPlusData &state);

    void WritePredefinedTables(EnergyPlusData &state);

    void WriteComponentSizing(EnergyPlusData &state);

    void WriteSurfaceShadowing(EnergyPlusData &state);

    void WriteEioTables(EnergyPlusData &state);

    int unitsFromHeading(EnergyPlusData &state, std::string &heading);

    int unitsFromHeading(EnergyPlusData &state, std::string &heading, iUnitsStyle unitsStyle_para);

    std::vector<std::string> splitCommaString(std::string const &inputString);

    void AddTOCLoadComponentTableSummaries(EnergyPlusData &state);

    void AllocateLoadComponentArrays(EnergyPlusData &state);

    void DeallocateLoadComponentArrays(EnergyPlusData &state);

    void ComputeLoadComponentDecayCurve(EnergyPlusData &state);

    void GatherComponentLoadsSurface(EnergyPlusData &state);

    void GatherComponentLoadsHVAC(EnergyPlusData &state);

    void WriteLoadComponentSummaryTables(EnergyPlusData &state);

    void GetDelaySequences(EnergyPlusData &state,
                           int const &desDaySelected,
                           bool const &isCooling,
                           int const &zoneIndex,
                           Array1D<Real64> &peopleDelaySeq,
                           Array1D<Real64> &equipDelaySeq,
                           Array1D<Real64> &hvacLossDelaySeq,
                           Array1D<Real64> &powerGenDelaySeq,
                           Array1D<Real64> &lightDelaySeq,
                           Array1D<Real64> &feneSolarDelaySeq,
                           Array3D<Real64> &feneCondInstantSeq,
                           Array2D<Real64> &surfDelaySeq);

    Real64 MovingAvgAtMaxTime(EnergyPlusData &state, Array1S<Real64> const &dataSeq, int const &numTimeSteps, int const &maxTimeStep);

    void ComputeTableBodyUsingMovingAvg(EnergyPlusData &state,
                                        Array2D<Real64> &resultCells,
                                        Array2D_bool &resultCellsUsed,
                                        int const &desDaySelected,
                                        int const &timeOfMax,
                                        int const &zoneIndex,
                                        Array1D<Real64> const &peopleDelaySeq,
                                        Array1D<Real64> const &equipDelaySeq,
                                        Array1D<Real64> const &hvacLossDelaySeq,
                                        Array1D<Real64> const &powerGenDelaySeq,
                                        Array1D<Real64> const &lightDelaySeq,
                                        Array1D<Real64> const &feneSolarDelaySeq,
                                        Array3D<Real64> const &feneCondInstantSeqLoc,
                                        Array2D<Real64> const &surfDelaySeq);

    void CollectPeakZoneConditions(EnergyPlusData &state,
                                   CompLoadTablesType &compLoad,
                                   int const &desDaySelected,
                                   int const &timeOfMax,
                                   int const &zoneIndex,
                                   bool const &isCooling);

    void ComputeEngineeringChecks(CompLoadTablesType &compLoad);

    void GetZoneComponentAreas(EnergyPlusData &state, Array1D<ZompComponentAreasType> &areas);

    void AddAreaColumnForZone(int const &zoneNum, Array1D<ZompComponentAreasType> const &compAreas, CompLoadTablesType &compLoadTotal);

    void CombineLoadCompResults(CompLoadTablesType &compLoadTotal, CompLoadTablesType const &compLoadPartial, Real64 const &multiplier);

    void AddTotalRowsForLoadSummary(CompLoadTablesType &compLoadTotal);

    void ComputePeakDifference(CompLoadTablesType &compLoad);

    void LoadSummaryUnitConversion(EnergyPlusData &state, CompLoadTablesType &compLoadTotal);

    void LoadSummaryUnitConversion(EnergyPlusData &state, CompLoadTablesType &compLoadTotal, iUnitsStyle unitsStyle_para);

    void CreateListOfZonesForAirLoop(EnergyPlusData &state, CompLoadTablesType &compLoad, Array1D_int const &zoneToAirLoop, int const &curAirLoop);

    void OutputCompLoadSummary(EnergyPlusData &state,
                               iOutputType const &kind,
                               CompLoadTablesType const &compLoadCool,
                               CompLoadTablesType const &compLoadHeat,
                               int const &zoneOrAirLoopIndex,
                               iUnitsStyle unitsStyle_para,
                               bool produceTabular_para,
                               bool produceSQLite_para);

    void WriteReportHeaders(EnergyPlusData &state,
                            std::string const &reportName,
                            std::string const &objectName,
                            OutputProcessor::StoreType const averageOrSum);

    void WriteSubtitle(EnergyPlusData &state, std::string const &subtitle);

    void WriteTextLine(EnergyPlusData &state, std::string const &lineOfText, Optional_bool_const isBold = _);

    void WriteTable(EnergyPlusData &state,
                    Array2S_string const body, // row,column
                    const Array1D_string &rowLabels,
                    const Array1D_string &columnLabels,
                    Array1D_int &widthColumn,
                    Optional_bool_const transposeXML = _,
                    Optional_string_const footnoteText = _);

    bool produceDualUnitsFlags(const int &iUnit_Sys,
                               const iUnitsStyle &unitsStyle_Tab,
                               const iUnitsStyle &unitsStyle_Sql,
                               iUnitsStyle &unitsStyle_Cur,
                               bool &produce_Tab,
                               bool &produce_Sql);

    std::string MakeAnchorName(std::string const &reportString, std::string const &objectString);

    std::string InsertCurrencySymbol(EnergyPlusData &state,
                                     std::string const &inString, // Input String
                                     bool const isHTML            // True if an HTML string
    );

    std::string ConvertToElementTag(std::string const &inString); // Input String

    std::string ConvertUnicodeToUTF8(unsigned long const codepoint);

    std::string ConvertToEscaped(std::string const &inString, // Input String
                                 bool isXML = true);          // isXML if false assumes HTML and will not convert quotes and apostrophes, for HTML4

    void DetermineBuildingFloorArea(EnergyPlusData &state);

    /* Tables with Subcategories in particular have a blank for rowHead for display in the HTML output.
     * This routine will fill up the blanks for output to Sql in particular */
    void FillRowHead(Array1D_string &rowHead);

    //======================================================================================================================
    //======================================================================================================================

    //    ROUTINES TO RESET GATHERED VALUES TO ZERO

    //======================================================================================================================
    //======================================================================================================================

    void ResetTabularReports(EnergyPlusData &state);

    void ResetMonthlyGathering(EnergyPlusData &state);

    void ResetBinGathering(EnergyPlusData &state);

    void ResetBEPSGathering(EnergyPlusData &state);

    void ResetSourceEnergyEndUseGathering(EnergyPlusData &state);

    void ResetPeakDemandGathering(EnergyPlusData &state);

    void ResetHeatGainGathering(EnergyPlusData &state);

    void ResetRemainingPredefinedEntries(EnergyPlusData &state);

    void ResetAdaptiveComfort(EnergyPlusData &state);

    //======================================================================================================================
    //======================================================================================================================

    //    ROUTINES RELATED TO IF VALUE IS IN A RANGE

    //======================================================================================================================
    //======================================================================================================================

    bool isInTriangle(
        Real64 const qx, Real64 const qy, Real64 const x1, Real64 const y1, Real64 const x2, Real64 const y2, Real64 const x3, Real64 const y3);

    bool isInQuadrilateral(Real64 const qx,
                           Real64 const qy,
                           Real64 const ax,
                           Real64 const ay,
                           Real64 const bx,
                           Real64 const by,
                           Real64 const cx,
                           Real64 const cy,
                           Real64 const dx,
                           Real64 const dy);

    //======================================================================================================================
    //======================================================================================================================

    //    SUPPORT ROUTINES

    //======================================================================================================================
    //======================================================================================================================

    std::string RealToStr(Real64 const RealIn, int const numDigits);

    Real64 StrToReal(std::string const &stringIn);

    std::string DateToString(int const codedDate); // word containing encoded month, day, hour, minute

    bool isNumber(std::string const &s);

    int digitsAferDecimal(std::string s);

    void AddTOCEntry(EnergyPlusData &state, std::string const &nameSection, std::string const &nameReport);

    void SetupUnitConversions(EnergyPlusData &state);

    std::string GetUnitSubString(std::string const &inString); // Input String

    void LookupSItoIP(EnergyPlusData &state, std::string const &stringInWithSI, int &unitConvIndex, std::string &stringOutWithIP);

    void LookupJtokWH(EnergyPlusData &state, std::string const &stringInWithJ, int &unitConvIndex, std::string &stringOutWithKWH);

    Real64 ConvertIP(EnergyPlusData &state, int const unitConvIndex, Real64 const SIvalue);

    Real64 ConvertIPdelta(EnergyPlusData &state, int const unitConvIndex, Real64 const SIvalue);

    void GetUnitConversion(EnergyPlusData &state, int const unitConvIndex, Real64 &multiplier, Real64 &offset, std::string &IPunit);

    Real64 getSpecificUnitMultiplier(EnergyPlusData &state, std::string const &SIunit, std::string const &IPunit);

    Real64 getSpecificUnitDivider(EnergyPlusData &state, std::string const &SIunit, std::string const &IPunit);

    Real64 getSpecificUnitIndex(EnergyPlusData &state, std::string const &SIunit, std::string const &IPunit);

} // namespace OutputReportTabular

struct OutputReportTabularData : BaseGlobalStruct
{

    OutputReportTabular::iUnitsStyle unitsStyle = OutputReportTabular::iUnitsStyle::None;
    OutputReportTabular::iUnitsStyle unitsStyle_SQLite = OutputReportTabular::iUnitsStyle::NotFound;
    int OutputTableBinnedCount = 0;
    int BinResultsTableCount = 0;
    int BinResultsIntervalCount = 0;
    int MonthlyInputCount = 0;
    int sizeMonthlyInput = 0;
    int MonthlyFieldSetInputCount = 0;
    int sizeMonthlyFieldSetInput = 0;
    int MonthlyTablesCount = 0;
    int MonthlyColumnsCount = 0;
    Array1D_bool IsMonthGathered = Array1D_bool(12, false); // shown as true for any month used
    int TOCEntriesCount = 0;
    int TOCEntriesSize = 0;
    int UnitConvSize = 0;
    bool WriteTabularFiles = false;
    bool GetInput = true;

    // From Report:Table:Style
    int numStyles = 0;
    std::ofstream csv_stream; // CSV table stream
    std::ofstream tab_stream; // Tab table stream
    std::ofstream fix_stream; // Fixed table stream
    std::ofstream htm_stream; // HTML table stream
    std::ofstream xml_stream; // XML table stream
    Array1D<std::ofstream *> TabularOutputFile = Array1D<std::ofstream *>(
        OutputReportTabular::maxNumStyles, {&csv_stream, &tab_stream, &fix_stream, &htm_stream, &xml_stream}); // Table stream array
    Array1D_string del = Array1D_string(OutputReportTabular::maxNumStyles);                                    // the delimiter to use
    Array1D<OutputReportTabular::iTableStyle> TableStyle = Array1D<OutputReportTabular::iTableStyle>(
        OutputReportTabular::maxNumStyles, OutputReportTabular::iTableStyle::Unassigned); // see list of parameters

    Real64 timeInYear = 0.0;

    // Flags for predefined tabular reports
    bool displayTabularBEPS = false;
    bool displayLEEDSummary = false;
    bool displayTabularCompCosts = false;
    bool displayTabularVeriSum = false;
    bool displayComponentSizing = false;
    bool displaySurfaceShadowing = false;
    bool displayDemandEndUse = false;
    bool displayAdaptiveComfort = false;
    bool displaySourceEnergyEndUseSummary = false;
    bool displayZoneComponentLoadSummary = false;
    bool displayAirLoopComponentLoadSummary = false;
    bool displayFacilityComponentLoadSummary = false;
    bool displayLifeCycleCostReport = false;
    bool displayTariffReport = false;
    bool displayEconomicResultSummary = false;
    bool displayHeatEmissionsSummary = false;
    bool displayEioSummary = false;
    bool displayThermalResilienceSummary = false;
    bool displayCO2ResilienceSummary = false;
    bool displayVisualResilienceSummary = false;
    bool displayThermalResilienceSummaryExplicitly = false;
    bool displayCO2ResilienceSummaryExplicitly = false;
    bool displayVisualResilienceSummaryExplicitly = false;
    // BEPS Report Related Variables
    // From Report:Table:Predefined - BEPS
    // arrays that hold the meter numbers that are initialized at get input
    Array1D_int meterNumTotalsBEPS = Array1D_int(OutputReportTabular::numResourceTypes, 0);
    Array1D_int meterNumTotalsSource = Array1D_int(OutputReportTabular::numSourceTypes, 0);
    Array1D_bool fuelfactorsused = Array1D_bool(OutputReportTabular::numSourceTypes, false);
    Array1D_bool ffUsed = Array1D_bool(OutputReportTabular::numResourceTypes, false);
    Array1D<Real64> SourceFactors = Array1D<Real64>(OutputReportTabular::numResourceTypes, 0.0);
    Array1D_bool ffSchedUsed = Array1D_bool(OutputReportTabular::numResourceTypes, false);
    Array1D_int ffSchedIndex = Array1D_int(OutputReportTabular::numResourceTypes, 0);
    Array2D_int meterNumEndUseBEPS = Array2D_int(OutputReportTabular::numResourceTypes, DataGlobalConstantsData::iEndUseSize, 0);
    Array3D_int meterNumEndUseSubBEPS;
    // arrays that hold the names of the resource and end uses
    Array1D_string resourceTypeNames = Array1D_string(OutputReportTabular::numResourceTypes);
    Array1D_string sourceTypeNames = Array1D_string(OutputReportTabular::numSourceTypes);
    Array1D_string endUseNames = Array1D_string(DataGlobalConstantsData::iEndUseSize);
    // arrays that hold the actual values for the year
    Array1D<Real64> gatherTotalsBEPS = Array1D<Real64>(OutputReportTabular::numResourceTypes, 0.0);
    Array1D<Real64> gatherTotalsBySourceBEPS = Array1D<Real64>(OutputReportTabular::numResourceTypes, 0.0);
    Array1D<Real64> gatherTotalsSource = Array1D<Real64>(OutputReportTabular::numSourceTypes, 0.0);
    Array1D<Real64> gatherTotalsBySource = Array1D<Real64>(OutputReportTabular::numSourceTypes, 0.0);
    Array2D<Real64> gatherEndUseBEPS = Array2D<Real64>(OutputReportTabular::numResourceTypes, DataGlobalConstantsData::iEndUseSize, 0.0);
    Array2D<Real64> gatherEndUseBySourceBEPS = Array2D<Real64>(OutputReportTabular::numResourceTypes, DataGlobalConstantsData::iEndUseSize, 0.0);
    Array3D<Real64> gatherEndUseSubBEPS;
    Array1D_bool needOtherRowLEED45 = Array1D_bool(DataGlobalConstantsData::iEndUseSize);

    // arrays the hold the demand values
    Array1D<Real64> gatherDemandTotal = Array1D<Real64>(OutputReportTabular::numResourceTypes, 0.0);
    Array2D<Real64> gatherDemandEndUse = Array2D<Real64>(OutputReportTabular::numResourceTypes, DataGlobalConstantsData::iEndUseSize, 0.0);
    Array2D<Real64> gatherDemandIndEndUse = Array2D<Real64>(OutputReportTabular::numResourceTypes, DataGlobalConstantsData::iEndUseSize, 0.0);
    Array3D<Real64> gatherDemandEndUseSub;
    Array3D<Real64> gatherDemandIndEndUseSub;
    Array1D_int gatherDemandTimeStamp = Array1D_int(OutputReportTabular::numResourceTypes, 0);

    // to keep track of hours for the BEPS report gathering
    Real64 gatherElapsedTimeBEPS = 0.0;
    // for normalization of results
    Real64 buildingGrossFloorArea = 0.0;
    Real64 buildingConditionedFloorArea = 0.0;
    // keep track if schedules are used in fuel factors
    bool fuelFactorSchedulesUsed = false;
    // for electric load components on BEPS report
    int meterNumPowerFuelFireGen = 0;
    Real64 gatherPowerFuelFireGen = 0.0;
    int meterNumPowerPV = 0;
    Real64 gatherPowerPV = 0.0;
    int meterNumPowerWind = 0;
    Real64 gatherPowerWind = 0.0;
    Real64 OverallNetEnergyFromStorage = 0.0;
    int meterNumPowerHTGeothermal = 0;
    Real64 gatherPowerHTGeothermal = 0.0;
    int meterNumElecProduced = 0;
    Real64 gatherElecProduced = 0.0;
    int meterNumElecPurchased = 0;
    Real64 gatherElecPurchased = 0.0;
    int meterNumElecSurplusSold = 0;
    Real64 gatherElecSurplusSold = 0.0;
    int meterNumElecStorage = 0;
    Real64 gatherElecStorage = 0.0;
    int meterNumPowerConversion = 0;
    Real64 gatherPowerConversion = 0.0;
    // for on site thermal source components on BEPS report
    int meterNumWaterHeatRecovery = 0;
    Real64 gatherWaterHeatRecovery = 0.0;
    int meterNumAirHeatRecoveryCool = 0;
    Real64 gatherAirHeatRecoveryCool = 0.0;
    int meterNumAirHeatRecoveryHeat = 0;
    Real64 gatherAirHeatRecoveryHeat = 0.0;
    int meterNumHeatHTGeothermal = 0;
    Real64 gatherHeatHTGeothermal = 0.0;
    int meterNumHeatSolarWater = 0;
    Real64 gatherHeatSolarWater = 0.0;
    int meterNumHeatSolarAir = 0;
    Real64 gatherHeatSolarAir = 0.0;
    // for on site water components on BEPS report
    int meterNumRainWater = 0;
    Real64 gatherRainWater = 0.0;
    int meterNumCondensate = 0;
    Real64 gatherCondensate = 0.0;
    int meterNumGroundwater = 0;
    Real64 gatherWellwater = 0.0;
    int meterNumMains = 0;
    Real64 gatherMains = 0.0;
    int meterNumWaterEndUseTotal = 0;
    Real64 gatherWaterEndUseTotal = 0.0;
    // for source energy conversion factors on BEPS report
    Real64 sourceFactorElectric = 0.0;
    Real64 sourceFactorNaturalGas = 0.0;
    Real64 efficiencyDistrictCooling = 0.0;
    Real64 efficiencyDistrictHeating = 0.0;
    Real64 sourceFactorSteam = 0.0;
    Real64 sourceFactorGasoline = 0.0;
    Real64 sourceFactorDiesel = 0.0;
    Real64 sourceFactorCoal = 0.0;
    Real64 sourceFactorFuelOil1 = 0.0;
    Real64 sourceFactorFuelOil2 = 0.0;
    Real64 sourceFactorPropane = 0.0;
    Real64 sourceFactorOtherFuel1 = 0.0;
    Real64 sourceFactorOtherFuel2 = 0.0;

    Array1D_int td = Array1D_int(8);
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
    int DesignDayCount = 0;

    // arrays related to pulse and load component reporting
    Array2D_int radiantPulseTimestep;
    Array2D<Real64> radiantPulseReceived;
    Array3D<Real64> loadConvectedNormal;
    Array3D<Real64> loadConvectedWithPulse;
    Array3D<Real64> netSurfRadSeq;
    Array2D<Real64> decayCurveCool;
    Array2D<Real64> decayCurveHeat;
    Array3D<Real64> ITABSFseq; // used for determining the radiant fraction on each surface
    Array3D<Real64> TMULTseq;  // used for determining the radiant fraction on each surface

    Array3D<Real64> peopleInstantSeq;
    Array3D<Real64> peopleLatentSeq;
    Array3D<Real64> peopleRadSeq;

    Array3D<Real64> lightInstantSeq;
    Array3D<Real64> lightRetAirSeq;
    Array3D<Real64> lightLWRadSeq; // long wave thermal radiation
    Array3D<Real64> lightSWRadSeq; // short wave visible radiation

    Array3D<Real64> equipInstantSeq;
    Array3D<Real64> equipLatentSeq;
    Array3D<Real64> equipRadSeq;

    Array3D<Real64> refrigInstantSeq;
    Array3D<Real64> refrigRetAirSeq;
    Array3D<Real64> refrigLatentSeq;

    Array3D<Real64> waterUseInstantSeq;
    Array3D<Real64> waterUseLatentSeq;

    Array3D<Real64> hvacLossInstantSeq;
    Array3D<Real64> hvacLossRadSeq;

    Array3D<Real64> powerGenInstantSeq;
    Array3D<Real64> powerGenRadSeq;
    Array3D<Real64> infilInstantSeq;
    Array3D<Real64> infilLatentSeq;

    Array3D<Real64> zoneVentInstantSeq;
    Array3D<Real64> zoneVentLatentSeq;

    Array3D<Real64> interZoneMixInstantSeq;
    Array3D<Real64> interZoneMixLatentSeq;

    Array3D<Real64> feneCondInstantSeq;
    Array3D<Real64> feneSolarRadSeq;

    int maxUniqueKeyCount = 0;

    // for the XML report must keep track fo the active sub-table name and report set by other routines
    std::string activeSubTableName;
    std::string activeReportNameNoSpace;
    std::string activeReportName;
    std::string activeForName;
    std::string prevReportName;

    // Object Data
    Array1D<OutputReportTabular::OutputTableBinnedType> OutputTableBinned;
    Array2D<OutputReportTabular::BinResultsType> BinResults;      // table number, number of intervals
    Array1D<OutputReportTabular::BinResultsType> BinResultsBelow; // time below the lowest defined bin
    Array1D<OutputReportTabular::BinResultsType> BinResultsAbove; // time above the highest defined bin
    Array1D<OutputReportTabular::BinObjVarIDType> BinObjVarID;
    Array1D<OutputReportTabular::BinStatisticsType> BinStatistics;
    Array1D<OutputReportTabular::NamedMonthlyType> namedMonthly; // for predefined monthly report titles
    Array1D<OutputReportTabular::MonthlyFieldSetInputType> MonthlyFieldSetInput;
    Array1D<OutputReportTabular::MonthlyInputType> MonthlyInput;
    Array1D<OutputReportTabular::MonthlyTablesType> MonthlyTables;
    Array1D<OutputReportTabular::MonthlyColumnsType> MonthlyColumns;
    Array1D<OutputReportTabular::TOCEntriesType> TOCEntries;
    Array1D<OutputReportTabular::UnitConvType> UnitConv;

    bool GatherMonthlyResultsForTimestepRunOnce = true;
    bool UpdateTabularReportsGetInput = true;
    bool GatherHeatGainReportfirstTime = true;
    bool AllocateLoadComponentArraysDoAllocate = true;
    bool initAdjFenDone = false;
    int numPeopleAdaptive = 0;

    Real64 BigNum = 0.0;
    bool VarWarning = true;
    int ErrCount1 = 0;
    Array1D<OutputProcessor::VariableType> MonthlyColumnsTypeOfVar;
    Array1D<OutputProcessor::TimeStepType> MonthlyColumnsStepType;
    Array1D<OutputReportTabular::iAggType> MonthlyColumnsAggType;
    Array1D_int MonthlyColumnsVarNum;
    Array1D_int MonthlyTablesNumColumns;
    int curFirstColumn = 0;
    int iZoneGHGR = 0;
    int iRadiantGHGR = 0;
    int iunitGHGR = 0;
    int curZoneGHGR = 0;
    Real64 eqpSensGHGR = 0.0;
    Real64 totalGHGR = 0.0; // the following arrays store the radiant total for each timestep
    Array1D<Real64> radiantHeat;
    Array1D<Real64> radiantCool;
    Array1D<Real64> ATUHeat;
    Array1D<Real64> ATUCool;
    int timestepTimeStampGHGR = 0;
    Real64 bldgHtPk = 0.0;
    Real64 bldgClPk = 0.0;
    Real64 timeStepRatio = 0.0;
    Real64 totalVolume = 0.0;
    int numUncondZones = 0;
    int numCondZones = 0;
    Real64 HrsPerWeek = 0.0; // sensible heat gain report totals
    Real64 totalZoneEqHt = 0.0;
    Real64 totalZoneEqCl = 0.0;
    Real64 totalHvacATUHt = 0.0;
    Real64 totalHvacATUCl = 0.0;
    Real64 totalSurfHt = 0.0;
    Real64 totalSurfCl = 0.0;
    Real64 totalPeoplAdd = 0.0;
    Real64 totalLiteAdd = 0.0;
    Real64 totalEquipAdd = 0.0;
    Real64 totalWindAdd = 0.0;
    Real64 totalIzaAdd = 0.0;
    Real64 totalInfilAdd = 0.0;
    Real64 totalOtherAdd = 0.0;
    Real64 totalEquipRem = 0.0;
    Real64 totalWindRem = 0.0;
    Real64 totalIzaRem = 0.0;
    Real64 totalInfilRem = 0.0;
    Real64 totalOtherRem = 0.0;
    Real64 curConversionOffset = 0.0;
    Real64 leedSiteIntLite = 0.0;
    Real64 leedSiteSpHeat = 0.0;
    Real64 leedSiteSpCool = 0.0;
    Real64 leedSiteFanInt = 0.0;
    Real64 leedSiteSrvWatr = 0.0;
    Real64 leedSiteRecept = 0.0;
    Real64 leedSiteTotal = 0.0;
    Real64 m2_unitConv = 0.0;
    int unitConvIndexWCCT = 0;
    int grandTotal = 1;
    int condTotal = 2;
    int uncondTotal = 3;
    int notpartTotal = 4;
    int unitConvIndexWVST = 0;
    Real64 m_unitConv = 0.0;
    Real64 m2_unitConvWVST = 0.0;
    Real64 m3_unitConv = 0.0;
    Real64 Wm2_unitConv = 0.0;
    Array1D<Real64> zstArea = Array1D<Real64>(4);
    Array1D<Real64> zstVolume = Array1D<Real64>(4);
    Array1D<Real64> zstWallArea = Array1D<Real64>(4);
    Array1D<Real64> zstUndWallArea = Array1D<Real64>(4);
    Array1D<Real64> zstWindowArea = Array1D<Real64>(4);
    Array1D<Real64> zstOpeningArea = Array1D<Real64>(4);
    Array1D<Real64> zstLight = Array1D<Real64>(4);
    Array1D<Real64> zstPeople = Array1D<Real64>(4);
    Array1D<Real64> zstPlug = Array1D<Real64>(4);
    int indexUnitConvWCS = 0;
    Real64 curValueSIWCS = 0.0;
    Real64 curValueWCS = 0.0;
    int ZoneNumCLCDC = 0;
    int SurfNumCLCDC = 0;
    int TimeStepCLCDC = 0;
    int TimeOfPulseCLCDC = 0;
    int CoolDesSelectedCLCDC = 0; // design day selected for cooling
    int HeatDesSelectedCLCDC = 0; // design day selected for heating
    int iSurfGCLS = 0;
    int ZoneNumGCLS = 0;
    int TimeStepInDayGCLS = 0;
    int iZoneGCLH = 0;
    int TimeStepInDayGCLH = 0;
    Array1D_int IntGainTypesTubularGCLS = Array1D_int(1, {DataHeatBalance::IntGainTypeOf_DaylightingDeviceTubular});
    Array3D_bool adjFenDone;
    Real64 BigNumRMG = 0.0;
    int foundGsui = 0;
    int iUnitGsui = 0;
    int foundGsum = 0;
    int iUnitGsum = 0;
    std::string footnote;
    std::string m_unitName;
    std::string m2_unitName;
    std::string m3_unitName;
    std::string Wm2_unitName;
    std::string curColHeadWithSI;
    std::string curColHead;

    void clear_state() override
    {
        this->unitsStyle = OutputReportTabular::iUnitsStyle::None;
        this->unitsStyle_SQLite = OutputReportTabular::iUnitsStyle::NotFound;
        this->OutputTableBinnedCount = 0;
        this->BinResultsTableCount = 0;
        this->BinResultsIntervalCount = 0;
        this->MonthlyInputCount = 0;
        this->sizeMonthlyInput = 0;
        this->MonthlyFieldSetInputCount = 0;
        this->sizeMonthlyFieldSetInput = 0;
        this->MonthlyTablesCount = 0;
        this->MonthlyColumnsCount = 0;
        this->IsMonthGathered = Array1D_bool(12, false);
        this->TOCEntriesCount = 0;
        this->TOCEntriesSize = 0;
        this->UnitConvSize = 0;
        this->WriteTabularFiles = false;
        this->GetInput = true;
        this->numStyles = 0;
        this->TabularOutputFile = Array1D<std::ofstream *>(
            OutputReportTabular::maxNumStyles, {&this->csv_stream, &this->tab_stream, &this->fix_stream, &this->htm_stream, &this->xml_stream});
        this->del = Array1D_string(OutputReportTabular::maxNumStyles);
        this->TableStyle = Array1D<OutputReportTabular::iTableStyle>(OutputReportTabular::maxNumStyles, OutputReportTabular::iTableStyle::Unassigned);
        this->timeInYear = 0.0;
        this->displayTabularBEPS = false;
        this->displayLEEDSummary = false;
        this->displayTabularCompCosts = false;
        this->displayTabularVeriSum = false;
        this->displayComponentSizing = false;
        this->displaySurfaceShadowing = false;
        this->displayDemandEndUse = false;
        this->displayAdaptiveComfort = false;
        this->displaySourceEnergyEndUseSummary = false;
        this->displayZoneComponentLoadSummary = false;
        this->displayAirLoopComponentLoadSummary = false;
        this->displayFacilityComponentLoadSummary = false;
        this->displayLifeCycleCostReport = false;
        this->displayTariffReport = false;
        this->displayEconomicResultSummary = false;
        this->displayHeatEmissionsSummary = false;
        this->displayEioSummary = false;
        this->displayThermalResilienceSummary = false;
        this->displayCO2ResilienceSummary = false;
        this->displayVisualResilienceSummary = false;
        this->displayThermalResilienceSummaryExplicitly = false;
        this->displayCO2ResilienceSummaryExplicitly = false;
        this->displayVisualResilienceSummaryExplicitly = false;
        this->meterNumTotalsBEPS = Array1D_int(OutputReportTabular::numResourceTypes, 0);
        this->meterNumTotalsSource = Array1D_int(OutputReportTabular::numSourceTypes, 0);
        this->fuelfactorsused = Array1D_bool(OutputReportTabular::numSourceTypes, false);
        this->ffUsed = Array1D_bool(OutputReportTabular::numResourceTypes, false);
        this->SourceFactors = Array1D<Real64>(OutputReportTabular::numResourceTypes, 0.0);
        this->ffSchedUsed = Array1D_bool(OutputReportTabular::numResourceTypes, false);
        this->ffSchedIndex = Array1D_int(OutputReportTabular::numResourceTypes, 0);
        this->meterNumEndUseBEPS = Array2D_int(OutputReportTabular::numResourceTypes, DataGlobalConstantsData::iEndUseSize, 0);
        this->meterNumEndUseSubBEPS.deallocate();
        this->resourceTypeNames = Array1D_string(OutputReportTabular::numResourceTypes);
        this->sourceTypeNames = Array1D_string(OutputReportTabular::numSourceTypes);
        this->endUseNames = Array1D_string(DataGlobalConstantsData::iEndUseSize);
        this->gatherTotalsBEPS = Array1D<Real64>(OutputReportTabular::numResourceTypes, 0.0);
        this->gatherTotalsBySourceBEPS = Array1D<Real64>(OutputReportTabular::numResourceTypes, 0.0);
        this->gatherTotalsSource = Array1D<Real64>(OutputReportTabular::numSourceTypes, 0.0);
        this->gatherTotalsBySource = Array1D<Real64>(OutputReportTabular::numSourceTypes, 0.0);
        this->gatherEndUseBEPS = Array2D<Real64>(OutputReportTabular::numResourceTypes, DataGlobalConstantsData::iEndUseSize, 0.0);
        this->gatherEndUseBySourceBEPS = Array2D<Real64>(OutputReportTabular::numResourceTypes, DataGlobalConstantsData::iEndUseSize, 0.0);
        this->gatherEndUseSubBEPS.deallocate();
        this->needOtherRowLEED45 = Array1D_bool(DataGlobalConstantsData::iEndUseSize);
        this->gatherDemandTotal = Array1D<Real64>(OutputReportTabular::numResourceTypes, 0.0);
        this->gatherDemandEndUse = Array2D<Real64>(OutputReportTabular::numResourceTypes, DataGlobalConstantsData::iEndUseSize, 0.0);
        this->gatherDemandIndEndUse = Array2D<Real64>(OutputReportTabular::numResourceTypes, DataGlobalConstantsData::iEndUseSize, 0.0);
        this->gatherDemandEndUseSub.deallocate();
        this->gatherDemandIndEndUseSub.deallocate();
        this->gatherDemandTimeStamp = Array1D_int(OutputReportTabular::numResourceTypes, 0);
        this->gatherElapsedTimeBEPS = 0.0;
        this->buildingGrossFloorArea = 0.0;
        this->buildingConditionedFloorArea = 0.0;
        this->fuelFactorSchedulesUsed = false;
        this->meterNumPowerFuelFireGen = 0;
        this->gatherPowerFuelFireGen = 0.0;
        this->meterNumPowerPV = 0;
        this->gatherPowerPV = 0.0;
        this->meterNumPowerWind = 0;
        this->gatherPowerWind = 0.0;
        this->OverallNetEnergyFromStorage = 0.0;
        this->meterNumPowerHTGeothermal = 0;
        this->gatherPowerHTGeothermal = 0.0;
        this->meterNumElecProduced = 0;
        this->gatherElecProduced = 0.0;
        this->meterNumElecPurchased = 0;
        this->gatherElecPurchased = 0.0;
        this->meterNumElecSurplusSold = 0;
        this->gatherElecSurplusSold = 0.0;
        this->meterNumElecStorage = 0;
        this->gatherElecStorage = 0.0;
        this->meterNumPowerConversion = 0;
        this->gatherPowerConversion = 0.0;
        this->meterNumWaterHeatRecovery = 0;
        this->gatherWaterHeatRecovery = 0.0;
        this->meterNumAirHeatRecoveryCool = 0;
        this->gatherAirHeatRecoveryCool = 0.0;
        this->meterNumAirHeatRecoveryHeat = 0;
        this->gatherAirHeatRecoveryHeat = 0.0;
        this->meterNumHeatHTGeothermal = 0;
        this->gatherHeatHTGeothermal = 0.0;
        this->meterNumHeatSolarWater = 0;
        this->gatherHeatSolarWater = 0.0;
        this->meterNumHeatSolarAir = 0;
        this->gatherHeatSolarAir = 0.0;
        this->meterNumRainWater = 0;
        this->gatherRainWater = 0.0;
        this->meterNumCondensate = 0;
        this->gatherCondensate = 0.0;
        this->meterNumGroundwater = 0;
        this->gatherWellwater = 0.0;
        this->meterNumMains = 0;
        this->gatherMains = 0.0;
        this->meterNumWaterEndUseTotal = 0;
        this->gatherWaterEndUseTotal = 0.0;
        this->sourceFactorElectric = 0.0;
        this->sourceFactorNaturalGas = 0.0;
        this->efficiencyDistrictCooling = 0.0;
        this->efficiencyDistrictHeating = 0.0;
        this->sourceFactorSteam = 0.0;
        this->sourceFactorGasoline = 0.0;
        this->sourceFactorDiesel = 0.0;
        this->sourceFactorCoal = 0.0;
        this->sourceFactorFuelOil1 = 0.0;
        this->sourceFactorFuelOil2 = 0.0;
        this->sourceFactorPropane = 0.0;
        this->sourceFactorOtherFuel1 = 0.0;
        this->sourceFactorOtherFuel2 = 0.0;
        this->td = Array1D_int(8);
        this->DesignDayName.deallocate();
        this->DesignDayCount = 0;
        this->radiantPulseTimestep.deallocate();
        this->radiantPulseReceived.deallocate();
        this->loadConvectedNormal.deallocate();
        this->loadConvectedWithPulse.deallocate();
        this->netSurfRadSeq.deallocate();
        this->decayCurveCool.deallocate();
        this->decayCurveHeat.deallocate();
        this->ITABSFseq.deallocate();
        this->TMULTseq.deallocate();
        this->peopleInstantSeq.deallocate();
        this->peopleLatentSeq.deallocate();
        this->peopleRadSeq.deallocate();
        this->lightInstantSeq.deallocate();
        this->lightRetAirSeq.deallocate();
        this->lightLWRadSeq.deallocate();
        this->lightSWRadSeq.deallocate();
        this->equipInstantSeq.deallocate();
        this->equipLatentSeq.deallocate();
        this->equipRadSeq.deallocate();
        this->refrigInstantSeq.deallocate();
        this->refrigRetAirSeq.deallocate();
        this->refrigLatentSeq.deallocate();
        this->waterUseInstantSeq.deallocate();
        this->waterUseLatentSeq.deallocate();
        this->hvacLossInstantSeq.deallocate();
        this->hvacLossRadSeq.deallocate();
        this->powerGenInstantSeq.deallocate();
        this->powerGenRadSeq.deallocate();
        this->infilInstantSeq.deallocate();
        this->infilLatentSeq.deallocate();
        this->zoneVentInstantSeq.deallocate();
        this->zoneVentLatentSeq.deallocate();
        this->interZoneMixInstantSeq.deallocate();
        this->interZoneMixLatentSeq.deallocate();
        this->feneCondInstantSeq.deallocate();
        this->feneSolarRadSeq.deallocate();
        this->maxUniqueKeyCount = 0;
        this->activeSubTableName.clear();
        this->activeReportNameNoSpace.clear();
        this->activeReportName.clear();
        this->activeForName.clear();
        this->prevReportName.clear();
        this->OutputTableBinned.deallocate();
        this->BinResults.deallocate();
        this->BinResultsBelow.deallocate();
        this->BinResultsAbove.deallocate();
        this->BinObjVarID.deallocate();
        this->BinStatistics.deallocate();
        this->namedMonthly.deallocate();
        this->MonthlyFieldSetInput.deallocate();
        this->MonthlyInput.deallocate();
        this->MonthlyTables.deallocate();
        this->MonthlyColumns.deallocate();
        this->TOCEntries.deallocate();
        this->UnitConv.deallocate();
        this->GatherMonthlyResultsForTimestepRunOnce = true;
        this->UpdateTabularReportsGetInput = true;
        this->GatherHeatGainReportfirstTime = true;
        this->AllocateLoadComponentArraysDoAllocate = true;
        this->initAdjFenDone = false;
        this->numPeopleAdaptive = 0;

        this->BigNum = 0.0;
        this->VarWarning = true;
        this->ErrCount1 = 0;
        this->MonthlyColumnsTypeOfVar.clear();
        this->MonthlyColumnsStepType.clear();
        this->MonthlyColumnsAggType.clear();
        this->MonthlyColumnsVarNum.clear();
        this->MonthlyTablesNumColumns.clear();
        this->curFirstColumn = 0;
        this->iZoneGHGR = 0;
        this->iRadiantGHGR = 0;
        this->iunitGHGR = 0;
        this->curZoneGHGR = 0;
        this->eqpSensGHGR = 0.0;
        this->totalGHGR = 0.0;
        // the following arrays store the radiant total for each timestep
        this->radiantHeat.clear();
        this->radiantCool.clear();
        this->ATUHeat.clear();
        this->ATUCool.clear();
        this->timestepTimeStampGHGR = 0;
        this->bldgHtPk = 0.0;
        this->bldgClPk = 0.0;
        this->timeStepRatio = 0.0;
        this->totalVolume = 0.0;
        this->numUncondZones = 0;
        this->numCondZones = 0;
        this->HrsPerWeek = 0.0;
        // sensible heat gain report totals
        this->totalZoneEqHt = 0.0;
        this->totalZoneEqCl = 0.0;
        this->totalHvacATUHt = 0.0;
        this->totalHvacATUCl = 0.0;
        this->totalSurfHt = 0.0;
        this->totalSurfCl = 0.0;
        this->totalPeoplAdd = 0.0;
        this->totalLiteAdd = 0.0;
        this->totalEquipAdd = 0.0;
        this->totalWindAdd = 0.0;
        this->totalIzaAdd = 0.0;
        this->totalInfilAdd = 0.0;
        this->totalOtherAdd = 0.0;
        this->totalEquipRem = 0.0;
        this->totalWindRem = 0.0;
        this->totalIzaRem = 0.0;
        this->totalInfilRem = 0.0;
        this->totalOtherRem = 0.0;
        this->curConversionOffset = 0.0;
        this->leedSiteIntLite = 0.0;
        this->leedSiteSpHeat = 0.0;
        this->leedSiteSpCool = 0.0;
        this->leedSiteFanInt = 0.0;
        this->leedSiteSrvWatr = 0.0;
        this->leedSiteRecept = 0.0;
        this->leedSiteTotal = 0.0;
        this->m2_unitConv = 0.0;
        this->unitConvIndexWCCT = 0;
        this->grandTotal = 1;
        this->condTotal = 2;
        this->uncondTotal = 3;
        this->notpartTotal = 4;
        this->unitConvIndexWVST = 0;
        this->m_unitConv = 0.0;
        this->m2_unitConvWVST = 0.0;
        this->m3_unitConv = 0.0;
        this->Wm2_unitConv = 0.0;
        this->zstArea = Array1D<Real64>(4);
        this->zstVolume = Array1D<Real64>(4);
        this->zstWallArea = Array1D<Real64>(4);
        this->zstUndWallArea = Array1D<Real64>(4);
        this->zstWindowArea = Array1D<Real64>(4);
        this->zstOpeningArea = Array1D<Real64>(4);
        this->zstLight = Array1D<Real64>(4);
        this->zstPeople = Array1D<Real64>(4);
        this->zstPlug = Array1D<Real64>(4);
        this->indexUnitConvWCS = 0;
        this->curValueSIWCS = 0.0;
        this->curValueWCS = 0.0;
        this->ZoneNumCLCDC = 0;
        this->SurfNumCLCDC = 0;
        this->TimeStepCLCDC = 0;
        this->TimeOfPulseCLCDC = 0;
        this->CoolDesSelectedCLCDC = 0; // design day selected for cooling
        this->HeatDesSelectedCLCDC = 0; // design day selected for heating
        this->iSurfGCLS = 0;
        this->ZoneNumGCLS = 0;
        this->TimeStepInDayGCLS = 0;
        this->iZoneGCLH = 0;
        this->TimeStepInDayGCLH = 0;
        this->IntGainTypesTubularGCLS = Array1D_int(1, {DataHeatBalance::IntGainTypeOf_DaylightingDeviceTubular});
        this->adjFenDone.clear();
        this->BigNumRMG = 0.0;
        this->foundGsui = 0;
        this->iUnitGsui = 0;
        this->foundGsum = 0;
        this->iUnitGsum = 0;
        this->footnote.clear();
        this->m_unitName.clear();
        this->m2_unitName.clear();
        this->m3_unitName.clear();
        this->Wm2_unitName.clear();
        this->curColHeadWithSI.clear();
        this->curColHead.clear();
    }
};

} // namespace EnergyPlus

#endif
