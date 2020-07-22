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

#ifndef Base_hh_INCLUDED
#define Base_hh_INCLUDED

#include <EnergyPlus/AirLoopHVACDOAS.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/api/TypeDefs.h>

namespace EnergyPlus {

enum class AutoSizingType
{
    CoolingWaterDesAirInletHumRatSizing,
    CoolingWaterDesAirOutletHumRatSizing,
    CoolingWaterDesWaterInletTempSizing,
    CoolingWaterNumofTubesPerRowSizing,
    HeatingAirflowUASizing,
    HeatingWaterDesAirInletHumRatSizing,
    HeatingWaterDesAirInletTempSizing,
    HeatingWaterDesCoilWaterVolFlowUsedForUASizing,
    HeatingWaterflowSizing,
    MaxHeaterOutletTempSizing,
    ZoneCoolingLoadSizing,
    ZoneHeatingLoadSizing,
    Unknown
};

enum class AutoSizingResultType
{
    NoError,    // no errors found
    ErrorType1, // sizing error
    ErrorType2  // uninitialized sizing type
};

struct BaseSizer
{

    Real64 stdRhoAir = 0.0;

    bool getCoilReportObject = false; // provides access to coil reporting
    bool initialized = false;     // indicates initializeWithinEP was called
    AutoSizingResultType errorType = AutoSizingResultType::NoError;
    AutoSizingType sizingType = AutoSizingType::Unknown;
    std::string sizingString = "";
    Real64 originalValue = 0.0;
    Real64 autoSizedValue = 0.0;
    bool wasAutoSized = false;
    bool hardSizeNoDesignRun = false;
    bool sizingDesRunThisAirSys = false;
    bool sizingDesRunThisZone = false;
    bool sizingDesValueFromParent = false;
    bool airLoopSysFlag = false;
    bool oaSysFlag = false;
    std::string compType = "";
    std::string compName = "";

    bool sysSizingRunDone = false;
    bool zoneSizingRunDone = false;
    int curSysNum = 0;
    int curOASysNum = 0;
    int curZoneEqNum = 0;
    int curDuctType = 0;
    int curTermUnitSizingNum = 0; // index in zone equipment vector - for single duct, IU, and PIU
    int numPrimaryAirSys = 0;
    int numSysSizInput = 0;
    bool doSystemSizing = false;
    int numZoneSizingInput = 0;
    bool doZoneSizing = false;
    bool autoCalculate = false; // indicator that AutoCalculate is used

    // terminal units
    bool termUnitSingDuct = false; // single duct terminal unit
    bool termUnitPIU = false;      // powered induction unit
    bool termUnitIU = false;       // induction terminal unit
    bool zoneEqFanCoil = false;    // fan coil zone equipment
    bool otherEqType = false;      // this covers the ELSE type switch
    bool zoneEqUnitHeater = false; // unit heater zone equipment
    bool zoneEqUnitVent = false;   // unit ventilator zone equipment
    bool zoneEqVentedSlab = false; // ventilated slab zone equipment

    // global sizing data
    Real64 minOA = 0.0;

    // global Data* sizing constants

    // HeatingWaterflowSizer
    Real64 dataConstantUsedForSizing = 0.0;
    Real64 dataFractionUsedForSizing = 0.0;

    // HeatingWaterDesCoilWaterVolFlowUsedForUASizer
    int dataPltSizHeatNum = 0;
    // HeatingWaterDesCoilWaterVolFlowUsedForUASizer, HeaterWaterflowSizing
    int dataWaterLoopNum = 0;
    // CoolingWaterflowSizing
    int dataFanIndex = -1;
    int dataFanEnumType = -1;
    Real64 dataWaterCoilSizCoolDeltaT = 0.0;
    // HeaterWaterflowSizing
    Real64 dataWaterCoilSizHeatDeltaT = 0.0;
    Real64 dataCapacityUsedForSizing = 0.0;

    // CoolingWaterDesWaterInletTempSizer, CoolingWaterNumofTubesPerRowSizer
    int dataPltSizCoolNum = 0;

    // CoolingWaterDesAirInletHumRatSizer, CoolingWaterDesAirOutletHumRatSizer
    Real64 dataDesInletAirHumRat = 0.0;

    // CoolingWaterDesAirInletHumRatSizer, HeatingWaterDesAirInletHumRatSizer,
    // HeatingWaterDesAirInletTempSizer
    Real64 dataFlowUsedForSizing = 0.0;

    // CoolingWaterDesAirOutletHumRatSizer
    Real64 dataDesOutletAirHumRat = 0.0;
    Real64 dataDesInletWaterTemp = 0.0;
    Real64 dataDesOutletAirTemp = 0.0;

    // CoolingWaterNumofTubesPerRowSizer, HeatingWaterDesCoilWaterVolFlowUsedForUASizer
    Real64 dataWaterFlowUsedForSizing = 0.0;

    bool printWarningFlag = false;
    std::string callingRoutine = "";
    Array1D<DataSizing::SystemSizingInputData> sysSizingInputData;
    Array1D<DataSizing::ZoneSizingInputData> zoneSizingInput;
    Array1D<DataSizing::ZoneEqSizingData> unitarySysEqSizing;
    Array1D<DataSizing::ZoneEqSizingData> oaSysEqSizing;
    Array1D<DataSizing::ZoneEqSizingData> zoneEqSizing;
    Array1D<DataAirLoop::OutsideAirSysProps> outsideAirSys;
    Array1D<DataSizing::TermUnitSizingData> termUnitSizing;
    Array1D<DataSizing::ZoneSizingData> termUnitFinalZoneSizing;
    Array1D<DataSizing::ZoneSizingData> finalZoneSizing;
    Array1D<DataSizing::SystemSizingData> finalSysSizing;
    Array1D<DataSizing::PlantSizingData> plantSizData;
    Array1D<DataAirSystems::DefinePrimaryAirSystem> primaryAirSystem;
    std::vector<AirLoopHVACDOAS::AirLoopDOAS> airloopDOAS;

    // public methods

    virtual void initializeWithinEP(EnergyPlusData &state,
                                    std::string const &_compType,
                                    std::string const &_compName,
                                    bool const &_printWarningFlag,
                                    std::string const &_callingRoutine);

    virtual Real64 size(Real64 originalValue, bool &errorsFound) = 0;

    std::string getLastErrorMessages();

    void overrideSizingString(std::string const &string);

protected:

    std::string lastErrorMessages = "";

    void addErrorMessage(std::string const &s);

    void initializeFromAPI(Real64 elevation); // don't accidentally call this direct component from outside

    void preSize(Real64 originalValue);

    static void reportSizerOutput(std::string const &CompType,
                                  std::string const &CompName,
                                  std::string const &VarDesc,
                                  Real64 VarValue,
                                  Optional_string_const UsrDesc = _,
                                  Optional<Real64 const> UsrValue = _);

    void selectSizerOutput(bool &errorsFound);

    static bool isValidCoilType(std::string const &compType);

    bool checkInitialized(bool &errorsFound);

    void clearState();

};

void autosizing_clear_state();

extern bool oneTimeCompRptHeaderFlag;

} // namespace EnergyPlus

#endif
