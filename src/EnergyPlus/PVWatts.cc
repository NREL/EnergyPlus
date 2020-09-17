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

// C++ Headers
#include <math.h>
#include <stdexcept>

// ObjexxFCL Headers

// EnergyPlus Headers

#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PVWatts.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WeatherManager.hh>

// SAM Headers
//#include <../third_party/ssc/shared/lib_irradproc.h>
//#include <../third_party/ssc/shared/lib_pvwatts.h>
//#include <../third_party/ssc/shared/lib_pvshade.h>
//#include <../third_party/ssc/shared/lib_pv_incidence_modifier.h>
#include <../third_party/ssc/ssc/sscapi.h>

namespace EnergyPlus {

namespace PVWatts {

    std::map<int, PVWattsGenerator> PVWattsGenerators;

    PVWattsGenerator::PVWattsGenerator(const std::string &name,
                                       const Real64 dcSystemCapacity,
                                       ModuleType moduleType,
                                       ArrayType arrayType,
                                       Real64 systemLosses,
                                       GeometryType geometryType,
                                       Real64 tilt,
                                       Real64 azimuth,
                                       size_t surfaceNum,
                                       Real64 groundCoverageRatio)
        : m_moduleType(moduleType),
          m_arrayType(arrayType),
          m_geometryType(geometryType),
          m_DCtoACRatio(1.1),
          m_inverterEfficiency(0.96),
          m_outputDCPower(1000.0),
          m_cellTemperature(-9999),
          m_planeOfArrayIrradiance(-9999)

    {
        using General::RoundSigDigits;
        bool errorsFound(false);

        if (name.empty()) {
            ShowSevereError("PVWatts: name cannot be blank.");
            errorsFound = true;
        }
        m_name = name;

        if (dcSystemCapacity <= 0) {
            ShowSevereError("PVWatts: DC system capacity must be greater than zero.");
            errorsFound = true;
        }
        m_dcSystemCapacity = dcSystemCapacity;

        if (systemLosses > 1.0 || systemLosses < 0.0) {
            ShowSevereError("PVWatts: Invalid system loss value " + RoundSigDigits(systemLosses, 2));
            errorsFound = true;
        }
        m_systemLosses = systemLosses;

        if (m_geometryType == GeometryType::TILT_AZIMUTH) {
            if (tilt < 0 || tilt > 90) {
                ShowSevereError("PVWatts: Invalid tilt: " + RoundSigDigits(tilt, 2));
                errorsFound = true;
            }
            m_tilt = tilt;
            if (azimuth < 0 || azimuth >= 360) {
                ShowSevereError("PVWatts: Invalid azimuth: " + RoundSigDigits(azimuth, 2));
            }
            m_azimuth = azimuth;
        } else if (m_geometryType == GeometryType::SURFACE) {
            if (surfaceNum == 0 || surfaceNum > DataSurfaces::Surface.size()) {
                ShowSevereError("PVWatts: SurfaceNum not in Surfaces: " + std::to_string(surfaceNum));
                errorsFound = true;
            } else {
                m_surfaceNum = surfaceNum;
                m_tilt = getSurface().Tilt;
                m_azimuth = getSurface().Azimuth;
                // TODO: Do some bounds checking on Tilt and Azimuth.
            }
        } else {
            assert(false);
        }

        if (groundCoverageRatio > 1.0 || groundCoverageRatio < 0.0) {
            ShowSevereError("PVWatts: Invalid ground coverage ratio: " + RoundSigDigits(groundCoverageRatio, 2));
            errorsFound = true;
        }
        m_groundCoverageRatio = groundCoverageRatio;

        if (errorsFound) {
            ShowFatalError("Errors found in getting PVWatts input");
        }

    }

    void PVWattsGenerator::setupOutputVariables()
    {
        // Set up output variables
        SetupOutputVariable("Generator Produced DC Electricity Rate", OutputProcessor::Unit::W, m_outputDCPower, "System", "Average", m_name);
        SetupOutputVariable("Generator Produced DC Electricity Energy",
                            OutputProcessor::Unit::J,
                            m_outputDCEnergy,
                            "System",
                            "Sum",
                            m_name,
                            _,
                            "ElectricityProduced",
                            "Photovoltaics",
                            _,
                            "Plant");
        SetupOutputVariable("Generator PV Cell Temperature", OutputProcessor::Unit::C, m_cellTemperature, "System", "Average", m_name);
        SetupOutputVariable("Plane of Array Irradiance", OutputProcessor::Unit::W_m2, m_planeOfArrayIrradiance, "System", "Average", m_name);
    }

    PVWattsGenerator PVWattsGenerator::createFromIdfObj(int objNum)
    {
        Array1D_string cAlphaFieldNames;
        Array1D_string cNumericFieldNames;
        Array1D_bool lNumericFieldBlanks;
        Array1D_bool lAlphaFieldBlanks;
        Array1D_string cAlphaArgs;
        Array1D<Real64> rNumericArgs;
        const int maxAlphas = 6;  // from idd
        const int maxNumeric = 5; // from idd
        cAlphaFieldNames.allocate(maxAlphas);
        cNumericFieldNames.allocate(maxNumeric);
        lNumericFieldBlanks.allocate(maxNumeric);
        lAlphaFieldBlanks.allocate(maxAlphas);
        cAlphaArgs.allocate(maxAlphas);
        rNumericArgs.allocate(maxNumeric);
        int NumAlphas;
        int NumNums;
        int IOStat;
        bool errorsFound = false;

        inputProcessor->getObjectItem("Generator:PVWatts",
                                      objNum,
                                      cAlphaArgs,
                                      NumAlphas,
                                      rNumericArgs,
                                      NumNums,
                                      IOStat,
                                      lNumericFieldBlanks,
                                      lAlphaFieldBlanks,
                                      cAlphaFieldNames,
                                      cNumericFieldNames);

        const std::string name(cAlphaArgs(AlphaFields::NAME));
        const Real64 dcSystemCapacity(rNumericArgs(NumFields::DC_SYSTEM_CAPACITY));
        const std::map<std::string, ModuleType> moduleTypeMap = {
            {"STANDARD", ModuleType::STANDARD}, {"PREMIUM", ModuleType::PREMIUM}, {"THINFILM", ModuleType::THIN_FILM}};
        ModuleType moduleType;
        auto moduleTypeIt = moduleTypeMap.find(cAlphaArgs(AlphaFields::MODULE_TYPE));
        if (moduleTypeIt == moduleTypeMap.end()) {
            ShowSevereError("PVWatts: Invalid Module Type: " + cAlphaArgs(AlphaFields::MODULE_TYPE));
            errorsFound = true;
        } else {
            moduleType = moduleTypeIt->second;
        }

        const std::map<std::string, ArrayType> arrayTypeMap = {{"FIXEDOPENRACK", ArrayType::FIXED_OPEN_RACK},
                                                               {"FIXEDROOFMOUNTED", ArrayType::FIXED_ROOF_MOUNTED},
                                                               {"ONEAXIS", ArrayType::ONE_AXIS},
                                                               {"ONEAXISBACKTRACKING", ArrayType::ONE_AXIS_BACKTRACKING},
                                                               {"TWOAXIS", ArrayType::TWO_AXIS}};
        ArrayType arrayType;
        auto arrayTypeIt = arrayTypeMap.find(cAlphaArgs(AlphaFields::ARRAY_TYPE));
        if (arrayTypeIt == arrayTypeMap.end()) {
            ShowSevereError("PVWatts: Invalid Array Type: " + cAlphaArgs(AlphaFields::ARRAY_TYPE));
            errorsFound = true;
        } else {
            arrayType = arrayTypeIt->second;
        }

        const Real64 systemLosses(rNumericArgs(NumFields::SYSTEM_LOSSES));
        const std::map<std::string, GeometryType> geometryTypeMap{{"TILTAZIMUTH", GeometryType::TILT_AZIMUTH}, {"SURFACE", GeometryType::SURFACE}};
        GeometryType geometryType;
        auto geometryTypeIt = geometryTypeMap.find(cAlphaArgs(AlphaFields::GEOMETRY_TYPE));
        if (geometryTypeIt == geometryTypeMap.end()) {
            ShowSevereError("PVWatts: Invalid Geometry Type: " + cAlphaArgs(AlphaFields::GEOMETRY_TYPE));
            errorsFound = true;
        } else {
            geometryType = geometryTypeIt->second;
        }

        const Real64 tilt(rNumericArgs(NumFields::TILT_ANGLE));
        const Real64 azimuth(rNumericArgs(NumFields::AZIMUTH_ANGLE));
        int surfaceNum;
        if (lAlphaFieldBlanks(AlphaFields::SURFACE_NAME)) {
            surfaceNum = 0;
        } else {
            surfaceNum = UtilityRoutines::FindItemInList(cAlphaArgs(AlphaFields::SURFACE_NAME), DataSurfaces::Surface);
        }

        if (errorsFound) {
            ShowFatalError("Errors found in getting PVWatts input");
        }

        if (NumNums < NumFields::GROUND_COVERAGE_RATIO) {
            return PVWattsGenerator(name, dcSystemCapacity, moduleType, arrayType, systemLosses, geometryType, tilt, azimuth, surfaceNum, 0.4);
        }
        const Real64 groundCoverageRatio(rNumericArgs(NumFields::GROUND_COVERAGE_RATIO));

        PVWattsGenerator pvwattsGenerator(
            name, dcSystemCapacity, moduleType, arrayType, systemLosses, geometryType, tilt, azimuth, surfaceNum, groundCoverageRatio);
        return pvwattsGenerator;
    }

    Real64 PVWattsGenerator::getDCSystemCapacity()
    {
        return m_dcSystemCapacity;
    }

    ModuleType PVWattsGenerator::getModuleType()
    {
        return m_moduleType;
    }

    ArrayType PVWattsGenerator::getArrayType()
    {
        return m_arrayType;
    }

    Real64 PVWattsGenerator::getSystemLosses()
    {
        return m_systemLosses;
    }

    GeometryType PVWattsGenerator::getGeometryType()
    {
        return m_geometryType;
    }

    Real64 PVWattsGenerator::getTilt()
    {
        return m_tilt;
    }

    Real64 PVWattsGenerator::getAzimuth()
    {
        return m_azimuth;
    }

    DataSurfaces::SurfaceData &PVWattsGenerator::getSurface()
    {
        return DataSurfaces::Surface(m_surfaceNum);
    }

    Real64 PVWattsGenerator::getGroundCoverageRatio()
    {
        return m_groundCoverageRatio;
    }

    Real64 PVWattsGenerator::getCellTempearture()
    {
        return m_cellTemperature;
    }

    void PVWattsGenerator::setCellTemperature(Real64 cellTemp)
    {
        m_cellTemperature = cellTemp;
    }

    Real64 PVWattsGenerator::getPlaneOfArrayIrradiance()
    {
        return m_planeOfArrayIrradiance;
    }

    void PVWattsGenerator::setPlaneOfArrayIrradiance(Real64 poa)
    {
        m_planeOfArrayIrradiance = poa;
    }

    void PVWattsGenerator::setDCtoACRatio(Real64 const dc2ac)
    {
        m_DCtoACRatio = dc2ac;
    }

    void PVWattsGenerator::setInverterEfficiency(Real64 const inverterEfficiency)
    {
        m_inverterEfficiency = inverterEfficiency;
    }

    void PVWattsGenerator::calc()
    {
        using DataGlobals::HourOfDay;
        using DataGlobals::SecInHour;
        using DataGlobals::TimeStep;
        using DataGlobals::TimeStepZone;
        using DataHVACGlobals::TimeStepSys;

        // We only run this once for each zone time step.
        if (!DataGlobals::BeginTimeStepFlag) {
            m_outputDCEnergy = m_outputDCPower * TimeStepSys * SecInHour;
            m_outputACEnergy = m_outputACPower * TimeStepSys * SecInHour;
            return;
        }
        ssc_module_t pvwattsModule = ssc_module_create("pvwattsv5_1ts");
        assert(pvwattsModule != nullptr);
        ssc_data_t pvwattsData = ssc_data_create();

        // SSC Inputs
        // Time
        ssc_data_set_number(pvwattsData, "year", DataEnvironment::Year);
        ssc_data_set_number(pvwattsData, "month", DataEnvironment::Month);
        ssc_data_set_number(pvwattsData, "day", DataEnvironment::DayOfMonth);
        ssc_data_set_number(pvwattsData, "hour", DataGlobals::HourOfDay - 1);
        ssc_data_set_number(pvwattsData, "minute", (TimeStep - 0.5) * DataGlobals::MinutesPerTimeStep);
        ssc_data_set_number(pvwattsData, "lat", WeatherManager::WeatherFileLatitude);
        ssc_data_set_number(pvwattsData, "lon", WeatherManager::WeatherFileLongitude);
        ssc_data_set_number(pvwattsData, "tz", WeatherManager::WeatherFileTimeZone);

        // Weather Conditions
        ssc_data_set_number(pvwattsData, "beam", DataEnvironment::BeamSolarRad);
        ssc_data_set_number(pvwattsData, "diffuse", DataEnvironment::DifSolarRad);
        ssc_data_set_number(pvwattsData, "tamb", DataEnvironment::OutDryBulbTemp);
        ssc_data_set_number(pvwattsData, "wspd", DataEnvironment::WindSpeed);
        Real64 albedo = WeatherManager::TodayAlbedo(TimeStep, HourOfDay);
        if (!(std::isfinite(albedo) && albedo > 0.0 && albedo < 1)) {
            albedo = 0.2;
        }
        ssc_data_set_number(pvwattsData, "alb", albedo);

        // System Properties
        ssc_data_set_number(pvwattsData, "time_step", DataGlobals::TimeStepZone);
        ssc_data_set_number(pvwattsData, "system_capacity", m_dcSystemCapacity * 0.001);
        ssc_data_set_number(pvwattsData, "module_type", static_cast<int>(m_moduleType));
        ssc_data_set_number(pvwattsData, "dc_ac_ratio", m_DCtoACRatio);
        ssc_data_set_number(pvwattsData, "inv_eff", m_inverterEfficiency * 100.0);
        ssc_data_set_number(pvwattsData, "losses", m_systemLosses * 100.0);
        ssc_data_set_number(pvwattsData, "array_type", static_cast<int>(m_arrayType));
        ssc_data_set_number(pvwattsData, "tilt", m_tilt);
        ssc_data_set_number(pvwattsData, "azimuth", m_azimuth);
        ssc_data_set_number(pvwattsData, "gcr", m_groundCoverageRatio);

        // In/Out Properties
        ssc_data_set_number(pvwattsData, "tcell", m_cellTemperature);
        ssc_data_set_number(pvwattsData, "poa", m_planeOfArrayIrradiance);


        // TODO: Get the shad_beam from the geometry again.
//        Real64 shad_beam = 1.0;
//        if (m_geometryType == GeometryType::SURFACE) {
//            shad_beam = DataHeatBalance::SunlitFrac(TimeStep, HourOfDay, m_surfaceNum);
//        }

        if ( ssc_module_exec(pvwattsModule, pvwattsData) == 0) {
            // Error
            const char *errtext;
            int sscErrType;
            float time;
            int i = 0;
            while( (errtext = ssc_module_log(pvwattsModule, i++, &sscErrType, &time)) ) {
                std::string err("PVWatts: ");
                switch (sscErrType)
                {
                case SSC_WARNING:
                    err.append(errtext);
                    ShowWarningMessage(err);
                    break;
                case SSC_ERROR:
                    err.append(errtext);
                    ShowErrorMessage(err);
                    break;
                default:
                    break;
                }
            }
        } else {
            // Report Out
            ssc_data_get_number(pvwattsData, "dc", &m_outputDCPower);
            m_outputDCEnergy = m_outputDCPower * TimeStepSys * SecInHour;
            ssc_data_get_number(pvwattsData, "ac", &m_outputACPower);
            m_outputACEnergy = m_outputACPower * TimeStepSys * SecInHour;
            ssc_data_get_number(pvwattsData, "tcell", &m_cellTemperature);
            ssc_data_get_number(pvwattsData, "poa", &m_planeOfArrayIrradiance);
        }

        ssc_data_free(pvwattsData);
        ssc_module_free(pvwattsModule);
    }

    void PVWattsGenerator::getResults(Real64 &GeneratorPower, Real64 &GeneratorEnergy, Real64 &ThermalPower, Real64 &ThermalEnergy)
    {
        GeneratorPower = m_outputDCPower;
        GeneratorEnergy = m_outputDCEnergy;
        ThermalPower = 0.0;
        ThermalEnergy = 0.0;
    }

    PVWattsGenerator &GetOrCreatePVWattsGenerator(std::string const &GeneratorName)
    {
        // Find the generator, and create a new one if it hasn't been loaded yet.
        int ObjNum = inputProcessor->getObjectItemNum("Generator:PVWatts", UtilityRoutines::MakeUPPERCase(GeneratorName));
        assert(ObjNum >= 0);
        if (ObjNum == 0) {
            ShowFatalError("Cannot find Generator:PVWatts " + GeneratorName);
        }
        auto it = PVWattsGenerators.find(ObjNum);
        if (it == PVWattsGenerators.end()) {
            // It's not in the map, add it.
            PVWattsGenerators.insert(std::make_pair(ObjNum, PVWattsGenerator::createFromIdfObj(ObjNum)));
            PVWattsGenerator &pvw(PVWattsGenerators.find(ObjNum)->second);
            pvw.setupOutputVariables();
            return pvw;
        } else {
            return it->second;
        }
    }

    void clear_state()
    {
        PVWattsGenerators.clear();
    }

} // namespace PVWatts

} // namespace EnergyPlus
