// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataHeatBalance.hh>
#include <General.hh>
#include <InputProcessing/InputProcessor.hh>
#include <OutputProcessor.hh>
#include <PVWatts.hh>
#include <PVWattsSSC.hh>
#include <UtilityRoutines.hh>
#include <WeatherManager.hh>

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
        : m_lastCellTemperature(20.0), m_lastPlaneOfArrayIrradiance(0.0), m_cellTemperature(20.0), m_planeOfArrayIrradiance(0.0),
          m_outputDCPower(1000.0)
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

        m_moduleType = moduleType;
        switch (m_moduleType) {
        case ModuleType::STANDARD:
            m_gamma = -0.0047;
            m_useARGlass = false;
            break;
        case ModuleType::PREMIUM:
            m_gamma = -0.0035;
            m_useARGlass = true;
            break;
        case ModuleType::THIN_FILM:
            m_gamma = -0.0020;
            m_useARGlass = false;
            break;
        }

        m_arrayType = arrayType;
        switch (m_arrayType) {
        case ArrayType::FIXED_OPEN_RACK:
            m_trackMode = 0;
            m_inoct = 45;
            m_shadeMode1x = 0;
            break;
        case ArrayType::FIXED_ROOF_MOUNTED:
            m_trackMode = 0;
            m_inoct = 49;
            m_shadeMode1x = 0;
            break;
        case ArrayType::ONE_AXIS:
            m_trackMode = 1;
            m_inoct = 45;
            m_shadeMode1x = 0;
            break;
        case ArrayType::ONE_AXIS_BACKTRACKING:
            m_trackMode = 1;
            m_inoct = 45;
            m_shadeMode1x = 1;
            break;
        case ArrayType::TWO_AXIS:
            m_trackMode = 2;
            m_inoct = 45;
            m_shadeMode1x = 0;
            break;
        }

        if (systemLosses > 1.0 || systemLosses < 0.0) {
            ShowSevereError("PVWatts: Invalid system loss value " + RoundSigDigits(systemLosses, 2));
            errorsFound = true;
        }
        m_systemLosses = systemLosses;

        m_geometryType = geometryType;

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

        // Set up the pvwatts cell temperature member
        const Real64 pvwatts_height = 5.0;
        m_tccalc = std::unique_ptr<pvwatts_celltemp>(new pvwatts_celltemp(m_inoct + 273.15, pvwatts_height, DataGlobals::TimeStepZone));
    }

    void PVWattsGenerator::setupOutputVariables()
    {
        // Set up output variables
        SetupOutputVariable("Generator Produced DC Electric Power", OutputProcessor::Unit::W, m_outputDCPower, "System", "Average", m_name);
        SetupOutputVariable("Generator Produced DC Electric Energy",
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
            return;
        }

        m_lastCellTemperature = m_cellTemperature;
        m_lastPlaneOfArrayIrradiance = m_planeOfArrayIrradiance;

        // initialize_cell_temp
        m_tccalc->set_last_values(m_lastCellTemperature, m_lastPlaneOfArrayIrradiance);

        Real64 albedo = WeatherManager::TodayAlbedo(TimeStep, HourOfDay);
        if (!(std::isfinite(albedo) && albedo > 0.0 && albedo < 1)) {
            albedo = 0.2;
        }

        // process_irradiance
        IrradianceOutput irr_st = processIrradiance(DataEnvironment::Year,
                                                    DataEnvironment::Month,
                                                    DataEnvironment::DayOfMonth,
                                                    HourOfDay - 1,
                                                    (TimeStep - 0.5) * DataGlobals::MinutesPerTimeStep,
                                                    TimeStepZone,
                                                    WeatherManager::WeatherFileLatitude,
                                                    WeatherManager::WeatherFileLongitude,
                                                    WeatherManager::WeatherFileTimeZone,
                                                    DataEnvironment::BeamSolarRad,
                                                    DataEnvironment::DifSolarRad,
                                                    albedo);

        // powerout
        Real64 shad_beam = 1.0;
        if (m_geometryType == GeometryType::SURFACE) {
            shad_beam = DataHeatBalance::SunlitFrac(TimeStep, HourOfDay, m_surfaceNum);
        }
        DCPowerOutput pwr_st =
            powerout(shad_beam, 1.0, DataEnvironment::BeamSolarRad, albedo, DataEnvironment::WindSpeed, DataEnvironment::OutDryBulbTemp, irr_st);

        // Report out
        m_cellTemperature = pwr_st.pvt;
        m_planeOfArrayIrradiance = pwr_st.poa;
        m_outputDCPower = pwr_st.dc;
        m_outputDCEnergy = m_outputDCPower * TimeStepSys * SecInHour;
    }

    void PVWattsGenerator::getResults(Real64 &GeneratorPower, Real64 &GeneratorEnergy, Real64 &ThermalPower, Real64 &ThermalEnergy)
    {
        GeneratorPower = m_outputDCPower;
        GeneratorEnergy = m_outputDCEnergy;
        ThermalPower = 0.0;
        ThermalEnergy = 0.0;
    }

    IrradianceOutput PVWattsGenerator::processIrradiance(
        int year, int month, int day, int hour, Real64 minute, Real64 ts_hour, Real64 lat, Real64 lon, Real64 tz, Real64 dn, Real64 df, Real64 alb)
    {
        IrradianceOutput out;

        using DataGlobals::HourOfDay;
        using DataGlobals::TimeStep;

        irrad irr;
        irr.set_time(year, month, day, hour, minute, ts_hour);
        irr.set_location(lat, lon, tz);
        irr.set_sky_model(2, alb);
        irr.set_beam_diffuse(dn, df);
        irr.set_surface(m_trackMode, m_tilt, m_azimuth, 45.0, m_shadeMode1x == 1, m_groundCoverageRatio);

        int irrRetCode = irr.calc();

        if (irrRetCode != 0) {
            ShowFatalError("PVWatts: Failed to calculate plane of array irradiance with given input parameters.");
        }

        irr.get_sun(&out.solazi, &out.solzen, &out.solalt, 0, 0, 0, &out.sunup, 0, 0, 0);
        irr.get_angles(&out.aoi, &out.stilt, &out.sazi, &out.rot, &out.btd);
        irr.get_poa(&out.ibeam, &out.iskydiff, &out.ignddiff, 0, 0, 0);

        return out;
    }

    DCPowerOutput
    PVWattsGenerator::powerout(Real64 &shad_beam, Real64 shad_diff, Real64 dni, Real64 alb, Real64 wspd, Real64 tdry, IrradianceOutput &irr_st)
    {

        using DataGlobals::DegToRadians;
        using DataGlobals::RadToDeg;
        using General::RoundSigDigits;

        const Real64 &gcr = m_groundCoverageRatio;

        Real64 poa, tpoa, pvt, dc;

        if (irr_st.sunup > 0) {
            if (m_trackMode == 1 && m_shadeMode1x == 0) {
                Real64 shad1xf = shade_fraction_1x(irr_st.solazi, irr_st.solzen, m_tilt, m_azimuth, m_groundCoverageRatio, irr_st.rot);
                shad_beam *= 1 - shad1xf;

                if (irr_st.iskydiff > 0) {
                    Real64 reduced_skydiff = irr_st.iskydiff;
                    Real64 Fskydiff = 1.0;
                    Real64 reduced_gnddiff = irr_st.ignddiff;
                    Real64 Fgnddiff = 1.0;

                    // worst-case mask angle using calculated surface tilt
                    Real64 phi0 = RadToDeg * std::atan2(std::sin(irr_st.stilt * DegToRadians),
                                                        1.0 / m_groundCoverageRatio - std::cos(irr_st.stilt * DegToRadians));

                    // calculate sky and gnd diffuse derate factors
                    // based on view factor reductions from self-shading
                    diffuse_reduce(irr_st.solzen,
                                   irr_st.stilt,
                                   dni,
                                   irr_st.iskydiff + irr_st.ignddiff,
                                   gcr,
                                   phi0,
                                   alb,
                                   1000,

                                   // outputs (pass by reference)
                                   reduced_skydiff,
                                   Fskydiff,
                                   reduced_gnddiff,
                                   Fgnddiff);

                    if (Fskydiff >= 0 && Fskydiff <= 1)
                        irr_st.iskydiff *= Fskydiff;
                    else
                        ShowWarningError("PVWatts: sky diffuse reduction factor invalid: fskydiff=" + RoundSigDigits(Fskydiff, 7) +
                                         ", stilt=" + RoundSigDigits(irr_st.stilt, 7));

                    if (Fgnddiff >= 0 && Fgnddiff <= 1)
                        irr_st.ignddiff *= Fgnddiff;
                    else
                        ShowWarningError("PVWatts: gnd diffuse reduction factor invalid: fgnddiff=" + RoundSigDigits(Fgnddiff, 7) +
                                         ", stilt=" + RoundSigDigits(irr_st.stilt, 7));
                }
            }

            // apply hourly shading factors to beam (if none enabled, factors are 1.0)
            irr_st.ibeam *= shad_beam;

            // apply sky diffuse shading factor (specified as constant, nominally 1.0 if disabled in UI)
            irr_st.iskydiff *= shad_diff;

            poa = irr_st.ibeam + irr_st.iskydiff + irr_st.ignddiff;

            Real64 wspd_corr = wspd < 0 ? 0 : wspd;

            // module cover
            tpoa = poa;
            if (irr_st.aoi > 0.5 && irr_st.aoi < 89.5) {
                double mod = iam(irr_st.aoi, m_useARGlass);
                tpoa = poa - (1.0 - mod) * dni * cosd(irr_st.aoi);
                if (tpoa < 0.0) tpoa = 0.0;
                if (tpoa > poa) tpoa = poa;
            }

            // cell temperature
            pvt = (*m_tccalc)(poa, wspd_corr, tdry);

            // dc power output (Watts)
            dc = m_dcSystemCapacity * (1.0 + m_gamma * (pvt - 25.0)) * tpoa / 1000.0;

            // dc losses
            dc *= 1.0 - m_systemLosses;

        } else {
            poa = 0.0;
            tpoa = 0.0;
            pvt = tdry;
            dc = 0.0;
        }

        DCPowerOutput pwrOutput = {poa, tpoa, pvt, dc};

        return pwrOutput;
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
