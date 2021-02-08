#include <memory>
#include <vector>

#include "lib_pv_io_manager.h"

static const int __nday[12] = { 31,28,31,30,31,30,31,31,30,31,30,31 };

PVIOManager::PVIOManager(compute_module* cm, std::string cmName)
{
    std::unique_ptr<Irradiance_IO> ptr(new Irradiance_IO(cm, cmName));
    m_IrradianceIO = std::move(ptr);

    std::unique_ptr<Simulation_IO> ptr2(new Simulation_IO(cm, *m_IrradianceIO));
    m_SimulationIO = std::move(ptr2);

    std::unique_ptr<ShadeDB8_mpp> shadeDatabase(new ShadeDB8_mpp());
    m_shadeDatabase = std::move(shadeDatabase);
    m_shadeDatabase->init();

    std::unique_ptr<Inverter_IO> ptrInv(new Inverter_IO(cm, cmName));
    m_InverterIO = std::move(ptrInv);

    // Gather subarrays which are enabled
    nSubarrays = 1;
    std::unique_ptr<Subarray_IO> subarray1(new Subarray_IO(cm, cmName, 1));
    m_SubarraysIO.push_back(std::move(subarray1));

    for (size_t subarray = 2; subarray <= 4; subarray++)
    {
        // can eventually create a module for each Subarray to allow flexibility.
        std::unique_ptr<Subarray_IO> ptr3(new Subarray_IO(cm, cmName, subarray));
        if (ptr3->enable) {
            m_SubarraysIO.push_back(std::move(ptr3));
            nSubarrays++;
        }
    }

    // Aggregate Subarray outputs in different structure
    std::unique_ptr<PVSystem_IO> pvSystem(new PVSystem_IO(cm, cmName, m_SimulationIO.get(), m_IrradianceIO.get(), getSubarrays(), m_InverterIO.get()));
    m_PVSystemIO = std::move(pvSystem);

    // Allocate outputs, moved to here due to previous difficult to debug crashes due to misallocation
    allocateOutputs(cm);

    m_computeModule = cm;
    m_computeModuleName = cmName;
}

void PVIOManager::allocateOutputs(compute_module* cm)
{
    m_IrradianceIO->AllocateOutputs(cm);
    m_PVSystemIO->AllocateOutputs(cm);
}

Irradiance_IO* PVIOManager::getIrradianceIO() { return m_IrradianceIO.get(); }
compute_module* PVIOManager::getComputeModule() { return m_computeModule; }
Subarray_IO* PVIOManager::getSubarrayIO(size_t subarrayNumber) { return m_SubarraysIO[subarrayNumber].get(); }
ShadeDB8_mpp* PVIOManager::getShadeDatabase() { return m_shadeDatabase.get(); }

std::vector<Subarray_IO*> PVIOManager::getSubarrays()
{
    std::vector<Subarray_IO*> subarrays;
    for (size_t subarray = 0; subarray < m_SubarraysIO.size(); subarray++) {
        subarrays.push_back(m_SubarraysIO[subarray].get());
    }
    return subarrays;
}

PVSystem_IO* PVIOManager::getPVSystemIO() { return m_PVSystemIO.get(); }

Simulation_IO* PVIOManager::getSimulationIO() { return m_SimulationIO.get(); }


Simulation_IO::Simulation_IO(compute_module* cm, Irradiance_IO& IrradianceIO)
{
    numberOfWeatherFileRecords = IrradianceIO.numberOfWeatherFileRecords;
    stepsPerHour = IrradianceIO.stepsPerHour;
    dtHour = IrradianceIO.dtHour;

    useLifetimeOutput = false;
    if (cm->is_assigned("system_use_lifetime_output")) useLifetimeOutput = cm->as_integer("system_use_lifetime_output");
    numberOfYears = 1;
    saveLifetimeVars = 0;
    if (useLifetimeOutput) {
        numberOfYears = cm->as_integer("analysis_period");
        saveLifetimeVars = cm->as_integer("save_full_lifetime_variables");
    }
    numberOfSteps = numberOfYears * numberOfWeatherFileRecords;
    annualSimulation = IrradianceIO.weatherDataProvider->annualSimulation();
}

Irradiance_IO::Irradiance_IO(compute_module* cm, std::string cmName)
{
    numberOfSubarrays = 4;
    radiationMode = cm->as_integer("irrad_mode");
    skyModel = cm->as_integer("sky_model");

    if (cm->is_assigned("solar_resource_file")) {
        weatherDataProvider = std::unique_ptr<weather_data_provider>(new weatherfile(cm->as_string("solar_resource_file")));
        weatherfile* weatherFile = dynamic_cast<weatherfile*>(weatherDataProvider.get());
        if (!weatherFile->ok()) throw exec_error(cmName, weatherFile->message());
        if (weatherFile->has_message()) cm->log(weatherFile->message(), SSC_WARNING);
    }
    else if (cm->is_assigned("solar_resource_data")) {
        weatherDataProvider = std::unique_ptr<weather_data_provider>(new weatherdata(cm->lookup("solar_resource_data")));
        if (weatherDataProvider->has_message()) cm->log(weatherDataProvider->message(), SSC_WARNING);
    }
    else {
        throw exec_error(cmName, "No weather data supplied");
    }

    // assumes instantaneous values, unless hourly file with no minute column specified
    tsShiftHours = 0.0;
    instantaneous = true;
    if (weatherDataProvider->has_data_column(weather_data_provider::MINUTE))
    {
        // if we have an file with a minute column, then
        // the starting time offset equals the time
        // of the first record (for correct plotting)
        // this holds true even for hourly data with a minute column
        weather_record rec;
        if (weatherDataProvider->read(&rec))
            tsShiftHours = rec.minute / 60.0;

        weatherDataProvider->rewind();
    }
    else if (weatherDataProvider->annualSimulation() && weatherDataProvider->nrecords() == 8760)
    {
        // hourly file with no minute data column.  assume
        // integrated/averaged values and use mid point convention for interpreting results
        instantaneous = false;
        tsShiftHours = 0.5;
    }
    else
        throw exec_error(cmName, "subhourly and non-annual weather files must specify the minute for each record");

    weatherDataProvider->header(&weatherHeader);

    //total number of records in the weather file (i.e. 8760 * timestep)
    numberOfWeatherFileRecords = weatherDataProvider->nrecords();
    dtHour = 1.0; //initialize these values to 1 for non-annual simulations
    stepsPerHour = 1.0;
    if (weatherDataProvider->annualSimulation())
    {
        stepsPerHour = numberOfWeatherFileRecords / 8760;
        if (stepsPerHour > 0)
            dtHour /= stepsPerHour;
    }

    if (weatherDataProvider->annualSimulation() && numberOfWeatherFileRecords % 8760 != 0)
        throw exec_error(cmName, util::format("invalid number of data records (%zu): must be an integer multiple of 8760", numberOfWeatherFileRecords));
    if (weatherDataProvider->annualSimulation() && (stepsPerHour < 1 || stepsPerHour > 60))
        throw exec_error(cmName, util::format("%d timesteps per hour found. Weather data should be single year.", stepsPerHour));

    useWeatherFileAlbedo = cm->as_boolean("use_wf_albedo");
    userSpecifiedMonthlyAlbedo = cm->as_vector_double("albedo");

    checkWeatherFile(cm, cmName);
}

void Irradiance_IO::checkWeatherFile(compute_module* cm, std::string cmName)
{
    for (size_t idx = 0; idx < numberOfWeatherFileRecords; idx++)
    {
        if (!weatherDataProvider->read(&weatherRecord))
            throw exec_error(cmName, "could not read data line " + util::to_string((int)(idx + 1)) + " in weather file");

        // Check for missing data
        if ((weatherRecord.gh != weatherRecord.gh) && (radiationMode == irrad::DN_GH || radiationMode == irrad::GH_DF)) {
            cm->log(util::format("missing global irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
                weatherRecord.gh, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_ERROR, (float)idx);
            return;
        }
        if ((weatherRecord.dn != weatherRecord.dn) && (radiationMode == irrad::DN_DF || radiationMode == irrad::DN_GH)) {
            cm->log(util::format("missing beam irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
                weatherRecord.dn, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_ERROR, (float)idx);
            return;
        }
        if ((weatherRecord.df != weatherRecord.df) && (radiationMode == irrad::DN_DF || radiationMode == irrad::GH_DF)) {
            cm->log(util::format("missing diffuse irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
                weatherRecord.df, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_ERROR, (float)idx);
            return;
        }
        if ((weatherRecord.poa != weatherRecord.poa) && (radiationMode == irrad::POA_R || radiationMode == irrad::POA_P)) {
            cm->log(util::format("missing POA irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
                weatherRecord.poa, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_ERROR, (float)idx);
            return;
        }
        if (weatherRecord.tdry != weatherRecord.tdry) {
            cm->log(util::format("missing temperature %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
                weatherRecord.tdry, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_ERROR, (float)idx);
            return;
        }
        if (weatherRecord.wspd != weatherRecord.wspd) {
            cm->log(util::format("missing wind speed %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
                weatherRecord.wspd, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_ERROR, (float)idx);
            return;
        }

        // Check for bad data
        if ((weatherRecord.gh < 0 || weatherRecord.gh >  irrad::irradiationMax) && (radiationMode == irrad::DN_GH || radiationMode == irrad::GH_DF))
        {
            cm->log(util::format("out of range global irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero",
                weatherRecord.gh, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_WARNING, (float)idx);
            weatherRecord.gh = 0;
        }
        if ((weatherRecord.dn < 0 || weatherRecord.dn >  irrad::irradiationMax) && (radiationMode == irrad::DN_DF || radiationMode == irrad::DN_GH))
        {
            cm->log(util::format("out of range beam irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero",
                weatherRecord.dn, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_WARNING, (float)idx);
            weatherRecord.dn = 0;
        }
        if ((weatherRecord.df < 0 || weatherRecord.df >  irrad::irradiationMax) && (radiationMode == irrad::DN_DF || radiationMode == irrad::GH_DF))
        {
            cm->log(util::format("out of range diffuse irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero",
                weatherRecord.df, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_WARNING, (float)idx);
            weatherRecord.df = 0;
        }
        if ((weatherRecord.poa < 0 || weatherRecord.poa >  irrad::irradiationMax) && (radiationMode == irrad::POA_R || radiationMode == irrad::POA_P))
        {
            cm->log(util::format("out of range POA irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero",
                weatherRecord.poa, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_WARNING, (float)idx);
            weatherRecord.poa = 0;
        }
        //albedo is allowed to be missing in the weather file- will be filled in from user-entered monthly array.
        //only throw an error if there's a value that isn't reasonable somewhere
        int month_idx = weatherRecord.month - 1;
        bool albedoError = true;
        if (useWeatherFileAlbedo && std::isfinite(weatherRecord.alb) && weatherRecord.alb > 0 && weatherRecord.alb < 1) {
            albedoError = false;
        }
        else if (month_idx >= 0 && month_idx < 12) {
            if (userSpecifiedMonthlyAlbedo[month_idx] > 0 && userSpecifiedMonthlyAlbedo[month_idx] < 1) {
                albedoError = false;
                weatherRecord.alb = userSpecifiedMonthlyAlbedo[month_idx];
            }
        }
        if (albedoError) {
            throw exec_error(cmName,
                util::format("Error retrieving albedo value: Invalid month in weather file or invalid albedo value in weather file"));
        }
    }
    weatherDataProvider->rewind();
}

void Irradiance_IO::AllocateOutputs(compute_module* cm)
{
    p_weatherFileGHI = cm->allocate("gh", numberOfWeatherFileRecords);
    p_weatherFileDNI = cm->allocate("dn", numberOfWeatherFileRecords);
    p_weatherFileDHI = cm->allocate("df", numberOfWeatherFileRecords);
    p_sunPositionTime = cm->allocate("sunpos_hour", numberOfWeatherFileRecords);
    p_weatherFileWindSpeed = cm->allocate("wspd", numberOfWeatherFileRecords);
    p_weatherFileAmbientTemp = cm->allocate("tdry", numberOfWeatherFileRecords);
    p_weatherFileAlbedo = cm->allocate("alb", numberOfWeatherFileRecords);
    p_weatherFileSnowDepth = cm->allocate("snowdepth", numberOfWeatherFileRecords);

    // If using input POA, must have POA for every subarray or assume POA applies to each subarray
    for (size_t subarray = 0; subarray != numberOfSubarrays; subarray++) {
        std::string wfpoa = "wfpoa" + util::to_string(static_cast<int>(subarray + 1));
        p_weatherFilePOA.push_back(cm->allocate(wfpoa, numberOfWeatherFileRecords));
    }

    //set up the calculated components of irradiance such that they aren't reported if they aren't assigned
    //three possible calculated irradiance: gh, df, dn
    if (radiationMode == irrad::DN_DF) p_IrradianceCalculated[0] = cm->allocate("gh_calc", numberOfWeatherFileRecords); //don't calculate global for POA models
    if (radiationMode == irrad::DN_GH || radiationMode == irrad::POA_R || radiationMode == irrad::POA_P) p_IrradianceCalculated[1] = cm->allocate("df_calc", numberOfWeatherFileRecords);
    if (radiationMode == irrad::GH_DF || radiationMode == irrad::POA_R || radiationMode == irrad::POA_P) p_IrradianceCalculated[2] = cm->allocate("dn_calc", numberOfWeatherFileRecords);

    //output arrays for solar position calculations- same for all four subarrays
    p_sunZenithAngle = cm->allocate("sol_zen", numberOfWeatherFileRecords);
    p_sunAltitudeAngle = cm->allocate("sol_alt", numberOfWeatherFileRecords);
    p_sunAzimuthAngle = cm->allocate("sol_azi", numberOfWeatherFileRecords);
    p_absoluteAirmass = cm->allocate("airmass", numberOfWeatherFileRecords);
    p_sunUpOverHorizon = cm->allocate("sunup", numberOfWeatherFileRecords);
}

void Irradiance_IO::AssignOutputs(compute_module* cm)
{
    cm->assign("ts_shift_hours", var_data((ssc_number_t)tsShiftHours));
}

Subarray_IO::Subarray_IO(compute_module* cm, const std::string& cmName, size_t subarrayNumber)
{
    prefix = "subarray" + util::to_string(static_cast<int>(subarrayNumber)) + "_";

    enable = true;
    if (subarrayNumber > 1)
        enable = cm->as_boolean(prefix + "enable");

    if (enable)
    {
        int n = cm->as_integer(prefix + "nstrings");
        if (n < 0) {
            throw exec_error(cmName, "invalid string allocation between subarrays.  all subarrays must have zero or positive number of strings.");
        }

        nStrings = n;
        if (nStrings == 0) //subarrays with no strings need to be treated as if they are disabled to avoid divide by zero issues in the subarray setup
        {
            enable = false;
            return;
        }

        nModulesPerString = cm->as_integer(prefix + "modules_per_string");
        mpptInput = cm->as_integer(prefix + "mppt_input");
        trackMode = cm->as_integer(prefix + "track_mode");
        tiltEqualLatitude = 0;
        if (cm->is_assigned(prefix + "tilt_eq_lat")) tiltEqualLatitude = cm->as_boolean(prefix + "tilt_eq_lat");

        //tilt required for fixed tilt, single axis, and azimuth axis- can't check for this in variable table so check here
        tiltDegrees = std::numeric_limits<double>::quiet_NaN();
        if (trackMode == irrad::FIXED_TILT || trackMode == irrad::SINGLE_AXIS || trackMode == irrad::AZIMUTH_AXIS)
            if (!tiltEqualLatitude && !cm->is_assigned(prefix + "tilt"))
                throw exec_error(cmName, "Subarray " + util::to_string((int)subarrayNumber) + " tilt required but not assigned.");
        if (cm->is_assigned(prefix + "tilt")) tiltDegrees = fabs(cm->as_double(prefix + "tilt"));
        //monthly tilt required if seasonal tracking mode selected- can't check for this in variable table so check here
        if (trackMode == irrad::SEASONAL_TILT && !cm->is_assigned(prefix + "monthly_tilt"))
            throw exec_error(cmName, "Subarray " + util::to_string((int)subarrayNumber) + " monthly tilt required but not assigned.");
        if (cm->is_assigned(prefix + "monthly_tilt")) monthlyTiltDegrees = cm->as_vector_double(prefix + "monthly_tilt");
        //azimuth required for fixed tilt, single axis, and seasonal tilt- can't check for this in variable table so check here
        azimuthDegrees = std::numeric_limits<double>::quiet_NaN();
        if (trackMode == irrad::FIXED_TILT || trackMode == irrad::SINGLE_AXIS || trackMode == irrad::SEASONAL_TILT)
            if (!cm->is_assigned(prefix + "azimuth"))
                throw exec_error(cmName, "Subarray " + util::to_string((int)subarrayNumber) + " azimuth required but not assigned.");
        if (cm->is_assigned(prefix + "azimuth")) azimuthDegrees = cm->as_double(prefix + "azimuth");

        trackerRotationLimitDegrees = cm->as_double(prefix + "rotlim");
        groundCoverageRatio = cm->as_double(prefix + "gcr");

        //check that backtracking input is assigned here because cannot check in the variable table
        backtrackingEnabled = 0;
        if (trackMode == irrad::SINGLE_AXIS)
            if (!cm->is_assigned(prefix + "backtrack"))
                throw exec_error(cmName, "Subarray " + util::to_string((int)subarrayNumber) + " backtrack required but not assigned.");
        if (cm->is_assigned(prefix + "backtrack")) backtrackingEnabled = cm->as_boolean(prefix + "backtrack");
        moduleAspectRatio = cm->as_double("module_aspect_ratio");
        usePOAFromWeatherFile = false;

        // losses
        rearIrradianceLossPercent = cm->as_double(prefix + "rear_irradiance_loss") / 100;
        dcOptimizerLossPercent = cm->as_double("dcoptimizer_loss") / 100;
        mismatchLossPercent = cm->as_double(prefix + "mismatch_loss") / 100;
        diodesLossPercent = cm->as_double(prefix + "diodeconn_loss") / 100;
        dcWiringLossPercent = cm->as_double(prefix + "dcwiring_loss") / 100;
        trackingLossPercent = cm->as_double(prefix + "tracking_loss") / 100;
        nameplateLossPercent = cm->as_double(prefix + "nameplate_loss") / 100;

        dcLossTotalPercent = 1 - (
            (1 - dcOptimizerLossPercent) *
            (1 - mismatchLossPercent) *
            (1 - diodesLossPercent) *
            (1 - dcWiringLossPercent) *
            (1 - trackingLossPercent) *
            (1 - nameplateLossPercent));

        if (groundCoverageRatio < 0.01)
            throw exec_error(cmName, "array ground coverage ratio must obey 0.01 < gcr");


        monthlySoiling = cm->as_vector_double(prefix + "soiling");
        if (monthlySoiling.size() != 12) throw exec_error(cmName, "soiling loss array must have 12 values: subarray " + util::to_string((int)(subarrayNumber)));

        //convert from % to derate
        for (size_t m = 0; m < monthlySoiling.size(); m++)
            monthlySoiling[m] = 1.0 - monthlySoiling[m] / 100.0;

        // Shading database
        enableSelfShadingOutputs = false;
        if (!shadeCalculator.setup(cm, prefix)) {
            throw exec_error(cmName, prefix + "_shading: " + shadeCalculator.get_error());
        }

        shadeMode = cm->as_integer(prefix + "shade_mode");
        selfShadingInputs.mod_orient = cm->as_integer(prefix + "mod_orient"); //although these inputs are stored in self-shading structure, they are also used for snow model and bifacial model, so required for all enabled subarrays
        selfShadingInputs.nmody = cm->as_integer(prefix + "nmody"); //same as above
        selfShadingInputs.nmodx = cm->as_integer(prefix + "nmodx"); //same as above
        selfShadingInputs.nstrx = selfShadingInputs.nmodx / nModulesPerString;
        poa.nonlinearDCShadingDerate = 1;
        selfShadingSkyDiffTable.init(tiltDegrees, groundCoverageRatio);

        if (trackMode == irrad::FIXED_TILT || trackMode == irrad::SEASONAL_TILT || (trackMode == irrad::SINGLE_AXIS))
        {
            if (shadeMode != NO_SHADING)
            {
                // Calculate the number of rows given the module dimensions of each row.
                selfShadingInputs.nrows = (int)floor((nStrings * nModulesPerString) / (selfShadingInputs.nmodx * selfShadingInputs.nmody));

                //if nrows comes out to be zero, this will cause a divide by zero error. Give an error in this case.
                if (selfShadingInputs.nrows == 0 && nStrings != 0)
                    throw exec_error(cmName, "Self shading: Number of rows calculated for subarray " + util::to_string(int(subarrayNumber)) + " was zero. Please check your inputs.");

                // Otherwise, if self-shading configuration does not have equal number of modules as specified on system design page for that subarray,
                // compute dc derate using the self-shading configuration and apply it to the whole subarray. Give warning.
                if ((size_t)(selfShadingInputs.nmodx * selfShadingInputs.nmody * selfShadingInputs.nrows) != (size_t)(nStrings * nModulesPerString))
                    cm->log(util::format("The product of number of modules along side and bottom for subarray %d is not equal to the number of modules in the subarray. Check your inputs for self shading.",
                        int(subarrayNumber)), SSC_WARNING);

                // assume aspect ratio of 1.7 (see variable "aspect_ratio" below to change this assumption)
                selfShadingInputs.str_orient = 1;	//assume horizontal wiring
                selfShadingInputs.mask_angle_calc_method = 0; //assume worst case mask angle calc method
                selfShadingInputs.ndiode = 3;	//assume 3 diodes- maybe update this assumption based on number of cells in the module?
            }
        }

        // Snow model
        subarrayEnableSnow = cm->as_boolean("en_snow_model");
        if (subarrayEnableSnow)
        {
            if (trackMode == irrad::SEASONAL_TILT)
                throw exec_error(cmName, "Time-series tilt input may not be used with the snow model at this time: subarray " + util::to_string((int)(subarrayNumber)));
            // if tracking mode is 1-axis tracking, don't need to limit tilt angles
            if (snowModel.setup(selfShadingInputs.nmody, (float)tiltDegrees, (trackMode != irrad::SINGLE_AXIS))) {
                if (!snowModel.good) {
                    cm->log(snowModel.msg, SSC_ERROR);
                }
            }
        }

        // Initialize module model
        std::unique_ptr<Module_IO> tmp(new Module_IO(cm, cmName, 1 - dcLossTotalPercent));
        Module = std::move(tmp);
    }
}
void Subarray_IO::AssignOutputs(compute_module* cm)
{
    //assign output dc loss
    double tmp = dcLossTotalPercent * 100;
    cm->assign(prefix + "dcloss", var_data((ssc_number_t)tmp));

    Module->AssignOutputs(cm);
}

void PVSystem_IO::SetupPOAInput()
{

    // Check if a POA model is used, if so load all POA data into the poaData struct
    if (Irradiance->radiationMode == irrad::POA_R || Irradiance->radiationMode == irrad::POA_P) {
        for (size_t nn = 0; nn < Subarrays.size(); nn++) {
            if (!Subarrays[nn]->enable) continue;

            std::unique_ptr<poaDecompReq> tmp(new poaDecompReq);
            Subarrays[nn]->poa.poaAll = std::move(tmp);
            Subarrays[nn]->poa.poaAll->elev = Irradiance->weatherHeader.elev;

            if (Irradiance->stepsPerHour > 1) {
                Subarrays[nn]->poa.poaAll->stepScale = 'm';
                Subarrays[nn]->poa.poaAll->stepSize = 60.0 / Irradiance->stepsPerHour;
            }

            Subarrays[nn]->poa.poaAll->POA.reserve(Irradiance->numberOfWeatherFileRecords);
            Subarrays[nn]->poa.poaAll->inc.reserve(Irradiance->numberOfWeatherFileRecords);
            Subarrays[nn]->poa.poaAll->tilt.reserve(Irradiance->numberOfWeatherFileRecords);
            Subarrays[nn]->poa.poaAll->zen.reserve(Irradiance->numberOfWeatherFileRecords);
            Subarrays[nn]->poa.poaAll->exTer.reserve(Irradiance->numberOfWeatherFileRecords);

            for (size_t i = 0; i < Irradiance->numberOfWeatherFileRecords; i++) {
                Subarrays[nn]->poa.poaAll->POA.push_back(0);
                Subarrays[nn]->poa.poaAll->inc.push_back(0);
                Subarrays[nn]->poa.poaAll->tilt.push_back(0);
                Subarrays[nn]->poa.poaAll->zen.push_back(0);
                Subarrays[nn]->poa.poaAll->exTer.push_back(0);
            }


            double ts_hour = Simulation->dtHour;
            weather_header hdr = Irradiance->weatherHeader;
            weather_data_provider* wdprov = Irradiance->weatherDataProvider.get();
            weather_record wf = Irradiance->weatherRecord;
            wdprov->rewind();

            for (size_t ii = 0; ii < Irradiance->numberOfWeatherFileRecords; ii++) {

                if (!wdprov->read(&wf)) {
                    throw exec_error("pvsamv1", "could not read data line " + util::to_string((int)(ii + 1)) + " in weather file while loading POA data");
                }
                int month_idx = wf.month - 1;

                if (Subarrays[nn]->trackMode == irrad::SEASONAL_TILT)
                    Subarrays[nn]->tiltDegrees = Subarrays[nn]->monthlyTiltDegrees[month_idx]; //overwrite the tilt input with the current tilt to be used in calculations

                // save POA data
                if (wf.poa > 0)
                    Subarrays[nn]->poa.poaAll->POA[ii] = wf.poa;
                else
                    Subarrays[nn]->poa.poaAll->POA[ii] = -999;

                // Calculate incident angle
                double t_cur = wf.hour + wf.minute / 60;

                // Calculate sunrise and sunset hours in local standard time for the current day
                double sun[9], angle[5];
                int tms[3];

                static const int dut1 = 0; //leap second, assumed not used for analysis
                solarpos_spa(wf.year, wf.month, wf.day, 12, 0.0, 0.0, hdr.lat, hdr.lon, hdr.tz, dut1, hdr.elev, wf.pres, wf.tdry, 0, 180, sun);

                double t_sunrise = sun[4];
                double t_sunset = sun[5];

                if (t_sunset > 24) //sunset is legitimately the next day, so recalculate sunset from the previous day
                {
                    double sunanglestemp[9];
                    if (wf.day > 1) //simply decrement day during month
                        solarpos_spa(wf.year, wf.month, wf.day - 1, 12, 0.0, 0.0, hdr.lat, hdr.lon, hdr.tz, dut1, hdr.elev, wf.pres, wf.tdry, 0, 180, sunanglestemp);
                    else if (wf.month > 1) //on the 1st of the month, need to switch to the last day of previous month
                        solarpos_spa(wf.year, wf.month - 1, __nday[wf.month - 2], 12, 0.0, 0.0, hdr.lat, hdr.lon, hdr.tz, dut1, hdr.elev, wf.pres, wf.tdry, 0, 180, sunanglestemp);//month is 1-indexed and __nday is 0 indexed
                    else //on the first day of the year, need to switch to Dec 31 of last year
                        solarpos_spa(wf.year - 1, 12, 31, 12, 0.0, 0.0, hdr.lat, hdr.lon, hdr.tz, dut1, hdr.elev, wf.pres, wf.tdry, 0, 180, sunanglestemp);
                    //if sunset from yesterday WASN'T today, then it's ok to leave sunset > 24, which will cause the sun to rise today and not set today
                    if (sunanglestemp[5] >= 24)
                        t_sunset = sunanglestemp[5] - 24.0;
                }

                if (t_sunrise < 0) //sunrise is legitimately the previous day, so recalculate for next day
                {
                    double sunanglestemp[9];
                    if (wf.day < __nday[wf.month - 1]) //simply increment the day during the month, month is 1-indexed and __nday is 0-indexed
                        solarpos_spa(wf.year, wf.month, wf.day + 1, 12, 0.0, 0.0, hdr.lat, hdr.lon, hdr.tz, dut1, hdr.elev, wf.pres, wf.tdry, 0, 180, sunanglestemp);
                    else if (wf.month < 12) //on the last day of the month, need to switch to the first day of the next month
                        solarpos_spa(wf.year, wf.month + 1, 1, 12, 0.0, 0.0, hdr.lat, hdr.lon, hdr.tz, dut1, hdr.elev, wf.pres, wf.tdry, 0, 180, sunanglestemp);
                    else //on the last day of the year, need to switch to Jan 1 of the next year
                        solarpos_spa(wf.year + 1, 1, 1, 12, 0.0, 0.0, hdr.lat, hdr.lon, hdr.tz, dut1, hdr.elev, wf.pres, wf.tdry, 0, 180, sunanglestemp);
                    //if sunrise from tomorrow isn't today, then it's ok to leave sunrise < 0, which will cause the sun to set at the right time and not rise until tomorrow
                    if (sunanglestemp[4] < 0)
                        t_sunrise = sunanglestemp[4] + 24.0;

                }

                // time step encompasses the sunrise
                if (t_cur >= t_sunrise - ts_hour / 2.0 && t_cur < t_sunrise + ts_hour / 2.0)
                {
                    double t_calc = (t_sunrise + (t_cur + ts_hour / 2.0)) / 2.0; // midpoint of sunrise and end of timestep
                    int hr_calc = (int)t_calc;
                    double min_calc = (t_calc - hr_calc) * 60.0;

                    tms[0] = hr_calc;
                    tms[1] = (int)min_calc;

                    solarpos_spa(wf.year, wf.month, wf.day, hr_calc, min_calc, 0, hdr.lat, hdr.lon, hdr.tz, dut1, hdr.elev, wf.pres, wf.tdry, 0, 180, sun);


                    tms[2] = 2;
                }
                // timestep encompasses the sunset
                else if (t_cur > t_sunset - ts_hour / 2.0 && t_cur <= t_sunset + ts_hour / 2.0)
                {
                    double t_calc = ((t_cur - ts_hour / 2.0) + t_sunset) / 2.0; // midpoint of beginning of timestep and sunset
                    int hr_calc = (int)t_calc;
                    double min_calc = (t_calc - hr_calc) * 60.0;

                    tms[0] = hr_calc;
                    tms[1] = (int)min_calc;

                    solarpos_spa(wf.year, wf.month, wf.day, hr_calc, min_calc, 0.0, hdr.lat, hdr.lon, hdr.tz, dut1, hdr.elev, wf.pres, wf.tdry, 0, 180, sun);

                    tms[2] = 3;
                }

                // timestep is not sunrise nor sunset, but sun is up  (calculate position at provided t_cur)
                else if ((t_sunrise < t_sunset && t_cur >= t_sunrise && t_cur <= t_sunset) || //this captures normal daylight cases
                    (t_sunrise > t_sunset && (t_cur <= t_sunset || t_cur >= t_sunrise))) //this captures cases where sunset (from previous day) is 1:30AM, sunrise 2:30AM, in arctic circle
                {
                    // timestep is not sunrise nor sunset, but sun is up  (calculate position at provided t_cur)
                    tms[0] = wf.hour;
                    tms[1] = (int)wf.minute;
                    solarpos_spa(wf.year, wf.month, wf.day, wf.hour, wf.minute, 0.0, hdr.lat, hdr.lon, hdr.tz, dut1, hdr.elev, wf.pres, wf.tdry, 0, 180, sun);
                    tms[2] = 1;
                }
                else
                {
                    // sun is down, assign sundown values
                    solarpos_spa(wf.year, wf.month, wf.day, wf.hour, wf.minute, 0.0, hdr.lat, hdr.lon, hdr.tz, dut1, hdr.elev, wf.pres, wf.tdry, 0, 180, sun);
                    tms[0] = wf.hour;
                    tms[1] = (int)wf.minute;
                    tms[2] = 0;
                }


                if (tms[2] > 0) {
                    incidence(Subarrays[nn]->trackMode, Subarrays[nn]->tiltDegrees, Subarrays[nn]->azimuthDegrees, Subarrays[nn]->trackerRotationLimitDegrees, sun[1], sun[0], Subarrays[nn]->backtrackingEnabled, Subarrays[nn]->groundCoverageRatio, false, 0.0, angle);
                }
                else {
                    angle[0] = -999;
                    angle[1] = -999;
                    angle[2] = -999;
                    angle[3] = -999;
                    angle[4] = -999;
                }

                Subarrays[nn]->poa.poaAll->inc[ii] = angle[0];
                Subarrays[nn]->poa.poaAll->tilt[ii] = angle[1];
                Subarrays[nn]->poa.poaAll->zen[ii] = sun[1];
                Subarrays[nn]->poa.poaAll->exTer[ii] = sun[8];

            }
            wdprov->rewind();
        }
    }
}


PVSystem_IO::PVSystem_IO(compute_module* cm, std::string cmName, Simulation_IO* SimulationIO, Irradiance_IO* IrradianceIO, std::vector<Subarray_IO*> SubarraysAll, Inverter_IO* InverterIO)
{
    Irradiance = IrradianceIO;
    Simulation = SimulationIO;
    Subarrays = SubarraysAll;
    Inverter = InverterIO;

    numberOfSubarrays = Subarrays.size();
    stringsInParallel = 0;
    for (size_t s = 0; s < numberOfSubarrays; s++) {
        stringsInParallel += static_cast<int>(Subarrays[s]->nStrings);
    }

    numberOfInverters = cm->as_integer("inverter_count");
    ratedACOutput = Inverter->ratedACOutput * numberOfInverters;
    acDerate = 1 - cm->as_double("acwiring_loss") / 100;
    acLossPercent = (1 - acDerate) * 100;
    transmissionDerate = 1 - cm->as_double("transmission_loss") / 100;
    transmissionLossPercent = (1 - transmissionDerate) * 100;

    enableDCLifetimeLosses = cm->as_boolean("en_dc_lifetime_losses");
    enableACLifetimeLosses = cm->as_boolean("en_ac_lifetime_losses");
    enableSnowModel = cm->as_boolean("en_snow_model");

    // The shared inverter of the PV array and a tightly-coupled DC connected battery
    std::unique_ptr<SharedInverter> tmpSharedInverter(new SharedInverter(Inverter->inverterType, numberOfInverters, &Inverter->sandiaInverter, &Inverter->partloadInverter, &Inverter->ondInverter));
    m_sharedInverter = std::move(tmpSharedInverter);

    // Register shared inverter with inverter_IO
    InverterIO->setupSharedInverter(cm, m_sharedInverter.get());

    // PV Degradation, place into intermediate variable since pointer outputs are not allocated yet
    if (Simulation->useLifetimeOutput)
    {
        std::vector<double> dc_degrad = cm->as_vector_double("dc_degradation");

        // degradation assumed to start at year 2
        dcDegradationFactor.push_back(1.0);
        dcDegradationFactor.push_back(1.0);

        if (dc_degrad.size() == 1)
        {
            for (size_t i = 1; i < Simulation->numberOfYears; i++)
                dcDegradationFactor.push_back(pow((1.0 - dc_degrad[0] / 100.0), i));
        }
        else if (dc_degrad.size() > 0)
        {
            for (size_t i = 1; i < Simulation->numberOfYears && i < dc_degrad.size(); i++)
                dcDegradationFactor.push_back(1.0 - dc_degrad[i] / 100.0);
        }

        //read in optional DC and AC lifetime daily losses, error check length of arrays
        if (enableDCLifetimeLosses)
        {
            if (!Simulation->annualSimulation)
                throw exec_error(cmName, "Lifetime daily losses cannot be entered with non-annual weather data");

            dcLifetimeLosses = cm->as_vector_double("dc_lifetime_losses");
            if (dcLifetimeLosses.size() != Simulation->numberOfYears * 365)
                throw exec_error(cmName, "Length of the lifetime daily DC losses array must be equal to the analysis period * 365 days/year");
        }
        if (enableACLifetimeLosses)
        {
            if (!Simulation->annualSimulation)
                throw exec_error(cmName, "Lifetime daily losses cannot be entered with non-annual weather data");

            acLifetimeLosses = cm->as_vector_double("ac_lifetime_losses");
            if (acLifetimeLosses.size() != Simulation->numberOfYears * 365)
                throw exec_error(cmName, "Length of the lifetime daily AC losses array must be equal to the analysis period * 365 days/year");
        }
    }

    // Transformer losses
    transformerLoadLossFraction = cm->as_number("transformer_load_loss") * (ssc_number_t)(util::percent_to_fraction);
    transformerNoLoadLossFraction = cm->as_number("transformer_no_load_loss") * (ssc_number_t)(util::percent_to_fraction);

    // Determine whether MPPT clipping should be enabled or not
    clipMpptWindow = false;

    if (InverterIO->mpptLowVoltage > 0 && InverterIO->mpptHiVoltage > InverterIO->mpptLowVoltage)
    {
        int modulePowerModel = Subarrays[0]->Module->modulePowerModel;
        if (modulePowerModel == 1     // cec with database
            || modulePowerModel == 2   // cec with user specs
            || modulePowerModel == 4 // iec61853 single diode
            || modulePowerModel == 5) // PVYield single diode
        {
            clipMpptWindow = true;
        }
        else
        {
            cm->log("The simple efficiency and Sandia module models do not allow limiting module voltage to the MPPT tracking range of the inverter.", SSC_NOTICE);
        }
    }
    else
    {
        cm->log("Inverter MPPT voltage tracking window not defined - modules always operate at MPPT.", SSC_NOTICE);
    }

    // Check that system MPPT inputs align correctly
    for (size_t n_subarray = 0; n_subarray < numberOfSubarrays; n_subarray++) {
        if (Subarrays[n_subarray]->enable) {
            if (Subarrays[n_subarray]->mpptInput > (int)Inverter->nMpptInputs)
                throw exec_error(cmName, "Subarray " + util::to_string((int)n_subarray) + " MPPT input is greater than the number of inverter MPPT inputs.");
        }
    }
    for (size_t mppt = 1; mppt <= (size_t)Inverter->nMpptInputs; mppt++) //indexed at 1 to match mppt input numbering convention
    {
        std::vector<int> mppt_n; //create a temporary vector to hold which subarrays are on this mppt input
        //find all subarrays on this mppt input
        for (size_t n_subarray = 0; n_subarray < Subarrays.size(); n_subarray++) //jmf update this so that all subarray markers are consistent, get rid of "enable" check
            if (Subarrays[n_subarray]->enable)
                if (Subarrays[n_subarray]->mpptInput == (int)mppt)
                    mppt_n.push_back((int)n_subarray);
        if (mppt_n.size() < 1)
            throw exec_error(cmName, "At least one subarray must be assigned to each inverter MPPT input.");
        mpptMapping.push_back(mppt_n); //add the subarrays on this input to the total mppt mapping vector
    }

    // Only one multi-MPPT inverter is allowed at the moment
    if (Inverter->nMpptInputs > 1 && numberOfInverters > 1)
        throw exec_error(cmName, "At this time, only one multiple-MPPT-input inverter may be modeled per system. See help for details.");

    //Subarray mismatch calculations
    enableMismatchVoltageCalc = cm->as_boolean("enable_mismatch_vmax_calc");
    if (enableMismatchVoltageCalc &&
        Subarrays[0]->Module->modulePowerModel != MODULE_CEC_DATABASE &&
        Subarrays[0]->Module->modulePowerModel != MODULE_CEC_USER_INPUT &&
        Subarrays[0]->Module->modulePowerModel != MODULE_IEC61853) {
        throw exec_error(cmName, "String level subarray mismatch can only be calculated using a single-diode based module model.");
    }
    if (enableMismatchVoltageCalc && numberOfSubarrays <= 1)
        throw exec_error(cmName, "Subarray voltage mismatch calculation requires more than one subarray. Please check your inputs.");

    // Setup POA inputs if needed
    SetupPOAInput();
}

void PVSystem_IO::AllocateOutputs(compute_module* cm)
{
    size_t numberOfWeatherFileRecords = Irradiance->numberOfWeatherFileRecords;
    if (Simulation->saveLifetimeVars == 1) {
        numberOfWeatherFileRecords = Simulation->numberOfSteps;
    }
    size_t numberOfLifetimeRecords = Simulation->numberOfSteps;
    size_t numberOfYears = Simulation->numberOfYears;

    for (size_t subarray = 0; subarray < Subarrays.size(); subarray++)
    {
        if (Subarrays[subarray]->enable)
        {
            std::string prefix = Subarrays[subarray]->prefix;
            p_angleOfIncidence.push_back(cm->allocate(prefix + "aoi", numberOfWeatherFileRecords));
            p_angleOfIncidenceModifier.push_back(cm->allocate(prefix + "aoi_modifier", numberOfWeatherFileRecords));
            p_surfaceTilt.push_back(cm->allocate(prefix + "surf_tilt", numberOfWeatherFileRecords));
            p_surfaceAzimuth.push_back(cm->allocate(prefix + "surf_azi", numberOfWeatherFileRecords));
            p_axisRotation.push_back(cm->allocate(prefix + "axisrot", numberOfWeatherFileRecords));
            p_idealRotation.push_back(cm->allocate(prefix + "idealrot", numberOfWeatherFileRecords));
            p_poaNominalFront.push_back(cm->allocate(prefix + "poa_nom", numberOfWeatherFileRecords));
            p_poaShadedFront.push_back(cm->allocate(prefix + "poa_shaded", numberOfWeatherFileRecords));
            p_poaShadedSoiledFront.push_back(cm->allocate(prefix + "poa_shaded_soiled", numberOfWeatherFileRecords));
            p_poaBeamFront.push_back(cm->allocate(prefix + "poa_eff_beam", numberOfWeatherFileRecords));
            p_poaDiffuseFront.push_back(cm->allocate(prefix + "poa_eff_diff", numberOfWeatherFileRecords));
            p_poaTotal.push_back(cm->allocate(prefix + "poa_eff", numberOfWeatherFileRecords));
            p_poaRear.push_back(cm->allocate(prefix + "poa_rear", numberOfWeatherFileRecords));
            p_poaFront.push_back(cm->allocate(prefix + "poa_front", numberOfWeatherFileRecords));
            p_derateSoiling.push_back(cm->allocate(prefix + "soiling_derate", numberOfWeatherFileRecords));
            p_beamShadingFactor.push_back(cm->allocate(prefix + "beam_shading_factor", numberOfWeatherFileRecords));
            p_temperatureCell.push_back(cm->allocate(prefix + "celltemp", numberOfWeatherFileRecords));
            p_temperatureCellSS.push_back(cm->allocate(prefix + "celltempSS", numberOfWeatherFileRecords));
            p_moduleEfficiency.push_back(cm->allocate(prefix + "modeff", numberOfWeatherFileRecords));
            p_dcStringVoltage.push_back(cm->allocate(prefix + "dc_voltage", numberOfWeatherFileRecords));
            p_voltageOpenCircuit.push_back(cm->allocate(prefix + "voc", numberOfWeatherFileRecords));
            p_currentShortCircuit.push_back(cm->allocate(prefix + "isc", numberOfWeatherFileRecords));
            p_dcPowerGross.push_back(cm->allocate(prefix + "dc_gross", numberOfWeatherFileRecords));
            p_derateLinear.push_back(cm->allocate(prefix + "linear_derate", numberOfWeatherFileRecords));
            p_derateSelfShading.push_back(cm->allocate(prefix + "ss_derate", numberOfWeatherFileRecords));
            p_derateSelfShadingDiffuse.push_back(cm->allocate(prefix + "ss_diffuse_derate", numberOfWeatherFileRecords));
            p_derateSelfShadingReflected.push_back(cm->allocate(prefix + "ss_reflected_derate", numberOfWeatherFileRecords));

            if (enableSnowModel) {
                p_snowLoss.push_back(cm->allocate(prefix + "snow_loss", numberOfWeatherFileRecords));
                p_snowCoverage.push_back(cm->allocate(prefix + "snow_coverage", numberOfWeatherFileRecords));
            }

            if (Subarrays[subarray]->enableSelfShadingOutputs)
            {
                // ShadeDB validation
                p_shadeDB_GPOA.push_back(cm->allocate("shadedb_" + prefix + "gpoa", numberOfWeatherFileRecords));
                p_shadeDB_DPOA.push_back(cm->allocate("shadedb_" + prefix + "dpoa", numberOfWeatherFileRecords));
                p_shadeDB_temperatureCell.push_back(cm->allocate("shadedb_" + prefix + "pv_cell_temp", numberOfWeatherFileRecords));
                p_shadeDB_modulesPerString.push_back(cm->allocate("shadedb_" + prefix + "mods_per_str", numberOfWeatherFileRecords));
                p_shadeDB_voltageMaxPowerSTC.push_back(cm->allocate("shadedb_" + prefix + "str_vmp_stc", numberOfWeatherFileRecords));
                p_shadeDB_voltageMPPTLow.push_back(cm->allocate("shadedb_" + prefix + "mppt_lo", numberOfWeatherFileRecords));
                p_shadeDB_voltageMPPTHigh.push_back(cm->allocate("shadedb_" + prefix + "mppt_hi", numberOfWeatherFileRecords));
            }
            p_shadeDBShadeFraction.push_back(cm->allocate("shadedb_" + prefix + "shade_frac", numberOfWeatherFileRecords));
        }
    }

    for (size_t mppt_input = 0; mppt_input < Inverter->nMpptInputs; mppt_input++)
    {
        p_mpptVoltage.push_back(cm->allocate("inverterMppt" + std::to_string(mppt_input + 1) + "_DCVoltage", numberOfLifetimeRecords));
        p_dcPowerNetPerMppt.push_back(cm->allocate("inverterMppt" + std::to_string(mppt_input + 1) + "_NetDCPower", numberOfLifetimeRecords));
    }

    p_transformerNoLoadLoss = cm->allocate("xfmr_nll_ts", numberOfWeatherFileRecords);
    p_transformerLoadLoss = cm->allocate("xfmr_ll_ts", numberOfWeatherFileRecords);
    p_transformerLoss = cm->allocate("xfmr_loss_ts", numberOfWeatherFileRecords);

    p_poaFrontNominalTotal = cm->allocate("poa_nom", numberOfWeatherFileRecords);
    p_poaFrontBeamNominalTotal = cm->allocate("poa_beam_nom", numberOfWeatherFileRecords);
    p_poaFrontBeamTotal = cm->allocate("poa_beam_eff", numberOfWeatherFileRecords);
    p_poaFrontShadedTotal = cm->allocate("poa_shaded", numberOfWeatherFileRecords);
    p_poaFrontShadedSoiledTotal = cm->allocate("poa_shaded_soiled", numberOfWeatherFileRecords);
    p_poaFrontTotal = cm->allocate("poa_front", numberOfWeatherFileRecords);
    p_poaRearTotal = cm->allocate("poa_rear", numberOfWeatherFileRecords);
    p_poaTotalAllSubarrays = cm->allocate("poa_eff", numberOfWeatherFileRecords);

    p_snowLossTotal = cm->allocate("dc_snow_loss", numberOfWeatherFileRecords);

    p_inverterEfficiency = cm->allocate("inv_eff", numberOfWeatherFileRecords);
    p_inverterClipLoss = cm->allocate("inv_cliploss", numberOfWeatherFileRecords);
    p_inverterMPPTLoss = cm->allocate("dc_invmppt_loss", numberOfWeatherFileRecords);

    p_inverterPowerConsumptionLoss = cm->allocate("inv_psoloss", numberOfWeatherFileRecords);
    p_inverterNightTimeLoss = cm->allocate("inv_pntloss", numberOfWeatherFileRecords);
    p_inverterThermalLoss = cm->allocate("inv_tdcloss", numberOfWeatherFileRecords);
    p_inverterTotalLoss = cm->allocate("inv_total_loss", numberOfWeatherFileRecords);

    p_acWiringLoss = cm->allocate("ac_wiring_loss", numberOfWeatherFileRecords);
    p_transmissionLoss = cm->allocate("ac_transmission_loss", numberOfWeatherFileRecords);
    p_systemDCPower = cm->allocate("dc_net", numberOfLifetimeRecords);
    p_systemACPower = cm->allocate("gen", numberOfLifetimeRecords);

    if (Simulation->useLifetimeOutput)
    {
        p_dcDegradationFactor = cm->allocate("dc_degrade_factor", numberOfYears + 1);
    }

}
void PVSystem_IO::AssignOutputs(compute_module* cm)
{
    cm->assign("ac_loss", var_data((ssc_number_t)(acLossPercent + transmissionLossPercent)));
}

Module_IO::Module_IO(compute_module* cm, std::string cmName, double dcLoss)
{
    modulePowerModel = cm->as_integer("module_model");

    simpleEfficiencyForceNoPOA = false;
    mountingSpecificCellTemperatureForceNoPOA = false;
    selfShadingFillFactor = 0;
    isConcentratingPV = false;
    isBifacial = false;

    if (modulePowerModel == MODULE_SIMPLE_EFFICIENCY)
    {
        simpleEfficiencyModel.VmpNominal = cm->as_double("spe_vmp");
        simpleEfficiencyModel.VocNominal = cm->as_double("spe_voc");
        simpleEfficiencyModel.Area = cm->as_double("spe_area");
        referenceArea = simpleEfficiencyModel.Area;
        isBifacial = cm->as_boolean("spe_is_bifacial");
        bifaciality = cm->as_double("spe_bifaciality");
        bifacialTransmissionFactor = cm->as_double("spe_bifacial_transmission_factor");
        groundClearanceHeight = cm->as_double("spe_bifacial_ground_clearance_height");
        for (int i = 0; i < 5; i++)
        {
            simpleEfficiencyModel.Rad[i] = cm->as_double(util::format("spe_rad%d", i));
            simpleEfficiencyModel.Eff[i] = 0.01 * cm->as_double(util::format("spe_eff%d", i));
            if (i > 0 && simpleEfficiencyModel.Rad[i] <= simpleEfficiencyModel.Rad[i - 1])
                throw exec_error(cmName, "simpleEfficiencyModel model radiation levels must increase monotonically");
        }

        simpleEfficiencyModel.Gamma = cm->as_double("spe_temp_coeff");
        simpleEfficiencyModel.Reference = cm->as_integer("spe_reference");

        switch (cm->as_integer("spe_module_structure"))
        {
        case 0: //glass/cell/polymer sheet - open rack
            sandiaCellTemp.a = -3.56;
            sandiaCellTemp.b = -0.0750;
            sandiaCellTemp.DT0 = 3;
            break;
        case 1: //glass/cell/glass - open rack
            sandiaCellTemp.a = -3.47;
            sandiaCellTemp.b = -0.0594;
            sandiaCellTemp.DT0 = 3;
            break;
        case 2: //polymer/thin film/steel - open rack
            sandiaCellTemp.a = -3.58;
            sandiaCellTemp.b = -0.113;
            sandiaCellTemp.DT0 = 3;
            break;
        case 3: //Insulated back (building-integrated PV)
            sandiaCellTemp.a = -2.81;
            sandiaCellTemp.b = -0.0455;
            sandiaCellTemp.DT0 = 0;
            break;
        case 4: //close roof mount
            sandiaCellTemp.a = -2.98;
            sandiaCellTemp.b = -0.0471;
            sandiaCellTemp.DT0 = 1;
            break;
        case 5: //user defined
            sandiaCellTemp.a = cm->as_double("spe_a");
            sandiaCellTemp.b = cm->as_double("spe_b");
            sandiaCellTemp.DT0 = cm->as_double("spe_dT");
            break;
        default:
            throw exec_error(cmName, "invalid simpleEfficiencyModel module structure and mounting");
        }

        simpleEfficiencyModel.fd = cm->as_double("spe_fd");
        sandiaCellTemp.fd = simpleEfficiencyModel.fd;

        if (simpleEfficiencyModel.fd < 1.0)
            simpleEfficiencyForceNoPOA = true;

        cellTempModel = &sandiaCellTemp;
        moduleModel = &simpleEfficiencyModel;
        moduleWattsSTC = simpleEfficiencyModel.WattsStc();
        voltageMaxPower = simpleEfficiencyModel.VmpNominal;
    }
    else if (modulePowerModel == MODULE_CEC_DATABASE)
    {
        isBifacial = cm->as_boolean("cec_is_bifacial");
        bifaciality = cm->as_double("cec_bifaciality");
        bifacialTransmissionFactor = cm->as_double("cec_bifacial_transmission_factor");
        groundClearanceHeight = cm->as_double("cec_bifacial_ground_clearance_height");
        cecModel.Area = cm->as_double("cec_area");
        referenceArea = cecModel.Area;
        cecModel.Vmp = cm->as_double("cec_v_mp_ref");
        cecModel.Imp = cm->as_double("cec_i_mp_ref");
        cecModel.Voc = cm->as_double("cec_v_oc_ref");
        cecModel.Isc = cm->as_double("cec_i_sc_ref");
        cecModel.alpha_isc = cm->as_double("cec_alpha_sc");
        cecModel.beta_voc = cm->as_double("cec_beta_oc");
        cecModel.a = cm->as_double("cec_a_ref");
        cecModel.Il = cm->as_double("cec_i_l_ref");
        cecModel.Io = cm->as_double("cec_i_o_ref");
        cecModel.Rs = cm->as_double("cec_r_s");
        cecModel.Rsh = cm->as_double("cec_r_sh_ref");
        cecModel.Adj = cm->as_double("cec_adjust");

        selfShadingFillFactor = cecModel.Vmp * cecModel.Imp / cecModel.Voc / cecModel.Isc;
        voltageMaxPower = cecModel.Vmp;

        if (cm->as_integer("cec_temp_corr_mode") == 0)
        {
            nominalOperatingCellTemp.Tnoct = cm->as_double("cec_t_noct");
            int standoff = cm->as_integer("cec_standoff");
            nominalOperatingCellTemp.standoff_tnoct_adj = 0;
            switch (standoff)

            { //source for standoff adjustment constants: https://prod-ng.sandia.gov/techlib-noauth/access-control.cgi/1985/850330.pdf page 12
            case 2: nominalOperatingCellTemp.standoff_tnoct_adj = 2; break; // between 2.5 and 3.5 inches
            case 3: nominalOperatingCellTemp.standoff_tnoct_adj = 6; break; // between 1.5 and 2.5 inches
            case 4: nominalOperatingCellTemp.standoff_tnoct_adj = 11; break; // between 0.5 and 1.5 inches
            case 5: nominalOperatingCellTemp.standoff_tnoct_adj = 18; break; // less than 0.5 inches
                                                            // note: all others, standoff_tnoct_adj = 0;
            }

            int height = cm->as_integer("cec_height");
            nominalOperatingCellTemp.ffv_wind = 0.51;
            if (height == 1)
                nominalOperatingCellTemp.ffv_wind = 0.61;

            cellTempModel = &nominalOperatingCellTemp;
        }
        else
        {
            /*	int MC; // Mounting configuration (1=rack,2=flush,3=integrated,4=gap)
            int HTD; // Heat transfer dimension (1=Module,2=Array)
            int MSO; // Mounting structure orientation (1=does not impede flow beneath, 2=vertical supports, 3=horizontal supports)
            int Nrows, Ncols; // number of modules in rows and columns, when using array heat transfer dimensions
            double Length; // module length, along horizontal dimension, (m)
            double Width; // module width, along vertical dimension, (m)
            double Wgap;  // gap width spacing (m)
            double TbackInteg; */

            mountingSpecificCellTemp.DcDerate = dcLoss;
            mountingSpecificCellTemp.MC = cm->as_integer("cec_mounting_config") + 1;
            mountingSpecificCellTemp.HTD = cm->as_integer("cec_heat_transfer") + 1;
            mountingSpecificCellTemp.MSO = cm->as_integer("cec_mounting_orientation") + 1;
            mountingSpecificCellTemp.Wgap = cm->as_double("cec_gap_spacing");
            mountingSpecificCellTemp.Length = cm->as_double("cec_module_length");
            mountingSpecificCellTemp.Width = cm->as_double("cec_module_width");
            mountingSpecificCellTemp.Nrows = cm->as_integer("cec_array_rows");
            mountingSpecificCellTemp.Ncols = cm->as_integer("cec_array_cols");
            mountingSpecificCellTemp.TbackInteg = cm->as_double("cec_backside_temp");

            cellTempModel = &mountingSpecificCellTemp;
            mountingSpecificCellTemperatureForceNoPOA = true;
        }

        moduleModel = &cecModel;
        moduleWattsSTC = cecModel.Vmp * cecModel.Imp;
    }
    else if (modulePowerModel == MODULE_CEC_USER_INPUT)
    {
        isBifacial = cm->as_boolean("6par_is_bifacial");
        bifaciality = cm->as_double("6par_bifaciality");
        bifacialTransmissionFactor = cm->as_double("6par_bifacial_transmission_factor");
        groundClearanceHeight = cm->as_double("6par_bifacial_ground_clearance_height");

        int tech_id = module6par::monoSi;
        int type = cm->as_integer("6par_celltech"); // "monoSi,multiSi,CdTe,CIS,CIGS,Amorphous"
        switch (type)
        {
        case 0: tech_id = module6par::monoSi; break;
        case 1: tech_id = module6par::multiSi; break;
        case 2: tech_id = module6par::CdTe; break;
        case 3: tech_id = module6par::CIS; break;
        case 4: tech_id = module6par::CIGS; break;
        case 5: tech_id = module6par::Amorphous; break;
        }

        double Vmp = cm->as_double("6par_vmp");
        double Imp = cm->as_double("6par_imp");
        double Voc = cm->as_double("6par_voc");
        double Isc = cm->as_double("6par_isc");
        double alpha = cm->as_double("6par_aisc");
        double beta = cm->as_double("6par_bvoc");
        double gamma = cm->as_double("6par_gpmp");
        int nser = cm->as_integer("6par_nser");

        module6par m(tech_id, Vmp, Imp, Voc, Isc, beta, alpha, gamma, nser, 298.15);
        int err = m.solve_with_sanity_and_heuristics<double>(300, 1e-7);

        if (err != 0)
            throw exec_error(cmName, "CEC 6 parameter model:  Could not solve for normalized coefficients.  Please check your inputs.");

        cecModel.Area = cm->as_double("6par_area");
        referenceArea = cecModel.Area;
        cecModel.Vmp = Vmp;
        cecModel.Imp = Imp;
        cecModel.Voc = Voc;
        cecModel.Isc = Isc;
        cecModel.alpha_isc = alpha;
        cecModel.beta_voc = beta;
        cecModel.a = m.a;
        cecModel.Il = m.Il;
        cecModel.Io = m.Io;
        cecModel.Rs = m.Rs;
        cecModel.Rsh = m.Rsh;
        cecModel.Adj = m.Adj;

        selfShadingFillFactor = cecModel.Vmp * cecModel.Imp / cecModel.Voc / cecModel.Isc;
        voltageMaxPower = cecModel.Vmp;

        setupNOCTModel(cm, "6par");
        cellTempModel = &nominalOperatingCellTemp;
        moduleModel = &cecModel;
        moduleWattsSTC = cecModel.Vmp * cecModel.Imp;
    }
    else if (modulePowerModel == MODULE_SANDIA)
    {
        sandiaModel.A0 = cm->as_double("snl_a0");
        sandiaModel.A1 = cm->as_double("snl_a1");
        sandiaModel.A2 = cm->as_double("snl_a2");
        sandiaModel.A3 = cm->as_double("snl_a3");
        sandiaModel.A4 = cm->as_double("snl_a4");
        sandiaModel.aImp = cm->as_double("snl_aimp");
        sandiaModel.aIsc = cm->as_double("snl_aisc");
        sandiaModel.Area = cm->as_double("snl_area");
        referenceArea = sandiaModel.Area;
        sandiaModel.B0 = cm->as_double("snl_b0");
        sandiaModel.B1 = cm->as_double("snl_b1");
        sandiaModel.B2 = cm->as_double("snl_b2");
        sandiaModel.B3 = cm->as_double("snl_b3");
        sandiaModel.B4 = cm->as_double("snl_b4");
        sandiaModel.B5 = cm->as_double("snl_b5");
        sandiaModel.BVmp0 = cm->as_double("snl_bvmpo");
        sandiaModel.BVoc0 = cm->as_double("snl_bvoco");
        sandiaModel.C0 = cm->as_double("snl_c0");
        sandiaModel.C1 = cm->as_double("snl_c1");
        sandiaModel.C2 = cm->as_double("snl_c2");
        sandiaModel.C3 = cm->as_double("snl_c3");
        sandiaModel.C4 = cm->as_double("snl_c4");
        sandiaModel.C5 = cm->as_double("snl_c5");
        sandiaModel.C6 = cm->as_double("snl_c6");
        sandiaModel.C7 = cm->as_double("snl_c7");
        sandiaModel.fd = cm->as_double("snl_fd");
        sandiaModel.Imp0 = cm->as_double("snl_impo");
        sandiaModel.Isc0 = cm->as_double("snl_isco");
        sandiaModel.Ix0 = cm->as_double("snl_ixo");
        sandiaModel.Ixx0 = cm->as_double("snl_ixxo");
        sandiaModel.mBVmp = cm->as_double("snl_mbvmp");
        sandiaModel.mBVoc = cm->as_double("snl_mbvoc");
        sandiaModel.DiodeFactor = cm->as_double("snl_n");
        sandiaModel.NcellSer = cm->as_integer("snl_series_cells");
        sandiaModel.Vmp0 = cm->as_double("snl_vmpo");
        sandiaModel.Voc0 = cm->as_double("snl_voco");

        selfShadingFillFactor = sandiaModel.Vmp0 * sandiaModel.Imp0 / sandiaModel.Voc0 / sandiaModel.Isc0;
        voltageMaxPower = sandiaModel.Vmp0;

        if (sandiaModel.fd == 0) {
            isConcentratingPV = true;
        }

        // by default, use database values
        double A = cm->as_double("snl_a");
        double B = cm->as_double("snl_b");
        double DT = cm->as_double("snl_dtc");

        switch (cm->as_integer("snl_module_structure"))
        {
        case 1: //glass/cell/polymer sheet - open rack
            A = -3.56;
            B = -0.0750;
            DT = 3;
            break;
        case 2: //glass/cell/glass - open rack
            A = -3.47;
            B = -0.0594;
            DT = 3;
            break;
        case 3: //polymer/thin film/steel - open rack
            A = -3.58;
            B = -0.113;
            DT = 3;
            break;
        case 4: //Insulated back (building-integrated PV)
            A = -2.81;
            B = -0.0455;
            DT = 0;
            break;
        case 5: //close roof mount
            A = -2.98;
            B = -0.0471;
            DT = 1;
            break;
        case 6: //user defined
            A = cm->as_double("snl_ref_a");
            B = cm->as_double("snl_ref_b");
            DT = cm->as_double("snl_ref_dT");
            break;

        default:
            break;
        }

        sandiaCellTemp.a = A;
        sandiaCellTemp.b = B;
        sandiaCellTemp.DT0 = DT;
        sandiaCellTemp.fd = sandiaModel.fd;

        cellTempModel = &sandiaCellTemp;
        moduleModel = &sandiaModel;
        moduleWattsSTC = sandiaModel.Vmp0 * sandiaModel.Imp0;
    }
    else if (modulePowerModel == MODULE_IEC61853)
    {
        // IEC 61853 model
        elevenParamSingleDiodeModel.NcellSer = cm->as_integer("sd11par_nser");
        elevenParamSingleDiodeModel.Area = cm->as_double("sd11par_area");
        elevenParamSingleDiodeModel.AMA[0] = cm->as_double("sd11par_AMa0");
        elevenParamSingleDiodeModel.AMA[1] = cm->as_double("sd11par_AMa1");
        elevenParamSingleDiodeModel.AMA[2] = cm->as_double("sd11par_AMa2");
        elevenParamSingleDiodeModel.AMA[3] = cm->as_double("sd11par_AMa3");
        elevenParamSingleDiodeModel.AMA[4] = cm->as_double("sd11par_AMa4");
        elevenParamSingleDiodeModel.GlassAR = cm->as_boolean("sd11par_glass");

        setupNOCTModel(cm, "sd11par");

        elevenParamSingleDiodeModel.Vmp0 = cm->as_double("sd11par_Vmp0");
        elevenParamSingleDiodeModel.Imp0 = cm->as_double("sd11par_Imp0");
        elevenParamSingleDiodeModel.Voc0 = cm->as_double("sd11par_Voc0");
        elevenParamSingleDiodeModel.Isc0 = cm->as_double("sd11par_Isc0");
        elevenParamSingleDiodeModel.alphaIsc = cm->as_double("sd11par_alphaIsc");
        elevenParamSingleDiodeModel.n = cm->as_double("sd11par_n");
        elevenParamSingleDiodeModel.Il = cm->as_double("sd11par_Il");
        elevenParamSingleDiodeModel.Io = cm->as_double("sd11par_Io");
        elevenParamSingleDiodeModel.Egref = cm->as_double("sd11par_Egref");
        elevenParamSingleDiodeModel.D1 = cm->as_double("sd11par_d1");
        elevenParamSingleDiodeModel.D2 = cm->as_double("sd11par_d2");
        elevenParamSingleDiodeModel.D3 = cm->as_double("sd11par_d3");
        elevenParamSingleDiodeModel.C1 = cm->as_double("sd11par_c1");
        elevenParamSingleDiodeModel.C2 = cm->as_double("sd11par_c2");
        elevenParamSingleDiodeModel.C3 = cm->as_double("sd11par_c3");

        cellTempModel = &nominalOperatingCellTemp;
        moduleModel = &elevenParamSingleDiodeModel;
        moduleWattsSTC = elevenParamSingleDiodeModel.Vmp0 * elevenParamSingleDiodeModel.Imp0;
        referenceArea = elevenParamSingleDiodeModel.Area;
        selfShadingFillFactor = elevenParamSingleDiodeModel.Vmp0 * elevenParamSingleDiodeModel.Imp0 / elevenParamSingleDiodeModel.Voc0 / elevenParamSingleDiodeModel.Isc0;
        voltageMaxPower = elevenParamSingleDiodeModel.Vmp0;
    }
    else if (modulePowerModel == MODULE_PVYIELD)
    {
        // Mermoud/Lejeune single-diode model
        size_t elementCount1 = 0;
        size_t elementCount2 = 0;
        ssc_number_t* arrayIncAngle = 0;
        ssc_number_t* arrayIamValue = 0;

        mlModuleModel.N_series = cm->as_integer("mlm_N_series");
        mlModuleModel.N_parallel = cm->as_integer("mlm_N_parallel");
        mlModuleModel.N_diodes = cm->as_integer("mlm_N_diodes");
        mlModuleModel.Width = cm->as_double("mlm_Width");
        mlModuleModel.Length = cm->as_double("mlm_Length");
        mlModuleModel.V_mp_ref = cm->as_double("mlm_V_mp_ref");
        mlModuleModel.I_mp_ref = cm->as_double("mlm_I_mp_ref");
        mlModuleModel.V_oc_ref = cm->as_double("mlm_V_oc_ref");
        mlModuleModel.I_sc_ref = cm->as_double("mlm_I_sc_ref");
        mlModuleModel.S_ref = cm->as_double("mlm_S_ref");
        mlModuleModel.T_ref = cm->as_double("mlm_T_ref");
        mlModuleModel.R_shref = cm->as_double("mlm_R_shref");
        mlModuleModel.R_sh0 = cm->as_double("mlm_R_sh0");
        mlModuleModel.R_shexp = cm->as_double("mlm_R_shexp");
        mlModuleModel.R_s = cm->as_double("mlm_R_s");
        mlModuleModel.alpha_isc = cm->as_double("mlm_alpha_isc");
        mlModuleModel.beta_voc_spec = cm->as_double("mlm_beta_voc_spec");
        mlModuleModel.E_g = cm->as_double("mlm_E_g");
        mlModuleModel.n_0 = cm->as_double("mlm_n_0");
        mlModuleModel.mu_n = cm->as_double("mlm_mu_n");
        mlModuleModel.D2MuTau = cm->as_double("mlm_D2MuTau");
        mlModuleModel.T_mode = cm->as_integer("mlm_T_mode");
        mlModuleModel.T_c_no_tnoct = cm->as_double("mlm_T_c_no_tnoct");
        mlModuleModel.T_c_no_mounting = cm->as_integer("mlm_T_c_no_mounting");
        mlModuleModel.T_c_no_standoff = cm->as_integer("mlm_T_c_no_standoff");
        mlModuleModel.T_c_fa_alpha = cm->as_double("mlm_T_c_fa_alpha");
        mlModuleModel.T_c_fa_U0 = cm->as_double("mlm_T_c_fa_U0");
        mlModuleModel.T_c_fa_U1 = cm->as_double("mlm_T_c_fa_U1");
        mlModuleModel.AM_mode = cm->as_integer("mlm_AM_mode");
        mlModuleModel.AM_c_sa[0] = cm->as_double("mlm_AM_c_sa0");
        mlModuleModel.AM_c_sa[1] = cm->as_double("mlm_AM_c_sa1");
        mlModuleModel.AM_c_sa[2] = cm->as_double("mlm_AM_c_sa2");
        mlModuleModel.AM_c_sa[3] = cm->as_double("mlm_AM_c_sa3");
        mlModuleModel.AM_c_sa[4] = cm->as_double("mlm_AM_c_sa4");
        mlModuleModel.AM_c_lp[0] = cm->as_double("mlm_AM_c_lp0");
        mlModuleModel.AM_c_lp[1] = cm->as_double("mlm_AM_c_lp0");
        mlModuleModel.AM_c_lp[2] = cm->as_double("mlm_AM_c_lp0");
        mlModuleModel.AM_c_lp[3] = cm->as_double("mlm_AM_c_lp0");
        mlModuleModel.AM_c_lp[4] = cm->as_double("mlm_AM_c_lp0");
        mlModuleModel.AM_c_lp[5] = cm->as_double("mlm_AM_c_lp0");
        mlModuleModel.IAM_mode = cm->as_integer("mlm_IAM_mode");
        mlModuleModel.IAM_c_as = cm->as_double("mlm_IAM_c_as");
        mlModuleModel.IAM_c_sa[0] = cm->as_double("mlm_IAM_c_sa0");
        mlModuleModel.IAM_c_sa[1] = cm->as_double("mlm_IAM_c_sa1");
        mlModuleModel.IAM_c_sa[2] = cm->as_double("mlm_IAM_c_sa2");
        mlModuleModel.IAM_c_sa[3] = cm->as_double("mlm_IAM_c_sa3");
        mlModuleModel.IAM_c_sa[4] = cm->as_double("mlm_IAM_c_sa4");
        mlModuleModel.IAM_c_sa[5] = cm->as_double("mlm_IAM_c_sa5");
        mlModuleModel.groundRelfectionFraction = cm->as_double("mlm_groundRelfectionFraction");

        arrayIncAngle = cm->as_array("mlm_IAM_c_cs_incAngle", &elementCount1);
        arrayIamValue = cm->as_array("mlm_IAM_c_cs_iamValue", &elementCount2);
        mlModuleModel.IAM_c_cs_elements = (int)elementCount1; // as_integer("mlm_IAM_c_cs_elements");

        if (mlModuleModel.IAM_mode == 3)
        {
            if (elementCount1 != elementCount2)
            {
                throw exec_error(cmName, "Spline IAM: Number of entries for incidence angle and IAM value different.");
            }
            for (int i = 0; i <= mlModuleModel.IAM_c_cs_elements - 1; i = i + 1) {
                mlModuleModel.IAM_c_cs_incAngle[i] = arrayIncAngle[i];
                mlModuleModel.IAM_c_cs_iamValue[i] = arrayIamValue[i];
            }
        }
        if (mlModuleModel.T_mode == 1) {
            setupNOCTModel(cm, "mlm_T_c_no");
            cellTempModel = &nominalOperatingCellTemp;
        }
        else if (mlModuleModel.T_mode == 2) {
            cellTempModel = &mockCellTemp;
        }
        else {
            throw exec_error(cmName, "invalid temperature model type");
        }

        mlModuleModel.initializeManual();


        moduleModel = &mlModuleModel;
        moduleWattsSTC = mlModuleModel.V_oc_ref * mlModuleModel.I_mp_ref;
        referenceArea = mlModuleModel.Width * mlModuleModel.Length;
        selfShadingFillFactor = mlModuleModel.V_mp_ref * mlModuleModel.I_mp_ref / mlModuleModel.V_oc_ref / mlModuleModel.I_sc_ref;
        voltageMaxPower = mlModuleModel.V_mp_ref;


    }
    else
        throw exec_error(cmName, "invalid pv module model type");
}
void Module_IO::setupNOCTModel(compute_module* cm, const std::string& prefix)
{
    nominalOperatingCellTemp.Tnoct = cm->as_double(prefix + "_tnoct");
    nominalOperatingCellTemp.ffv_wind = 0.51; // less than 22ft high (1 story)
    if (cm->as_integer(prefix + "_mounting") == 1) nominalOperatingCellTemp.ffv_wind = 0.61;  // greater than 22ft high (2 story)

    int standoff = cm->as_integer(prefix + "_standoff"); // bipv,3.5in,2.5-3.5in,1.5-2.5in,0.5-1.5in,ground/rack
    nominalOperatingCellTemp.standoff_tnoct_adj = 0;
    switch (standoff)
    { //source for standoff adjustment constants: https://prod-ng.sandia.gov/techlib-noauth/access-control.cgi/1985/850330.pdf page 12
    case 2: nominalOperatingCellTemp.standoff_tnoct_adj = 2; break; // between 2.5 and 3.5 inches
    case 3: nominalOperatingCellTemp.standoff_tnoct_adj = 6; break; // between 1.5 and 2.5 inches
    case 4: nominalOperatingCellTemp.standoff_tnoct_adj = 11; break; // between 0.5 and 1.5 inches
    case 5: nominalOperatingCellTemp.standoff_tnoct_adj = 18; break; // less than 0.5 inches
    }
}

void Module_IO::AssignOutputs(compute_module* cm)
{
    cm->assign("6par_a", var_data((ssc_number_t)cecModel.a));
    cm->assign("6par_Io", var_data((ssc_number_t)cecModel.Io));
    cm->assign("6par_Il", var_data((ssc_number_t)cecModel.Il));
    cm->assign("6par_Rs", var_data((ssc_number_t)cecModel.Rs));
    cm->assign("6par_Rsh", var_data((ssc_number_t)cecModel.Rsh));
    cm->assign("6par_Adj", var_data((ssc_number_t)cecModel.Adj));
}

Inverter_IO::Inverter_IO(compute_module* cm, std::string cmName)
{
    inverterType = cm->as_integer("inverter_model");
    nMpptInputs = cm->as_unsigned_long("inv_num_mppt");

    if (inverterType == 4)
    {
        mpptLowVoltage = cm->as_double("ond_VMppMin");
        mpptHiVoltage = cm->as_double("ond_VMppMax");
    }
    else
    {
        mpptLowVoltage = cm->as_double("mppt_low_inverter");
        mpptHiVoltage = cm->as_double("mppt_hi_inverter");
    }

    if (inverterType == 0) // cec database
    {
        sandiaInverter.Paco = cm->as_double("inv_snl_paco");
        sandiaInverter.Pdco = cm->as_double("inv_snl_pdco");
        sandiaInverter.Vdco = cm->as_double("inv_snl_vdco");
        sandiaInverter.Pso = cm->as_double("inv_snl_pso");
        sandiaInverter.Pntare = cm->as_double("inv_snl_pnt");
        sandiaInverter.C0 = cm->as_double("inv_snl_c0");
        sandiaInverter.C1 = cm->as_double("inv_snl_c1");
        sandiaInverter.C2 = cm->as_double("inv_snl_c2");
        sandiaInverter.C3 = cm->as_double("inv_snl_c3");
        ratedACOutput = sandiaInverter.Paco;
    }
    else if (inverterType == 1) // datasheet data
    {
        double eff_ds = cm->as_double("inv_ds_eff") / 100.0;
        sandiaInverter.Paco = cm->as_double("inv_ds_paco");
        if (eff_ds != 0)
            sandiaInverter.Pdco = sandiaInverter.Paco / eff_ds;
        else
            sandiaInverter.Pdco = 0;
        sandiaInverter.Vdco = cm->as_double("inv_ds_vdco");
        sandiaInverter.Pso = cm->as_double("inv_ds_pso");
        sandiaInverter.Pntare = cm->as_double("inv_ds_pnt");
        sandiaInverter.C0 = 0;
        sandiaInverter.C1 = 0;
        sandiaInverter.C2 = 0;
        sandiaInverter.C3 = 0;
        ratedACOutput = sandiaInverter.Paco;
    }
    else if (inverterType == 2) // partload curve
    {
        partloadInverter.Vdco = cm->as_double("inv_pd_vdco");
        partloadInverter.Paco = cm->as_double("inv_pd_paco");
        partloadInverter.Pdco = cm->as_double("inv_pd_pdco");
        partloadInverter.Pntare = cm->as_double("inv_pd_pnt");

        std::vector<double> pl_pd = cm->as_vector_double("inv_pd_partload");
        std::vector<double> eff_pd = cm->as_vector_double("inv_pd_efficiency");

        partloadInverter.Partload = pl_pd;
        partloadInverter.Efficiency = eff_pd;
        ratedACOutput = partloadInverter.Paco;
    }
    else if (inverterType == 3) // coefficient generator
    {
        sandiaInverter.Paco = cm->as_double("inv_cec_cg_paco");
        sandiaInverter.Pdco = cm->as_double("inv_cec_cg_pdco");
        sandiaInverter.Vdco = cm->as_double("inv_cec_cg_vdco");
        sandiaInverter.Pso = cm->as_double("inv_cec_cg_psco");
        sandiaInverter.Pntare = cm->as_double("inv_cec_cg_pnt");
        sandiaInverter.C0 = cm->as_double("inv_cec_cg_c0");
        sandiaInverter.C1 = cm->as_double("inv_cec_cg_c1");
        sandiaInverter.C2 = cm->as_double("inv_cec_cg_c2");
        sandiaInverter.C3 = cm->as_double("inv_cec_cg_c3");
        ratedACOutput = sandiaInverter.Paco;
    }
    else if (inverterType == 4) // PVYield (ondInverter)
    {
        size_t elementCount = 0;
        size_t rows = 0;
        size_t cols = 0;
        ssc_number_t* VNomEffArray;
        ssc_number_t* effCurve_PdcArray;
        ssc_number_t* effCurve_PacArray;
        ssc_number_t* effCurve_etaArray;

        ondInverter.PNomConv = cm->as_double("ond_PNomConv");
        ondInverter.PMaxOUT = cm->as_double("ond_PMaxOUT");
        ondInverter.VOutConv = cm->as_double("ond_VOutConv");
        ondInverter.VMppMin = cm->as_double("ond_VMppMin");
        ondInverter.VMPPMax = cm->as_double("ond_VMPPMax");
        ondInverter.VAbsMax = cm->as_double("ond_VAbsMax");
        ondInverter.PSeuil = cm->as_double("ond_PSeuil");
        ondInverter.ModeOper = cm->as_string("ond_ModeOper");
        ondInverter.CompPMax = cm->as_string("ond_CompPMax");
        ondInverter.CompVMax = cm->as_string("ond_CompVMax");
        ondInverter.ModeAffEnum = cm->as_string("ond_ModeAffEnum");
        ondInverter.PNomDC = cm->as_double("ond_PNomDC");
        ondInverter.PMaxDC = cm->as_double("ond_PMaxDC");
        ondInverter.IMaxDC = cm->as_double("ond_IMaxDC");
        ondInverter.INomDC = cm->as_double("ond_INomDC");
        ondInverter.INomAC = cm->as_double("ond_INomAC");
        ondInverter.IMaxAC = cm->as_double("ond_IMaxAC");
        ondInverter.TPNom = cm->as_double("ond_TPNom");
        ondInverter.TPMax = cm->as_double("ond_TPMax");
        ondInverter.TPLim1 = cm->as_double("ond_TPLim1");
        ondInverter.TPLimAbs = cm->as_double("ond_TPLimAbs");
        ondInverter.PLim1 = cm->as_double("ond_PLim1");
        ondInverter.PLimAbs = cm->as_double("ond_PLimAbs");
        VNomEffArray = cm->as_array("ond_VNomEff", &elementCount);
        ondInverter.NbInputs = cm->as_integer("ond_NbInputs");
        ondInverter.NbMPPT = cm->as_integer("ond_NbMPPT");
        ondInverter.Aux_Loss = cm->as_double("ond_Aux_Loss");
        ondInverter.Night_Loss = cm->as_double("ond_Night_Loss");
        ondInverter.lossRDc = cm->as_double("ond_lossRDc");
        ondInverter.lossRAc = cm->as_double("ond_lossRAc");
        ondInverter.effCurve_elements = cm->as_integer("ond_effCurve_elements");
        effCurve_PdcArray = cm->as_matrix("ond_effCurve_Pdc", &rows, &cols);
        effCurve_PacArray = cm->as_matrix("ond_effCurve_Pac", &rows, &cols);
        effCurve_etaArray = cm->as_matrix("ond_effCurve_eta", &rows, &cols);
        ondInverter.doAllowOverpower = cm->as_integer("ond_doAllowOverpower");
        ondInverter.doUseTemperatureLimit = cm->as_integer("ond_doUseTemperatureLimit");
        int matrixIndex;
        const int MAX_ELEMENTS = 100;
        for (int i = 0; i <= 2; i = i + 1) {
            ondInverter.VNomEff[i] = VNomEffArray[i];
            for (int j = 0; j <= MAX_ELEMENTS - 1; j = j + 1) {
                matrixIndex = i * MAX_ELEMENTS + j;
                ondInverter.effCurve_Pdc[i][j] = effCurve_PdcArray[matrixIndex];
                ondInverter.effCurve_Pac[i][j] = effCurve_PacArray[matrixIndex];
                ondInverter.effCurve_eta[i][j] = effCurve_etaArray[matrixIndex];
            }
        }

        ondInverter.initializeManual();
        ratedACOutput = ondInverter.PNomConv;

    }
    else
    {
        throw exec_error("pvsamv1", "invalid inverter model type");
    }
}

void Inverter_IO::setupSharedInverter(compute_module* cm, SharedInverter* a_sharedInverter)
{
    sharedInverter = a_sharedInverter;

    // Inverter thermal derate curves
    util::matrix_t<double> inv_tdc;
    if (inverterType == 0) // cec database
    {
        inv_tdc = cm->as_matrix("inv_tdc_cec_db");
    }
    else if (inverterType == 1) // datasheet data
    {
        inv_tdc = cm->as_matrix("inv_tdc_ds");
    }
    else if (inverterType == 2) // partload curve
    {
        inv_tdc = cm->as_matrix("inv_tdc_plc");
    }
    else if (inverterType == 3) // coefficient generator
    {
        inv_tdc = cm->as_matrix("inv_tdc_cec_cg");
    }
    else
    {
        return;
    }

    // Parse into std::vector form
    std::vector<std::vector<double>> thermalDerateCurves;
    for (size_t r = 0; r < inv_tdc.nrows(); r++) {
        std::vector<double> row;
        for (size_t c = 0; c < inv_tdc.row(r).ncells(); c++) {
            row.push_back(inv_tdc.at(r, c));
        }
        thermalDerateCurves.push_back(row);
    }
    int err = sharedInverter->setTempDerateCurves(thermalDerateCurves);
    if (err > 0) {
        throw exec_error("pvsamv1", "Inverter temperature derate curve row " + util::to_string((int)(err - 1)) + " is invalid.");
    }
}
