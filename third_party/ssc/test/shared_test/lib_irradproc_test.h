#ifndef __LIB_IRRADPROC_TEST_H_
#define __LIB_IRRADPROC_TEST_H_

#include <cstdlib>
#include <string>
#include <iosfwd>
#include <fstream>
#include <iterator>

#include <gtest/gtest.h>

#include "lib_irradproc.h"
#include "core.h"
#include "../test/input_cases/code_generator_utilities.h"

/**
* \class IrradTest
*
* Month: 1-12, Hour: 0-23, Minute: 0-59.
*
*/

class IrradTest : public ::testing::Test {
protected:
    double lat, lon, tz, alb, tilt, azim, rotlim, gcr, elev, pres, tdry;
    int year, month, day, skymodel, tracking;
    bool backtrack_on;
    double calc_sunrise, calc_sunset;
    double e;

    void SetUp() override {
        // parameters
        lat = 31.6340;
        lon = 74.8723;
        tz = 5.5;
        year = 2017;
        month = 7;
        day = 19;
        skymodel = 2;
        alb = 0.2;
        tracking = 0;
        tilt = 10;
        azim = 180;
        rotlim = 0;
        backtrack_on = false;
        gcr = 0;
        e = 0.0001;
        pres = 1013.25;
        elev = 234;
        tdry = 15;

        // correct sunrise and sunset times
        calc_sunrise = 5.70924; // 5:43 am
        calc_sunset = 19.5179;  // 7:31 pm
    }
};

class NightCaseIrradProc : public IrradTest {
protected:
    // Test time: 1:30 am
    irrad irr_hourly_night;
    // Test time: 1:15 am
    irrad irr_15m_night;

    void SetUp() {
        IrradTest::SetUp();
        int night_hr(1);
        irr_hourly_night.set_time(year, month, day, night_hr, 30, 1);
        irr_hourly_night.set_location(lat, lon, tz);
        irr_hourly_night.set_optional(elev, pres, tdry);
        irr_hourly_night.set_sky_model(skymodel, alb);
        irr_hourly_night.set_beam_diffuse(0, 0);
        irr_hourly_night.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr, false, 0.0);
        irr_15m_night.set_time(year, month, day, night_hr, 15, -1);
        irr_15m_night.set_location(lat, lon, tz);
        irr_15m_night.set_optional(elev, pres, tdry);
        irr_15m_night.set_sky_model(skymodel, alb);
        irr_15m_night.set_beam_diffuse(0, 0);
        irr_15m_night.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr, false, 0.0);
    }
};

class SunriseCaseIrradProc : public IrradTest {
protected:
    // Test time: 5:30 am
    irrad irr_hourly_sunrise;
    // Test time: 5:30 am
    irrad irr_15m_sunrise;

    void SetUp() {
        IrradTest::SetUp();
        int sr_hr(5);
        irr_hourly_sunrise.set_time(year, month, day, sr_hr, 30, 1);
        irr_hourly_sunrise.set_location(lat, lon, tz);
        irr_hourly_sunrise.set_optional(elev, pres, tdry);
        irr_hourly_sunrise.set_sky_model(skymodel, alb);
        irr_hourly_sunrise.set_beam_diffuse(0, 1);
        irr_hourly_sunrise.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr, false, 0.0);
        irr_15m_sunrise.set_time(year, month, day, sr_hr, 30, 1);
        irr_15m_sunrise.set_location(lat, lon, tz);
        irr_15m_sunrise.set_optional(elev, pres, tdry);
        irr_15m_sunrise.set_sky_model(skymodel, alb);
        irr_15m_sunrise.set_beam_diffuse(0, 1);
        irr_15m_sunrise.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr, false, 0.0);
    }
};

class DayCaseIrradProc : public IrradTest {
protected:
    // Test time: 12:30 pm
    irrad irr_hourly_day;
    // Test time: 12:45 pm
    irrad irr_15m_day;

    void SetUp() {
        IrradTest::SetUp();
        int day_hr(12);
        irr_hourly_day.set_time(year, month, day, day_hr, 30, 1);
        irr_hourly_day.set_location(lat, lon, tz);
        irr_hourly_day.set_optional(elev, pres, tdry);
        irr_hourly_day.set_sky_model(skymodel, alb);
        irr_hourly_day.set_beam_diffuse(2, 2);
        irr_hourly_day.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr, false, 0.0);
        irr_15m_day.set_time(year, month, day, day_hr, 45, 1);
        irr_15m_day.set_location(lat, lon, tz);
        irr_15m_day.set_optional(elev, pres, tdry);
        irr_15m_day.set_sky_model(skymodel, alb);
        irr_15m_day.set_beam_diffuse(2, 2);
        irr_15m_day.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr, false, 0.0);
    }
};

class SunsetCaseIrradProc : public IrradTest {
protected:
    // Test time: 7:30 pm
    irrad irr_hourly_sunset;
    // Test time: 7:30 pm
    irrad irr_15m_sunset;

    virtual void SetUp() {
        IrradTest::SetUp();
        int ss_hr(19);
        irr_hourly_sunset.set_time(year, month, day, ss_hr, 30, 1);
        irr_hourly_sunset.set_location(lat, lon, tz);
        irr_hourly_sunset.set_optional(elev, pres, tdry);
        irr_hourly_sunset.set_sky_model(skymodel, alb);
        irr_hourly_sunset.set_beam_diffuse(0, 1);
        irr_hourly_sunset.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr, false, 0.0);
        irr_15m_sunset.set_time(year, month, day, ss_hr, 30, 1);
        irr_15m_sunset.set_location(lat, lon, tz);
        irr_15m_sunset.set_optional(elev, pres, tdry);
        irr_15m_sunset.set_sky_model(skymodel, alb);
        irr_15m_sunset.set_beam_diffuse(0, 1);
        irr_15m_sunset.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr, false, 0.0);
    }
};
/**
*    Test which uses the example in bifacialvf.py within github.com/NREL/bifacialvf
*/

class BifacialIrradTest : public ::testing::Test {
protected:
    double tilt, azim, transmissionFactor, bifaciality, gcr, rotlim, albedo;
    double slopeLength, rowToRow, clearanceGround, distanceBetweenRows, verticalHeight, horizontalLength;
    int year, month, day, hour, minute;
    double solarAzimuthRadians, solarZenithRadians, solarElevationRadians;
    double lat, lon, tz;
    int tracking, skyModel;
    bool backtrack;
    irrad* irr;
    double beam, diffuse;
    double e;
    double elev, tdry, pres;

    std::string frontSkyConfigFactorsFile;
    std::string rearSkyConfigFactorsFile;
    std::string pvBackSHFile;
    std::string pvFrontSHFile;
    std::string frontGroundShadeFile;
    std::string rearGroundShadeFile;
    std::string frontGroundGHIFile;
    std::string rearGroundGHIFile;
    std::string frontIrradianceFile;
    std::string frontReflectedFile;
    std::string rearIrradianceFile;
    std::string weatherFile;
    std::string averageIrradianceFile;

    // Scalar variables at all time steps
    std::vector<double> expectedPVFrontShadeFraction;
    std::vector<double> expectedPVRearShadeFraction;

    // Array variable at a single time step
    std::vector<double> expectedFrontSkyConfigFactors;
    std::vector<double> expectedRearSkyConfigFactors;
    std::vector<int> expectedFrontGroundShade;
    std::vector<int> expectedRearGroundShade;
    std::vector<double> expectedFrontGroundGHI;
    std::vector<double> expectedRearGroundGHI;
    std::vector<double> expectedFrontReflected;
    std::vector<double> expectedFrontIrradiance;
    std::vector<double> expectedRearIrradiance;
    std::vector<double> expectedAverageIrradiance;

    // Randomly sample some of the timesteps
    size_t numberOfTimeSteps;
    size_t numberOfSamples;
    std::vector<int> samples;

    void SetUp() {

        // parameters
        tilt = 10;
        azim = 180;
        gcr = 0.666667;
        e = 0.01;
        tracking = 0;
        rotlim = 90;
        backtrack = false;
        albedo = 0.62;
        skyModel = 2; // perez

        // bifacial
        transmissionFactor = 0.013;
        bifaciality = 0.65;

        slopeLength = 1.;										/// The unit slope length of the panel
        rowToRow = 1.5;						                    /// Row to row spacing between the front of one row to the front of the next row
        double tiltRadian = this->tilt * M_PI / 180.;
        clearanceGround = 0.2;       							/// The normalized clearance from the bottom edge of module to ground
        distanceBetweenRows = rowToRow - std::cos(tiltRadian);	/// The normalized distance from the read of module to front of module in next row
        verticalHeight = std::sin(tiltRadian);
        horizontalLength = std::cos(tiltRadian);

        lat = 37.517;
        lon = -77.317;
        tz = -5.0;
        elev = 1730;
        tdry = 15;
        pres = 1013.25;
        /*
        char frontSkyConfigFactorsFile[256];
        char resource_matrix[256];
        int nb2 = sprintf(resource_matrix, "%s/test/input_cases/mhk/wave_resource_matrix.csv", SSCDIR);
        int nb3 = sprintf(device_matrix, "%s/test/input_cases/mhk/wave_power_matrix.csv", SSCDIR);
        */
        std::string sscdir(SSCDIR);
        // Truth datasets for github.com/NREL/bifacialvf test case
        frontSkyConfigFactorsFile = sscdir + "/test/input_cases/bifacialvf_data/expectedFrontSkyConfigFactors.txt";
        rearSkyConfigFactorsFile = sscdir + "/test/input_cases/bifacialvf_data/expectedRearSkyConfigFactors.txt";
        pvFrontSHFile = sscdir + "/test/input_cases/bifacialvf_data/expectedPVFrontSH.txt";
        pvBackSHFile = sscdir + "/test/input_cases/bifacialvf_data/expectedPVBackSH.txt";
        frontGroundShadeFile = sscdir + "/test/input_cases/bifacialvf_data/expectedFrontGroundShade.txt";
        rearGroundShadeFile = sscdir + "/test/input_cases/bifacialvf_data/expectedRearGroundShade.txt";
        frontGroundGHIFile = sscdir + "/test/input_cases/bifacialvf_data/expectedFrontGroundGHI.txt";
        rearGroundGHIFile = sscdir + "/test/input_cases/bifacialvf_data/expectedRearGroundGHI.txt";
        frontIrradianceFile = sscdir + "/test/input_cases/bifacialvf_data/expectedFrontIrradiance.txt";
        rearIrradianceFile = sscdir + "/test/input_cases/bifacialvf_data/expectedRearIrradiance.txt";
        frontReflectedFile = sscdir + "/test/input_cases/bifacialvf_data/expectedFrontReflected.txt";
        weatherFile = sscdir + "/test/input_cases/bifacialvf_data/expectedWeather.txt";
        averageIrradianceFile = sscdir + "/test/input_cases/bifacialvf_data/expectedAverageIrradiance.txt";


        readDataFromTextFile(pvBackSHFile, expectedPVRearShadeFraction);
        readDataFromTextFile(pvFrontSHFile, expectedPVFrontShadeFraction);
        readDataFromTextFile(frontSkyConfigFactorsFile, expectedFrontSkyConfigFactors);
        readDataFromTextFile(rearSkyConfigFactorsFile, expectedRearSkyConfigFactors);

        numberOfTimeSteps = expectedPVRearShadeFraction.size();
        numberOfSamples = 10;
        createSamples();

        // Initialize irradiation for first timestep
        irr = new irrad();
        runIrradCalc(0);
    }
    void TearDown() {
        if (irr) {
            delete irr;
        }
    }

    void createSamples()
    {
        for (size_t i = 0; i < numberOfSamples; i++) {
            int index = rand() % numberOfTimeSteps;
            samples.push_back(index);
        }
    }

    void runIrradCalc(size_t index)
    {
        std::vector<double> expectedWeather;
        readLineFromTextFile<double>(weatherFile, index, expectedWeather);

        year = (int)expectedWeather[0];
        month = (int)expectedWeather[1];
        day = (int)expectedWeather[2];
        hour = (int)expectedWeather[3];
        minute = (int)expectedWeather[4];
        beam = expectedWeather[5];
        diffuse = expectedWeather[6];
        solarAzimuthRadians = expectedWeather[7];
        solarZenithRadians = expectedWeather[8];
        solarElevationRadians = expectedWeather[9];

        irr->set_surface(tracking, tilt, azim, rotlim, backtrack, gcr, false, 0.0);
        irr->set_beam_diffuse(beam, diffuse);
        irr->set_time(year, month, day, hour, minute, 1);
        irr->set_location(lat, lon, tz);
        irr->set_optional(elev, pres, tdry);
        irr->set_sky_model(skyModel, albedo);
        irr->calc();

        // there are minor deviations in sun positions calculated in bifacialvf and SAM, likely rounding differences, but does affect decisions
        irr->set_sun_component(0, solarAzimuthRadians);
        irr->set_sun_component(1, solarZenithRadians);
        irr->set_sun_component(2, solarElevationRadians);

    }
    template <class T >
    void readLineFromTextFile(std::string fileName, size_t lineNumber, std::vector<T>& data)
    {
        size_t n = data.size();
        if (n > 0) {
            data.clear();
            data.reserve(n);
        }

        std::ifstream dataFile;
        std::string line;

        /*
        FILE *p;
        p = fopen("cwd.txt", "w");
        char path[300];
        getcwd(path, sizeof(path));
        fprintf(p, "cwd is: %s\n", path);
        */

        size_t count = 0;
        size_t maxTries = 5;
        dataFile.open(fileName);
        while (!dataFile.is_open() && count < maxTries)
        {
            std::string prefix = "../";
            prefix += fileName;
            fileName = prefix;
            //	fprintf(p, "Trying to open: %s\n", fileName.c_str());
            dataFile.open(fileName);
            count++;
        }
        //fclose(p);
        if (dataFile.is_open())
        {

            for (size_t i = 0; i <= lineNumber; i++) {
                std::getline(dataFile, line);
            }
            std::stringstream ss(line);
            std::istream_iterator<std::string> begin(ss);
            std::istream_iterator<std::string> end;
            std::vector<std::string> tmpData;
            std::copy(begin, end, back_inserter(tmpData));

            for (size_t i = 0; i < tmpData.size(); i++) {
                data.push_back(static_cast<T>(std::stod(tmpData[i])));
            }
        }
    }

    void readDataFromTextFile(std::string fileName, std::vector<double>& data)
    {
        size_t n = data.size();
        if (n > 0) {
            data.clear();
            data.reserve(n);
        }

        // variation which dumps entire file into vector, good for reading scalar at all timesteps
        std::ifstream dataFile;
        std::string line;

        size_t count = 0;
        size_t maxTries = 5;
        dataFile.open(fileName);
        while (!dataFile.is_open() && count < maxTries)
        {
            std::string prefix = "../";
            prefix += fileName;
            fileName = prefix;
            dataFile.open(fileName);
            count++;
        }

        if (dataFile.is_open())
        {

            while (std::getline(dataFile, line))
            {

                std::stringstream ss(line);
                std::istream_iterator<std::string> begin(ss);
                std::istream_iterator<std::string> end;
                std::vector<std::string> tmpData;
                std::copy(begin, end, back_inserter(tmpData));


                for (size_t i = 0; i < tmpData.size(); i++) {
                    data.push_back(std::stod(tmpData[i]));
                }

            }
        }
    }
};

#endif // !__LIB_IRRADPROC_TEST_H_

