#include <stdlib.h>

#include "lib_irradproc_test.h"

using std::vector;

/**
 * Solar Position Function Tests
 * Output: sun[] = azimuth (rad), zenith(rad), elevation(rad), declination(rad), sunrise time, sunset time,
 * eccentricity correction factor, true solar time, extraterrestrial solar irradiance on horizontal (W/m2)
 */

TEST_F(NightCaseIrradProc, solarposTest_lib_irradproc) {

    double sun[9];
    vector<double> sunrise_times;
    vector<double> sunset_times;
    e = 0.0001;
    /* Just before sunrise test case */
    solarpos(year, month, day, 4, 30, lat, lon, tz, sun);
    vector<double> solution = { 0.95662, 1.79457, -0.223771, 0.363938, 5.70882, 19.5183, 0.968276, 3.88646, 0 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "hourly before-sunrise case, parameter " << i << " fail\n";
    }
    solarpos(year, month, day, 5, 15, lat, lon, tz, sun);
    solution = { 1.0744, 1.65255, -0.0817513, 0.363839, 5.7091, 19.518, 0.96828, 4.63642, 0 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "15m before-sunrise case, parameter " << i << " fail\n";
    }

    /* Just after sunset test case */
    solarpos(year, month, day, 20, 30, lat, lon, tz, sun);
    solution = { 5.28748, 1.75391, -0.183117, 0.361807, 5.71544, 19.5131, 0.968361, 19.8857, 0 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "hourly after-sunset case, parameter " << i << " fail\n";
    }
    solarpos(year, month, day, 19, 45, lat, lon, tz, sun);
    solution = { 5.17431, 1.60864, -0.0378397, 0.361908, 5.71513, 19.5133, 0.968357, 19.1358, 0 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "15m after-sunrise case, parameter " << i << " fail\n";
    }
}

TEST_F(SunriseCaseIrradProc, solarposTest_lib_irradproc) {
    double sun[9];
    vector<double> sunrise_times;
    vector<double> sunset_times;

    solarpos(year, month, day, 5, 30, lat, lon, tz, sun);
    vector<double> solution = { 1.11047, 1.6031, -0.0323028, 0.363806, 5.70924, 19.5179, 0.968281, 4.88641, 0 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "sunrise case, parameter " << i << " fail\n";
    }
}

TEST_F(IrradTest, sunriseAndSunsetAtDifferentLocationsTest_lib_irradproc) {
    /*locations to test:
    western hemisphere: Golden CO
    eastern hemisphere: Berlin Germany
    southern hemisphere: Lima Peru
    location near Greenwich meridian with negative longitude and positive time zone: Madrid Spain
    location near the international dateline with positive longitude and negative time zone: Lomaji, Fiji
    arctic circle: Kotzebue, Alaska
    arctic circle #2: Point Hope, Alaska
    arctic circle #3: Kotzebue, Alaska on the first day of continuous days
    */
    e = 0.001;
    vector<double> latitudes = { 39.77, 52.5, -12.03, 40.43, -17.75, 66.9, 68.35, 66.9 };
    vector<double> longitudes = { -105.22, 13.3, -77.06, -3.72, -179.3, -162.6, -166.8, -162.6 };
    vector<double> time_zones = { -7, 1, -5, 1, 12, -9, -9, -9 };
    vector<double> sunrise_times = { 4.636, 3.849, 6.521, 5.833, 6.513, -100.0, 2.552, -100.0 };
    vector<double> sunset_times = { 19.455, 20.436, 17.814, 20.723, 17.449, 100.0, 25.885, 100.0 };
    vector<int> month = { 6, 6, 6, 6, 6, 6, 7, 6 };
    vector<int> day = { 21, 21, 21, 21, 21, 21, 14, 11 };


    double sun_results[9]; //vector to hold the results of solarpos function
    for (size_t i = 0; i < latitudes.size(); i++)
    {
        //run the solarpos function and check sunrise and sunset for each location
        solarpos(2010, month[i], day[i], 14, 30, latitudes[i], longitudes[i], time_zones[i], sun_results);
        EXPECT_NEAR((double)sun_results[4], sunrise_times[i], e) << "sunrise time for lat " << latitudes[i] << " long " << longitudes[i] << " failed\n";
        EXPECT_NEAR((double)sun_results[5], sunset_times[i], e) << "sunset time for lat" << latitudes[i] << " long " << longitudes[i] << "failed\n";
    }
}

TEST_F(IrradTest, sunriseAndSunsetAtDifferentLocationsTest_spa_lib_irradproc) {
    /*locations to test:
    western hemisphere: Golden CO
    eastern hemisphere: Berlin Germany
    southern hemisphere: Lima Peru
    location near Greenwich meridian with negative longitude and positive time zone: Madrid Spain
    location near the international dateline with positive longitude and negative time zone: Lomaji, Fiji
    arctic circle: Kotzebue, Alaska
    arctic circle #2: Point Hope, Alaska
    arctic circle #3: Kotzebue, Alaska on the first day of continuous days
    */
    e = 0.001;
    vector<double> latitudes = { 39.77, 52.5, -12.03, 40.43, -17.75, 66.9, 68.35, 66.9 };
    vector<double> longitudes = { -105.22, 13.3, -77.06, -3.72, -179.3, -162.6, -166.8, -162.6 };
    vector<double> time_zones = { -7, 1, -5, 1, 12, -9, -9, -9 };
    vector<double> sunrise_times = { 4.549, 3.726, 6.458, 5.745, 6.451, -100.0, 2.7831, -100.0 };
    vector<double> sunset_times = { 19.541, 20.559, 17.877, 20.810, 17.514, 100.0, 25.4795, 100.0 };
    vector<int> month = { 6, 6, 6, 6, 6, 6, 7, 6 };
    vector<int> day = { 21, 21, 21, 21, 21, 21, 20, 11 };
    vector<int> alt = { 1730, 34, 154, 667, 0, 6, 2, 6 };

    double sun_results[9]; //vector to hold the results of solarpos function
    for (size_t i = 0; i < latitudes.size(); i++)
    {
        //run the solarpos function and check sunrise and sunset for each location
        solarpos_spa(2010, month[i], day[i], 14, 30, 0, latitudes[i], longitudes[i], time_zones[i], 0, alt[i], 0, 1016, 15, 180, sun_results);
        EXPECT_NEAR((double)sun_results[4], sunrise_times[i], e) << "sunrise time for lat " << latitudes[i] << " long " << longitudes[i] << " failed\n";
        EXPECT_NEAR((double)sun_results[5], sunset_times[i], e) << "sunset time for lat " << latitudes[i] << " long " << longitudes[i] << "failed\n";
    }
}

TEST_F(IrradTest, sunriseAndSunsetAlaskaTest_spa_lib_irradproc) {
    e = 0.001;
    double latitude = -17.75;
    double longitude = -179.3;
    double time_zone = 12;
    double sunrise_time = 6.451;
    double sunset_time = 17.514;
    int month = 6;
    int day = 21;
    double sun_results[9];
    double alt = 0;
    solarpos_spa(2010, month, day, 14, 30, 0, latitude, longitude, time_zone, 0, 2, 1016, 15, latitude, 180, sun_results);
    EXPECT_NEAR((double)sun_results[4], sunrise_time, e) << "sunrise time for lat " << latitude << " long " << longitude << " failed\n";
    EXPECT_NEAR((double)sun_results[5], sunset_time, e) << "sunrise time for lat " << latitude << " long " << longitude << " failed\n";
}

TEST_F(IrradTest, atmos_refractionTest_spa_lib_irradproc) {
    //Test to check for atmospheric refraction correction occuring only if sun is above horizon
    double latitude = 31.6430;
    double longitude = 74.8723;
    double time_zone = 5.5;
    double elevation_angle = -.00175; //topocentric elevation angle corrected for atmospheric refraction (radians)
    //double sunset_time = 17.514;
    int month = 7;
    int day = 19;
    double sun_results[9];
    double alt = 0;
    solarpos_spa(2017, month, day, 5, 39, 0, latitude, longitude, time_zone, 0, 234, 1013.25, 15, latitude, 180, sun_results);
    EXPECT_NEAR((double)sun_results[2], elevation_angle, e) << "elevation angle for lat " << latitude << " long " << longitude << " failed\n";
}


TEST_F(DayCaseIrradProc, solarposTest_lib_irradproc) {
    double sun[9];
    vector<double> sunrise_times;
    vector<double> sunset_times;

    /* Just before sunset test case */
    solarpos(year, month, day, 18, 30, lat, lon, tz, sun);
    vector<double>solution = { 5.01022, 1.3584, 0.212397, 0.362076, 5.71461, 19.5137, 0.96835, 17.8858, 279.08756 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "hourly before-sunset case, parameter " << i << " fail\n";
    }
    solarpos(year, month, day, 19, 15, lat, lon, tz, sun);
    solution = { 5.10579, 1.51295, 0.0578472, 0.361975, 5.71492, 19.5135, 0.968354, 18.6358, 76.5423 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "15m before-sunset case, parameter " << i << " fail\n";
    }

    /* Sunset time test case */
    solarpos(year, month, day, 19, 30, lat, lon, tz, sun);
    solution = { 5.13947, 1.55886, 0.0119379, 0.361941, 5.71503, 19.5134, 0.968356, 18.8858, 15.8044 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "sunset case, parameter " << i << " fail\n";
    }
}

TEST_F(SunsetCaseIrradProc, solarposTest_lib_irradproc) {
    double sun[9];
    vector<double> sunrise_times;
    vector<double> sunset_times;

    /* Sunset time test case */
    solarpos(year, month, day, 19, 30, lat, lon, tz, sun);
    vector<double>solution = { 5.13947, 1.55886, 0.0119379, 0.361941, 5.71503, 19.5134, 0.968356, 18.8858, 15.8044 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "sunset case, parameter " << i << " fail\n";
    }
}

/**
* Solar Incidence Function Test
* Mode = 0 for fixed tilt.
* Output: angle[] = incident angle (rad), tilt angle (rad), surface azimuth (rad), tracking axis rotation angle for single axis tracker (rad),
* backtracking angle difference: rot - ideal_rot (rad)
*/

TEST_F(NightCaseIrradProc, solarpos_spaTest_lib_irradproc) {
    double sun[9];
    double needed[13];
    vector<double> sunrise_times;
    vector<double> sunset_times;
    e = 0.0001;
    /* Just before sunrise test case */
    solarpos_spa(year, month, day, 4, 30, 0, lat, lon, tz, 0, 0, 1016, 15, lat, 180, sun);
    vector<double> solution = { 0.95668, 1.80432, -0.233522, 0.363905, 5.636927, 19.584888, 0.968276, 3.88691, 0 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "hourly before-sunrise case, parameter " << i << " fail\n";
    }
    solarpos_spa(year, month, day, 5, 15, 0, lat, lon, tz, 0, 0, 1016, 15, lat, 180, sun);
    solution = { 1.0744, 1.6623, -0.091497, 0.363809, 5.636927, 19.584888, 0.96828, 4.63687, 0 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "15m before-sunrise case, parameter " << i << " fail\n";
    }

    /* Just after sunset test case */
    solarpos_spa(year, month, day, 20, 30, 0, lat, lon, tz, 0, 234, 1016, 15, lat, 180, sun);
    solution = { 5.28754, 1.76380, -0.19300, 0.361775, 5.636927, 19.584888, 0.968361, 19.88618, 0 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "hourly after-sunset case, parameter " << i << " fail\n";
    }
    solarpos_spa(year, month, day, 19, 45, 0, lat, lon, tz, 0, 234, 1016, 15, lat, 180, sun);
    solution = { 5.17436, 1.618526, -0.047730, 0.361878, 5.636927, 19.584888, 0.968357, 19.13621, 0 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "15m after-sunrise case, parameter " << i << " fail\n";
    }
}

TEST_F(SunriseCaseIrradProc, solarpos_spaTest_lib_irradproc) {
    double sun[9];
    double needed[13];
    vector<double> sunrise_times;
    vector<double> sunset_times;
    e = 0.0001;
    solarpos_spa(year, month, day, 5, 30, 0, lat, lon, tz, 0, 234, 1016, 15, lat, 180, sun);
    vector<double> solution = { 1.11053, 1.61284, -0.0420474, 0.363777, 5.636927, 19.584888, 0.968281, 4.88686, 0 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "sunrise case, parameter " << i << " fail\n";
    }
}

TEST_F(DayCaseIrradProc, solarpos_spaTest_lib_irradproc) {
    double sun[9];
    double needed[13];
    vector<double> sunrise_times;
    vector<double> sunset_times;

    /* Just before sunset test case */
    solarpos_spa(year, month, day, 18, 30, 0, lat, lon, tz, 0, 234, 1016, 15, lat, 180, sun);
    vector<double>solution = { 5.01026, 1.35848, 0.212317, 0.36205, 5.636927, 19.584888, 0.96835, 17.88626, 278.9899 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "hourly before-sunset case, parameter " << i << " fail\n";
    }
    solarpos_spa(year, month, day, 19, 15, 0, lat, lon, tz, 0, 234, 1016, 15, lat, 180, sun);
    solution = { 5.10583, 1.51323, 0.057570, 0.361947, 5.636927, 19.584888, 0.968358, 18.6362, 76.1906 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "15m before-sunset case, parameter " << i << " fail\n";
    }

    /* Sunset time test case */
    solarpos_spa(year, month, day, 19, 30, 0, lat, lon, tz, 0, 234, 1016, 15, lat, 180, sun);
    solution = { 5.13951, 1.56025, 0.010544, 0.361913, 5.636927, 19.584888, 0.968356, 18.88622, 13.9890 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "sunset case, parameter " << i << " fail\n";
    }
}

TEST_F(SunsetCaseIrradProc, solarpos_spaTest_lib_irradproc) {
    double sun[9];
    double needed[13];
    vector<double> sunrise_times;
    vector<double> sunset_times;
    e = 0.0001;
    solarpos_spa(year, month, day, 19, 30, 0, lat, lon, tz, 0, 234, 1016, 15, lat, 180, sun);
    vector<double> solution = { 5.13951, 1.56025, 0.010544, 0.361913, 5.636927, 19.584888, 0.968356, 18.88622, 13.98903 };
    sunrise_times.push_back(solution[4]);
    sunset_times.push_back(solution[5]);
    for (int i = 0; i < 9; i++) {
        EXPECT_NEAR((double)sun[i], solution[i], e) << "sunset case, parameter " << i << " fail\n";
    }
}

TEST_F(NightCaseIrradProc, incidenceTest_lib_irradproc) {
    int mode = 0;
    double angle[5] = { 0 };
    double sun_zen, sun_azm;
    vector<double> solutions;

    /* Just before sunrise test case */
    sun_azm = 0.95662;
    sun_zen = 1.79457;
    incidence(mode, tilt, azim, rotlim, sun_zen, sun_azm, backtrack_on, gcr, false, 0.0, angle);
    solutions = { 1.89243, 0.174533, 3.14159, 0, 0 };
    for (int i = 0; i < 5; i++) {
        EXPECT_NEAR(angle[i], solutions[i], e) << "before-sunrise case";
    }
}

TEST_F(SunriseCaseIrradProc, incidenceTest_lib_irradproc) {
    int mode = 0;
    double angle[5] = { 0 };
    double sun_zen, sun_azm;
    double solution;
    vector<double> solutions;

    sun_azm = 1.11047;
    sun_zen = 1.6031;
    incidence(mode, tilt, azim, rotlim, sun_zen, sun_azm, backtrack_on, gcr, false, 0.0, angle);
    solution = 1.67992;
    EXPECT_NEAR(angle[0], solution, e) << "sunrise case";
}

TEST_F(DayCaseIrradProc, incidenceTest_lib_irradproc) {
    int mode = 0;
    double angle[5] = { 0 };
    double sun_zen, sun_azm;
    double solution;
    vector<double> solutions;

    sun_azm = 0;
    sun_zen = 0;
    incidence(mode, tilt, azim, rotlim, sun_zen, sun_azm, backtrack_on, gcr, false, 0.0, angle);
    solution = 0.174533;
    EXPECT_NEAR(angle[0], solution, e) << "noon case";
}

TEST_F(SunsetCaseIrradProc, incidenceTest_lib_irradproc) {
    int mode = 0;
    double angle[5] = { 0 };
    double sun_zen, sun_azm;
    double solution;
    vector<double> solutions;

    sun_azm = 5.13947;
    sun_zen = 1.55886;
    incidence(mode, tilt, azim, rotlim, sun_zen, sun_azm, backtrack_on, gcr, false, 0.0, angle);
    solution = 1.631;
    EXPECT_NEAR(angle[0], solution, e) << "sunset case";
}

/**
* Calc Function Tests
* Output:
* sun[] =	azimuth (rad), zenith(rad), elevation(rad), declination(rad), sunrise time, sunset time,
*			eccentricity correction factor, true solar time, extraterrestrial solar irradiance on horizontal (W/m2);
* angle_p[] = incident angle (rad), tilt angle (rad), surface azimuth (rad), tracking axis rotation angle for single axis tracker (rad),
*			backtracking angle difference: rot - ideal_rot (rad);
* poa_p[] = incident beam, incident sky diffuse, incident ground diffuse, diffuse isotropic, diffuse circumsolar, horizon brightening (W/m2);
* irrad parameters: ghi, dni, dhi
*/

TEST_F(NightCaseIrradProc, CalcTestRadMode0_lib_irradproc) {
    vector<double> sun_p;
    sun_p.resize(10);
    int sunup = false;
    vector<double> angle_p;
    angle_p.resize(5);
    vector<double> poa_p;
    poa_p.resize(6);
    vector<double> rad_p = { 1, 1, 1 };

    irr_hourly_night.set_beam_diffuse(rad_p[1], rad_p[2]);
    irr_15m_night.set_beam_diffuse(rad_p[1], rad_p[2]);

    /* Hourly during the night */
    irr_hourly_night.calc();
    irr_hourly_night.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
    irr_hourly_night.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
    irr_hourly_night.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
    irr_hourly_night.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

    sun_p[6] = (double)sunup;
    vector<double> sun_solution = { 15.406943, 125.967030, -35.967029, 20.872514, 5.636927, 19.584888, 0, 0.968315, 0.887054, 0 }; 
    for (int i = 0; i < 10; i++) {
        EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "hourly_night, sun parameter " << i << " fail\n";
    }
    vector<double> angle_solution = { 0, 0, 0, 0, 0 };	// azim & tilt returned as 0 when sun is down
    for (int i = 0; i < 5; i++) {
        EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "hourly_night, angle parameter " << i << " fail\n";
    }
    vector<double> poa_solution = { 0, 0, 0, 0, 0, 0 };
    for (int i = 0; i < 6; i++) {
        EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "hourly_night, poa parameter " << i << " fail\n";
    }
    vector<double>  rad_solution = { 0, 0, 0 };
    for (int i = 0; i < 3; i++) {
        EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "hourly_night, irradiance parameter " << i << " fail\n";
    }

    /* 15m during the night */
    irr_15m_night.calc();
    irr_15m_night.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
    irr_15m_night.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
    irr_15m_night.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
    irr_15m_night.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

    sun_p[6] = (double)sunup;
    sun_solution = { 11.153456, 126.698858, -36.698858, 20.874383, 5.636927, 19.584888, 0, 0.968315, 0.637066, 0 }; 
    for (int i = 0; i < 10; i++) {
        EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "15m_night, sun parameter " << i << " fail\n";
    }
    angle_solution = { 0, 0, 0, 0, 0 };
    for (int i = 0; i < 5; i++) {
        EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "15m_night, angle parameter " << i << " fail\n";
    }
    poa_solution = { 0, 0, 0, 0, 0, 0 };
    for (int i = 0; i < 6; i++) {
        EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "15m_night, poa parameter " << i << " fail\n";
    }
    rad_solution = { 0, 0, 0 };
    for (int i = 0; i < 3; i++) {
        EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "15m_night, irradiance parameter " << i << " fail\n";
    }
}

TEST_F(SunriseCaseIrradProc, CalcTestRadMode0_lib_irradproc) {
    vector<double> sun_p;
    sun_p.resize(10);
    int sunup = true;
    vector<double> angle_p;
    angle_p.resize(5);
    vector<double> poa_p;
    poa_p.resize(6);
    vector<double> rad_p = { 1, 1, 1 };

    irr_hourly_sunrise.set_beam_diffuse(rad_p[1], rad_p[2]);
    irr_15m_sunrise.set_beam_diffuse(rad_p[1], rad_p[2]);

    /* hourly during sunrise */
    irr_hourly_sunrise.calc();
    irr_hourly_sunrise.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
    irr_hourly_sunrise.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
    irr_hourly_sunrise.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
    irr_hourly_sunrise.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

    sun_p[6] = (double)sunup;
    vector<double> sun_solution = { 66.140193, 88.414600, 1.585400, 20.840523, 5.636927, 19.584888, 2.0, 0.968283, 5.205311, 36.628086 }; 
    for (int i = 0; i < 10; i++) {
        EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "hourly_sunrise, sun parameter " << i << " fail\n";
    }
    vector<double> angle_solution = { 92.462599, tilt, azim, 0, 0 }; 
    for (int i = 0; i < 5; i++) {
        EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "hourly_sunrise, angle parameter " << i << " fail\n";
    }
    vector<double> poa_solution = { 0, 0.992404, 0, 0.992404, 0, 0 };
    for (int i = 0; i < 6; i++) {
        EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "hourly_sunrise, poa parameter " << i << " fail\n";
    }
    vector<double> rad_solution = { -999, 1, 1 };
    for (int i = 0; i < 3; i++) {
        EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "hourly_sunrise, irradiance parameter " << i << " fail\n";
    }

    /* 15m during sunrise */
    irr_15m_sunrise.calc();
    irr_15m_sunrise.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
    irr_15m_sunrise.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
    irr_15m_sunrise.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
    irr_15m_sunrise.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

    sun_p[6] = (double)sunup;
    sun_solution = { 66.140193, 88.414600, 1.585400, 20.840523, 5.636927, 19.584888, 2.0, 0.968283, 5.205311, 36.628086 }; 
    for (int i = 0; i < 10; i++) {
        EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "15m_sunrise, sun parameter " << i << " fail\n";
    }
    angle_solution = { 92.462599, tilt, azim, 0, 0 }; 
    for (int i = 0; i < 5; i++) {
        EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "15m_sunrise, angle parameter " << i << " fail\n";
    }
    poa_solution = { 0, 0.992404, 0, 0.992404, 0, 0 };
    for (int i = 0; i < 6; i++) {
        EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "15m_sunrise, poa parameter " << i << " fail\n";
    }
    rad_solution = { -999, 1, 1 };
    for (int i = 0; i < 3; i++) {
        EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "15m_sunrise, irradiance parameter " << i << " fail\n";
    }
}

TEST_F(DayCaseIrradProc, CalcTestRadMode0_lib_irradproc) {
    vector<double> sun_p;
    sun_p.resize(10);
    int sunup = true;
    vector<double> angle_p;
    angle_p.resize(5);
    vector<double> poa_p;
    poa_p.resize(6);
    vector<double> rad_p = { 1, 1, 1 };

    irr_hourly_day.set_beam_diffuse(rad_p[1], rad_p[2]);
    irr_15m_day.set_beam_diffuse(rad_p[1], rad_p[2]);

    /* Hourly during the day */
    irr_hourly_day.calc();
    irr_hourly_day.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
    irr_hourly_day.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
    irr_hourly_day.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
    irr_hourly_day.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

    sun_p[6] = (double)sunup;
    vector<double> sun_solution = { 171.589918, 10.946708, 79.053292, 20.790629, 5.636927, 19.584888, 1.0, 0.968318, 11.886537, 1299.830748 }; 
    for (int i = 0; i < 10; i++) {
        EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "hourly_day, sun parameter " << i << " fail\n";
    }
    vector<double> angle_solution = { 1.795696, tilt, azim, 0, 0 }; 
    for (int i = 0; i < 5; i++) {
        EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "hourly_day, angle parameter " << i << " fail\n";
    }
    vector<double> poa_solution = { 0.999509, 1.052130, 0.003011, 0.194810, 0.818170, 0.039150 };
    for (int i = 0; i < 6; i++) {
        EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "hourly_day, poa parameter " << i << " fail\n";
    }
    vector<double> rad_solution = { -999, 1.0, 1.0 };
    for (int i = 0; i < 3; i++) {
        EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "hourly_day, irradiance parameter " << i << " fail\n";
    }

    /* 15m during the day */
    irr_15m_day.calc();
    irr_15m_day.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
    irr_15m_day.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
    irr_15m_day.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
    irr_15m_day.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

    sun_p[6] = (double)sunup;
    sun_solution = { 190.080739, 10.995677, 79.004323, 20.788721, 5.636927, 19.584888, 1.000000, 0.968318, 12.136525, 1299.615402 }; 
    for (int i = 0; i < 10; i++) {
        EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "15m_day, sun parameter " << i << " fail\n";
    }
    angle_solution = { 2.08540, tilt, azim, 0, 0 }; 
    for (int i = 0; i < 5; i++) {
        EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "15m_day, angle parameter " << i << " fail\n";
    }
    poa_solution = { 0.999343, 1.052130, 0.003011, 0.195107, 0.817860, 0.039150 };
    for (int i = 0; i < 6; i++) {
        EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "15m_day, poa parameter " << i << " fail\n";
    }
    rad_solution = { -999, 1.0, 1.0 };
    for (int i = 0; i < 3; i++) {
        EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "15m_day, irradiance parameter " << i << " fail\n";
    }
}

TEST_F(SunsetCaseIrradProc, CalcTestRadMode0_lib_irradproc) {
    vector<double> sun_p;
    sun_p.resize(10);
    int sunup = false;
    vector<double> angle_p;
    angle_p.resize(5);
    vector<double> poa_p;
    poa_p.resize(6);
    vector<double> rad_p = { 1, 1, 1 };
    irr_hourly_sunset.set_beam_diffuse(rad_p[1], rad_p[2]);
    irr_15m_sunset.set_beam_diffuse(rad_p[1], rad_p[2]);

    /* hourly during sunset */
    irr_hourly_sunset.calc();
    irr_hourly_sunset.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
    irr_hourly_sunset.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
    irr_hourly_sunset.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
    irr_hourly_sunset.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

    sun_p[6] = (double)sunup;
    vector<double>sun_solution = { 292.796442, 87.076168, 2.923832, 20.737781, 5.636927, 19.584888, 3.0, 0.968355, 18.678677, 67.531571 }; 
    for (int i = 0; i < 10; i++) {
        EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "hourly_sunset, sun parameter " << i << " fail\n";
    }
    vector<double>angle_solution = { 90.971808, tilt, azim, 0, 0 }; 
    for (int i = 0; i < 5; i++) {
        EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "hourly_sunset, angle parameter " << i << " fail\n";
    }
    vector<double>poa_solution = { 0, 0.981644, 0.001605, 0.992404, 0, -0.010760 };
    for (int i = 0; i < 6; i++) {
        EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "hourly_sunset, poa parameter " << i << " fail\n";
    }
    vector<double>rad_solution = { -999, 1, 1 };
    for (int i = 0; i < 3; i++) {
        EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "hourly_sunset, irradiance parameter " << i << " fail\n";
    }

    /* 15m during sunset */
    irr_15m_sunset.calc();
    irr_15m_sunset.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
    irr_15m_sunset.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
    irr_15m_sunset.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
    irr_15m_sunset.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

    sun_p[6] = (double)sunup;
    sun_solution = { 292.796442, 87.076168, 2.923832, 20.737781, 5.636927, 19.584888, 3.0, 0.968355, 18.678677, 67.531571 }; 
    for (int i = 0; i < 10; i++) {
        EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "15m_sunset, sun parameter " << i << " fail\n";
    }
    angle_solution = { 90.971808, tilt, azim, 0, 0 }; 
    for (int i = 0; i < 5; i++) {
        EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "15m_sunset, angle parameter " << i << " fail\n";
    }
    poa_solution = { 0, 0.981644, 0.001605, 0.992404, 0, -0.010760 };
    for (int i = 0; i < 6; i++) {
        EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "15m_sunset, poa parameter " << i << " fail\n";
    }
    rad_solution = { -999, 1, 1 };
    for (int i = 0; i < 3; i++) {
        EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "15m_sunset, irradiance parameter " << i << " fail\n";
    }

    /*
    printf("sun:%f, %f, %f, %f, %f, %f, %f, %f, %f, %f", sun_p[0], sun_p[1], sun_p[2], sun_p[3], sun_p[4], sun_p[5], (double)sunup, sun_p[6], sun_p[7], sun_p[8]);
    printf("angles: %f, %f, %f, %f, %f \n", angle_p[0], angle_p[1], angle_p[2], angle_p[3], angle_p[4]);
    printf("poa: %f, %f, %f, %f, %f, %f \n", poa_p[0], poa_p[1], poa_p[2], poa_p[3], poa_p[4], poa_p[5]);
    printf("irrad: %f, %f, %f \n", &rad_p[0], &rad_p[1], &rad_p[2]);
    */
}

/**
*   Test Sky Configuration factors.  These factors do not change with time, just system geometry
*/
TEST_F(BifacialIrradTest, TestSkyConfigFactors)
{
    // Determine the factors for points on the ground from the leading edge of one row of PV panels to the edge of the next row of panels behind
    std::vector<double> rearSkyConfigFactors, frontSkyConfigFactors;
    irr->getSkyConfigurationFactors(rowToRow, verticalHeight, clearanceGround, distanceBetweenRows, horizontalLength, rearSkyConfigFactors, frontSkyConfigFactors);

    ASSERT_EQ(rearSkyConfigFactors.size(), expectedRearSkyConfigFactors.size());

    for (size_t i = 0; i != rearSkyConfigFactors.size(); i++) {
        ASSERT_NEAR(rearSkyConfigFactors[i], expectedRearSkyConfigFactors[i], e);
        ASSERT_NEAR(frontSkyConfigFactors[i], expectedFrontSkyConfigFactors[i], e);
    }
}
/**
*   Test Ground Shade factors.  These factors do not change with time, just system geometry
*/
TEST_F(BifacialIrradTest, TestGroundShadeFactors)
{
    for (size_t s = 0; s < numberOfSamples; s++)
    {
        size_t t = samples[s];
        runIrradCalc(t);

        readLineFromTextFile(frontGroundShadeFile, t, expectedFrontGroundShade);
        readLineFromTextFile(rearGroundShadeFile, t, expectedRearGroundShade);

        // Determine if ground is shading from direct beam radio for points on the ground from leading edge of PV panels to leading edge of next row behind
        double maxShadow, pvBackShadeFraction, pvFrontShadeFraction;
        maxShadow = pvBackShadeFraction = pvFrontShadeFraction = 0;
        std::vector<int> rearGroundShade, frontGroundShade;
        irr->getGroundShadeFactors(rowToRow, verticalHeight, clearanceGround, distanceBetweenRows, horizontalLength, irr->get_sun_component(0), irr->get_sun_component(2), rearGroundShade, frontGroundShade, maxShadow, pvBackShadeFraction, pvFrontShadeFraction);

        ASSERT_EQ(rearGroundShade.size(), expectedRearGroundShade.size()) << "Failed at t = " << t;
        ASSERT_EQ(frontGroundShade.size(), expectedFrontGroundShade.size()) << "Failed at t = " << t;;
        ASSERT_NEAR(pvFrontShadeFraction, expectedPVFrontShadeFraction[t], e) << "Failed at t = " << t;;
        ASSERT_NEAR(pvBackShadeFraction, expectedPVRearShadeFraction[t], e) << "Failed at t = " << t;;

        for (size_t i = 0; i != rearGroundShade.size(); i++) {
            ASSERT_NEAR(rearGroundShade[i], expectedRearGroundShade[i], e) << "Failed at t = " << t << " i = " << i;;
            ASSERT_NEAR(frontGroundShade[i], expectedFrontGroundShade[i], e) << "Failed at t = " << t << " i = " << i;;
        }
    }
}
/**
*   Test calculation of ground GHI.  This changes with sun position and system geometry
*/
TEST_F(BifacialIrradTest, TestGroundGHI)
{
    for (size_t s = 0; s < numberOfSamples; s++)
    {
        size_t t = samples[s];
        runIrradCalc(t);
        readLineFromTextFile(frontGroundShadeFile, t, expectedFrontGroundShade);
        readLineFromTextFile(rearGroundShadeFile, t, expectedRearGroundShade);
        readLineFromTextFile(frontGroundGHIFile, t, expectedFrontGroundGHI);
        readLineFromTextFile(rearGroundGHIFile, t, expectedRearGroundGHI);

        std::vector<double> rearGroundGHI, frontGroundGHI;
        irr->getGroundGHI(transmissionFactor, expectedRearSkyConfigFactors, expectedFrontSkyConfigFactors, expectedRearGroundShade, expectedFrontGroundShade, rearGroundGHI, frontGroundGHI);

        ASSERT_EQ(rearGroundGHI.size(), expectedRearGroundGHI.size()) << "Failed at t = " << t;
        ASSERT_EQ(frontGroundGHI.size(), expectedFrontGroundGHI.size()) << "Failed at t = " << t;

        for (size_t i = 0; i != rearGroundGHI.size(); i++) {
            ASSERT_NEAR(rearGroundGHI[i], expectedRearGroundGHI[i], e) << "Failed at t = " << t << " i = " << i;
            ASSERT_NEAR(frontGroundGHI[i], expectedFrontGroundGHI[i], e) << "Failed at t = " << t << " i = " << i;
        }
    }
}

/**
*   Test calculation of front surface irradiances.  This changes with sun position and system geometry
*/
TEST_F(BifacialIrradTest, TestFrontSurfaceIrradiance)
{
    for (size_t s = 0; s < numberOfSamples; s++)
    {
        size_t t = samples[s];
        runIrradCalc(t);
        readLineFromTextFile<double>(frontGroundGHIFile, t, expectedFrontGroundGHI);
        readLineFromTextFile<double>(averageIrradianceFile, t, expectedAverageIrradiance);
        readLineFromTextFile<double>(frontIrradianceFile, t, expectedFrontIrradiance);
        readLineFromTextFile<double>(frontReflectedFile, t, expectedFrontReflected);

        std::vector<double> frontIrradiance, frontReflected;
        double frontAverageIrradiance = 0;
        irr->getFrontSurfaceIrradiances(expectedPVFrontShadeFraction[t], rowToRow, verticalHeight, clearanceGround, distanceBetweenRows, horizontalLength, expectedFrontGroundGHI, frontIrradiance, frontAverageIrradiance, frontReflected);

        ASSERT_EQ(frontIrradiance.size(), expectedFrontIrradiance.size()) << "Failed at t = " << t;
        ASSERT_NEAR(frontAverageIrradiance, expectedAverageIrradiance[0], e) << "Failed at t = " << t;

        for (size_t i = 0; i != frontIrradiance.size(); i++) {
            ASSERT_NEAR(frontIrradiance[i], expectedFrontIrradiance[i], e) << "Failed at t = " << t << " i = " << i;
            ASSERT_NEAR(frontReflected[i], expectedFrontReflected[i], e) << "Failed at t = " << t << " i = " << i;
        }
    }
}

/**
*   Test calculation of rear surface irradiances.  This changes with sun position and system geometry
*/
TEST_F(BifacialIrradTest, TestRearSurfaceIrradiance)
{
    for (size_t s = 0; s < numberOfSamples; s++)
    {
        size_t t = samples[s];
        runIrradCalc(t);

        readLineFromTextFile<double>(frontGroundGHIFile, t, expectedFrontGroundGHI);
        readLineFromTextFile<double>(rearGroundGHIFile, t, expectedRearGroundGHI);
        readLineFromTextFile<double>(frontReflectedFile, t, expectedFrontReflected);
        readLineFromTextFile<double>(rearIrradianceFile, t, expectedRearIrradiance);
        readLineFromTextFile<double>(averageIrradianceFile, t, expectedAverageIrradiance);

        std::vector<double> rearIrradiance;
        double rearAverageIrradiance = 0;
        irr->getBackSurfaceIrradiances(expectedPVRearShadeFraction[t], rowToRow, verticalHeight, clearanceGround, distanceBetweenRows, horizontalLength, expectedRearGroundGHI, expectedFrontGroundGHI, expectedFrontReflected, rearIrradiance, rearAverageIrradiance);

        ASSERT_EQ(rearIrradiance.size(), expectedRearIrradiance.size()) << "Failed at t = " << t;
        ASSERT_NEAR(rearAverageIrradiance, expectedAverageIrradiance[1], e) << "Failed at t = " << t;

        for (size_t i = 0; i != rearIrradiance.size(); i++) {
            ASSERT_NEAR(rearIrradiance[i], expectedRearIrradiance[i], e) << "Failed at t = " << t << " i = " << i;
        }
    }
}

/**
*   Test single-axis tracking and bactracking rotations and shaded fraction
*/
TEST(SingleAxisTrackingTest, TrackingBacktracking) {
	std::vector<double> solar_zeniths = {0, 10, 80, 85, 90};
	std::vector<double> solar_azimuths = {180, 100, 250,   87.163, 300};
	std::vector<double> axis_tilts = {0, 0, 10, 10, 10};
	std::vector<double> axis_azimuths = {10, 150, 240, 180, 0};
	std::vector<double> expected_truetracking = {0.0, -7.69263, 26.74021, -85.55932, -84.27489};
	std::vector<double> expected_backtracking = {0.0, -7.69263, 26.74021, -6.72036, -8.71628};
	std::vector<double> expected_shadefraction = {0.0, 0.0, 0.0, 0.80643, 0.75061};
	double gcr = 0.4;

	double tt, bt, fs_tt, fs_bt;

	for(int i = 0; i < expected_truetracking.size(); i++)
	{
		tt = truetrack(solar_azimuths[i], solar_zeniths[i], axis_tilts[i], axis_azimuths[i]);
		ASSERT_NEAR(tt, expected_truetracking[i], 1e-4);
		bt = backtrack(tt, 0.4);
		ASSERT_NEAR(bt, expected_backtracking[i], 1e-4);
		fs_tt = shadeFraction1x(solar_azimuths[i], solar_zeniths[i], axis_tilts[i], axis_azimuths[i], gcr, tt);
		ASSERT_NEAR(fs_tt, expected_shadefraction[i], 1e-4);
		fs_bt = shadeFraction1x(solar_azimuths[i], solar_zeniths[i], axis_tilts[i], axis_azimuths[i], gcr, bt);
		ASSERT_NEAR(fs_bt, 0, 1e-10);  // no self-shading when backtracking
	}
}

/**
*   Test backtracking when sun is underneath the plane containing the tracker axes
*/
TEST(SingleAxisTrackingTest, SunBelowTiltedArray) {
	double tt = truetrack(300, 89, 10, 180);
	ASSERT_NEAR(tt, 94.59707, 1e-4);  // true-tracking rotation > 90 when sun is below system plane
	double bt = backtrack(tt, 0.4);
	ASSERT_NEAR(bt, 16.15566, 1e-4);
}


