#include "lib_time_test.h"
#include "lib_time.h"

// Single year is 60 min, lifetime is 30 min, 25 years
TEST_F(libTimeTest_lib_time, single_year_to_lifetime_interpolated_Lifetime)
{
	is_lifetime = true;
	std::vector<float> lifetime_from_single;
	size_t n_rec_lifetime = lifetime30min.size();
	size_t n_rec_singleyear;
	double dt_hour;
	single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, n_rec_lifetime,
		singleyear60min, scaleFactors, interpolation_factor, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year * 2 * n_years);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year * 2);
	EXPECT_EQ(dt_hour, 0.5);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);

	for (size_t y = 0; y < n_years; y++) {
		size_t idx = y * singleyear60min.size();
		for (size_t i = 0; i < singleyear60min.size(); i+=increment) {
			EXPECT_EQ(lifetime_from_single[idx*2], singleyear60min[i]);
			idx += increment;
		}
	}

}
TEST_F(libTimeTest_lib_time, single_year_to_lifetime_interpolated_SingleYear)
{
	is_lifetime = false;
	std::vector<float> lifetime_from_single;
	size_t n_rec_lifetime = singleyear60min.size();
	size_t n_rec_singleyear;
	double dt_hour;
	single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, n_rec_lifetime,
		singleyear60min, scaleFactors, interpolation_factor, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year);
	EXPECT_EQ(dt_hour, 1.0);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);


	for (size_t i = 0; i < n_rec_singleyear; i += increment) {
		EXPECT_EQ(lifetime_from_single[i], singleyear60min[i]);
	}
}

TEST_F(libTimeTest_lib_time, single_year_to_lifetime_with_escalation)
{
    is_lifetime = true;
    std::vector<float> lifetime_from_single;
    size_t n_rec_lifetime = util::hours_per_year * n_years;
    size_t n_rec_singleyear;
    std::vector<float> load_scale(n_years);
    for (size_t i = 0; i < n_years; i++)
    {
        load_scale[i] = pow((double)(1 + 2.5 * 0.01), (double)i);
    }
    double dt_hour;
    single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, n_rec_lifetime,
        singleyear60min, load_scale, interpolation_factor, lifetime_from_single, n_rec_singleyear, dt_hour);

    EXPECT_EQ(n_rec_lifetime, util::hours_per_year * n_years);
    EXPECT_EQ(n_rec_singleyear, util::hours_per_year);
    EXPECT_EQ(dt_hour, 1.0);
    EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);


    for (size_t i = 0; i < n_rec_singleyear; i += increment) {
        EXPECT_NEAR(lifetime_from_single[i + n_rec_singleyear], singleyear60min[i] * 1.025, 0.0001);
    }
}

TEST_F(libTimeTest_lib_time, single_year_to_lifetime_interpolated_SingleValue)
{
    is_lifetime = false;
    std::vector<float> lifetime_from_single;
    size_t n_rec_lifetime = 8760;
    size_t n_rec_singleyear;
    std::vector<float> single_val = {1.};
    double dt_hour;
    single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, n_rec_lifetime,
                                                single_val, scaleFactors, interpolation_factor, lifetime_from_single, n_rec_singleyear, dt_hour);

    EXPECT_EQ(n_rec_lifetime, util::hours_per_year);
    EXPECT_EQ(n_rec_singleyear, util::hours_per_year);
    EXPECT_EQ(dt_hour, 1.0);
    EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);


    for (size_t i = 0; i < n_rec_singleyear; i += increment) {
        EXPECT_EQ(lifetime_from_single[i], 1);
    }
}

TEST_F(libTimeTest_lib_time, single_year_to_lifetime_interpolated_SingleYearSubhourly)
{
	is_lifetime = false;
	std::vector<float> lifetime_from_single;
	size_t n_rec_lifetime = singleyear30min.size();
	size_t n_rec_singleyear;
	double dt_hour;
	single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, n_rec_lifetime,
		singleyear30min, scaleFactors, interpolation_factor, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year * 2);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year * 2);
	EXPECT_EQ(dt_hour, 0.5);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);

	for (size_t i = 0; i < n_rec_singleyear; i += increment) {
		EXPECT_EQ(lifetime_from_single[i], singleyear30min[i]);
	}
}

TEST_F(libTimeTest_lib_time, single_year_to_lifetime_interpolated_LifetimeSubhourly)
{
	is_lifetime = true;
	std::vector<float> lifetime_from_single;
	size_t n_rec_lifetime = lifetime30min.size();
	size_t n_rec_singleyear;
	double dt_hour;
	single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, n_rec_lifetime,
		singleyear30min, scaleFactors, interpolation_factor, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year * 2 * n_years);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year * 2);
	EXPECT_EQ(dt_hour, 0.5);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);

	for (size_t y = 0; y < n_years; y++) {
		size_t idx = y * singleyear30min.size();
		for (size_t i = 0; i < singleyear30min.size(); i += increment) {
			EXPECT_EQ(lifetime_from_single[idx], singleyear30min[i]);
			idx += increment;
		}
	}
}

// Test downsample
TEST_F(libTimeTest_lib_time, single_year_to_lifetime_interpolated_DownsampleLifetime)
{
	is_lifetime = true;
	std::vector<float> lifetime_from_single;
	size_t n_rec_lifetime = lifetime60min.size();
	size_t n_rec_singleyear;
	double dt_hour;
	single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, n_rec_lifetime,
		singleyear30min, scaleFactors, interpolation_factor, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year * n_years);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year);
	EXPECT_EQ(dt_hour, 1.0);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);

	for (size_t y = 0; y < n_years; y++) {
		size_t idx = y * singleyear60min.size();
		for (size_t i = 0; i < n_rec_singleyear; i += increment) {
			EXPECT_EQ(lifetime_from_single[idx], singleyear30min[i*2]);
			idx += increment;
		}
	}
}

// Test downsample
TEST_F(libTimeTest_lib_time, single_year_to_lifetime_interpolated_DownsampleLifetime_w_interpolation)
{
    is_lifetime = true;
    std::vector<float> lifetime_from_single;
    size_t n_rec_lifetime = lifetime60min.size();
    size_t n_rec_singleyear;
    double dt_hour;
    interpolation_factor = 1.0 / 2.0;
    single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, n_rec_lifetime,
        singleyear30min, scaleFactors, interpolation_factor, lifetime_from_single, n_rec_singleyear, dt_hour);

    EXPECT_EQ(n_rec_lifetime, util::hours_per_year * n_years);
    EXPECT_EQ(n_rec_singleyear, util::hours_per_year);
    EXPECT_EQ(dt_hour, 1.0);
    EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);

    for (size_t y = 0; y < n_years; y++) {
        size_t idx = y * singleyear60min.size();
        for (size_t i = 0; i < n_rec_singleyear; i += increment) {
            EXPECT_EQ(lifetime_from_single[idx], singleyear30min[i * 2] / interpolation_factor);
            idx += increment;
        }
    }
}

// Test downsample
TEST_F(libTimeTest_lib_time, single_year_to_lifetime_interpolated_DownsampleSingleYear)
{
	is_lifetime = false;
	std::vector<float> lifetime_from_single;
	size_t n_rec_lifetime = singleyear60min.size();
	size_t n_rec_singleyear;
	double dt_hour;
	single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, n_rec_lifetime,
		singleyear30min, scaleFactors, interpolation_factor, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year);
	EXPECT_EQ(dt_hour, 1.0);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);

	for (size_t i = 0; i < n_rec_singleyear; i+=increment) {
		EXPECT_EQ(lifetime_from_single[i], singleyear30min[i*2]);
	}
}

// Test diurnal to flat
TEST_F(libTimeTest_lib_time, flatten_diurnal_Schedule)
{
	std::vector<double> flat = flatten_diurnal(schedule, schedule, 1, sched_values, multiplier);
	std::vector<double> flat30min = flatten_diurnal(schedule, schedule, 2, sched_values, multiplier);

	EXPECT_EQ(flat.size(), util::hours_per_year);
	EXPECT_EQ(flat30min.size(), util::hours_per_year * 2);
	for (size_t h = 0; h < flat.size(); h++) {
		if (h % 24 > 11 && h % 24 < 19) {
			EXPECT_NEAR(flat[h], 0.6, 0.0001);
		}
		else {
			EXPECT_NEAR(flat[h], 0.2, 0.0001);
		}
	}
	size_t i = 0;
	for (size_t h = 0; h < util::hours_per_year; h++) {
		for (size_t s = 0; s < 2; s++) {
			if (h % 24 > 11 && h % 24 < 19) {
				EXPECT_NEAR(flat30min[i], 0.6, 0.0001);
			}
			else {
				EXPECT_NEAR(flat30min[i], 0.2, 0.0001);
			}
			i++;
		}
	}
}

// Test diurnal to flat
TEST_F(libTimeTest_lib_time, flatten_diurnal_ScheduleTOD)
{
    std::vector<double> flat = flatten_diurnal(schedule, schedule, 1, sched_values, multiplier);

    EXPECT_EQ(flat.size(), util::hours_per_year);
    for (size_t h = 0; h < flat.size(); h++) {
        if (h % 24 > 11 && h % 24 < 19) {
            EXPECT_NEAR(flat[h], 0.6, 0.0001);
        }
        else {
            EXPECT_NEAR(flat[h], 0.2, 0.0001);
        }
    }
}

TEST_F(libTimeTest_lib_time, TestDiurnalToFlat)
{
    std::vector<size_t> wk = {6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 6, 6, 6, 9, 9, 9, 9, 9, 9, 8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 8, 8, 8, 9, 9, 9, 9, 9, 9, 8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 8, 8, 8, 9, 9, 9, 9, 9, 9, 8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 8, 8, 8, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 3, 3, 3, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 6, 6, 6};
    std::vector<size_t> we = {6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 6, 6, 6, 9, 9, 9, 9, 9, 9, 8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 8, 8, 8, 9, 9, 9, 9, 9, 9, 8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 8, 8, 8, 9, 9, 9, 9, 9, 9, 8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 8, 8, 8, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 3, 3, 3, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 6, 6, 6};
    util::matrix_t<size_t> weekday(12, 24, &wk);
    util::matrix_t<size_t> weekend(12, 24, &we);
    std::vector<double> sched_values = { 2.2304, 0.8067, 0.9569, 1.1982, 0.7741, 0.9399, 1.1941, 0.6585, 0.9299};

    std::vector<double> flat = flatten_diurnal(weekday, weekend, 1, sched_values, 1.0);


    EXPECT_EQ(flat.size(), util::hours_per_year);
}
