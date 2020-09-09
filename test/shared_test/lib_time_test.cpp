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
		singleyear60min, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year * 2 * n_years);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year * 2);
	EXPECT_EQ(dt_hour, 0.5);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);

	for (size_t y = 0; y < n_years; y++) {
		size_t idx = y * singleyear60min.size();
		for (size_t i = 0; i < singleyear60min.size(); i+=increment) {
			EXPECT_EQ(lifetime_from_single[idx*2], singleyear60min[i]/2);
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
		singleyear60min, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year);
	EXPECT_EQ(dt_hour, 1.0);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);


	for (size_t i = 0; i < n_rec_singleyear; i += increment) {
		EXPECT_EQ(lifetime_from_single[i], singleyear60min[i]);
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
                                                single_val, lifetime_from_single, n_rec_singleyear, dt_hour);

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
		singleyear30min, lifetime_from_single, n_rec_singleyear, dt_hour);

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
		singleyear30min, lifetime_from_single, n_rec_singleyear, dt_hour);

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
		singleyear30min, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year * n_years);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year);
	EXPECT_EQ(dt_hour, 1.0);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);

	for (size_t y = 0; y < n_years; y++) {
		size_t idx = y * singleyear60min.size();
		for (size_t i = 0; i < n_rec_singleyear; i += increment) {
			EXPECT_EQ(lifetime_from_single[idx], singleyear30min[i*2] * 2);
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
		singleyear30min, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year);
	EXPECT_EQ(dt_hour, 1.0);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);

	for (size_t i = 0; i < n_rec_singleyear; i+=increment) {
		EXPECT_EQ(lifetime_from_single[i], singleyear30min[i*2]*2);
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
