#include <string>
#include <gtest/gtest.h>
#include <lib_util.h>
#include "sscapi.h"

#include "vartab.h"

TEST(libUtilTests, testFormat_lib_util)
{
	// test single input
	std::string str = "invalid number of data records (43): must be an integer multiple of 8760";
	ASSERT_EQ(util::format("invalid number of data records (%d): must be an integer multiple of 8760", 43), str);

	// test multiple inputs
	str = "query point (301.3, 10.4) is too far out of convex hull of data (dist=4.3)... estimating value from 5 parameter modele at (2.2, 2.1)=2.4";
	ASSERT_EQ(util::format("query point (%lg, %lg) is too far out of convex hull of data (dist=%lg)... estimating value from 5 parameter modele at (%lg, %lg)=%lg",
		301.3, 10.4, 4.3, 2.2, 2.1, 2.4), str);
}

TEST(sscapiTest, SSC_DATARR_test)
{
    // create data entries
    ssc_var_t vd[2];
    for (size_t i = 0; i < 2; i++){
        vd[i] = ssc_var_create();
        ssc_var_set_number(vd[i], 2 + i);
    }

    // set using ssc_data
    auto data = ssc_data_create();
    ssc_data_set_data_array(data, "array", &vd[0], 2);

    // get using ssc_data
    int n;
    ssc_var_t data_arr = ssc_data_get_data_array(data, "array", &n);

    ssc_var_size(data_arr, &n, nullptr);
    EXPECT_EQ(n, 2);
//    ssc_var_size(data_arr, &n, nullptr);
    for (int i = 0; i < n; i++){
        double var = ssc_var_get_number(ssc_var_get_var_array(data_arr, i));
        EXPECT_EQ(var, 2 + i);
    }

    for (size_t i = 0; i < 2; i++)
        ssc_var_free(vd[i]);
    ssc_data_free(data);
}

TEST(sscapiTest, SSC_DATMAT_test)
{
    // create data entries
    ssc_var_t vd[4];
    for (size_t i = 0; i < 4; i++){
        vd[i] = ssc_var_create();
        ssc_var_set_number(vd[i], 2 + i);
    }

    // set using ssc_data
    auto data = ssc_data_create();
    ssc_data_set_data_matrix(data, "matrix", &vd[0], 2, 2);

    // get using ssc_data
    int n, m;
    ssc_var_t data_mat = ssc_data_get_data_matrix(data, "matrix", &n, &m);

    ssc_var_size(data_mat, &n, &m);
    EXPECT_EQ(n, 2);
    EXPECT_EQ(m, 2);

    for (int i = 0; i < n; i++){
        for (int j = 0; j < m; j++){
            double var = ssc_var_get_number(ssc_var_get_var_matrix(data_mat, i, j));
            EXPECT_EQ(var, 2 + i * n + j);
        }
    }

    for (size_t i = 0; i < 4; i++)
        ssc_var_free(vd[i]);
    ssc_data_free(data);
}