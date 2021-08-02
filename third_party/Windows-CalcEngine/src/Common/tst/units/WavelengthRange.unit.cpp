#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"


using namespace FenestrationCommon;

class WavelengthRangeTest : public testing::Test
{
protected:
    void SetUp() override
    {}
};

TEST_F(WavelengthRangeTest, Test1)
{
    SCOPED_TRACE("Begin Test: Creation of IR range.");

    CWavelengthRange aRange = CWavelengthRange(WavelengthRange::IR);

    EXPECT_NEAR(5.0, aRange.minLambda(), 1e-6);
    EXPECT_NEAR(100.0, aRange.maxLambda(), 1e-6);
}

TEST_F(WavelengthRangeTest, Test2)
{
    SCOPED_TRACE("Begin Test: Creation of Solar range.");

    CWavelengthRange aRange = CWavelengthRange(WavelengthRange::Solar);

    EXPECT_NEAR(0.3, aRange.minLambda(), 1e-6);
    EXPECT_NEAR(2.5, aRange.maxLambda(), 1e-6);
}

TEST_F(WavelengthRangeTest, Test3)
{
    SCOPED_TRACE("Begin Test: Creation of Visible range.");

    CWavelengthRange aRange = CWavelengthRange(WavelengthRange::Visible);

    EXPECT_NEAR(0.38, aRange.minLambda(), 1e-6);
    EXPECT_NEAR(0.78, aRange.maxLambda(), 1e-6);
}

TEST_F(WavelengthRangeTest, TestCondensedSpectrum)
{
    SCOPED_TRACE("Begin Test: Creation of condensed spectrum range.");

    const size_t numOfVisibleBands{5u};
    const size_t numOfIRBands{10u};

    const auto condensendSpectrum =
      FenestrationCommon::generateSpectrum(numOfVisibleBands, numOfIRBands);

    std::vector<double> correctSpectrum{0.3,
                                        0.38,
                                        0.46,
                                        0.54,
                                        0.62,
                                        0.7,
                                        0.78,
                                        0.952,
                                        1.124,
                                        1.296,
                                        1.468,
                                        1.64,
                                        1.812,
                                        1.984,
                                        2.156,
                                        2.328,
                                        2.5};

    EXPECT_EQ(condensendSpectrum.size(), correctSpectrum.size());
    for(size_t i = 0u; i < condensendSpectrum.size(); ++i)
    {
        EXPECT_NEAR(condensendSpectrum[i], correctSpectrum[i], 1e-6);
    }
}

TEST_F(WavelengthRangeTest, TestISO9050Spectrum)
{
    const auto wavelenghts{FenestrationCommon::generateISO9050Wavelengths()};

    std::vector<double> correctWavelengths{
      0.300,  0.305,  0.310,  0.315,  0.320,  0.325,  0.330,  0.335,  0.340,  0.345,  0.350,
      0.355,  0.360,  0.365,  0.370,  0.375,  0.380,  0.385,  0.390,  0.395,  0.400,  0.410,
      0.420,  0.430,  0.440,  0.450,  0.460,  0.470,  0.480,  0.490,  0.500,  0.510,  0.520,
      0.530,  0.540,  0.550,  0.560,  0.570,  0.580,  0.590,  0.600,  0.610,  0.620,  0.630,
      0.640,  0.650,  0.660,  0.670,  0.680,  0.690,  0.700,  0.710,  0.720,  0.730,  0.740,
      0.750,  0.760,  0.770,  0.780,  0.790,  0.800,  0.850,  0.900,  0.950,  1.000,  1.050,
      1.100,  1.150,  1.200,  1.250,  1.300,  1.350,  1.400,  1.450,  1.500,  1.550,  1.600,
      1.650,  1.700,  1.750,  1.800,  1.850,  1.900,  1.950,  2.000,  2.050,  2.100,  2.150,
      2.200,  2.250,  2.300,  2.350,  2.400,  2.450,  2.500,  5.000,  6.000,  7.000,  8.000,
      9.000,  10.000, 11.000, 12.000, 13.000, 14.000, 15.000, 16.000, 17.000, 18.000, 19.000,
      20.000, 21.000, 22.000, 23.000, 24.000, 25.000, 26.000, 27.000, 28.000, 29.000, 30.000,
      31.000, 32.000, 33.000, 34.000, 35.000, 36.000, 37.000, 38.000, 39.000, 40.000};

    EXPECT_EQ(wavelenghts.size(), correctWavelengths.size());
    for(size_t i = 0u; i < wavelenghts.size(); ++i)
    {
        EXPECT_NEAR(wavelenghts[i], correctWavelengths[i], 1e-6);
    }
}
