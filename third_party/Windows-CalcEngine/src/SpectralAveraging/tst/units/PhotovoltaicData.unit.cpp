#include <memory>
#include <gtest/gtest.h>

#include "WCESpectralAveraging.hpp"

class TestPhotovoltaicData : public testing::Test
{
private:
protected:
    void SetUp() override
    {}
};

TEST_F(TestPhotovoltaicData, TestDifferentSizeOfInputData)
{
    SCOPED_TRACE("Test incorrect input data for photovoltaic.");

    SpectralAveraging::CSpectralSampleData sampleData({{0.300, 0.0020, 0.0470, 0.0480},
                                                       {0.305, 0.0030, 0.0470, 0.0480},
                                                       {0.310, 0.0090, 0.0470, 0.0480},
                                                       {0.315, 0.0350, 0.0470, 0.0480},
                                                       {0.320, 0.1000, 0.0470, 0.0480},
                                                       {0.325, 0.2180, 0.0490, 0.0500},
                                                       {0.330, 0.3560, 0.0530, 0.0540}});

    try
    {
        SpectralAveraging::PhotovoltaicSampleData pvData{
          sampleData,
          {{0.300, {0.00569, 0.83, 0.41}, {0.0, 0.83, 0.41}},
           {0.305, {0.036081, 0.83, 0.41}, {0.024239, 0.83, 0.41}},
           {0.310, {0.098415, 0.83, 0.41}, {0.031977, 0.83, 0.41}},
           {0.315, {0.140809, 0.83, 0.41}, {0.033776, 0.83, 0.41}},
           {0.320, {0.175428, 0.83, 0.41}, {0.036737, 0.83, 0.41}},
           {0.325, {0.220630, 0.83, 0.41}, {0.042653, 0.83, 0.41}}}};
    }
    catch(const std::exception & err)
    {
        EXPECT_EQ(
          err.what(),
          std::string(
            "Photovoltaic measurements do not have same amount of data as specular measurements."));
    }
}

TEST_F(TestPhotovoltaicData, TestDifferentWavelengthsOfInputData)
{
    SCOPED_TRACE("Test inconsistent wavelengths.");

    SpectralAveraging::CSpectralSampleData sampleData({{0.300, 0.0020, 0.0470, 0.0480},
                                                       {0.305, 0.0030, 0.0470, 0.0480},
                                                       {0.310, 0.0090, 0.0470, 0.0480},
                                                       {0.315, 0.0350, 0.0470, 0.0480},
                                                       {0.320, 0.1000, 0.0470, 0.0480},
                                                       {0.325, 0.2180, 0.0490, 0.0500},
                                                       {0.330, 0.3560, 0.0530, 0.0540}});

    try
    {
        SpectralAveraging::PhotovoltaicSampleData pvData{
          sampleData,
          {{0.300, {0.00569, 0.83, 0.41}, {0.0, 0.83, 0.41}},
           {0.305, {0.036081, 0.83, 0.41}, {0.024239, 0.83, 0.41}},
           {0.310, {0.098415, 0.83, 0.41}, {0.031977, 0.83, 0.41}},
           {0.315, {0.140809, 0.83, 0.41}, {0.033776, 0.83, 0.41}},
           {0.320, {0.175428, 0.83, 0.41}, {0.036737, 0.83, 0.41}},
           {0.325, {0.220630, 0.83, 0.41}, {0.042653, 0.83, 0.41}},
           {0.335, {0.271745, 0.83, 0.41}, {0.053813, 0.83, 0.41}}}};
    }
    catch(const std::exception & err)
    {
        EXPECT_EQ(
          err.what(),
          std::string("Wavelengths in spectral and photovoltaic measurements do not match."));
    }
}
