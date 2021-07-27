#include <memory>
#include <gtest/gtest.h>

#include <WCECommon.hpp>
#include "WCESpectralAveraging.hpp"

using SpectralAveraging::CPhotovoltaicSample;
using SpectralAveraging::CSpectralSampleData;
using SpectralAveraging::PhotovoltaicSampleData;
using FenestrationCommon::CSeries;

class TestPhotovoltaicSample : public testing::Test
{
private:
    std::unique_ptr<CPhotovoltaicSample> m_PhotovoltaicSample;

protected:
    CSeries getSolarRadiation() const
    {
        return CSeries({{0.3000, 0.0},
                        {0.3050, 3.4},
                        {0.3100, 15.6},
                        {0.3150, 41.1},
                        {0.3200, 71.2},
                        {0.3250, 100.2},
                        {0.3300, 152.4}});
    }

    std::shared_ptr<CSpectralSampleData> getMeasurements() const
    {
        auto aMeasurements = CSpectralSampleData::create({{0.300, 0.0064, 0.0478, 0.1521},
                                                          {0.305, 0.0138, 0.0479, 0.1462},
                                                          {0.310, 0.0243, 0.0487, 0.1536},
                                                          {0.315, 0.0373, 0.0546, 0.1713},
                                                          {0.320, 0.0475, 0.0617, 0.1883},
                                                          {0.325, 0.0554, 0.0709, 0.2067},
                                                          {0.330, 0.0610, 0.0827, 0.2249}});
        return aMeasurements;
    }

    void SetUp() override
    {
        std::vector<SpectralAveraging::PVMeasurementRow> pvMeasurements{
          {0.300, {0.00569, 0.83, 0.41}, {0.0, 0.81, 0.43}},
          {0.305, {0.036081, 0.83, 0.41}, {0.024239, 0.81, 0.43}},
          {0.310, {0.098415, 0.83, 0.41}, {0.031977, 0.81, 0.43}},
          {0.315, {0.140809, 0.83, 0.41}, {0.033776, 0.81, 0.43}},
          {0.320, {0.175428, 0.83, 0.41}, {0.036737, 0.81, 0.43}},
          {0.325, {0.220630, 0.83, 0.41}, {0.042653, 0.81, 0.43}},
          {0.330, {0.271745, 0.83, 0.41}, {0.053813, 0.81, 0.43}}};

        std::shared_ptr<PhotovoltaicSampleData> pvData =
          std::make_shared<PhotovoltaicSampleData>(*getMeasurements(), pvMeasurements);
        CSeries solarRadiation{getSolarRadiation()};
        m_PhotovoltaicSample = wce::make_unique<CPhotovoltaicSample>(pvData, solarRadiation);
    }

public:
    SpectralAveraging::CPhotovoltaicSample getSample()
    {
        return *m_PhotovoltaicSample;
    }
};

TEST_F(TestPhotovoltaicSample, TestPCEFront)
{
    SCOPED_TRACE("Test photovoltaic sample pce front.");

    auto sample = getSample();
    auto pce{sample.pce(FenestrationCommon::Side::Front)};

    std::vector<double> correctPCE{
      0.000439235, 0.002831664, 0.007850301, 0.011413117, 0.014444822, 0.018450631, 0.023074846};

    EXPECT_EQ(pce.size(), correctPCE.size());

    for(auto i = 0u; i < pce.size(); ++i)
    {
        EXPECT_NEAR(pce[i].value(), correctPCE[i], 1e-6);
    }
}

TEST_F(TestPhotovoltaicSample, TestPCEBack)
{
    SCOPED_TRACE("Test photovoltaic sample pce back.");

    auto sample = getSample();
    auto pce{sample.pce(FenestrationCommon::Side::Back)};

    std::vector<double> correctPCE{
      0.0, 0.001946942, 0.002610585, 0.002801929, 0.003095937, 0.00365066, 0.004676701};

    EXPECT_EQ(pce.size(), correctPCE.size());

    for(auto i = 0u; i < pce.size(); ++i)
    {
        EXPECT_NEAR(pce[i].value(), correctPCE[i], 1e-6);
    }
}

TEST_F(TestPhotovoltaicSample, TestWFront)
{
    SCOPED_TRACE("Test photovoltaic sample w front.");

    auto sample = getSample();
    auto w{sample.w(FenestrationCommon::Side::Front)};

    std::vector<double> correctW{
      0.999535612, 0.996982248, 0.991531819, 0.987432347, 0.983785051, 0.978882987, 0.973053866};

    EXPECT_EQ(w.size(), correctW.size());

    for(auto i = 0u; i < w.size(); ++i)
    {
        EXPECT_NEAR(w[i].value(), correctW[i], 1e-6);
    }
}

TEST_F(TestPhotovoltaicSample, TestWBack)
{
    SCOPED_TRACE("Test photovoltaic sample w back.");

    auto sample = getSample();
    auto w{sample.w(FenestrationCommon::Side::Back)};

    std::vector<double> correctW{
      1, 0.997682212, 0.996824492, 0.996459528, 0.995948787, 0.995052636, 0.993450916};

    EXPECT_EQ(w.size(), correctW.size());

    for(auto i = 0u; i < w.size(); ++i)
    {
        EXPECT_NEAR(w[i].value(), correctW[i], 1e-6);
    }
}