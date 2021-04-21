#include <memory>
#include <gtest/gtest.h>

#include <memory>

#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;

class TestPhiLimits2 : public testing::Test
{
private:
    CPhiLimits m_PhiLimits{ 1 };

public:
    const CPhiLimits & GetLimits() const
    {
        return m_PhiLimits;
    };
};

TEST_F(TestPhiLimits2, TestBSDFRingCreation)
{
    SCOPED_TRACE("Begin Test: BSDF Phi limits creation.");

    const auto aLimits = GetLimits();

    const auto & results = aLimits.getPhiLimits();

    std::vector<double> correctResults = {0, 360};
    EXPECT_EQ(results.size(), correctResults.size());

    for(size_t i = 0; i < results.size(); ++i)
    {
        EXPECT_NEAR(results[i], correctResults[i], 1e-6);
    }
}
