#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestCircularPerforatedShade2 : public testing::Test
{
private:
    std::shared_ptr<CBSDFLayer> m_Shade;

protected:
    virtual void SetUp()
    {
        // create material
        const auto Tmat = 0.2;
        const auto Rfmat = 0.8;
        const auto Rbmat = 0.8;
        const auto minLambda = 0.3;
        const auto maxLambda = 2.5;
        const auto aMaterial =
          Material::singleBandMaterial(Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda);

        // make cell geometry
        const auto x = 0.0225;           // m
        const auto y = 0.0381;           // m
        const auto thickness = 0.0050;   // m
        const auto radius = 0.0;         // m

        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Quarter);

        // make layer
        m_Shade =
          CBSDFLayerMaker::getCircularPerforatedLayer(aMaterial, aBSDF, x, y, thickness, radius);
    }

public:
    std::shared_ptr<CBSDFLayer> GetShade()
    {
        return m_Shade;
    };
};

TEST_F(TestCircularPerforatedShade2, TestSolarProperties)
{
    SCOPED_TRACE("Begin Test: Circular perforated cell - Solar properties.");

    std::shared_ptr<CBSDFLayer> aShade = GetShade();

    std::shared_ptr<CBSDFIntegrator> aResults = aShade->getResults();

    const double tauDiff = aResults->DiffDiff(Side::Front, PropertySimple::T);
    EXPECT_NEAR(0.2, tauDiff, 1e-6);

    const double RfDiff = aResults->DiffDiff(Side::Front, PropertySimple::R);
    EXPECT_NEAR(0.8, RfDiff, 1e-6);

    const double RbDiff = aResults->DiffDiff(Side::Back, PropertySimple::R);
    EXPECT_NEAR(0.8, RbDiff, 1e-6);

    auto aT = aResults->getMatrix(Side::Front, PropertySimple::T);

    const auto size = aT.size();

    // Test diagonal
    std::vector<double> correctResults{
      0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662,
      0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662,
      0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662,
      0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662,
      0.063662, 0.063662, 0.063662, 0.063662, 0.063662};


    std::vector<double> calculatedResults;
    for(size_t i = 0; i < size; ++i)
    {
        calculatedResults.push_back(aT(i, i));
    }

    EXPECT_EQ(correctResults.size(), calculatedResults.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], calculatedResults[i], 1e-5);
    }

    // Test first row
    correctResults = {0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662,
                      0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662,
                      0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662,
                      0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662,
                      0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662,
                      0.063662, 0.063662, 0.063662, 0.063662, 0.063662, 0.063662};

    calculatedResults.clear();
    for(size_t i = 0; i < size; ++i)
    {
        calculatedResults.push_back(aT(0, i));
    }

    EXPECT_EQ(correctResults.size(), calculatedResults.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], calculatedResults[i], 1e-5);
    }

    // Test first row for reflectance matrix
    auto aRf = aResults->getMatrix(Side::Front, PropertySimple::R);

    correctResults = {0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648,
                      0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648,
                      0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648,
                      0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648,
                      0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648,
                      0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648};

    calculatedResults.clear();
    for(size_t i = 0; i < size; ++i)
    {
        calculatedResults.push_back(aRf(0, i));
    }

    EXPECT_EQ(correctResults.size(), calculatedResults.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], calculatedResults[i], 1e-5);
    }

    // Test first row for reflectance matrix
    auto aRb = aResults->getMatrix(Side::Back, PropertySimple::R);

    correctResults = {0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648,
                      0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648,
                      0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648,
                      0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648,
                      0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648,
                      0.254648, 0.254648, 0.254648, 0.254648, 0.254648, 0.254648};

    calculatedResults.clear();
    for(size_t i = 0; i < size; ++i)
    {
        calculatedResults.push_back(aRb(0, i));
    }

    EXPECT_EQ(correctResults.size(), calculatedResults.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], calculatedResults[i], 1e-5);
    }
}
