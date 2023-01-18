#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestRectangularPerforatedShade2 : public testing::Test
{
private:
    std::shared_ptr<CBSDFLayer> m_Shade;

protected:
    virtual void SetUp()
    {
        // make material
        const auto Tmat = 0.1;
        const auto Rfmat = 0.5;
        const auto Rbmat = 0.6;
        const auto minLambda = 0.3;
        const auto maxLambda = 2.5;
        const auto aMaterial =
          Material::singleBandMaterial(Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda);

        // make cell geometry
        const auto x = 20.0;          // mm
        const auto y = 25.0;          // mm
        const auto thickness = 7.0;   // mm
        const auto xHole = 5.0;       // mm
        const auto yHole = 8.0;       // mm

        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Quarter);

        m_Shade = CBSDFLayerMaker::getRectangularPerforatedLayer(
          aMaterial, aBSDF, x, y, thickness, xHole, yHole);
    }

public:
    std::shared_ptr<CBSDFLayer> GetShade()
    {
        return m_Shade;
    };
};

TEST_F(TestRectangularPerforatedShade2, TestSolarProperties)
{
    SCOPED_TRACE("Begin Test: Rectangular perforated cell - Solar properties.");

    std::shared_ptr<CBSDFLayer> aShade = GetShade();

    std::shared_ptr<CBSDFIntegrator> aResults = aShade->getResults();

    const double tauDiff = aResults->DiffDiff(Side::Front, PropertySimple::T);
    EXPECT_NEAR(0.112843786, tauDiff, 1e-6);

    const double RfDiff = aResults->DiffDiff(Side::Front, PropertySimple::R);
    EXPECT_NEAR(0.492864523, RfDiff, 1e-6);

    const double RbDiff = aResults->DiffDiff(Side::Back, PropertySimple::R);
    EXPECT_NEAR(0.591437306, RbDiff, 1e-6);

    auto aT = aResults->getMatrix(Side::Front, PropertySimple::T);

    const size_t size = aT.size();

    // Test diagonal
    std::vector<double> correctResults{
      1.069864, 0.641828, 0.638318, 0.832716, 0.638318, 0.641828, 0.638318, 0.832716, 0.638318,
      0.031831, 0.116107, 0.260917, 0.409662, 0.260917, 0.116107, 0.031831, 0.116107, 0.260917,
      0.409662, 0.260917, 0.116107, 0.031831, 0.031831, 0.031831, 0.031831, 0.031831, 0.031831,
      0.031831, 0.031831, 0.031831, 0.031831, 0.031831, 0.031831, 0.031831, 0.031831, 0.031831,
      0.031831, 0.031831, 0.031831, 0.031831, 0.031831};


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
    correctResults = {1.069864, 0.030443, 0.030451, 0.030008, 0.030451, 0.030443, 0.030451,
                      0.030008, 0.030451, 0.031831, 0.031624, 0.031269, 0.030903, 0.031269,
                      0.031624, 0.031831, 0.031624, 0.031269, 0.030903, 0.031269, 0.031624,
                      0.031831, 0.031831, 0.031831, 0.031831, 0.031831, 0.031831, 0.031831,
                      0.031831, 0.031831, 0.031831, 0.031831, 0.031831, 0.031831, 0.031831,
                      0.031831, 0.031831, 0.031831, 0.031831, 0.031831, 0.031831};


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

    correctResults = {0.146423, 0.152214, 0.152254, 0.150042, 0.152254, 0.152214, 0.152254,
                      0.150042, 0.152254, 0.159155, 0.158120, 0.156343, 0.154517, 0.156343,
                      0.158120, 0.159155, 0.158120, 0.156343, 0.154517, 0.156343, 0.158120,
                      0.159155, 0.159155, 0.159155, 0.159155, 0.159155, 0.159155, 0.159155,
                      0.159155, 0.159155, 0.159155, 0.159155, 0.159155, 0.159155, 0.159155,
                      0.159155, 0.159155, 0.159155, 0.159155, 0.159155, 0.159155};

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
}
