#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestCircularPerforatedShadeNFRC18000 : public testing::Test
{
private:
    std::shared_ptr<CBSDFLayer> m_Shade;

protected:
    virtual void SetUp()
    {
        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Quarter);

        // create material
        const auto Tmat = 0.0;
        const auto Rfmat = 0.137;
        const auto Rbmat = 0.16;
        const auto minLambda = 5.0;
        const auto maxLambda = 100.0;
        const auto aMaterial =
          Material::singleBandMaterial(Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda);

        // make cell geometry
        const auto thickness_31111{0.00023};
        const auto x = 0.00169;        // m
        const auto y = 0.00169;        // m
        const auto radius = 0.00058;   // m

        // Perforated layer is created here
        m_Shade = CBSDFLayerMaker::getCircularPerforatedLayer(
          aMaterial, aBSDF, x, y, thickness_31111, radius);
    }

public:
    std::shared_ptr<CBSDFLayer> GetShade()
    {
        return m_Shade;
    };
};

TEST_F(TestCircularPerforatedShadeNFRC18000, TestSolarProperties)
{
    SCOPED_TRACE("Begin Test: Circular perforated cell - Solar properties.");

    std::shared_ptr<CBSDFLayer> aShade = GetShade();

    std::shared_ptr<CBSDFIntegrator> aResults = aShade->getResults();

    const double tauDiff = aResults->DiffDiff(Side::Front, PropertySimple::T);
    EXPECT_NEAR(0.257367, tauDiff, 1e-6);

    const double RfDiff = aResults->DiffDiff(Side::Front, PropertySimple::R);
    EXPECT_NEAR(0.101741, RfDiff, 1e-6);

    const double RbDiff = aResults->DiffDiff(Side::Back, PropertySimple::R);
    EXPECT_NEAR(0.118821, RbDiff, 1e-6);

    const double absfDiff = aResults->AbsDiffDiff(FenestrationCommon::Side::Front);
    EXPECT_NEAR(0.640892, absfDiff, 1e-6);

    const double absbDiff = aResults->AbsDiffDiff(FenestrationCommon::Side::Back);
    EXPECT_NEAR(0.623812, absbDiff, 1e-6);
}
