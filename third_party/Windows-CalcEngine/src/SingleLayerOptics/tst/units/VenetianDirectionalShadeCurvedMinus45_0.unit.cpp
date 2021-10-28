#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestVenetianDirectionalShadeCurvedMinus45_0 : public testing::Test
{
private:
    std::shared_ptr<CBSDFLayer> m_Shade;

protected:
    virtual void SetUp()
    {
        // create material
        const auto Tmat = 0.0;
        const auto Rfmat = 0.95;
        const auto Rbmat = 0.95;
        const auto minLambda = 0.3;
        const auto maxLambda = 2.5;
        const auto aMaterial =
          Material::singleBandMaterial(Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda);


        // make cell geometry
        const auto slatWidth = 0.025400;     // m
        const auto slatSpacing = 0.019000;   // m
        const auto slatTiltAngle = -45;
        const auto curvatureRadius = 0.041322;
        const size_t numOfSlatSegments = 5;

        // Method
        DistributionMethod aDistribution = DistributionMethod::DirectionalDiffuse;

        // create BSDF
        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Quarter);

        // make layer
        m_Shade = CBSDFLayerMaker::getVenetianLayer(aMaterial,
                                                    aBSDF,
                                                    slatWidth,
                                                    slatSpacing,
                                                    slatTiltAngle,
                                                    curvatureRadius,
                                                    numOfSlatSegments,
                                                    aDistribution);
    }

public:
    std::shared_ptr<CBSDFLayer> GetShade()
    {
        return m_Shade;
    };
};

TEST_F(TestVenetianDirectionalShadeCurvedMinus45_0, TestVenetian1)
{
    SCOPED_TRACE("Begin Test: Venetian shade (Curved, -45 degrees slats).");

    std::shared_ptr<CBSDFLayer> aShade = GetShade();

    std::shared_ptr<CBSDFIntegrator> aResults = aShade->getResults();

    double tauDiff = aResults->DiffDiff(Side::Front, PropertySimple::T);
    EXPECT_NEAR(0.382240, tauDiff, 1e-6);

    double RfDiff = aResults->DiffDiff(Side::Front, PropertySimple::R);
    EXPECT_NEAR(0.541774, RfDiff, 1e-6);
}
