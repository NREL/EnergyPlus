#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestVenetianUniformShadeCurvedZeroAngle_0 : public testing::Test
{
private:
    std::shared_ptr<CBSDFLayer> m_Shade;

protected:
    virtual void SetUp()
    {
        // create material
        const auto Tmat = 0.0;
        const auto Rfmat = 0.1;
        const auto Rbmat = 0.1;
        const auto minLambda = 0.3;
        const auto maxLambda = 2.5;
        const auto aMaterial =
          Material::singleBandMaterial(Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda);


        // make cell geometry
        const auto slatWidth = 0.0148;     // m
        const auto slatSpacing = 0.0127;   // m
        const auto slatTiltAngle = 0;
        const auto curvatureRadius = 0.03313057;
        const size_t numOfSlatSegments = 5;

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
                                                    DistributionMethod::UniformDiffuse);
    }

public:
    std::shared_ptr<CBSDFLayer> GetShade()
    {
        return m_Shade;
    };
};

TEST_F(TestVenetianUniformShadeCurvedZeroAngle_0, TestVenetian1)
{
    SCOPED_TRACE("Begin Test: Venetian shade (Curved, -45 degrees slats).");

    std::shared_ptr<CBSDFLayer> aShade = GetShade();

    std::shared_ptr<CBSDFIntegrator> aResults = aShade->getResults();

    double tauDiff = aResults->DiffDiff(Side::Front, PropertySimple::T);
    EXPECT_NEAR(0.422932, tauDiff, 1e-6);

    double RfDiff = aResults->DiffDiff(Side::Front, PropertySimple::R);
    EXPECT_NEAR(0.020573, RfDiff, 1e-6);

    auto theta{0.0};
    auto phi{0.0};
    double tauDir = aResults->DirDir(Side::Front, PropertySimple::T, theta, phi);
    EXPECT_NEAR(0.936759, tauDir, 1e-6);

    double rhoDir = aResults->DirDir(Side::Front, PropertySimple::R, theta, phi);
    EXPECT_NEAR(7.583e-05, rhoDir, 1e-6);

    double absIR = aResults->Abs(Side::Front, theta, phi);
    EXPECT_NEAR(0.059455, absIR, 1e-6);
}
