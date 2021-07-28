#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestVenetianUniformShadeFlat45_1 : public testing::Test
{
private:
    std::shared_ptr<CBSDFLayer> m_Shade;

protected:
    virtual void SetUp()
    {
        // create material
        const auto Tmat = 0.1;
        const auto Rfmat = 0.7;
        const auto Rbmat = 0.7;
        const auto minLambda = 0.3;
        const auto maxLambda = 2.5;
        const auto aMaterial =
          Material::singleBandMaterial(Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda);

        // make cell geometry
        const auto slatWidth = 0.010;     // m
        const auto slatSpacing = 0.010;   // m
        const auto slatTiltAngle = 45;
        const auto curvatureRadius = 0;
        const size_t numOfSlatSegments = 1;

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

TEST_F(TestVenetianUniformShadeFlat45_1, TestVenetian1)
{
    SCOPED_TRACE("Begin Test: Venetian cell (Flat, 45 degrees slats) - solar properties.");

    std::shared_ptr<CBSDFLayer> aShade = GetShade();

    std::shared_ptr<CBSDFIntegrator> aResults = aShade->getResults();

    const double tauDiff = aResults->DiffDiff(Side::Front, PropertySimple::T);
    EXPECT_NEAR(0.47624006362615717, tauDiff, 1e-6);

    const double RfDiff = aResults->DiffDiff(Side::Front, PropertySimple::R);
    EXPECT_NEAR(0.33488359240717491, RfDiff, 1e-6);

    auto aT = aResults->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix
    size_t size = aT.size();

    std::vector<double> correctResults;
    correctResults.push_back(3.861585);
    correctResults.push_back(4.158130);
    correctResults.push_back(6.423857);
    correctResults.push_back(7.362352);
    correctResults.push_back(6.423857);
    correctResults.push_back(4.158130);
    correctResults.push_back(1.892403);
    correctResults.push_back(0.953908);
    correctResults.push_back(1.892403);
    correctResults.push_back(3.858581);
    correctResults.push_back(7.178301);
    correctResults.push_back(9.608505);
    correctResults.push_back(10.49802);
    correctResults.push_back(9.608505);
    correctResults.push_back(7.178301);
    correctResults.push_back(3.858581);
    correctResults.push_back(0.538861);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.538861);
    correctResults.push_back(3.858581);
    correctResults.push_back(10.14755);
    correctResults.push_back(11.25443);
    correctResults.push_back(9.580691);
    correctResults.push_back(11.25443);
    correctResults.push_back(10.14755);
    correctResults.push_back(3.858581);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(3.670579);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(3.670579);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-5);
    }

    // Front reflectance
    auto aRf = aResults->getMatrix(Side::Front, PropertySimple::R);

    correctResults.clear();

    correctResults.push_back(0.113584);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.087488);
    correctResults.push_back(0.076678);
    correctResults.push_back(0.087488);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.139680);
    correctResults.push_back(0.150490);
    correctResults.push_back(0.139680);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.072322);
    correctResults.push_back(0.042116);
    correctResults.push_back(0.031060);
    correctResults.push_back(0.042116);
    correctResults.push_back(0.072322);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.154846);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.154846);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.035417);
    correctResults.push_back(0.009955);
    correctResults.push_back(0.019516);
    correctResults.push_back(0.009955);
    correctResults.push_back(0.035417);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-5);
    }

    // Back transmittance
    auto aTb = aResults->getMatrix(Side::Back, PropertySimple::T);

    correctResults.clear();

    correctResults.push_back(3.861585);
    correctResults.push_back(4.158130);
    correctResults.push_back(1.892403);
    correctResults.push_back(0.953908);
    correctResults.push_back(1.892403);
    correctResults.push_back(4.158130);
    correctResults.push_back(6.423857);
    correctResults.push_back(7.362352);
    correctResults.push_back(6.423857);
    correctResults.push_back(3.858581);
    correctResults.push_back(0.538861);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.538861);
    correctResults.push_back(3.858581);
    correctResults.push_back(7.178301);
    correctResults.push_back(9.608505);
    correctResults.push_back(10.49802);
    correctResults.push_back(9.608505);
    correctResults.push_back(7.178301);
    correctResults.push_back(3.858581);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(3.858581);
    correctResults.push_back(10.14755);
    correctResults.push_back(11.25443);
    correctResults.push_back(9.580691);
    correctResults.push_back(11.25443);
    correctResults.push_back(10.14755);
    correctResults.push_back(3.670579);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(3.670579);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);

    EXPECT_EQ(correctResults.size(), aTb.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aTb(i, i), 1e-5);
    }

    // Back reflectance
    auto aRb = aResults->getMatrix(Side::Back, PropertySimple::R);

    correctResults.clear();

    correctResults.push_back(0.113584);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.139680);
    correctResults.push_back(0.150490);
    correctResults.push_back(0.139680);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.087488);
    correctResults.push_back(0.076678);
    correctResults.push_back(0.087488);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.154846);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.154846);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.072322);
    correctResults.push_back(0.042116);
    correctResults.push_back(0.031060);
    correctResults.push_back(0.042116);
    correctResults.push_back(0.072322);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.035417);
    correctResults.push_back(0.009955);
    correctResults.push_back(0.019516);
    correctResults.push_back(0.009955);
    correctResults.push_back(0.035417);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.160632);
    correctResults.push_back(0.113584);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);
    correctResults.push_back(0.073329);

    EXPECT_EQ(correctResults.size(), aRb.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aRb(i, i), 1e-5);
    }
}
