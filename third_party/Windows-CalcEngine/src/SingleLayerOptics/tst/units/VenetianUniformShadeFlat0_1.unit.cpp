#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

// Creation of BSDF matrix for single layer venetian shade with given material and geometrical
// properties. Method for calculation is diffuse part of distribution is uniform.
class TestVenetianUniformShadeFlat0_1 : public testing::Test
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
        const auto slatTiltAngle = 0;
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

TEST_F(TestVenetianUniformShadeFlat0_1, TestVenetian1)
{
    SCOPED_TRACE("Begin Test: Venetian cell (Flat, 45 degrees slats) - solar properties.");

    std::shared_ptr<CBSDFLayer> aShade = GetShade();

    std::shared_ptr<CBSDFIntegrator> aResults = aShade->getResults();

    const double tauDiff = aResults->DiffDiff(Side::Front, PropertySimple::T);
    EXPECT_NEAR(0.65093991496438897, tauDiff, 1e-6);

    const double RfDiff = aResults->DiffDiff(Side::Front, PropertySimple::R);
    EXPECT_NEAR(0.188319, RfDiff, 1e-6);

    auto aT = aResults->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix
    size_t size = aT.size();

    std::vector<double> correctResults;
    correctResults.push_back(13.007243);
    correctResults.push_back(14.019711);
    correctResults.push_back(10.824270);
    correctResults.push_back(9.5006750);
    correctResults.push_back(10.824270);
    correctResults.push_back(14.019711);
    correctResults.push_back(10.824270);
    correctResults.push_back(9.5006750);
    correctResults.push_back(10.824270);
    correctResults.push_back(12.996987);
    correctResults.push_back(8.3160770);
    correctResults.push_back(4.8894140);
    correctResults.push_back(3.6351680);
    correctResults.push_back(4.8894140);
    correctResults.push_back(8.3160770);
    correctResults.push_back(12.996987);
    correctResults.push_back(8.3160770);
    correctResults.push_back(4.8894140);
    correctResults.push_back(3.6351680);
    correctResults.push_back(4.8894140);
    correctResults.push_back(8.3160770);
    correctResults.push_back(12.996987);
    correctResults.push_back(4.1293450);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);
    correctResults.push_back(4.1293450);
    correctResults.push_back(12.996987);
    correctResults.push_back(4.1293450);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);
    correctResults.push_back(4.1293450);
    correctResults.push_back(12.355109);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);
    correctResults.push_back(12.355109);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-5);
    }

    // Front reflectance
    auto aRf = aResults->getMatrix(Side::Front, PropertySimple::R);

    correctResults.clear();

    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.025629);
    correctResults.push_back(0.036244);
    correctResults.push_back(0.025629);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.025629);
    correctResults.push_back(0.036244);
    correctResults.push_back(0.025629);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.040522);
    correctResults.push_back(0.070187);
    correctResults.push_back(0.081045);
    correctResults.push_back(0.070187);
    correctResults.push_back(0.040522);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.040522);
    correctResults.push_back(0.070187);
    correctResults.push_back(0.081045);
    correctResults.push_back(0.070187);
    correctResults.push_back(0.040522);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.076767);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.076767);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.076767);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.076767);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-5);
    }

    // Back transmittance
    auto aTb = aResults->getMatrix(Side::Back, PropertySimple::T);

    correctResults.clear();

    correctResults.push_back(13.007243);
    correctResults.push_back(14.019711);
    correctResults.push_back(10.824270);
    correctResults.push_back(9.5006750);
    correctResults.push_back(10.824270);
    correctResults.push_back(14.019711);
    correctResults.push_back(10.824270);
    correctResults.push_back(9.5006750);
    correctResults.push_back(10.824270);
    correctResults.push_back(12.996987);
    correctResults.push_back(8.3160770);
    correctResults.push_back(4.8894140);
    correctResults.push_back(3.6351680);
    correctResults.push_back(4.8894140);
    correctResults.push_back(8.3160770);
    correctResults.push_back(12.996987);
    correctResults.push_back(8.3160770);
    correctResults.push_back(4.8894140);
    correctResults.push_back(3.6351680);
    correctResults.push_back(4.8894140);
    correctResults.push_back(8.3160770);
    correctResults.push_back(12.996987);
    correctResults.push_back(4.1293450);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);
    correctResults.push_back(4.1293450);
    correctResults.push_back(12.996987);
    correctResults.push_back(4.1293450);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);
    correctResults.push_back(4.1293450);
    correctResults.push_back(12.355109);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);
    correctResults.push_back(12.355109);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);
    correctResults.push_back(0.1115490);

    EXPECT_EQ(correctResults.size(), aTb.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aTb(i, i), 1e-5);
    }

    // Back reflectance
    auto aRb = aResults->getMatrix(Side::Back, PropertySimple::R);

    correctResults.clear();

    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.025629);
    correctResults.push_back(0.036244);
    correctResults.push_back(0.025629);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.025629);
    correctResults.push_back(0.036244);
    correctResults.push_back(0.025629);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.040522);
    correctResults.push_back(0.070187);
    correctResults.push_back(0.081045);
    correctResults.push_back(0.070187);
    correctResults.push_back(0.040522);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.040522);
    correctResults.push_back(0.070187);
    correctResults.push_back(0.081045);
    correctResults.push_back(0.070187);
    correctResults.push_back(0.040522);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.076767);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.076767);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.076767);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.076767);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);
    correctResults.push_back(0.111549);

    EXPECT_EQ(correctResults.size(), aRb.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aRb(i, i), 1e-5);
    }
}
