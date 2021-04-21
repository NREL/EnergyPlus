#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestVenetianDirectionalShadeFlat0_2 : public testing::Test
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

        // Method
        DistributionMethod aDistribution = DistributionMethod::DirectionalDiffuse;

        // create BSDF
        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Full);

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

TEST_F(TestVenetianDirectionalShadeFlat0_2, TestVenetian1)
{
    SCOPED_TRACE("Begin Test: Venetian cell (Flat, 45 degrees slats) - solar properties.");

    std::shared_ptr<CBSDFLayer> aShade = GetShade();

    std::shared_ptr<CBSDFIntegrator> aResults = aShade->getResults();

    double tauDiff = aResults->DiffDiff(Side::Front, PropertySimple::T);
    EXPECT_NEAR(0.45408806110142574, tauDiff, 1e-6);

    double RfDiff = aResults->DiffDiff(Side::Front, PropertySimple::R);
    EXPECT_NEAR(0.27657763790935469, RfDiff, 1e-6);
}
