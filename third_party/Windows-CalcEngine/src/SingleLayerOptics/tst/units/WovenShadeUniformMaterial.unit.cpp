#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestWovenShadeUniformMaterial : public testing::Test
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
        const auto minLambda = 5.0;
        const auto maxLambda = 40.0;
        const auto aMaterial = SingleLayerOptics::Material::singleBandMaterial(
          Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda);

        // make cell geometry
        const auto diameter = 0.002;   // m
        const auto spacing = 0.003;    // m

        // create BSDF
        const auto aBSDF{CBSDFHemisphere::create(BSDFBasis::Quarter)};

        // make layer
        m_Shade = CBSDFLayerMaker::getWovenLayer(aMaterial, aBSDF, diameter, spacing);
    }

public:
    std::shared_ptr<CBSDFLayer> GetShade()
    {
        return m_Shade;
    };
};

TEST_F(TestWovenShadeUniformMaterial, TestSolarProperties)
{
    SCOPED_TRACE("Begin Test: Woven shade uniform material.");

    std::shared_ptr<CBSDFLayer> aShade = GetShade();

    std::shared_ptr<CBSDFIntegrator> aResults = aShade->getResults();

    const double tauDiff = aResults->DiffDiff(Side::Front, PropertySimple::T);
    EXPECT_NEAR(0.037033896815761802, tauDiff, 1e-6);

    const double RfDiff = aResults->DiffDiff(Side::Front, PropertySimple::R);
    EXPECT_NEAR(0.096296610318422418, RfDiff, 1e-6);

    const double RbDiff = aResults->DiffDiff(Side::Back, PropertySimple::R);
    EXPECT_NEAR(0.096296610318422418, RbDiff, 1e-6);

    const double theta{0.0};
    const double phi{0.0};

    const double Emiss{aResults->Abs(Side::Front, theta, phi)};
    EXPECT_NEAR(0.8, Emiss, 1e-6);
}
