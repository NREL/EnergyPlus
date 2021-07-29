#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestRectangularPerforatedShade1 : public testing::Test
{
private:
    std::shared_ptr<CBSDFLayer> m_Shade;

protected:
    virtual void SetUp()
    {
        // create material
        const auto Tmat = 0.0;
        const auto Rfmat = 0.7;
        const auto Rbmat = 0.7;
        const auto minLambda = 0.3;
        const auto maxLambda = 2.5;
        const auto aMaterial =
          Material::singleBandMaterial(Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda);

        // make cell geometry
        const auto x = 19.05;         // mm
        const auto y = 19.05;         // mm
        const auto thickness = 0.6;   // mm
        const auto xHole = 3.175;     // mm
        const auto yHole = 6.35;      // mm

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

TEST_F(TestRectangularPerforatedShade1, TestSolarProperties)
{
    SCOPED_TRACE("Begin Test: Rectangular perforated cell - Solar properties.");

    std::shared_ptr<CBSDFLayer> aShade = GetShade();

    std::shared_ptr<CBSDFIntegrator> aResults = aShade->getResults();

    const double tauDiff = aResults->DiffDiff(Side::Front, PropertySimple::T);
    EXPECT_NEAR(0.041876313, tauDiff, 1e-6);

    const double RfDiff = aResults->DiffDiff(Side::Front, PropertySimple::R);
    EXPECT_NEAR(0.670686365, RfDiff, 1e-6);

    const auto theta{0};
    const auto phi{0};

    const auto tauDirHem{aResults->DirHem(Side::Front, PropertySimple::T, theta, phi)};
    EXPECT_NEAR(0.055556, tauDirHem, 1e-6);

    const double RfDirHem{aResults->DirHem(Side::Front, PropertySimple::R, theta, phi)};
    EXPECT_NEAR(0.661111, RfDirHem, 1e-6);

    auto aT = aResults->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix
    const size_t size = aT.size();

    std::vector<double> correctResults;
    correctResults.push_back(0.722625);
    correctResults.push_back(0.731048);
    correctResults.push_back(0.728881);
    correctResults.push_back(0.754961);
    correctResults.push_back(0.728881);
    correctResults.push_back(0.731048);
    correctResults.push_back(0.728881);
    correctResults.push_back(0.754961);
    correctResults.push_back(0.728881);
    correctResults.push_back(0.622917);
    correctResults.push_back(0.614362);
    correctResults.push_back(0.632505);
    correctResults.push_back(0.672486);
    correctResults.push_back(0.632505);
    correctResults.push_back(0.614362);
    correctResults.push_back(0.622917);
    correctResults.push_back(0.614362);
    correctResults.push_back(0.632505);
    correctResults.push_back(0.672486);
    correctResults.push_back(0.632505);
    correctResults.push_back(0.614362);
    correctResults.push_back(0.534246);
    correctResults.push_back(0.523031);
    correctResults.push_back(0.557403);
    correctResults.push_back(0.628150);
    correctResults.push_back(0.557403);
    correctResults.push_back(0.523031);
    correctResults.push_back(0.534246);
    correctResults.push_back(0.523031);
    correctResults.push_back(0.557403);
    correctResults.push_back(0.628150);
    correctResults.push_back(0.557403);
    correctResults.push_back(0.523031);
    correctResults.push_back(0.146104);
    correctResults.push_back(0.219651);
    correctResults.push_back(0.416249);
    correctResults.push_back(0.219651);
    correctResults.push_back(0.146104);
    correctResults.push_back(0.219651);
    correctResults.push_back(0.416249);
    correctResults.push_back(0.219651);

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

    auto aRf = aResults->getMatrix(Side::Front, PropertySimple::R);

    correctResults.clear();
    correctResults.push_back(0.210438);
    correctResults.push_back(0.211198);
    correctResults.push_back(0.211233);
    correctResults.push_back(0.210818);
    correctResults.push_back(0.211233);
    correctResults.push_back(0.211198);
    correctResults.push_back(0.211233);
    correctResults.push_back(0.210818);
    correctResults.push_back(0.211233);
    correctResults.push_back(0.212138);
    correctResults.push_back(0.212284);
    correctResults.push_back(0.211973);
    correctResults.push_back(0.211288);
    correctResults.push_back(0.211973);
    correctResults.push_back(0.212284);
    correctResults.push_back(0.212138);
    correctResults.push_back(0.212284);
    correctResults.push_back(0.211973);
    correctResults.push_back(0.211288);
    correctResults.push_back(0.211973);
    correctResults.push_back(0.212284);
    correctResults.push_back(0.213658);
    correctResults.push_back(0.213850);
    correctResults.push_back(0.213261);
    correctResults.push_back(0.212048);
    correctResults.push_back(0.213261);
    correctResults.push_back(0.213850);
    correctResults.push_back(0.213658);
    correctResults.push_back(0.213850);
    correctResults.push_back(0.213261);
    correctResults.push_back(0.212048);
    correctResults.push_back(0.213261);
    correctResults.push_back(0.213850);
    correctResults.push_back(0.220182);
    correctResults.push_back(0.218856);
    correctResults.push_back(0.215310);
    correctResults.push_back(0.218856);
    correctResults.push_back(0.220182);
    correctResults.push_back(0.218856);
    correctResults.push_back(0.215310);
    correctResults.push_back(0.218856);

    calculatedResults.clear();
    for(size_t i = 0; i < size; ++i)
    {
        calculatedResults.push_back(aRf(i, i));
    }

    EXPECT_EQ(correctResults.size(), calculatedResults.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], calculatedResults[i], 1e-5);
    }
}
