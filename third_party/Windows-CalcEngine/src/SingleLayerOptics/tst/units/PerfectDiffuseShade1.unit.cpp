#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestPerfectDiffuseShade1 : public testing::Test
{
private:
    std::shared_ptr<CBSDFLayer> m_Shade;

protected:
    virtual void SetUp()
    {
        // create material
        double Tmat = 0.00;
        double Rfmat = 0.55;
        double Rbmat = 0.55;
        double minLambda = 0.3;
        double maxLambda = 2.5;
        auto aMaterial =
          Material::singleBandMaterial(Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda);

        auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Quarter);

        m_Shade = CBSDFLayerMaker::getPerfectlyDiffuseLayer(aMaterial, aBSDF);
    }

public:
    std::shared_ptr<CBSDFLayer> GetShade()
    {
        return m_Shade;
    };
};

TEST_F(TestPerfectDiffuseShade1, TestSolarProperties)
{
    SCOPED_TRACE("Begin Test: Perfect diffuse shade - Solar properties.");

    std::shared_ptr<CBSDFLayer> aShade = GetShade();

    std::shared_ptr<CBSDFIntegrator> aResults = aShade->getResults();

    double tauDiff = aResults->DiffDiff(Side::Front, PropertySimple::T);
    EXPECT_NEAR(0.000000000, tauDiff, 1e-6);

    double RfDiff = aResults->DiffDiff(Side::Front, PropertySimple::R);
    EXPECT_NEAR(0.550000000, RfDiff, 1e-6);

    auto aT = aResults->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix
    const size_t size = aT.size();

    std::vector<double> correctResults(41, 0);

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

    correctResults = {0.175070, 0.175070, 0.175070, 0.175070, 0.175070, 0.175070, 0.175070,
                      0.175070, 0.175070, 0.175070, 0.175070, 0.175070, 0.175070, 0.175070,
                      0.175070, 0.175070, 0.175070, 0.175070, 0.175070, 0.175070, 0.175070,
                      0.175070, 0.175070, 0.175070, 0.175070, 0.175070, 0.175070, 0.175070,
                      0.175070, 0.175070, 0.175070, 0.175070, 0.175070, 0.175070, 0.175070,
                      0.175070, 0.175070, 0.175070, 0.175070, 0.175070, 0.175070};

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
