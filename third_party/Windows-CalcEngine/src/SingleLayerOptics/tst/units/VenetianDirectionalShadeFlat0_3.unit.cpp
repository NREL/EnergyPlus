#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestVenetianDirectionalShadeFlat0_3 : public testing::Test
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
        const auto slatWidth = 0.016;     // m
        const auto slatSpacing = 0.010;   // m
        const auto slatTiltAngle = 0;
        const auto curvatureRadius = 0;
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

TEST_F(TestVenetianDirectionalShadeFlat0_3, TestVenetian1)
{
    SCOPED_TRACE("Begin Test: Venetian cell (Flat, 45 degrees slats) - solar properties.");

    std::shared_ptr<CBSDFLayer> aShade = GetShade();

    std::shared_ptr<CBSDFIntegrator> aResults = aShade->getResults();

    const double tauDiff = aResults->DiffDiff(Side::Front, PropertySimple::T);
    EXPECT_NEAR(0.48775116654942097, tauDiff, 1e-6);

    const double RfDiff = aResults->DiffDiff(Side::Front, PropertySimple::R);
    EXPECT_NEAR(0.22509839868274970, RfDiff, 1e-6);

    auto aT = aResults->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix
    size_t size = aT.size();

    std::vector<double> correctResults;
    correctResults.push_back(13.00724272523622500);
    correctResults.push_back(14.01971111841941000);
    correctResults.push_back(8.879333707559864000);
    correctResults.push_back(6.757930491107369900);
    correctResults.push_back(8.879333707559864000);
    correctResults.push_back(14.01971111841941000);
    correctResults.push_back(8.879333707559864000);
    correctResults.push_back(6.757930491107371700);
    correctResults.push_back(8.879333707559864000);
    correctResults.push_back(12.99698697545699000);
    correctResults.push_back(5.476030728175789600);
    correctResults.push_back(0.098218010371277989);
    correctResults.push_back(0.095832434781168180);
    correctResults.push_back(0.098218010371277989);
    correctResults.push_back(5.476030728175789600);
    correctResults.push_back(12.99698697545699000);
    correctResults.push_back(5.476030728175789600);
    correctResults.push_back(0.098218010371277989);
    correctResults.push_back(0.095832434781168138);
    correctResults.push_back(0.098218010371277989);
    correctResults.push_back(5.476030728175789600);
    correctResults.push_back(12.99698697545700400);
    correctResults.push_back(0.094990521299407105);
    correctResults.push_back(0.062476409862337726);
    correctResults.push_back(0.052827014930858306);
    correctResults.push_back(0.062476409862337726);
    correctResults.push_back(0.094990521299407105);
    correctResults.push_back(12.99698697545700400);
    correctResults.push_back(0.094990521299407105);
    correctResults.push_back(0.062476409862337781);
    correctResults.push_back(0.052827014930858347);
    correctResults.push_back(0.062476409862337781);
    correctResults.push_back(0.094990521299407105);
    correctResults.push_back(12.35510909608257400);
    correctResults.push_back(0.034092954478813497);
    correctResults.push_back(0.026306763651200261);
    correctResults.push_back(0.034092954478813497);
    correctResults.push_back(12.35510909608257400);
    correctResults.push_back(0.034092954478813518);
    correctResults.push_back(0.026306763651200268);
    correctResults.push_back(0.034092954478813518);

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-6);
    }

    // Front reflectance
    auto aRf = aResults->getMatrix(Side::Front, PropertySimple::R);

    correctResults.clear();

    correctResults.push_back(0.000000000000000000);
    correctResults.push_back(0.000000000000000000);
    correctResults.push_back(0.013333908639926589);
    correctResults.push_back(0.026667817279853189);
    correctResults.push_back(0.013333908639926589);
    correctResults.push_back(0.000000000000000000);
    correctResults.push_back(0.013333908639926589);
    correctResults.push_back(0.026667817279853193);
    correctResults.push_back(0.013333908639926589);
    correctResults.push_back(0.000000000000000000);
    correctResults.push_back(0.033334771599816487);
    correctResults.push_back(0.098232428791164006);
    correctResults.push_back(0.101397330494051680);
    correctResults.push_back(0.098232428791164006);
    correctResults.push_back(0.033334771599816487);
    correctResults.push_back(0.000000000000000000);
    correctResults.push_back(0.033334771599816487);
    correctResults.push_back(0.098232428791163950);
    correctResults.push_back(0.101397330494051680);
    correctResults.push_back(0.098232428791163950);
    correctResults.push_back(0.033334771599816487);
    correctResults.push_back(0.000000000000000000);
    correctResults.push_back(0.097542640608995437);
    correctResults.push_back(0.116785354563414950);
    correctResults.push_back(0.125238941198961200);
    correctResults.push_back(0.116785354563414950);
    correctResults.push_back(0.097542640608995437);
    correctResults.push_back(0.000000000000000000);
    correctResults.push_back(0.097542640608995465);
    correctResults.push_back(0.116785354563414910);
    correctResults.push_back(0.125238941198961260);
    correctResults.push_back(0.116785354563414910);
    correctResults.push_back(0.097542640608995465);
    correctResults.push_back(0.000000000000000000);
    correctResults.push_back(0.180352627561802150);
    correctResults.push_back(0.147864609583900520);
    correctResults.push_back(0.180352627561802150);
    correctResults.push_back(0.000000000000000000);
    correctResults.push_back(0.180352627561802200);
    correctResults.push_back(0.147864609583900520);
    correctResults.push_back(0.180352627561802200);

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-6);
    }
}
