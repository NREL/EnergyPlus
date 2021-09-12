#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestVenetianDirectionalShadeFlat0_1 : public testing::Test
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
    std::shared_ptr<CBSDFLayer> GetShade() const
    {
        return m_Shade;
    };
};

TEST_F(TestVenetianDirectionalShadeFlat0_1, TestVenetian1)
{
    SCOPED_TRACE("Begin Test: Venetian cell (Flat, 45 degrees slats) - solar properties.");

    std::shared_ptr<CBSDFLayer> aShade = GetShade();

    std::shared_ptr<CBSDFIntegrator> aResults = aShade->getResults();

    const double tauDiff = aResults->DiffDiff(Side::Front, PropertySimple::T);
    EXPECT_NEAR(0.44649813630049223, tauDiff, 1e-6);

    const double RfDiff = aResults->DiffDiff(Side::Front, PropertySimple::R);
    EXPECT_NEAR(0.28386835793952669, RfDiff, 1e-6);

    const double theta = 23;
    const double phi = 198;

    const double tauHem = aResults->DirHem(Side::Front, PropertySimple::T, theta, phi);
    EXPECT_NEAR(0.42987405997685452, tauHem, 1e-6);

    auto aT = aResults->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix
    const size_t size = aT.size();

    std::vector<double> correctResults;
    correctResults.push_back(3.8537531201195208);
    correctResults.push_back(4.1502982467458276);
    correctResults.push_back(6.4100346243744326);
    correctResults.push_back(7.3474092442895333);
    correctResults.push_back(6.4100346243744326);
    correctResults.push_back(4.1502982467458276);
    correctResults.push_back(1.8952091768254709);
    correctResults.push_back(0.9624818646186141);
    correctResults.push_back(1.8952091768254709);
    correctResults.push_back(3.8507492805553354);
    correctResults.push_back(7.1631325677152899);
    correctResults.push_back(9.5953311156279479);
    correctResults.push_back(10.487134124145857);
    correctResults.push_back(9.5953311156279479);
    correctResults.push_back(7.1631325677152899);
    correctResults.push_back(3.8507492805553367);
    correctResults.push_back(0.5499842626659959);
    correctResults.push_back(0.076422070782678389);
    correctResults.push_back(0.072113581483518790);
    correctResults.push_back(0.076422070782678389);
    correctResults.push_back(0.54998426266599365);
    correctResults.push_back(3.8507492805553394);
    correctResults.push_back(10.135661402447086);
    correctResults.push_back(11.234248057098451);
    correctResults.push_back(9.5441761716221478);
    correctResults.push_back(11.234248057098451);
    correctResults.push_back(10.135661402447086);
    correctResults.push_back(3.8507492805553412);
    correctResults.push_back(0.073751825599575688);
    correctResults.push_back(0.056801188075067718);
    correctResults.push_back(0.052393583794556338);
    correctResults.push_back(0.056801188075067677);
    correctResults.push_back(0.073751825599575660);
    correctResults.push_back(3.6627476023802092);
    correctResults.push_back(0.064003709245804577);
    correctResults.push_back(0.039335031220287420);
    correctResults.push_back(0.064003709245804577);
    correctResults.push_back(3.6627476023802141);
    correctResults.push_back(0.031558258087563060);
    correctResults.push_back(0.024104538133300955);
    correctResults.push_back(0.031558258087563032);

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-5);
    }

    // Front reflectance
    auto aRf = aResults->getMatrix(Side::Front, PropertySimple::R);

    correctResults.clear();

    correctResults.push_back(0.12467701187757886);
    correctResults.push_back(0.12467701187757886);
    correctResults.push_back(0.11809576069675247);
    correctResults.push_back(0.11151450951592602);
    correctResults.push_back(0.11809576069675243);
    correctResults.push_back(0.12467701187757889);
    correctResults.push_back(0.11809576069675248);
    correctResults.push_back(0.11151450951592602);
    correctResults.push_back(0.11809576069675248);
    correctResults.push_back(0.12467701187757886);
    correctResults.push_back(0.10822388392551284);
    correctResults.push_back(0.056751264415241136);
    correctResults.push_back(0.039493797495131787);
    correctResults.push_back(0.056751264415241136);
    correctResults.push_back(0.10822388392551285);
    correctResults.push_back(0.12467701187757889);
    correctResults.push_back(0.10822388392551291);
    correctResults.push_back(0.065378671009513112);
    correctResults.push_back(0.048215999889970981);
    correctResults.push_back(0.065378671009513042);
    correctResults.push_back(0.10822388392551284);
    correctResults.push_back(0.12467701187757886);
    correctResults.push_back(0.046055713210097007);
    correctResults.push_back(0.0077108503397755159);
    correctResults.push_back(0.013944144090848406);
    correctResults.push_back(0.0077108503397755150);
    correctResults.push_back(0.046055713210097007);
    correctResults.push_back(0.12467701187757892);
    correctResults.push_back(0.054978145293713886);
    correctResults.push_back(0.011951549151406803);
    correctResults.push_back(0.023431122989891869);
    correctResults.push_back(0.011951549151406849);
    correctResults.push_back(0.054978145293713775);
    correctResults.push_back(0.12467701187757879);
    correctResults.push_back(0.031558258087563046);
    correctResults.push_back(0.024104538133300962);
    correctResults.push_back(0.031558258087563046);
    correctResults.push_back(0.12467701187757896);
    correctResults.push_back(0.064003709245804660);
    correctResults.push_back(0.039335031220287427);
    correctResults.push_back(0.064003709245804577);

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-5);
    }
}
