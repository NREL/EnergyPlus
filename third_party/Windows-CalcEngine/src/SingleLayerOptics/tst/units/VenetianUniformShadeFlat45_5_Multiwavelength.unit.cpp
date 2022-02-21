#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;
using namespace SpectralAveraging;

class TestVenetianUniformShadeFlat45_5_Multiwavelength : public testing::Test
{
private:
    std::shared_ptr<CBSDFLayer> m_Layer;

protected:
    virtual void SetUp()
    {
        // Solar range material
        const auto Tsol = 0.1;
        const auto Rfsol = 0.7;
        const auto Rbsol = 0.7;

        // Visible range
        const auto Tvis = 0.2;
        const auto Rfvis = 0.6;
        const auto Rbvis = 0.6;

        const auto aMaterial =
          Material::dualBandMaterial(Tsol, Tsol, Rfsol, Rbsol, Tvis, Tvis, Rfvis, Rbvis);

        // make cell geometry
        const auto slatWidth = 0.016;     // m
        const auto slatSpacing = 0.012;   // m
        const auto slatTiltAngle = 45;
        const auto curvatureRadius = 0;
        const size_t numOfSlatSegments = 5;

        // create BSDF
        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Quarter);

        // make layer
        m_Layer = CBSDFLayerMaker::getVenetianLayer(aMaterial,
                                                    aBSDF,
                                                    slatWidth,
                                                    slatSpacing,
                                                    slatTiltAngle,
                                                    curvatureRadius,
                                                    numOfSlatSegments,
                                                    DistributionMethod::UniformDiffuse,
                                                    true);
    }

public:
    std::shared_ptr<CBSDFLayer> getLayer()
    {
        return m_Layer;
    };
};

TEST_F(TestVenetianUniformShadeFlat45_5_Multiwavelength, TestVenetianMultiWavelength)
{
    SCOPED_TRACE("Begin Test: Venetian layer (multi range) - BSDF.");

    std::shared_ptr<CBSDFLayer> aLayer = getLayer();

    std::shared_ptr<std::vector<std::shared_ptr<CBSDFIntegrator>>> aResults =
      aLayer->getWavelengthResults();

    size_t correctSize = 5;

    EXPECT_EQ(correctSize, aResults->size());

    ///////////////////////////////////////////////////////////////////////
    //  Wavelength number 1
    ///////////////////////////////////////////////////////////////////////

    auto aT = (*aResults)[0]->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix
    size_t size = aT.size();

    std::vector<double> correctResults;
    correctResults.push_back(0.743897);
    correctResults.push_back(0.801801);
    correctResults.push_back(3.838654);
    correctResults.push_back(5.096560);
    correctResults.push_back(3.838654);
    correctResults.push_back(0.801801);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.743310);
    correctResults.push_back(5.194719);
    correctResults.push_back(8.453376);
    correctResults.push_back(9.646127);
    correctResults.push_back(8.453376);
    correctResults.push_back(5.194719);
    correctResults.push_back(0.743310);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.743310);
    correctResults.push_back(9.176180);
    correctResults.push_back(10.644505);
    correctResults.push_back(8.384925);
    correctResults.push_back(10.644505);
    correctResults.push_back(9.176180);
    correctResults.push_back(0.743310);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.706601);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.706601);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);
    correctResults.push_back(0.000000);

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-5);
    }

    // Front reflectance
    auto aRf = (*aResults)[0]->getMatrix(Side::Front, PropertySimple::R);

    correctResults.clear();

    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-5);
    }

    ///////////////////////////////////////////////////////////////////////
    //  Wavelength number 2
    ///////////////////////////////////////////////////////////////////////

    aT = (*aResults)[1]->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix
    size = aT.size();

    correctResults.clear();

    correctResults.push_back(0.802660);
    correctResults.push_back(0.860564);
    correctResults.push_back(3.883917);
    correctResults.push_back(5.136230);
    correctResults.push_back(3.883917);
    correctResults.push_back(0.860564);
    correctResults.push_back(0.054766);
    correctResults.push_back(0.050509);
    correctResults.push_back(0.054766);
    correctResults.push_back(0.802074);
    correctResults.push_back(5.232135);
    correctResults.push_back(8.475165);
    correctResults.push_back(9.662197);
    correctResults.push_back(8.475165);
    correctResults.push_back(5.232135);
    correctResults.push_back(0.802074);
    correctResults.push_back(0.049555);
    correctResults.push_back(0.043063);
    correctResults.push_back(0.040687);
    correctResults.push_back(0.043063);
    correctResults.push_back(0.049555);
    correctResults.push_back(0.802074);
    correctResults.push_back(9.194503);
    correctResults.push_back(10.672994);
    correctResults.push_back(8.440777);
    correctResults.push_back(10.672994);
    correctResults.push_back(9.194503);
    correctResults.push_back(0.802074);
    correctResults.push_back(0.041623);
    correctResults.push_back(0.035407);
    correctResults.push_back(0.033549);
    correctResults.push_back(0.035407);
    correctResults.push_back(0.041623);
    correctResults.push_back(0.765364);
    correctResults.push_back(0.121632);
    correctResults.push_back(0.104072);
    correctResults.push_back(0.121632);
    correctResults.push_back(0.765364);
    correctResults.push_back(0.027554);
    correctResults.push_back(0.024517);
    correctResults.push_back(0.027554);

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-5);
    }

    // Front reflectance
    aRf = (*aResults)[1]->getMatrix(Side::Front, PropertySimple::R);

    correctResults.clear();

    correctResults.push_back(0.148393);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.114299);
    correctResults.push_back(0.100177);
    correctResults.push_back(0.114299);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.168002);
    correctResults.push_back(0.173972);
    correctResults.push_back(0.168002);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.094486);
    correctResults.push_back(0.055023);
    correctResults.push_back(0.040579);
    correctResults.push_back(0.055023);
    correctResults.push_back(0.094486);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.175452);
    correctResults.push_back(0.185559);
    correctResults.push_back(0.189259);
    correctResults.push_back(0.185559);
    correctResults.push_back(0.175452);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.046270);
    correctResults.push_back(0.011282);
    correctResults.push_back(0.022117);
    correctResults.push_back(0.011282);
    correctResults.push_back(0.046270);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.187801);
    correctResults.push_back(0.197924);
    correctResults.push_back(0.201009);
    correctResults.push_back(0.197924);
    correctResults.push_back(0.187801);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.085962);
    correctResults.push_back(0.098907);
    correctResults.push_back(0.085962);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.211083);
    correctResults.push_back(0.216240);
    correctResults.push_back(0.211083);

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-5);
    }

    ///////////////////////////////////////////////////////////////////////
    //  Wavelength number 3
    ///////////////////////////////////////////////////////////////////////

    aT = (*aResults)[2]->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix
    size = aT.size();

    correctResults.clear();

    correctResults.push_back(0.822696);
    correctResults.push_back(0.880601);
    correctResults.push_back(3.899350);
    correctResults.push_back(5.149756);
    correctResults.push_back(3.899350);
    correctResults.push_back(0.880601);
    correctResults.push_back(0.076246);
    correctResults.push_back(0.072118);
    correctResults.push_back(0.076246);
    correctResults.push_back(0.822110);
    correctResults.push_back(5.244893);
    correctResults.push_back(8.482595);
    correctResults.push_back(9.667676);
    correctResults.push_back(8.482595);
    correctResults.push_back(5.244893);
    correctResults.push_back(0.822110);
    correctResults.push_back(0.071121);
    correctResults.push_back(0.064321);
    correctResults.push_back(0.061831);
    correctResults.push_back(0.064321);
    correctResults.push_back(0.071121);
    correctResults.push_back(0.822110);
    correctResults.push_back(9.200750);
    correctResults.push_back(10.669461);
    correctResults.push_back(8.433850);
    correctResults.push_back(10.669461);
    correctResults.push_back(9.200750);
    correctResults.push_back(0.822110);
    correctResults.push_back(0.062812);
    correctResults.push_back(0.055872);
    correctResults.push_back(0.053740);
    correctResults.push_back(0.055872);
    correctResults.push_back(0.062812);
    correctResults.push_back(0.785400);
    correctResults.push_back(0.107468);
    correctResults.push_back(0.093221);
    correctResults.push_back(0.107468);
    correctResults.push_back(0.785400);
    correctResults.push_back(0.046481);
    correctResults.push_back(0.042638);
    correctResults.push_back(0.046481);

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-5);
    }

    // Front reflectance
    aRf = (*aResults)[2]->getMatrix(Side::Front, PropertySimple::R);

    correctResults.clear();

    correctResults.push_back(0.129989);
    correctResults.push_back(0.129989);
    correctResults.push_back(0.100123);
    correctResults.push_back(0.087753);
    correctResults.push_back(0.100123);
    correctResults.push_back(0.129989);
    correctResults.push_back(0.146686);
    correctResults.push_back(0.151646);
    correctResults.push_back(0.146686);
    correctResults.push_back(0.129989);
    correctResults.push_back(0.082768);
    correctResults.push_back(0.048199);
    correctResults.push_back(0.035546);
    correctResults.push_back(0.048199);
    correctResults.push_back(0.082768);
    correctResults.push_back(0.129989);
    correctResults.push_back(0.152900);
    correctResults.push_back(0.161474);
    correctResults.push_back(0.164612);
    correctResults.push_back(0.161474);
    correctResults.push_back(0.152900);
    correctResults.push_back(0.129989);
    correctResults.push_back(0.040532);
    correctResults.push_back(0.015128);
    correctResults.push_back(0.029659);
    correctResults.push_back(0.015128);
    correctResults.push_back(0.040532);
    correctResults.push_back(0.129989);
    correctResults.push_back(0.163376);
    correctResults.push_back(0.172244);
    correctResults.push_back(0.174984);
    correctResults.push_back(0.172244);
    correctResults.push_back(0.163376);
    correctResults.push_back(0.129989);
    correctResults.push_back(0.108044);
    correctResults.push_back(0.120070);
    correctResults.push_back(0.108044);
    correctResults.push_back(0.129989);
    correctResults.push_back(0.184339);
    correctResults.push_back(0.189304);
    correctResults.push_back(0.184339);

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-5);
    }

    ///////////////////////////////////////////////////////////////////////
    //  Wavelength number 4
    ///////////////////////////////////////////////////////////////////////

    aT = (*aResults)[3]->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix
    size = aT.size();

    correctResults.clear();

    correctResults.push_back(0.802660);
    correctResults.push_back(0.860564);
    correctResults.push_back(3.883917);
    correctResults.push_back(5.136230);
    correctResults.push_back(3.883917);
    correctResults.push_back(0.860564);
    correctResults.push_back(0.054766);
    correctResults.push_back(0.050509);
    correctResults.push_back(0.054766);
    correctResults.push_back(0.802074);
    correctResults.push_back(5.232135);
    correctResults.push_back(8.475165);
    correctResults.push_back(9.662197);
    correctResults.push_back(8.475165);
    correctResults.push_back(5.232135);
    correctResults.push_back(0.802074);
    correctResults.push_back(0.049555);
    correctResults.push_back(0.043063);
    correctResults.push_back(0.040687);
    correctResults.push_back(0.043063);
    correctResults.push_back(0.049555);
    correctResults.push_back(0.802074);
    correctResults.push_back(9.194503);
    correctResults.push_back(10.672994);
    correctResults.push_back(8.440777);
    correctResults.push_back(10.672994);
    correctResults.push_back(9.194503);
    correctResults.push_back(0.802074);
    correctResults.push_back(0.041623);
    correctResults.push_back(0.035407);
    correctResults.push_back(0.033549);
    correctResults.push_back(0.035407);
    correctResults.push_back(0.041623);
    correctResults.push_back(0.765364);
    correctResults.push_back(0.121632);
    correctResults.push_back(0.104072);
    correctResults.push_back(0.121632);
    correctResults.push_back(0.765364);
    correctResults.push_back(0.027554);
    correctResults.push_back(0.024517);
    correctResults.push_back(0.027554);

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-5);
    }

    // Front reflectance
    aRf = (*aResults)[3]->getMatrix(Side::Front, PropertySimple::R);

    correctResults.clear();

    correctResults.push_back(0.148393);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.114299);
    correctResults.push_back(0.100177);
    correctResults.push_back(0.114299);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.168002);
    correctResults.push_back(0.173972);
    correctResults.push_back(0.168002);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.094486);
    correctResults.push_back(0.055023);
    correctResults.push_back(0.040579);
    correctResults.push_back(0.055023);
    correctResults.push_back(0.094486);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.175452);
    correctResults.push_back(0.185559);
    correctResults.push_back(0.189259);
    correctResults.push_back(0.185559);
    correctResults.push_back(0.175452);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.046270);
    correctResults.push_back(0.011282);
    correctResults.push_back(0.022117);
    correctResults.push_back(0.011282);
    correctResults.push_back(0.046270);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.187801);
    correctResults.push_back(0.197924);
    correctResults.push_back(0.201009);
    correctResults.push_back(0.197924);
    correctResults.push_back(0.187801);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.085962);
    correctResults.push_back(0.098907);
    correctResults.push_back(0.085962);
    correctResults.push_back(0.148393);
    correctResults.push_back(0.211083);
    correctResults.push_back(0.216240);
    correctResults.push_back(0.211083);

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-5);
    }
}
