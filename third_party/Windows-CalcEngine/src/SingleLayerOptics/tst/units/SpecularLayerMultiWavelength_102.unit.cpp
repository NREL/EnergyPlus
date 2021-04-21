#include <memory>
#include <gtest/gtest.h>

#include "WCESpectralAveraging.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;
using namespace SpectralAveraging;

// Test of calculating BSDF matrices over the entire wavelength range
class TestSpecularLayerMultiWavelength_102 : public testing::Test
{
private:
    std::shared_ptr<CBSDFLayer> m_Layer;

protected:
    virtual void SetUp()
    {
        //CSeries aSolarRadiation{
        //  {0.3000, 0.0}, {0.3050, 3.4}, {0.3100, 15.6}, {0.3150, 41.1}, {0.3200, 71.2}};

        const auto aMeasurements = CSpectralSampleData::create({{0.300, 0.0020, 0.0470, 0.0480},
                                                                {0.305, 0.0030, 0.0470, 0.0480},
                                                                {0.310, 0.0090, 0.0470, 0.0480},
                                                                {0.315, 0.0350, 0.0470, 0.0480},
                                                                {0.320, 0.1000, 0.0470, 0.0480}});


        const auto thickness = 3.048e-3;   // [m]
        const MaterialType aType = MaterialType::Monolithic;
        // WavelengthRange aRange = WavelengthRange::Solar;
        const auto minLambda = 0.3;
        const auto maxLambda = 2.5;
        const auto aMaterial =
          Material::nBandMaterial(aMeasurements, thickness, aType, minLambda, maxLambda);

        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Quarter);

        // make layer
        m_Layer = CBSDFLayerMaker::getSpecularLayer(aMaterial, aBSDF);
    }

public:
    std::shared_ptr<CBSDFLayer> getLayer()
    {
        return m_Layer;
    };
};

TEST_F(TestSpecularLayerMultiWavelength_102, TestSpecular1)
{
    SCOPED_TRACE("Begin Test: Specular layer - BSDF.");

    std::shared_ptr<CBSDFLayer> aLayer = getLayer();

    std::shared_ptr<std::vector<std::shared_ptr<CBSDFIntegrator>>> aResults =
      aLayer->getWavelengthResults();

    size_t correctSize = 5;

    EXPECT_EQ(correctSize, aResults->size());

    ///////////////////////////////////////////////////////////////////////
    ///  Wavelength number 1
    ///////////////////////////////////////////////////////////////////////

    auto aT = (*aResults)[0]->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix

    std::vector<double> correctResults{
      0.026014, 0.024743, 0.024743, 0.024743, 0.024743, 0.024743, 0.024743, 0.024743, 0.024743,
      0.015794, 0.015794, 0.015794, 0.015794, 0.015794, 0.015794, 0.015794, 0.015794, 0.015794,
      0.015794, 0.015794, 0.015794, 0.008638, 0.008638, 0.008638, 0.008638, 0.008638, 0.008638,
      0.008638, 0.008638, 0.008638, 0.008638, 0.008638, 0.008638, 0.002528, 0.002528, 0.002528,
      0.002528, 0.002528, 0.002528, 0.002528, 0.002528};

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < aT.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-5);
    }

    // Front reflectance
    auto aRf = (*aResults)[0]->getMatrix(Side::Front, PropertySimple::R);

    correctResults = {
      0.61134,  0.661508, 0.661508, 0.661508, 0.661508, 0.661508, 0.661508, 0.661508, 0.661508,
      0.659003, 0.659003, 0.659003, 0.659003, 0.659003, 0.659003, 0.659003, 0.659003, 0.659003,
      0.659003, 0.659003, 0.659003, 0.974838, 0.974838, 0.974838, 0.974838, 0.974838, 0.974838,
      0.974838, 0.974838, 0.974838, 0.974838, 0.974838, 0.974838, 3.654452, 3.654452, 3.654452,
      3.654452, 3.654452, 3.654452, 3.654452, 3.654452,
    };

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < aRf.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-5);
    }

    ///////////////////////////////////////////////////////////////////////
    ///  Wavelength number 2
    ///////////////////////////////////////////////////////////////////////

    aT = (*aResults)[1]->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix

    correctResults = {0.039022, 0.037422, 0.037422, 0.037422, 0.037422, 0.037422, 0.037422,
                      0.037422, 0.037422, 0.024475, 0.024475, 0.024475, 0.024475, 0.024475,
                      0.024475, 0.024475, 0.024475, 0.024475, 0.024475, 0.024475, 0.024475,
                      0.01389,  0.01389,  0.01389,  0.01389,  0.01389,  0.01389,  0.01389,
                      0.01389,  0.01389,  0.01389,  0.01389,  0.01389,  0.004252, 0.004252,
                      0.004252, 0.004252, 0.004252, 0.004252, 0.004252, 0.004252};

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < aT.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-5);
    }

    // Front reflectance
    aRf = (*aResults)[1]->getMatrix(Side::Front, PropertySimple::R);

    correctResults = {0.61134,  0.661507, 0.661507, 0.661507, 0.661507, 0.661507, 0.661507,
                      0.661507, 0.661507, 0.659001, 0.659001, 0.659001, 0.659001, 0.659001,
                      0.659001, 0.659001, 0.659001, 0.659001, 0.659001, 0.659001, 0.659001,
                      0.974835, 0.974835, 0.974835, 0.974835, 0.974835, 0.974835, 0.974835,
                      0.974835, 0.974835, 0.974835, 0.974835, 0.974835, 3.654449, 3.654449,
                      3.654449, 3.654449, 3.654449, 3.654449, 3.654449, 3.654449};

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < aRf.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-5);
    }

    ///////////////////////////////////////////////////////////////////////
    ///  Wavelength number 3
    ///////////////////////////////////////////////////////////////////////

    aT = (*aResults)[2]->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix

    correctResults = {0.117065, 0.114808, 0.114808, 0.114808, 0.114808, 0.114808, 0.114808,
                      0.114808, 0.114808, 0.080195, 0.080195, 0.080195, 0.080195, 0.080195,
                      0.080195, 0.080195, 0.080195, 0.080195, 0.080195, 0.080195, 0.080195,
                      0.050298, 0.050298, 0.050298, 0.050298, 0.050298, 0.050298, 0.050298,
                      0.050298, 0.050298, 0.050298, 0.050298, 0.050298, 0.017391, 0.017391,
                      0.017391, 0.017391, 0.017391, 0.017391, 0.017391, 0.017391};

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < aT.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-5);
    }

    // Front reflectance
    aRf = (*aResults)[2]->getMatrix(Side::Front, PropertySimple::R);

    correctResults = {0.61134,  0.661498, 0.661498, 0.661498, 0.661498, 0.661498, 0.661498,
                      0.661498, 0.661498, 0.658976, 0.658976, 0.658976, 0.658976, 0.658976,
                      0.658976, 0.658976, 0.658976, 0.658976, 0.658976, 0.658976, 0.658976,
                      0.974793, 0.974793, 0.974793, 0.974793, 0.974793, 0.974793, 0.974793,
                      0.974793, 0.974793, 0.974793, 0.974793, 0.974793, 3.654404, 3.654404,
                      3.654404, 3.654404, 3.654404, 3.654404, 3.654404, 3.654404};

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < aRf.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-5);
    }

    ///////////////////////////////////////////////////////////////////////
    ///  Wavelength number 4
    ///////////////////////////////////////////////////////////////////////

    aT = (*aResults)[3]->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix

    correctResults = {0.455253, 0.458994, 0.458994, 0.458994, 0.458994, 0.458994, 0.458994,
                      0.458994, 0.458994, 0.347745, 0.347745, 0.347745, 0.347745, 0.347745,
                      0.347745, 0.347745, 0.347745, 0.347745, 0.347745, 0.347745, 0.347745,
                      0.246756, 0.246756, 0.246756, 0.246756, 0.246756, 0.246756, 0.246756,
                      0.246756, 0.246756, 0.246756, 0.246756, 0.246756, 0.09914,  0.09914,
                      0.09914,  0.09914,  0.09914,  0.09914,  0.09914,  0.09914};

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < aT.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-5);
    }

    // Front reflectance
    aRf = (*aResults)[3]->getMatrix(Side::Front, PropertySimple::R);

    correctResults = {0.61134,  0.661398, 0.661398, 0.661398, 0.661398, 0.661398, 0.661398,
                      0.661398, 0.661398, 0.658665, 0.658665, 0.658665, 0.658665, 0.658665,
                      0.658665, 0.658665, 0.658665, 0.658665, 0.658665, 0.658665, 0.658665,
                      0.974245, 0.974245, 0.974245, 0.974245, 0.974245, 0.974245, 0.974245,
                      0.974245, 0.974245, 0.974245, 0.974245, 0.974245, 3.653868, 3.653868,
                      3.653868, 3.653868, 3.653868, 3.653868, 3.653868, 3.653868};

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < aRf.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-5);
    }

    ///////////////////////////////////////////////////////////////////////
    ///  Wavelength number 5
    ///////////////////////////////////////////////////////////////////////

    aT = (*aResults)[4]->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix

    correctResults = {1.300724, 1.339503, 1.339503, 1.339503, 1.339503, 1.339503, 1.339503,
                      1.339503, 1.339503, 1.080006, 1.080006, 1.080006, 1.080006, 1.080006,
                      1.080006, 1.080006, 1.080006, 1.080006, 1.080006, 1.080006, 1.080006,
                      0.842257, 0.842257, 0.842257, 0.842257, 0.842257, 0.842257, 0.842257,
                      0.842257, 0.842257, 0.842257, 0.842257, 0.842257, 0.379491, 0.379491,
                      0.379491, 0.379491, 0.379491, 0.379491, 0.379491, 0.379491};

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < aT.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-5);
    }

    // Front reflectance
    aRf = (*aResults)[4]->getMatrix(Side::Front, PropertySimple::R);

    correctResults = {0.61134,  0.660889, 0.660889, 0.660889, 0.660889, 0.660889, 0.660889,
                      0.660889, 0.660889, 0.657004, 0.657004, 0.657004, 0.657004, 0.657004,
                      0.657004, 0.657004, 0.657004, 0.657004, 0.657004, 0.657004, 0.657004,
                      0.971223, 0.971223, 0.971223, 0.971223, 0.971223, 0.971223, 0.971223,
                      0.971223, 0.971223, 0.971223, 0.971223, 0.971223, 3.65192,  3.65192,
                      3.65192,  3.65192,  3.65192,  3.65192,  3.65192,  3.65192};

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < aRf.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-5);
    }
}
