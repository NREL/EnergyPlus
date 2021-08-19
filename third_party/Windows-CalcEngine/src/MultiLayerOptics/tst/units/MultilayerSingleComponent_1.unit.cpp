#include <memory>
#include <gtest/gtest.h>

#include "WCEMultiLayerOptics.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace MultiLayerOptics;
using namespace FenestrationCommon;

// Calculation of equivalent layer data for single component.
// Layers are added to the back side
class TestMultilayerSingleComponent_1 : public testing::Test
{
private:
    // Additional layer added to the back side
    CMultiLayerSingleComponent m_IGU;

protected:
    virtual void SetUp()
    {
        m_IGU = CMultiLayerSingleComponent(0.46, 0.52, 0.64, 0.22);
        m_IGU.addLayer(0.56, 0.34, 0.49, 0.39);
        m_IGU.addLayer(0.12, 0.47, 0.33, 0.63);
    }

public:
    CMultiLayerSingleComponent & getIGU()
    {
        return m_IGU;
    };
};

TEST_F(TestMultilayerSingleComponent_1, TestOpticalProperties)
{
    SCOPED_TRACE("Begin Test: Combined layers optical properties.");

    auto & eqLayer = getIGU();

    double Tf = eqLayer.getProperty(Property::T, Side::Front);
    EXPECT_NEAR(0.042506037, Tf, 1e-6);

    double Rf = eqLayer.getProperty(Property::R, Side::Front);
    EXPECT_NEAR(0.684618188, Rf, 1e-6);

    double Af = eqLayer.getProperty(Property::Abs, Side::Front);
    EXPECT_NEAR(0.272875775, Af, 1e-6);

    double Tb = eqLayer.getProperty(Property::T, Side::Back);
    EXPECT_NEAR(0.142302818, Tb, 1e-6);

    double Rb = eqLayer.getProperty(Property::R, Side::Back);
    EXPECT_NEAR(0.652935221, Rb, 1e-6);

    double Ab = eqLayer.getProperty(Property::Abs, Side::Back);
    EXPECT_NEAR(0.20476196, Ab, 1e-6);
}

TEST_F(TestMultilayerSingleComponent_1, TestLayerAbsorptances)
{
    SCOPED_TRACE("Begin Test: Layer by layer absorptances.");

    auto & eqLayer = getIGU();

    double Af1 = eqLayer.getLayerAbsorptance(1, Side::Front);
    EXPECT_NEAR(0.056010229, Af1, 1e-6);

    double Af2 = eqLayer.getLayerAbsorptance(2, Side::Front);
    EXPECT_NEAR(0.071636587, Af2, 1e-6);

    double Af3 = eqLayer.getLayerAbsorptance(3, Side::Front);
    EXPECT_NEAR(0.145228959, Af3, 1e-6);

    double Ab1 = eqLayer.getLayerAbsorptance(1, Side::Back);
    EXPECT_NEAR(0.031128742, Ab1, 1e-6);

    double Ab2 = eqLayer.getLayerAbsorptance(2, Side::Back);
    EXPECT_NEAR(0.055271213, Ab2, 1e-6);

    double Ab3 = eqLayer.getLayerAbsorptance(3, Side::Back);
    EXPECT_NEAR(0.118362006, Ab3, 1e-6);
}
