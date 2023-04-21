#include <memory>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"


class TestGapLayerStandardPressure : public testing::Test
{
private:
    std::shared_ptr<Tarcog::ISO15099::CIGUGapLayer> m_GapLayer;
    Tarcog::ISO15099::CIGU m_IGU;

protected:
    void SetUp() override
    {
        // Gap layer construct is made in a way that it is not possible to create gap alone. In
        // order to test gap, entire IGU has to be created. Example is taken as part of double clear
        // air run from WINDOW 7 version of TARCOG
        auto surface1 = std::make_shared<Tarcog::ISO15099::CSurface>();
        ASSERT_TRUE(surface1 != nullptr);
        auto surface2 = std::make_shared<Tarcog::ISO15099::CSurface>();
        ASSERT_TRUE(surface2 != nullptr);
        auto surface3 = std::make_shared<Tarcog::ISO15099::CSurface>();
        ASSERT_TRUE(surface3 != nullptr);
        auto surface4 = std::make_shared<Tarcog::ISO15099::CSurface>();
        ASSERT_TRUE(surface4 != nullptr);

        auto solidLayerThickness = 0.005715;   // [m]
        auto solidLayerConductance = 1.0;
        auto gapSurface1Temperature = 262.756296539528;
        auto gapSurface2Temperature = 276.349093518906;
        // surfaces 2 and 3 are actually surfaces of the gap
        surface2->setTemperature(gapSurface1Temperature);
        surface3->setTemperature(gapSurface2Temperature);
        auto solidLayer1 = std::make_shared<Tarcog::ISO15099::CIGUSolidLayer>(
          solidLayerThickness, solidLayerConductance, surface1, surface2);

        ASSERT_TRUE(solidLayer1 != nullptr);
        auto solidLayer2 = std::make_shared<Tarcog::ISO15099::CIGUSolidLayer>(
          solidLayerThickness, solidLayerConductance, surface3, surface4);
        ASSERT_TRUE(solidLayer2 != nullptr);

        auto gapThickness = 0.012;
        m_GapLayer = Tarcog::ISO15099::Layers::gap(gapThickness);
        ASSERT_TRUE(m_GapLayer != nullptr);

        auto windowWidth = 1.0;
        auto windowHeight = 1.0;
        m_IGU = Tarcog::ISO15099::CIGU(windowWidth, windowHeight);
        m_IGU.addLayers({solidLayer1, m_GapLayer, solidLayer2});

        // Alternative way of adding layers.
        // m_IGU.addLayer(solidLayer1);
        // m_IGU.addLayer(m_GapLayer);
        // m_IGU.addLayer(solidLayer2);
    }

public:
    std::shared_ptr<Tarcog::ISO15099::CIGUGapLayer> GetLayer() const
    {
        return m_GapLayer;
    };
};

TEST_F(TestGapLayerStandardPressure, ConvectionHeatFlow)
{
    SCOPED_TRACE("Begin Test: Test Gap Layer - Convection heat flow [Pa = 101325 Pa]");

    auto aLayer = GetLayer();
    ASSERT_TRUE(aLayer != nullptr);

    auto convectionHeatFlow = aLayer->getConvectionConductionFlow();
    EXPECT_NEAR(27.673789062350764, convectionHeatFlow, 1e-6);
}
