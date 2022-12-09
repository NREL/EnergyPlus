#include <memory>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"

class TestGapLayerInBetweenVentilation : public testing::Test
{
private:
    std::shared_ptr<Tarcog::ISO15099::CSingleSystem> m_TarcogSystem;

protected:
    void SetUp() override
    {
        /////////////////////////////////////////////////////////
        // Outdoor
        /////////////////////////////////////////////////////////
        Real64 airTemperature = 255.15;   // Kelvins
        Real64 airSpeed = 5.5;            // meters per second
        Real64 tSky = 255.15;             // Kelvins
        Real64 solarRadiation = 0.0;

        auto Outdoor = Tarcog::ISO15099::Environments::outdoor(
          airTemperature, airSpeed, solarRadiation, tSky, Tarcog::ISO15099::SkyModel::AllSpecified);
        ASSERT_TRUE(Outdoor != nullptr);
        Outdoor->setHCoeffModel(Tarcog::ISO15099::BoundaryConditionsCoeffModel::CalculateH);

        /////////////////////////////////////////////////////////
        /// Indoor
        /////////////////////////////////////////////////////////

        auto roomTemperature = 295.15;

        auto Indoor = Tarcog::ISO15099::Environments::indoor(roomTemperature);
        ASSERT_TRUE(Indoor != nullptr);

        // IGU
        auto solidLayerThickness = 0.005715;   // [m]
        auto solidLayerConductance = 1.0;

        auto layer1 = Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        ASSERT_TRUE(layer1 != nullptr);

        auto layer2 = Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        ASSERT_TRUE(layer2 != nullptr);

        Real64 shadeLayerThickness = 0.01;
        Real64 shadeLayerConductance = 160.0;
        Real64 Atop = 0.1;
        Real64 Abot = 0.1;
        Real64 Aleft = 0.1;
        Real64 Aright = 0.1;
        Real64 Afront = 0.2;

        EffectiveLayers::ShadeOpenness openness{Afront, Aleft, Aright, Atop, Abot};

        double windowWidth = 1;
        double windowHeight = 1;

        EffectiveLayers::EffectiveLayerOther effectiveLayer{
          windowWidth, windowHeight, shadeLayerThickness, openness};

        EffectiveLayers::EffectiveOpenness effOpenness{effectiveLayer.getEffectiveOpenness()};

        auto shadeLayer = Tarcog::ISO15099::Layers::shading(
          shadeLayerThickness, shadeLayerConductance, effOpenness);

        ASSERT_TRUE(shadeLayer != nullptr);

        auto gapThickness = 0.0127;
        auto gap1 = Tarcog::ISO15099::Layers::gap(gapThickness);
        ASSERT_TRUE(gap1 != nullptr);

        auto gap2 = Tarcog::ISO15099::Layers::gap(gapThickness);
        ASSERT_TRUE(gap2 != nullptr);

        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayers({layer1, gap1, shadeLayer, gap2, layer2});

        // Alternative way of adding layers.
        // aIGU.addLayer(layer1);
        // aIGU.addLayer(gap1);
        // aIGU.addLayer(shadeLayer);
        // aIGU.addLayer(gap2);
        // aIGU.addLayer(layer2);

        /////////////////////////////////////////////////////////
        /// System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::make_shared<Tarcog::ISO15099::CSingleSystem>(aIGU, Indoor, Outdoor);
        ASSERT_TRUE(m_TarcogSystem != nullptr);
    }

public:
    std::shared_ptr<Tarcog::ISO15099::CBaseIGULayer> GetGap1() const
    {
        return m_TarcogSystem->getGapLayers()[0];
    };

    std::shared_ptr<Tarcog::ISO15099::CBaseIGULayer> GetGap2() const
    {
        return m_TarcogSystem->getGapLayers()[1];
    };
};

TEST_F(TestGapLayerInBetweenVentilation, VentilationFlow)
{
    SCOPED_TRACE("Begin Test: Test Ventilated Gap Layer - Intial Airflow");

    auto aLayer = GetGap1();

    // Airflow iterations are set to 1e-4 and it cannot exceed that precision

    ASSERT_TRUE(aLayer != nullptr);
    auto gainEnergy = aLayer->getGainFlow();
    EXPECT_NEAR(32.414571203538848, gainEnergy, 1e-4);

    aLayer = GetGap2();
    ASSERT_TRUE(aLayer != nullptr);
    gainEnergy = aLayer->getGainFlow();
    EXPECT_NEAR(-32.414571203538848, gainEnergy, 1e-4);
}
