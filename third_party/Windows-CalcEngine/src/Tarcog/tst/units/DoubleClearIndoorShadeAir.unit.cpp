#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>
#include <string>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

class TestDoubleClearIndoorShadeAir : public testing::Test
{
private:
    std::shared_ptr<Tarcog::ISO15099::CSingleSystem> m_TarcogSystem;

protected:
    void SetUp() override
    {
        /////////////////////////////////////////////////////////
        /// Outdoor
        /////////////////////////////////////////////////////////
        auto airTemperature = 255.15;   // Kelvins
        auto airSpeed = 5.5;            // meters per second
        auto tSky = 255.15;             // Kelvins
        auto solarRadiation = 0.0;

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

        /////////////////////////////////////////////////////////
        /// IGU
        /////////////////////////////////////////////////////////
        auto solidLayerThickness = 0.005715;   // [m]
        auto solidLayerConductance = 1.0;

        auto layer1 = Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        ASSERT_TRUE(layer1 != nullptr);

        auto layer2 = Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);

        auto shadeLayerThickness = 0.01;
        auto shadeLayerConductance = 160.0;
        auto dtop = 0.1;
        auto dbot = 0.1;
        auto dleft = 0.1;
        auto dright = 0.1;
        auto Afront = 0.2;
        EffectiveLayers::ShadeOpenness openness{Afront, dleft, dright, dtop, dbot};

        auto windowWidth = 1.0;
        auto windowHeight = 1.0;

        EffectiveLayers::EffectiveLayerOther effectiveLayer{
          windowWidth, windowHeight, shadeLayerThickness, openness};

        EffectiveLayers::EffectiveOpenness effOpenness{effectiveLayer.getEffectiveOpenness()};

        auto layer3 = Tarcog::ISO15099::Layers::shading(
            shadeLayerThickness, shadeLayerConductance, effOpenness);

        ASSERT_TRUE(layer3 != nullptr);

        auto gapThickness = 0.0127;
        auto gap1 = Tarcog::ISO15099::Layers::gap(gapThickness);
        ASSERT_TRUE(gap1 != nullptr);

        auto gap2 = Tarcog::ISO15099::Layers::gap(gapThickness);
        ASSERT_TRUE(gap2 != nullptr);

        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayers({layer1, gap1, layer2, gap2, layer3});

        // Alternative way to add layers
        // aIGU.addLayer(layer1);
        // aIGU.addLayer(gap1);
        // aIGU.addLayer(layer2);
        // aIGU.addLayer(gap2);
        // aIGU.addLayer(layer3);

        /////////////////////////////////////////////////////////
        // System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::make_shared<Tarcog::ISO15099::CSingleSystem>(aIGU, Indoor, Outdoor);
        ASSERT_TRUE(m_TarcogSystem != nullptr);

        m_TarcogSystem->solve();
    }

public:
    std::shared_ptr<Tarcog::ISO15099::CSingleSystem> GetSystem() const
    {
        return m_TarcogSystem;
    };
};

TEST_F(TestDoubleClearIndoorShadeAir, Test1)
{
    SCOPED_TRACE("Begin Test: Indoor Shade - Air");

    auto aSystem = GetSystem();

    auto temperature = aSystem->getTemperatures();
    auto radiosity = aSystem->getRadiosities();

    std::vector<double> correctTemp = {
      258.226548, 258.740345, 276.199456, 276.713252, 288.115819, 288.119712};
    std::vector<double> correctJ = {
      250.206503, 264.568471, 319.491011, 340.451996, 382.649045, 397.036105};

    EXPECT_EQ(correctTemp.size(), temperature.size());
    EXPECT_EQ(correctJ.size(), radiosity.size());

    for(size_t i = 0; i < temperature.size(); ++i)
    {
        EXPECT_NEAR(correctTemp[i], temperature[i], 1e-6);
        EXPECT_NEAR(correctJ[i], radiosity[i], 1e-6);
    }

    const auto numOfIter = aSystem->getNumberOfIterations();
    EXPECT_EQ(1, int(numOfIter));

    const auto ventilatedFlow = aSystem->getVentilationFlow(Tarcog::ISO15099::Environment::Indoor);
    EXPECT_NEAR(40.068453, ventilatedFlow, 1e-6);
}
