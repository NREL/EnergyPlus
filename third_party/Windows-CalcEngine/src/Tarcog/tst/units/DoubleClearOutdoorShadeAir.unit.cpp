#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

class TestDoubleClearOutdoorShadeAir : public testing::Test
{
private:
    std::shared_ptr<Tarcog::ISO15099::CSingleSystem> m_TarcogSystem;

protected:
    void SetUp() override
    {
        /////////////////////////////////////////////////////////
        // Outdoor
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
        // Indoor
        /////////////////////////////////////////////////////////

        auto roomTemperature = 295.15;

        auto Indoor = Tarcog::ISO15099::Environments::indoor(roomTemperature);
        ASSERT_TRUE(Indoor != nullptr);

        /////////////////////////////////////////////////////////
        // IGU
        /////////////////////////////////////////////////////////
        auto solidLayerThickness = 0.005715;   // [m]
        auto solidLayerConductance = 1.0;

        auto shadeLayerThickness = 0.01;
        auto shadeLayerConductance = 160.0;
        auto dtop = 0.1;
        auto dbot = 0.1;
        auto dleft = 0.1;
        auto dright = 0.1;
        auto Afront = 0.2;

        EffectiveLayers::ShadeOpenness openness{Afront, dleft, dright, dtop, dbot};

        double windowWidth = 1;
        double windowHeight = 1;

        EffectiveLayers::EffectiveLayerOther effectiveLayer{
          windowWidth, windowHeight, shadeLayerThickness, openness};

        EffectiveLayers::EffectiveOpenness effOpenness{effectiveLayer.getEffectiveOpenness()};

        auto layer1 = Tarcog::ISO15099::Layers::shading(
            shadeLayerThickness, shadeLayerConductance, effOpenness);

        ASSERT_TRUE(layer1 != nullptr);

        auto layer2 = Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        ASSERT_TRUE(layer2 != nullptr);

        auto layer3 = Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);

        auto gapThickness = 0.0127;
        auto gap1 = Tarcog::ISO15099::Layers::gap(gapThickness);
        ASSERT_TRUE(gap1 != nullptr);

        auto gap2 = Tarcog::ISO15099::Layers::gap(gapThickness);
        ASSERT_TRUE(gap2 != nullptr);

        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayers({layer1, gap1, layer2, gap2, layer3});

        // Alternative way of adding layers.
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
    std::shared_ptr<Tarcog::ISO15099::CSingleSystem> getSystem() const
    {
        return m_TarcogSystem;
    };
};

TEST_F(TestDoubleClearOutdoorShadeAir, Test1)
{
    SCOPED_TRACE("Begin Test: Outdoor Shade - Air");

    auto aSystem = getSystem();

    auto temperature = aSystem->getTemperatures();
    auto radiosity = aSystem->getRadiosities();

    std::vector<double> correctTemp = {
      256.984067, 256.988250, 269.437058, 269.879893, 284.039251, 284.482085};
    std::vector<double> correctJ = {
      246.160219, 254.398900, 291.700909, 310.191145, 359.624235, 380.773015};

    EXPECT_EQ(correctTemp.size(), temperature.size());
    EXPECT_EQ(correctJ.size(), radiosity.size());

    for(size_t i = 0; i < temperature.size(); ++i)
    {
        EXPECT_NEAR(correctTemp[i], temperature[i], 1e-6);
        EXPECT_NEAR(correctJ[i], radiosity[i], 1e-6);
    }

    const auto numOfIter = aSystem->getNumberOfIterations();
    EXPECT_EQ(23, int(numOfIter));

    const auto ventilatedFlow = aSystem->getVentilationFlow(Tarcog::ISO15099::Environment::Outdoor);
    EXPECT_NEAR(-23.934154, ventilatedFlow, 1e-6);
}
