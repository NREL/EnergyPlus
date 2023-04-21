#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

class TestDoubleClearSingleSystemWithSun : public testing::Test
{
private:
    std::shared_ptr<Tarcog::ISO15099::CSingleSystem> m_TarcogSystem;

protected:
    void SetUp() override
    {
        /////////////////////////////////////////////////////////
        /// Outdoor
        /////////////////////////////////////////////////////////
        auto airTemperature = 305.15;   // Kelvins
        auto airSpeed = 2.75;           // meters per second
        auto tSky = 305.15;             // Kelvins
        auto solarRadiation = 783.0;

        auto Outdoor = Tarcog::ISO15099::Environments::outdoor(
          airTemperature, airSpeed, solarRadiation, tSky, Tarcog::ISO15099::SkyModel::AllSpecified);
        ASSERT_TRUE(Outdoor != nullptr);
        Outdoor->setHCoeffModel(Tarcog::ISO15099::BoundaryConditionsCoeffModel::CalculateH);

        /////////////////////////////////////////////////////////
        /// Indoor
        /////////////////////////////////////////////////////////

        auto roomTemperature = 297.15;

        auto Indoor = Tarcog::ISO15099::Environments::indoor(roomTemperature);
        ASSERT_TRUE(Indoor != nullptr);

        /////////////////////////////////////////////////////////
        /// IGU
        /////////////////////////////////////////////////////////
        auto solidLayerThickness = 0.005715;   // [m]
        auto solidLayerConductance = 1.0;
        auto solarAbsorptance = 0.187443971634;

        auto layer1 = Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        layer1->setSolarAbsorptance(solarAbsorptance, solarRadiation);

        solarAbsorptance = 0.054178960621;
        auto layer2 = Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        layer2->setSolarAbsorptance(solarAbsorptance, solarRadiation);

        auto gapThickness = 0.012;
        auto m_GapLayer = Tarcog::ISO15099::Layers::gap(gapThickness);
        ASSERT_TRUE(m_GapLayer != nullptr);

        double windowWidth = 1;
        double windowHeight = 1;
        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayers({layer1, m_GapLayer, layer2});

        // Alternative way to add layers.
        // aIGU.addLayer(layer1);
        // aIGU.addLayer(m_GapLayer);
        // aIGU.addLayer(layer2);

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

TEST_F(TestDoubleClearSingleSystemWithSun, Test1)
{
    SCOPED_TRACE("Begin Test: Double Clear Single System - Surface temperatures");

    auto aSystem = GetSystem();
    ASSERT_TRUE(aSystem != nullptr);

    auto Temperature = aSystem->getTemperatures();
    std::vector<double> correctTemperature = {310.818074, 311.064868, 306.799522, 306.505704};
    ASSERT_EQ(correctTemperature.size(), Temperature.size());

    for(auto i = 0u; i < correctTemperature.size(); ++i)
    {
        EXPECT_NEAR(correctTemperature[i], Temperature[i], 1e-5);
    }

    auto Radiosity = aSystem->getRadiosities();
    std::vector<double> correctRadiosity = {523.148794, 526.906252, 506.252171, 491.059753};
    ASSERT_EQ(correctRadiosity.size(), Radiosity.size());

    for(auto i = 0u; i < correctRadiosity.size(); ++i)
    {
        EXPECT_NEAR(correctRadiosity[i], Radiosity[i], 1e-5);
    }

    const auto heatFlow = aSystem->getHeatFlow(Tarcog::ISO15099::Environment::Indoor);
    EXPECT_NEAR(-72.622787, heatFlow, 1e-5);

    const auto Uvalue = aSystem->getUValue();
    EXPECT_NEAR(9.077848, Uvalue, 1e-5);

    const auto numOfIter = aSystem->getNumberOfIterations();
    EXPECT_EQ(20u, numOfIter);
}
