#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

class TestTemperatureInitialGuess : public testing::Test
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
        auto tSky = airTemperature;
        auto airSpeed = 5.5;   // meters per second
        auto solarRadiation = 0.0;

        auto Outdoor = Tarcog::ISO15099::Environments::outdoor(
          airTemperature, airSpeed, solarRadiation, tSky, Tarcog::ISO15099::SkyModel::AllSpecified);
        ASSERT_TRUE(Outdoor != nullptr);

        /////////////////////////////////////////////////////////
        /// Indoor
        /////////////////////////////////////////////////////////
        auto roomTemperature = 294.15;
        auto Indoor = Tarcog::ISO15099::Environments::indoor(roomTemperature);
        ASSERT_TRUE(Indoor != nullptr);

        /////////////////////////////////////////////////////////
        /// IGU
        /////////////////////////////////////////////////////////
        auto solidLayerThickness = 0.005715;   // [m]
        auto solidLayerConductance = 1.0;
        auto solidLayer1 =
          Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        ASSERT_TRUE(solidLayer1 != nullptr);
        auto solidLayer2 =
          Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        ASSERT_TRUE(solidLayer2 != nullptr);

        auto gapThickness = 0.012;
        auto gap = Tarcog::ISO15099::Layers::gap(gapThickness);
        ASSERT_TRUE(gap != nullptr);

        auto windowWidth = 1.0;
        auto windowHeight = 1.0;
        Tarcog::ISO15099::CIGU aTarIGU(windowWidth, windowHeight);
        aTarIGU.addLayers({solidLayer1, gap, solidLayer2});

        // Alternative way of adding layers.
        // aTarIGU.addLayer(solidLayer1);
        // aTarIGU.addLayer(gap);
        // aTarIGU.addLayer(solidLayer2);

        /////////////////////////////////////////////////////////
        // System
        /////////////////////////////////////////////////////////
        m_TarcogSystem =
          std::make_shared<Tarcog::ISO15099::CSingleSystem>(aTarIGU, Indoor, Outdoor);
        ASSERT_TRUE(m_TarcogSystem != nullptr);
    }

public:
    std::shared_ptr<Tarcog::ISO15099::CIGUSolidLayer> getLayer1() const
    {
        return m_TarcogSystem->getSolidLayers()[0];
    }

    std::shared_ptr<Tarcog::ISO15099::CIGUSolidLayer> getLayer2() const
    {
        return m_TarcogSystem->getSolidLayers()[1];
    }
};

TEST_F(TestTemperatureInitialGuess, Test1)
{
    SCOPED_TRACE("Begin Test: Initial temperature and IR guess");

    using FenestrationCommon::Side;

    auto layer = getLayer1();

    auto temperature = layer->getTemperature(Side::Front);
    auto J = layer->J(Side::Front);
    EXPECT_NEAR(256.282733081615, temperature, 1e-6);
    EXPECT_NEAR(244.589307222020, J, 1e-6);

    temperature = layer->getTemperature(Side::Back);
    J = layer->J(Side::Back);
    EXPECT_NEAR(262.756302643044, temperature, 1e-6);
    EXPECT_NEAR(270.254322031419, J, 1e-6);

    layer = getLayer2();
    temperature = layer->getTemperature(Side::Front);
    J = layer->J(Side::Front);
    EXPECT_NEAR(276.349099622422, temperature, 1e-6);
    EXPECT_NEAR(330.668096601357, J, 1e-6);

    temperature = layer->getTemperature(Side::Back);
    J = layer->J(Side::Back);
    EXPECT_NEAR(282.822669183851, temperature, 1e-6);
    EXPECT_NEAR(362.757956247504, J, 1e-6);
}
