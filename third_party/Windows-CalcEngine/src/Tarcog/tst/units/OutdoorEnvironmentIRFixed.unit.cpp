#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"

class TestOutdoorEnvironmentIRFixed : public testing::Test
{
private:
    std::shared_ptr<Tarcog::ISO15099::CEnvironment> Outdoor;
    std::shared_ptr<Tarcog::ISO15099::CSingleSystem> m_TarcogSystem;

protected:
    void SetUp() override
    {
        /////////////////////////////////////////////////////////
        /// Outdoor
        /////////////////////////////////////////////////////////
        auto airTemperature = 300.0;   // Kelvins
        auto tSky = airTemperature;
        auto airSpeed = 5.5;   // meters per second
        auto solarRadiation = 0.0;
        auto IRRadiation = 370.0;   // [ W/m2 ]

        Outdoor = Tarcog::ISO15099::Environments::outdoor(
          airTemperature, airSpeed, solarRadiation, tSky, Tarcog::ISO15099::SkyModel::AllSpecified);
        ASSERT_TRUE(Outdoor != nullptr);
        Outdoor->setEnvironmentIR(IRRadiation);

        /////////////////////////////////////////////////////////
        /// Indoor
        /////////////////////////////////////////////////////////

        auto roomTemperature = 294.15;

        auto Indoor = Tarcog::ISO15099::Environments::indoor(roomTemperature);
        ASSERT_TRUE(Indoor != nullptr);

        /////////////////////////////////////////////////////////
        /// IGU
        /////////////////////////////////////////////////////////
        auto solidLayerThickness = 0.003048;   // [m]
        auto solidLayerConductance = 100.0;

        auto aSolidLayer =
          Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        ASSERT_TRUE(aSolidLayer != nullptr);

        auto windowWidth = 1.0;
        auto windowHeight = 1.0;
        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayer(aSolidLayer);

        /////////////////////////////////////////////////////////
        // System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::make_shared<Tarcog::ISO15099::CSingleSystem>(aIGU, Indoor, Outdoor);
        ASSERT_TRUE(m_TarcogSystem != nullptr);
    }

public:
    std::shared_ptr<Tarcog::ISO15099::CEnvironment> GetOutdoors() const
    {
        return Outdoor;
    };
};

TEST_F(TestOutdoorEnvironmentIRFixed, CalculateIRFixed)
{
    SCOPED_TRACE("Begin Test: Outdoors -> Infrared radiation fixed (user input).");

    auto aOutdoor = GetOutdoors();
    ASSERT_TRUE(aOutdoor != nullptr);

    auto radiosity = aOutdoor->getEnvironmentIR();
    EXPECT_NEAR(370, radiosity, 1e-6);

    auto hc = aOutdoor->getHc();
    EXPECT_NEAR(26, hc, 1e-6);
}
