#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

// Example of single clear system that runs in "EnergyPlus mode", meaning that all of the input
// provided in SetUp() routine is the way EnergyPlus is calling thermal routines.
class TestSingleClearSingleSystem_EPlusMode : public testing::Test
{
private:
    std::unique_ptr<Tarcog::ISO15099::CSingleSystem> m_TarcogSystem;

protected:
    void SetUp() override
    {
        /////////////////////////////////////////////////////////
        /// Outdoor
        /////////////////////////////////////////////////////////
        auto airTemperature = 252.0484;   // Kelvins
        auto pressure = 99100.0;          // Pascals
        auto airSpeed = 4.2967;           // meters per second
        auto tSky = 231.2005;             // Kelvins
        auto direction = Tarcog::ISO15099::AirHorizontalDirection::Windward;
        auto solarRadiation = 0.0;
        auto fclr = 1.0;

        auto Outdoor =
          Tarcog::ISO15099::Environments::outdoor(airTemperature,
                                                  airSpeed,
                                                  solarRadiation,
                                                  tSky,
                                                  Tarcog::ISO15099::SkyModel::AllSpecified,
                                                  pressure,
                                                  direction,
                                                  fclr);
        ASSERT_TRUE(Outdoor != nullptr);

        auto hcout = 21.8733;
        Outdoor->setHCoeffModel(Tarcog::ISO15099::BoundaryConditionsCoeffModel::HcPrescribed,
                                hcout);

        auto IR = 205.1969;
        Outdoor->setEnvironmentIR(IR);

        /////////////////////////////////////////////////////////
        /// Indoor
        /////////////////////////////////////////////////////////

        auto roomTemperature = 294.15;
        auto Indoor = Tarcog::ISO15099::Environments::indoor(roomTemperature, pressure);
        ASSERT_TRUE(Indoor != nullptr);

        auto hcin = 2.6262;
        Indoor->setHCoeffModel(Tarcog::ISO15099::BoundaryConditionsCoeffModel::CalculateH, hcin);

        IR = 389.8318;
        Indoor->setEnvironmentIR(IR);

        /////////////////////////////////////////////////////////
        // IGU
        /////////////////////////////////////////////////////////
        auto solidLayerThickness = 0.003048;   // [m]
        auto solidLayerConductance = 1.0;

        auto aSolidLayer =
          Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        ASSERT_TRUE(aSolidLayer != nullptr);

        auto windowWidth = 2.7130375;
        auto windowHeight = 3.02895;
        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayer(aSolidLayer);

        /////////////////////////////////////////////////////////
        // System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::unique_ptr<Tarcog::ISO15099::CSingleSystem>(
          new Tarcog::ISO15099::CSingleSystem(aIGU, Indoor, Outdoor));
        ASSERT_TRUE(m_TarcogSystem != nullptr);

        m_TarcogSystem->solve();
    }

public:
    Tarcog::ISO15099::CSingleSystem * GetSystem() const
    {
        return m_TarcogSystem.get();
    };
};

TEST_F(TestSingleClearSingleSystem_EPlusMode, Test1)
{
    SCOPED_TRACE("Begin Test: Single Clear - U-value");

    auto aSystem = GetSystem();
    ASSERT_TRUE(aSystem != nullptr);

    const auto Temperature = aSystem->getTemperatures();
    std::vector<double> correctTemperature = {259.293476, 259.907311};
    ASSERT_EQ(correctTemperature.size(), Temperature.size());

    for(auto i = 0u; i < correctTemperature.size(); ++i)
    {
        EXPECT_NEAR(correctTemperature[i], Temperature[i], 1e-5);
    }

    const auto Radiosity = aSystem->getRadiosities();
    std::vector<double> correctRadiosity = {248.112516, 279.699920};
    ASSERT_EQ(correctRadiosity.size(), Radiosity.size());

    for(auto i = 0u; i < correctRadiosity.size(); ++i)
    {
        EXPECT_NEAR(correctRadiosity[i], Radiosity[i], 1e-5);
    }

    const auto isToleranceAchieved = aSystem->isToleranceAchieved();
    EXPECT_EQ(isToleranceAchieved, true);

    const auto solutionTolerance = aSystem->solutionTolarance();
    EXPECT_NEAR(9.264811e-07, solutionTolerance, 1e-10);
}
