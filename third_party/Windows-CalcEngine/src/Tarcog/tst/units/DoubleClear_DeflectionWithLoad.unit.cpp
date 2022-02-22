#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

class TestDoubleClearDeflectionWithLoad : public testing::Test
{
private:
    std::shared_ptr<Tarcog::ISO15099::CSystem> m_TarcogSystem;

protected:
    void SetUp() override
    {
        /////////////////////////////////////////////////////////
        /// Outdoor
        /////////////////////////////////////////////////////////
        const auto airTemperature{300};      // Kelvins
        const auto airSpeed{5.5};            // meters per second
        const auto tSky{255.15};             // Kelvins
        const auto solarRadiation{0.0};
        const auto outsidePressure{102000};

        auto Outdoor =
          Tarcog::ISO15099::Environments::outdoor(airTemperature,
                                                  airSpeed,
                                                  solarRadiation,
                                                  tSky,
                                                  Tarcog::ISO15099::SkyModel::AllSpecified,
                                                  outsidePressure);
        ASSERT_TRUE(Outdoor != nullptr);
        Outdoor->setHCoeffModel(Tarcog::ISO15099::BoundaryConditionsCoeffModel::CalculateH);

        /////////////////////////////////////////////////////////
        /// Indoor
        /////////////////////////////////////////////////////////

        const auto roomTemperature{250.0};
        const auto roomPressure{100000};

        auto Indoor = Tarcog::ISO15099::Environments::indoor(roomTemperature, roomPressure);
        ASSERT_TRUE(Indoor != nullptr);

        /////////////////////////////////////////////////////////
        /// IGU
        /////////////////////////////////////////////////////////
        const auto solidLayerThickness{0.003048}; // [m]
        const auto solidLayerConductance{1.0};    // [W/m2K]

        auto aSolidLayer1 =
          Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        aSolidLayer1->setSolarAbsorptance(0.099839858711, solarRadiation);

        auto aSolidLayer2 =
          Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        aSolidLayer2->setSolarAbsorptance(0.076627746224, solarRadiation);

        const auto gapThickness1{0.006};
        const auto gapLayer1 = Tarcog::ISO15099::Layers::gap(gapThickness1);
        ASSERT_TRUE(gapLayer1 != nullptr);

        const auto windowWidth{1.0};
        const auto windowHeight{5.0};
        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayers({aSolidLayer1, gapLayer1, aSolidLayer2});

        /////////////////////////////////////////////////////////
        /// System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::make_shared<Tarcog::ISO15099::CSystem>(aIGU, Indoor, Outdoor);
        m_TarcogSystem->setAppliedLoad({2000, 1000});
        m_TarcogSystem->setDeflectionProperties(320, 102000);
        ASSERT_TRUE(m_TarcogSystem != nullptr);
    }

public:
    [[nodiscard]] std::shared_ptr<Tarcog::ISO15099::CSystem> GetSystem() const
    {
        return m_TarcogSystem;
    }
};

TEST_F(TestDoubleClearDeflectionWithLoad, Test1)
{
    SCOPED_TRACE("Begin Test: Double Clear - Deflection case with loads");

    auto aSystem = GetSystem();
    ASSERT_TRUE(aSystem != nullptr);

    //////////////////////////////////////////////////////////////////////
    /// UValue run
    //////////////////////////////////////////////////////////////////////

    auto aRun = Tarcog::ISO15099::System::Uvalue;

    auto Temperature = aSystem->getTemperatures(aRun);
    std::vector<double> correctTemperature = {292.076937, 291.609964, 272.797101, 272.330129};
    ASSERT_EQ(correctTemperature.size(), Temperature.size());

    for(auto i = 0u; i < correctTemperature.size(); ++i)
    {
        EXPECT_NEAR(correctTemperature[i], Temperature[i], 1e-5);
    }

    std::vector<double> correctDeflection = {-55.488195e-3, -54.644421e-3};

    auto deflection = aSystem->getMaxDeflections(Tarcog::ISO15099::System::Uvalue);

    for(auto i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-8);
    }
}
