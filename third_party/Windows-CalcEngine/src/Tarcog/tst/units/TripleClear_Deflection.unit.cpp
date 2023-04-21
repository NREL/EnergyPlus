#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

// Example of double clear window with inital guess for solution
class TestTripleClearDeflection : public testing::Test
{
private:
    std::shared_ptr<Tarcog::ISO15099::CSystem> m_TarcogSystem;

protected:
    void SetUp() override
    {
        /////////////////////////////////////////////////////////
        /// Outdoor
        /////////////////////////////////////////////////////////
        const auto airTemperature{250};      // Kelvins
        const auto airSpeed{5.5};            // meters per second
        const auto tSky{255.15};             // Kelvins
        const auto solarRadiation{783.0};

        auto Outdoor = Tarcog::ISO15099::Environments::outdoor(
          airTemperature, airSpeed, solarRadiation, tSky, Tarcog::ISO15099::SkyModel::AllSpecified);
        ASSERT_TRUE(Outdoor != nullptr);
        Outdoor->setHCoeffModel(Tarcog::ISO15099::BoundaryConditionsCoeffModel::CalculateH);

        /////////////////////////////////////////////////////////
        /// Indoor
        /////////////////////////////////////////////////////////

        const auto roomTemperature{293.0};

        auto Indoor = Tarcog::ISO15099::Environments::indoor(roomTemperature);
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

        auto aSolidLayer3 =
          Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        aSolidLayer3->setSolarAbsorptance(0.058234799653, solarRadiation);

        const auto gapThickness1{0.006};
        const auto gapLayer1 = Tarcog::ISO15099::Layers::gap(gapThickness1);
        ASSERT_TRUE(gapLayer1 != nullptr);

        const auto gapThickness2{0.025};
        const auto gapLayer2 = Tarcog::ISO15099::Layers::gap(gapThickness2);
        ASSERT_TRUE(gapLayer2 != nullptr);

        const auto windowWidth{1.0};
        const auto windowHeight{1.0};
        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayers({aSolidLayer1, gapLayer1, aSolidLayer2, gapLayer2, aSolidLayer3});

        //aIGU.setDeflectionProperties(273, 101325);

        // Alternative way of adding layers.
        // aIGU.addLayer(aSolidLayer1);
        // aIGU.addLayer(gapLayer);
        // aIGU.addLayer(aSolidLayer2);

        /////////////////////////////////////////////////////////
        /// System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::make_shared<Tarcog::ISO15099::CSystem>(aIGU, Indoor, Outdoor);

        m_TarcogSystem->setDeflectionProperties(273, 101325);        
        ASSERT_TRUE(m_TarcogSystem != nullptr);
    }

public:
    [[nodiscard]] std::shared_ptr<Tarcog::ISO15099::CSystem> GetSystem() const
    {
        return m_TarcogSystem;
    }
};

TEST_F(TestTripleClearDeflection, Test1)
{
    SCOPED_TRACE("Begin Test: Double Clear - Surface temperatures");

    auto aSystem = GetSystem();
    ASSERT_TRUE(aSystem != nullptr);

    //////////////////////////////////////////////////////////////////////
    /// UValue run
    //////////////////////////////////////////////////////////////////////

    auto aRun = Tarcog::ISO15099::System::Uvalue;

    auto Temperature = aSystem->getTemperatures(aRun);
    std::vector<double> correctTemperature = {253.145118, 253.399346, 265.491216, 265.745444, 281.162092, 281.416320};
    ASSERT_EQ(correctTemperature.size(), Temperature.size());

    for(auto i = 0u; i < correctTemperature.size(); ++i)
    {
        EXPECT_NEAR(correctTemperature[i], Temperature[i], 1e-5);
    }

    std::vector<double> correctDeflection = {-0.421986e-3, 0.265021e-3, 0.167762e-3};

    auto deflection = aSystem->getMaxDeflections(Tarcog::ISO15099::System::Uvalue);

    for(auto i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-8);
    }

    auto numOfIter = aSystem->getNumberOfIterations(aRun);
    EXPECT_EQ(20u, numOfIter);

    //////////////////////////////////////////////////////////////////////
    /// SHGC run
    //////////////////////////////////////////////////////////////////////

    aRun = Tarcog::ISO15099::System::SHGC;

    Temperature = aSystem->getTemperatures(aRun);
    correctTemperature = {257.435790, 257.952362, 276.186799, 276.492794, 289.163055, 289.308119};
    ASSERT_EQ(correctTemperature.size(), Temperature.size());

    for(auto i = 0u; i < correctTemperature.size(); ++i)
    {
        EXPECT_NEAR(correctTemperature[i], Temperature[i], 1e-5);
    }

    correctDeflection = {-0.421986e-3, 0.265021e-3, 0.167762e-3};

    deflection = aSystem->getMaxDeflections(Tarcog::ISO15099::System::Uvalue);

    for(auto i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-8);
    }

    numOfIter = aSystem->getNumberOfIterations(aRun);
    EXPECT_EQ(21u, numOfIter);

    //////////////////////////////////////////////////////////////////////
    /// General results
    //////////////////////////////////////////////////////////////////////
    const auto Uvalue = aSystem->getUValue();
    EXPECT_NEAR(Uvalue, 1.9522982371191091, 1e-5);

    const auto SHGC = aSystem->getSHGC(0.598424255848);
    EXPECT_NEAR(SHGC, 0.673282, 1e-5);

    const auto relativeHeatGain = aSystem->relativeHeatGain(0.703296);
    EXPECT_NEAR(relativeHeatGain, 579.484762, 1e-5);
}
