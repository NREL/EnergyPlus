#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

// Example of double clear window with inital guess for solution
class TestDoubleLoweUValueEnvironment : public testing::Test
{
private:
    std::shared_ptr<Tarcog::ISO15099::CSystem> m_TarcogSystem;

protected:
    void SetUp() override
    {
        /////////////////////////////////////////////////////////
        /// Outdoor
        /////////////////////////////////////////////////////////
        const auto airTemperature{255.15};   // Kelvins
        const auto airSpeed{5.5};            // meters per second
        const auto tSky{255.15};             // Kelvins
        const auto solarRadiation{0.0};

        auto Outdoor = Tarcog::ISO15099::Environments::outdoor(
          airTemperature, airSpeed, solarRadiation, tSky, Tarcog::ISO15099::SkyModel::AllSpecified);
        ASSERT_TRUE(Outdoor != nullptr);
        Outdoor->setHCoeffModel(Tarcog::ISO15099::BoundaryConditionsCoeffModel::CalculateH);

        /////////////////////////////////////////////////////////
        /// Indoor
        /////////////////////////////////////////////////////////

        const auto roomTemperature{294.15};
        const auto Indoor = Tarcog::ISO15099::Environments::indoor(roomTemperature);
        ASSERT_TRUE(Indoor != nullptr);

        /////////////////////////////////////////////////////////
        // IGU
        /////////////////////////////////////////////////////////
        const auto solidLayerThickness1{0.00318};   // [m]
        const auto solidLayerConductance1{1.0};
        const auto tIR1{0.0};
        const auto frontEmissivity1{0.84};
        const auto backEmissivity1{0.046578168869};

        const auto layer1 = Tarcog::ISO15099::Layers::solid(solidLayerThickness1,
                                                            solidLayerConductance1,
                                                            frontEmissivity1,
                                                            tIR1,
                                                            backEmissivity1,
                                                            tIR1);
        ASSERT_TRUE(layer1 != nullptr);

        const auto gapThickness{0.0127};
        auto gap{Tarcog::ISO15099::Layers::gap(gapThickness)};

        const auto solidLayerThickness2{0.005715};   // [m]
        const auto solidLayerConductance2{1.0};

        const auto layer2 =
          Tarcog::ISO15099::Layers::solid(solidLayerThickness2, solidLayerConductance2);

        const auto iguWidth{1.0};
        const auto iguHeight{1.0};
        Tarcog::ISO15099::CIGU aIGU(iguWidth, iguHeight);
        aIGU.addLayers({layer1, gap, layer2});

        /////////////////////////////////////////////////////////
        /// System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::make_shared<Tarcog::ISO15099::CSystem>(aIGU, Indoor, Outdoor);
        ASSERT_TRUE(m_TarcogSystem != nullptr);
    }

public:
    [[nodiscard]] std::shared_ptr<Tarcog::ISO15099::CSystem> GetSystem() const {
        return m_TarcogSystem;
    };
};

TEST_F(TestDoubleLoweUValueEnvironment, Test1)
{
    SCOPED_TRACE("Begin Test: Double Clear - Surface temperatures");

    auto aSystem = GetSystem();
    ASSERT_TRUE(aSystem != nullptr);

    //////////////////////////////////////////////////////////////////////
    /// UValue run
    //////////////////////////////////////////////////////////////////////

    auto aRun = Tarcog::ISO15099::System::Uvalue;

    auto Temperature = aSystem->getTemperatures(aRun);
    std::vector<double> correctTemperature = {257.398283, 257.607096, 284.597919, 284.973191};
    ASSERT_EQ(correctTemperature.size(), Temperature.size());

    for(auto i = 0u; i < correctTemperature.size(); ++i)
    {
        EXPECT_NEAR(correctTemperature[i], Temperature[i], 1e-5);
    }

    auto SolidLayerConductivities = aSystem->getSolidEffectiveLayerConductivities(aRun);
    std::vector<double> correctSolidConductivities{1, 1};
    EXPECT_EQ(SolidLayerConductivities.size(), correctSolidConductivities.size());

    for(auto i = 0u; i < SolidLayerConductivities.size(); ++i)
    {
        EXPECT_NEAR(correctSolidConductivities[i], SolidLayerConductivities[i], 1e-6);
    }

    auto GapLayerConductivities = aSystem->getGapEffectiveLayerConductivities(aRun);
    std::vector<double> correctGapConductivities{0.030897};
    EXPECT_EQ(GapLayerConductivities.size(), correctGapConductivities.size());

    for(auto i = 0u; i < GapLayerConductivities.size(); ++i)
    {
        EXPECT_NEAR(correctGapConductivities[i], GapLayerConductivities[i], 1e-6);
    }

    auto Radiosity = aSystem->getRadiosities(aRun);
    std::vector<double> correctRadiosity = {247.502661, 365.231909, 370.876831, 382.004324};
    ASSERT_EQ(correctRadiosity.size(), Radiosity.size());
    for(auto i = 0u; i < correctRadiosity.size(); ++i)
    {
        EXPECT_NEAR(correctRadiosity[i], Radiosity[i], 1e-5);
    }

    auto effectiveSystemConductivity{aSystem->getEffectiveSystemConductivity(aRun)};
    EXPECT_NEAR(0.051424, effectiveSystemConductivity, 1e-6);

    auto thickness{aSystem->thickness(aRun)};
    EXPECT_NEAR(0.021595, thickness, 1e-6);

    auto numOfIter = aSystem->getNumberOfIterations(aRun);
    EXPECT_EQ(21, int(numOfIter));

    //////////////////////////////////////////////////////////////////////
    /// General results
    //////////////////////////////////////////////////////////////////////
    const auto Uvalue = aSystem->getUValue();
    EXPECT_NEAR(Uvalue, 1.683701, 1e-5);

    const auto SHGC = aSystem->getSHGC(0.3716);
    EXPECT_NEAR(SHGC, 0.0, 1e-5);
}
