#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

// Example of double clear window with inital guess for solution
class TestDoubleClearSHGCEnvironment : public testing::Test
{
private:
    std::shared_ptr<Tarcog::ISO15099::CSystem> m_TarcogSystem;

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
        auto solidLayerThickness = 0.003048;   // [m]
        auto solidLayerConductance = 1.0;      // [W/m2K]

        auto aSolidLayer1 =
          Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        // aSolidLayer1->setSolarAbsorptance(0.096489921212);

        auto aSolidLayer2 =
          Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        // aSolidLayer2->setSolarAbsorptance(0.072256758809);

        auto gapThickness = 0.0127;
        auto gapLayer = Tarcog::ISO15099::Layers::gap(gapThickness);
        ASSERT_TRUE(gapLayer != nullptr);

        auto windowWidth = 1.0;
        auto windowHeight = 1.0;
        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayers({aSolidLayer1, gapLayer, aSolidLayer2});

        // Alternative way of adding layers.
        // aIGU.addLayer(aSolidLayer1);
        // aIGU.addLayer(gapLayer);
        // aIGU.addLayer(aSolidLayer2);

        /////////////////////////////////////////////////////////
        /// System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::make_shared<Tarcog::ISO15099::CSystem>(aIGU, Indoor, Outdoor);
        ASSERT_TRUE(m_TarcogSystem != nullptr);

        m_TarcogSystem->setAbsorptances({0.096489921212, 0.072256758809});
    }

public:
    std::shared_ptr<Tarcog::ISO15099::CSystem> GetSystem() const
    {
        return m_TarcogSystem;
    };
};

TEST_F(TestDoubleClearSHGCEnvironment, Test1)
{
    SCOPED_TRACE("Begin Test: Double Clear - Surface temperatures");

    auto aSystem = GetSystem();
    ASSERT_TRUE(aSystem != nullptr);

    //////////////////////////////////////////////////////////////////////
    /// UValue run
    //////////////////////////////////////////////////////////////////////

    auto aRun = Tarcog::ISO15099::System::Uvalue;

    auto Temperature = aSystem->getTemperatures(aRun);
    std::vector<double> correctTemperature = {304.025047, 303.955156, 300.484758, 300.414867};
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
    std::vector<double> correctGapConductivities{0.083913};
    EXPECT_EQ(GapLayerConductivities.size(), correctGapConductivities.size());

    for(auto i = 0u; i < GapLayerConductivities.size(); ++i)
    {
        EXPECT_NEAR(correctGapConductivities[i], GapLayerConductivities[i], 1e-6);
    }

    auto Radiosity = aSystem->getRadiosities(aRun);
    std::vector<double> correctRadiosity = {485.546128, 480.950664, 465.217923, 458.631346};
    ASSERT_EQ(correctRadiosity.size(), Radiosity.size());
    for(auto i = 0u; i < correctRadiosity.size(); ++i)
    {
        EXPECT_NEAR(correctRadiosity[i], Radiosity[i], 1e-5);
    }

    auto effectiveSystemConductivity{aSystem->getEffectiveSystemConductivity(aRun)};
    EXPECT_NEAR(0.119383, effectiveSystemConductivity, 1e-6);

    auto thickness{aSystem->thickness(aRun)};
    EXPECT_NEAR(0.018796, thickness, 1e-6);

    auto numOfIter = aSystem->getNumberOfIterations(aRun);
    EXPECT_EQ(1u, numOfIter);

    //////////////////////////////////////////////////////////////////////
    /// SHGC run
    //////////////////////////////////////////////////////////////////////

    aRun = Tarcog::ISO15099::System::SHGC;

    Temperature = aSystem->getTemperatures(aRun);
    correctTemperature = {308.185604, 308.260088, 306.318284, 306.191403};
    ASSERT_EQ(correctTemperature.size(), Temperature.size());

    for(auto i = 0u; i < correctTemperature.size(); ++i)
    {
        EXPECT_NEAR(correctTemperature[i], Temperature[i], 1e-5);
    }

    SolidLayerConductivities = aSystem->getSolidEffectiveLayerConductivities(aRun);
    correctSolidConductivities = {1, 1};
    EXPECT_EQ(SolidLayerConductivities.size(), correctSolidConductivities.size());

    for(auto i = 0u; i < SolidLayerConductivities.size(); ++i)
    {
        EXPECT_NEAR(correctSolidConductivities[i], SolidLayerConductivities[i], 1e-6);
    }

    GapLayerConductivities = aSystem->getGapEffectiveLayerConductivities(aRun);
    correctGapConductivities = {0.087241};
    EXPECT_EQ(GapLayerConductivities.size(), correctGapConductivities.size());

    for(auto i = 0u; i < GapLayerConductivities.size(); ++i)
    {
        EXPECT_NEAR(correctGapConductivities[i], GapLayerConductivities[i], 1e-6);
    }

    Radiosity = aSystem->getRadiosities(aRun);
    correctRadiosity = {508.280530, 510.189512, 500.936293, 489.338313};
    ASSERT_EQ(correctRadiosity.size(), Radiosity.size());

    for(auto i = 0u; i < correctRadiosity.size(); ++i)
    {
        EXPECT_NEAR(correctRadiosity[i], Radiosity[i], 1e-5);
    }

    effectiveSystemConductivity = aSystem->getEffectiveSystemConductivity(aRun);
    EXPECT_NEAR(0.658981, effectiveSystemConductivity, 1e-6);

    thickness = aSystem->thickness(aRun);
    EXPECT_NEAR(0.018796, thickness, 1e-6);

    numOfIter = aSystem->getNumberOfIterations(aRun);
    EXPECT_EQ(1, int(numOfIter));

    //////////////////////////////////////////////////////////////////////
    /// General results
    //////////////////////////////////////////////////////////////////////
    auto Uvalue = aSystem->getUValue();
    EXPECT_NEAR(Uvalue, 2.866261, 1e-5);

    auto SHGC = aSystem->getSHGC(0.703296);
    EXPECT_NEAR(SHGC, 0.763304, 1e-5);

    auto relativeHeatGain = aSystem->relativeHeatGain(0.703296);
    EXPECT_NEAR(relativeHeatGain, 575.826177, 1e-5);
}
