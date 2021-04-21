#include <memory>
#include <stdexcept>
#include <string>
#include <gtest/gtest.h>

#include "WCEGases.hpp"
#include "WCETarcog.hpp"

class TestInBetweenShadeAirArgon : public testing::Test
{
private:
    std::unique_ptr<Tarcog::ISO15099::CSingleSystem> m_TarcogSystem;

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

        // Solid layers
        auto solidLayerThickness = 0.005715;   // [m]
        auto solidLayerConductance = 1.0;

        auto layer1 = Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        ASSERT_TRUE(layer1 != nullptr);

        auto layer3 = Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        ASSERT_TRUE(layer3 != nullptr);

        auto shadeLayerThickness = 0.01;
        auto shadeLayerConductance = 160.0;
        auto Atop = 0.1;
        auto Abot = 0.1;
        auto Aleft = 0.1;
        auto Aright = 0.1;
        auto Afront = 0.2;

        EffectiveLayers::ShadeOpenness openness{Afront, Aleft, Aright, Atop, Abot};

        double windowWidth = 1;
        double windowHeight = 1;

        EffectiveLayers::EffectiveLayerOther effectiveLayer{
          windowWidth, windowHeight, shadeLayerThickness, openness};

        EffectiveLayers::EffectiveOpenness effOpenness{effectiveLayer.getEffectiveOpenness()};

        auto layer2 = Tarcog::ISO15099::Layers::shading(
            shadeLayerThickness, shadeLayerConductance, effOpenness);

        ASSERT_TRUE(layer2 != nullptr);

        // gap layers

        // Create coefficients for Air
        Gases::CIntCoeff AirCon{2.8733e-03, 7.76e-05, 0.0};
        Gases::CIntCoeff AirCp{1.002737e+03, 1.2324e-02, 0.0};
        Gases::CIntCoeff AirVisc{3.7233e-06, 4.94e-08, 0.0};

        Gases::CGasData AirData{"Air", 28.97, 1.4, AirCp, AirCon, AirVisc};

        // Create coefficients for Argon
        Gases::CIntCoeff ArgonCon{2.2848e-03, 5.1486e-05, 0.0};
        Gases::CIntCoeff ArgonCp{5.21929e+02, 0.0, 0.0};
        Gases::CIntCoeff ArgonVisc{3.3786e-06, 6.4514e-08, 0.0};

        Gases::CGasData ArgonData{"Argon", 39.948, 1.67, ArgonCp, ArgonCon, ArgonVisc};

        // Create gas mixture
        Gases::CGas Gas1({{0.1, AirData}, {0.9, ArgonData}});

        auto gapThickness = 0.0127;
        auto gap1 = Tarcog::ISO15099::Layers::gap(gapThickness, Gas1);
        ASSERT_TRUE(gap1 != nullptr);

        auto gap2 = Tarcog::ISO15099::Layers::gap(gapThickness, Gas1);
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
        /// System
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

TEST_F(TestInBetweenShadeAirArgon, Test1)
{
    SCOPED_TRACE("Begin Test: InBetween Shade - Air(10%)/Argon(90%)");

    auto aSystem = GetSystem();
    ASSERT_TRUE(aSystem != nullptr);

    const auto Temperature = aSystem->getTemperatures();
    std::vector<double> correctTemperature = {
      257.708550, 258.135695, 271.903647, 271.907946, 284.412984, 284.840129};
    ASSERT_EQ(correctTemperature.size(), Temperature.size());

    for(auto i = 0u; i < correctTemperature.size(); ++i)
    {
        EXPECT_NEAR(correctTemperature[i], Temperature[i], 1e-6);
    }

    const auto Radiosity = aSystem->getRadiosities();
    std::vector<double> correctRadiosity = {
      248.512463, 259.761987, 301.877098, 318.341741, 362.563088, 382.346347};
    ASSERT_EQ(correctRadiosity.size(), Radiosity.size());

    for(auto i = 0u; i < correctRadiosity.size(); ++i)
    {
        EXPECT_NEAR(correctRadiosity[i], Radiosity[i], 1e-6);
    }

    auto numOfIter = GetSystem()->getNumberOfIterations();
    EXPECT_EQ(21u, numOfIter);
}
