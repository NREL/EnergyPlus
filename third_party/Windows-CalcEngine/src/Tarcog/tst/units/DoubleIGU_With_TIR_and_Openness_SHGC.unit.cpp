#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

class DoubleIGU_With_TIR_and_Openness_SHGC : public testing::Test
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
        // IGU
        /////////////////////////////////////////////////////////
        auto solidLayerThickness = 0.003048;   // [m]
        auto solidLayerConductance = 1.0;

        auto layer1 = Tarcog::ISO15099::Layers::shading(solidLayerThickness, solidLayerConductance);
        ASSERT_TRUE(layer1 != nullptr);

        auto shadeLayerThickness = 0.001;
        auto shadeLayerConductance = 0.15;
        auto emissivity = 0.796259999275;
        auto tir = 0.10916;
        auto dtop = 0.0;
        auto dbot = 0.0;
        auto dleft = 0.0;
        auto dright = 0.0;
        auto Afront = 0.049855;

        EffectiveLayers::ShadeOpenness openness{Afront, dleft, dright, dtop, dbot};

        double windowWidth = 1;
        double windowHeight = 1;

        EffectiveLayers::EffectiveLayerBSDF effectiveLayer{
          windowWidth, windowHeight, shadeLayerThickness, openness};

        EffectiveLayers::EffectiveOpenness effOpenness{effectiveLayer.getEffectiveOpenness()};

        auto layer2 = Tarcog::ISO15099::Layers::shading(shadeLayerThickness,
                                                        shadeLayerConductance,
                                                        effOpenness,
                                                        emissivity,
                                                        tir,
                                                        emissivity,
                                                        tir);

        ASSERT_TRUE(layer2 != nullptr);

        auto gapThickness = 0.0127;
        auto gap1 = Tarcog::ISO15099::Layers::gap(gapThickness);
        ASSERT_TRUE(gap1 != nullptr);

        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayers({layer1, gap1, layer2});

        /////////////////////////////////////////////////////////
        // System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::make_shared<Tarcog::ISO15099::CSystem>(aIGU, Indoor, Outdoor);
        ASSERT_TRUE(m_TarcogSystem != nullptr);

        m_TarcogSystem->setAbsorptances({0.160476297140, 0.167158290744});
    }

public:
    std::shared_ptr<Tarcog::ISO15099::CSystem> getSystem() const
    {
        return m_TarcogSystem;
    };
};

TEST_F(DoubleIGU_With_TIR_and_Openness_SHGC, Test1)
{
    SCOPED_TRACE("Begin Test: Indoor Shade");

    auto aSystem = getSystem();

    auto temperature = aSystem->getTemperatures(Tarcog::ISO15099::System::SHGC);
    auto radiosity = aSystem->getRadiosities(Tarcog::ISO15099::System::SHGC);

    std::vector<double> correctTemp = {
      311.16511198886826, 311.35084358843835, 312.56143334708418, 312.18578373706208};
    std::vector<double> correctJ = {
      525.13728201703634, 532.26392522289359, 529.47497000248063, 528.72274402817823};

    EXPECT_EQ(correctTemp.size(), temperature.size());
    EXPECT_EQ(correctJ.size(), radiosity.size());

    for(size_t i = 0; i < temperature.size(); ++i)
    {
        EXPECT_NEAR(correctTemp[i], temperature[i], 1e-6);
        EXPECT_NEAR(correctJ[i], radiosity[i], 1e-6);
    }

    const auto numOfIter = aSystem->getNumberOfIterations(Tarcog::ISO15099::System::SHGC);
    EXPECT_EQ(1, int(numOfIter));

    const auto uValue = aSystem->getUValue();
    EXPECT_NEAR(3.2151172563457391, uValue, 1e-6);

    const auto Ttot_sol{0.119033947587};
    const auto shgc = aSystem->getSHGC(Ttot_sol);
    EXPECT_NEAR(0.25575797028047931, shgc, 1e-6);
}
