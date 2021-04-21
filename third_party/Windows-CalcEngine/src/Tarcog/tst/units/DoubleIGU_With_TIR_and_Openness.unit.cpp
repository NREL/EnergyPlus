#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

class DoubleIGU_With_TIR_and_Openness : public testing::Test
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
        auto airSpeed = 5.5;            // meters per second
        auto tSky = 255.15;             // Kelvins
        auto solarRadiation = 0.0;

        auto Outdoor = Tarcog::ISO15099::Environments::outdoor(
          airTemperature, airSpeed, solarRadiation, tSky, Tarcog::ISO15099::SkyModel::AllSpecified);
        ASSERT_TRUE(Outdoor != nullptr);
        Outdoor->setHCoeffModel(Tarcog::ISO15099::BoundaryConditionsCoeffModel::CalculateH);

        /////////////////////////////////////////////////////////
        // Indoor
        /////////////////////////////////////////////////////////

        auto roomTemperature = 294.15;

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

        auto layer2 = Tarcog::ISO15099::Layers::shading(
            shadeLayerThickness, shadeLayerConductance, effOpenness,
            emissivity, tir, emissivity, tir);

        ASSERT_TRUE(layer2 != nullptr);        

        auto gapThickness = 0.0127;
        auto gap1 = Tarcog::ISO15099::Layers::gap(gapThickness);
        ASSERT_TRUE(gap1 != nullptr);

        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayers({layer1, gap1, layer2});

        /////////////////////////////////////////////////////////
        // System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::make_shared<Tarcog::ISO15099::CSingleSystem>(aIGU, Indoor, Outdoor);
        ASSERT_TRUE(m_TarcogSystem != nullptr);

        m_TarcogSystem->solve();
    }

public:
    std::shared_ptr<Tarcog::ISO15099::CSingleSystem> getSystem() const
    {
        return m_TarcogSystem;
    };
};

TEST_F(DoubleIGU_With_TIR_and_Openness, Test1)
{
    SCOPED_TRACE("Begin Test: Outdoor Shade - Air");

    auto aSystem = getSystem();

    auto temperature = aSystem->getTemperatures();
    auto radiosity = aSystem->getRadiosities();

    std::vector<double> correctTemp = {
      259.333390, 259.706267, 279.738283, 280.415196};
    std::vector<double> correctJ = {
      253.860589, 272.423758, 348.553579, 349.022496};

    EXPECT_EQ(correctTemp.size(), temperature.size());
    EXPECT_EQ(correctJ.size(), radiosity.size());

    for(size_t i = 0; i < temperature.size(); ++i)
    {
        EXPECT_NEAR(correctTemp[i], temperature[i], 1e-6);
        EXPECT_NEAR(correctJ[i], radiosity[i], 1e-6);
    }

    const auto numOfIter = aSystem->getNumberOfIterations();
    EXPECT_EQ(20, int(numOfIter));

    const auto ventilatedFlow = aSystem->getVentilationFlow(Tarcog::ISO15099::Environment::Outdoor);
    EXPECT_NEAR(8.433710, ventilatedFlow, 1e-6);

    const auto uValue = aSystem->getUValue();
    EXPECT_NEAR(3.136796, uValue, 1e-6);
}
