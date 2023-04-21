#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

class DoubleClearDeflectionTPDefault : public testing::Test
{
private:
    std::shared_ptr<Tarcog::ISO15099::CSingleSystem> m_TarcogSystem;

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

        auto roomTemperature = 294.15;

        auto Indoor = Tarcog::ISO15099::Environments::indoor(roomTemperature);
        ASSERT_TRUE(Indoor != nullptr);

        /////////////////////////////////////////////////////////
        /// IGU
        /////////////////////////////////////////////////////////
        auto solidLayerThickness1 = 0.003048;   // [m]
        auto solidLayerThickness2 = 0.005715;
        auto solidLayerConductance = 1.0;

        auto layer1 = Tarcog::ISO15099::Layers::solid(solidLayerThickness1, solidLayerConductance);

        auto layer2 = Tarcog::ISO15099::Layers::solid(solidLayerThickness2, solidLayerConductance);

        auto gapThickness = 0.0127;
        auto gap = Tarcog::ISO15099::Layers::gap(gapThickness);
        ASSERT_TRUE(gap != nullptr);

        double windowWidth = 1;
        double windowHeight = 1;
        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayers({layer1, gap, layer2});

        // Alternative way of adding layers.
        // aIGU.addLayer(layer1);
        // aIGU.addLayer(gap);
        // aIGU.addLayer(layer2);

        // Deflection properties
        auto Tini = 303.15;
        auto Pini = 101325.0;
        aIGU.setDeflectionProperties(Tini, Pini);

        /////////////////////////////////////////////////////////
        /// System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::make_shared<Tarcog::ISO15099::CSingleSystem>(aIGU, Indoor, Outdoor);
        ASSERT_TRUE(m_TarcogSystem != nullptr);

        m_TarcogSystem->solve();
    }

public:
    std::shared_ptr<Tarcog::ISO15099::CSingleSystem> getSystem() const
    {
        return m_TarcogSystem;
    }
};

TEST_F(DoubleClearDeflectionTPDefault, Test1)
{
    SCOPED_TRACE("Begin Test: Double Clear - Calculated Deflection");

    auto aSystem = getSystem();
    ASSERT_TRUE(aSystem != nullptr);

    auto Temperature = aSystem->getTemperatures();
    std::vector<double> correctTemperature = {258.799455, 259.124629, 279.009115, 279.618815};
    ASSERT_EQ(correctTemperature.size(), Temperature.size());

    for(auto i = 0u; i < correctTemperature.size(); ++i)
    {
        EXPECT_NEAR(correctTemperature[i], Temperature[i], 1e-5);
    }

    auto Radiosity = aSystem->getRadiosities();
    std::vector<double> correctRadiosity = {252.092023, 267.753054, 331.451548, 359.055470};
    ASSERT_EQ(correctRadiosity.size(), Radiosity.size());

    for(auto i = 0u; i < correctRadiosity.size(); ++i)
    {
        EXPECT_NEAR(correctRadiosity[i], Radiosity[i], 1e-5);
    }

    const auto MaxDeflection = aSystem->getMaxDeflections();
    std::vector<double> correctMaxDeflection{-2.285903e-3, 0.483756e-3};
    ASSERT_EQ(correctMaxDeflection.size(), MaxDeflection.size());

    for(auto i = 0u; i < correctMaxDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctMaxDeflection[i], MaxDeflection[i], 1e-8);
    }

    const auto MeanDeflection = aSystem->getMeanDeflections();
    std::vector<double> correctMeanDeflection{-0.957652e-3, 0.202669e-3};
    ASSERT_EQ(correctMeanDeflection.size(), MeanDeflection.size());

    for(auto i = 0u; i < correctMaxDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctMeanDeflection[i], MeanDeflection[i], 1e-5);
    }

    auto numOfIter = aSystem->getNumberOfIterations();
    EXPECT_EQ(20u, numOfIter);
}
