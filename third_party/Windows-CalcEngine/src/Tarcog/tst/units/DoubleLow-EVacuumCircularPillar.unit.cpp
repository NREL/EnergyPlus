#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

class DoubleLowEVacuumCircularPillar : public testing::Test
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
        auto solidLayerThickness = 0.004;   // [m]
        auto solidLayerConductance = 1.0;
        auto TransmittanceIR = 0.0;
        auto emissivityFrontIR = 0.84;
        auto emissivityBackIR = 0.036749500781;

        auto layer1 = Tarcog::ISO15099::Layers::solid(solidLayerThickness,
                                                            solidLayerConductance,
                                                            emissivityFrontIR,
                                                            TransmittanceIR,
                                                            emissivityBackIR,
                                                            TransmittanceIR);

        solidLayerThickness = 0.003962399904;
        emissivityBackIR = 0.84;

        auto layer2 = Tarcog::ISO15099::Layers::solid(solidLayerThickness,
                                                            solidLayerConductance,
                                                            emissivityFrontIR,
                                                            TransmittanceIR,
                                                            emissivityBackIR,
                                                            TransmittanceIR);

        auto gapThickness = 0.0001;
        auto gapPressure = 0.1333;
        auto aGapLayer = Tarcog::ISO15099::Layers::gap(gapThickness, gapPressure);

        // Add support pillars
        auto pillarConductivity = 999.0;
        auto pillarSpacing = 0.03;
        auto pillarRadius = 0.0002;
        auto pillarGap = Tarcog::ISO15099::Layers::addCircularPillar(
          aGapLayer, pillarConductivity, pillarSpacing, pillarRadius);

        ASSERT_TRUE(pillarGap != nullptr);

        auto windowWidth = 1.0;   //[m]
        auto windowHeight = 1.0;
        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayers({layer1, pillarGap, layer2});

        // Alternative way to add layers.
        // aIGU.addLayer(layer1);
        // aIGU.addLayer(pillarGap);
        // aIGU.addLayer(layer2);

        /////////////////////////////////////////////////////////
        /// System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::make_shared<Tarcog::ISO15099::CSingleSystem>(aIGU, Indoor, Outdoor);
        ASSERT_TRUE(m_TarcogSystem != nullptr);

        m_TarcogSystem->solve();
    }

public:
    std::shared_ptr<Tarcog::ISO15099::CSingleSystem> GetSystem() const
    {
        return m_TarcogSystem;
    };
};

TEST_F(DoubleLowEVacuumCircularPillar, Test1)
{
    SCOPED_TRACE("Begin Test: Double Low-E - vacuum with circular pillar support");

    auto aSystem = GetSystem();

    ASSERT_TRUE(aSystem != nullptr);

    const auto Temperature = aSystem->getTemperatures();
    std::vector<double> correctTemperature = {255.997063, 256.095933, 290.398479, 290.496419};
    ASSERT_EQ(correctTemperature.size(), Temperature.size());

    for(auto i = 0u; i < correctTemperature.size(); ++i)
    {
        EXPECT_NEAR(correctTemperature[i], Temperature[i], 1e-5);
    }

    const auto Radiosity = aSystem->getRadiosities();
    std::vector<double> correctRadiosity = {242.987484, 396.293176, 402.108090, 407.071738};
    ASSERT_EQ(correctRadiosity.size(), Radiosity.size());

    for(auto i = 0u; i < correctRadiosity.size(); ++i)
    {
        EXPECT_NEAR(correctRadiosity[i], Radiosity[i], 1e-5);
    }

    const auto numOfIter = aSystem->getNumberOfIterations();
    EXPECT_EQ(21u, numOfIter);
}
