#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCEGases.hpp"
#include "WCETarcog.hpp"
#include "WCECommon.hpp"

class TestDoubleOutsidePerforatedShade_SHGC : public testing::Test
{
private:
    std::unique_ptr<Tarcog::ISO15099::CSystem> m_TarcogSystem;

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
        auto shadeLayerConductance = 0.12;

        // make cell geometry
        const auto thickness_31111{0.00023};
        const auto x = 0.00169;        // m
        const auto y = 0.00169;        // m
        const auto radius = 0.00058;   // m

        const auto CellDimension{
          ThermalPermeability::Perforated::diameterToXYDimension(2 * radius)};

        const auto frontOpenness{ThermalPermeability::Perforated::openness(
          ThermalPermeability::Perforated::Geometry::Circular,
          x,
          y,
          CellDimension.x,
          CellDimension.y)};

        const auto dl{0.0};
        const auto dr{0.0};
        const auto dtop{0.0};
        const auto dbot{0.0};

        EffectiveLayers::ShadeOpenness openness{frontOpenness, dl, dr, dtop, dbot};

        auto windowWidth = 1.0;
        auto windowHeight = 1.0;

        EffectiveLayers::EffectiveLayerPerforated effectiveLayerPerforated{
          windowWidth, windowHeight, thickness_31111, openness};

        EffectiveLayers::EffectiveOpenness effOpenness{
          effectiveLayerPerforated.getEffectiveOpenness()};

        const auto effectiveThickness{effectiveLayerPerforated.effectiveThickness()};

        auto Ef = 0.640892;
        auto Eb = 0.623812;
        auto Tirf = 0.257367;
        auto Tirb = 0.257367;

        auto aLayer1 = Tarcog::ISO15099::Layers::shading(
          effectiveThickness, shadeLayerConductance, effOpenness, Ef, Tirf, Eb, Tirb);

        aLayer1->setSolarAbsorptance(0.106659, solarRadiation);

        auto gapThickness = 0.0127;
        auto GapLayer1 = Tarcog::ISO15099::Layers::gap(gapThickness);
        ASSERT_TRUE(GapLayer1 != nullptr);

        auto solidLayerThickness = 0.003048;   // [m]
        auto solidLayerConductance = 1.0;
        auto aLayer2 = Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        ASSERT_TRUE(aLayer2 != nullptr);

        aLayer2->setSolarAbsorptance(0.034677, solarRadiation);


        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayers({aLayer1, GapLayer1, aLayer2});

        // Alternative option of adding layers.
        // aIGU.addLayer(aLayer1);
        // aIGU.addLayer(GapLayer1);
        // aIGU.addLayer(aLayer2);
        // aIGU.addLayer(GapLayer2);
        // aIGU.addLayer(aLayer3);

        /////////////////////////////////////////////////////////
        // System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::make_unique<Tarcog::ISO15099::CSystem>(aIGU, Indoor, Outdoor);
        ASSERT_TRUE(m_TarcogSystem != nullptr);
    }

public:
    [[nodiscard]] Tarcog::ISO15099::CSystem * GetSystem() const
    {
        return m_TarcogSystem.get();
    };
};

TEST_F(TestDoubleOutsidePerforatedShade_SHGC, Test1)
{
    SCOPED_TRACE("Begin Test: Outside perforated shade.");

    const auto aSystem = GetSystem();

    const auto uval = aSystem->getUValue();
    EXPECT_NEAR(3.193057, uval, 1e-6);

    const auto effectiveLayerConductivities{
      aSystem->getSolidEffectiveLayerConductivities(Tarcog::ISO15099::System::Uvalue)};

    const std::vector<double> correctEffectConductivites{0.115592, 1};
    EXPECT_EQ(correctEffectConductivites.size(), effectiveLayerConductivities.size());
    for(size_t i = 0u; i < correctEffectConductivites.size(); ++i)
    {
        EXPECT_NEAR(correctEffectConductivites[i], effectiveLayerConductivities[i], 1e-6);
    }

    const auto totSol{0.315236};
    const auto shgc{aSystem->getSHGC(totSol)};
    EXPECT_NEAR(0.348647, shgc, 1e-6);

    const auto heatflow =
      aSystem->getHeatFlow(Tarcog::ISO15099::System::SHGC, Tarcog::ISO15099::Environment::Indoor);
    EXPECT_NEAR(-51.705046, heatflow, 1e-6);
}
