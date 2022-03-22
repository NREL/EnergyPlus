#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCEGases.hpp"
#include "WCETarcog.hpp"
#include "WCECommon.hpp"

class TestDoubleOutsideVenetianShade_UValue : public testing::Test
{
private:
    std::unique_ptr<Tarcog::ISO15099::CSystem> m_TarcogSystem;

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
        auto shadeLayerConductance = 160.0;

        const auto matThickness{0.0001};   // m
        const auto slatWidth{0.0148};      // m
        const auto slatSpacing{0.0127};    // m
        const auto slatTiltAngle{0.0};
        const auto curvatureRadius{0.0331305656433105};   // m


        const auto frontOpenness{ThermalPermeability::Venetian::openness(
          slatTiltAngle, slatSpacing, matThickness, curvatureRadius, slatWidth)};

        const auto dl{0.0};
        const auto dr{0.0};
        const auto dtop{0.0};
        const auto dbot{0.0};

        EffectiveLayers::ShadeOpenness openness{frontOpenness, dl, dr, dtop, dbot};

        auto windowWidth = 1.0;
        auto windowHeight = 1.0;

        EffectiveLayers::EffectiveHorizontalVenetian effectiveVenetian{
          windowWidth, windowHeight, matThickness, openness, slatTiltAngle, slatWidth};

        EffectiveLayers::EffectiveOpenness effOpenness{effectiveVenetian.getEffectiveOpenness()};

        const auto effectiveThickness{effectiveVenetian.effectiveThickness()};

        auto Ef = 0.5564947806702053;
        auto Eb = 0.5564947806702053;
        auto Tirf = 0.42293224373137134;
        auto Tirb = 0.42293224373137134;

        auto aLayer1 = Tarcog::ISO15099::Layers::shading(
          effectiveThickness, shadeLayerConductance, effOpenness, Ef, Tirf, Eb, Tirb);

        // auto aLayer1 = Tarcog::ISO15099::Layers::solid(shadeLayerThickness,
        // shadeLayerConductance);

        // aLayer1->setSolarAbsorptance(0.306310117245, solarRadiation);

        auto gapThickness = 0.0127;
        auto GapLayer1 = Tarcog::ISO15099::Layers::gap(gapThickness);
        ASSERT_TRUE(GapLayer1 != nullptr);

        auto solidLayerThickness = 0.003048;   // [m]
        auto solidLayerConductance = 1.0;
        auto aLayer2 = Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        ASSERT_TRUE(aLayer2 != nullptr);

        // aLayer2->setSolarAbsorptance(0.086374, solarRadiation);


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
        m_TarcogSystem = std::unique_ptr<Tarcog::ISO15099::CSystem>(
          new Tarcog::ISO15099::CSystem(aIGU, Indoor, Outdoor));
        ASSERT_TRUE(m_TarcogSystem != nullptr);
    }

public:
    Tarcog::ISO15099::CSystem * GetSystem() const
    {
        return m_TarcogSystem.get();
    };
};

TEST_F(TestDoubleOutsideVenetianShade_UValue, Test1)
{
    SCOPED_TRACE("Begin Test: Outside venetian shade.");

    const auto aSystem = GetSystem();

    const auto effectiveLayerConductivities{
      aSystem->getSolidEffectiveLayerConductivities(Tarcog::ISO15099::System::Uvalue)};

    const std::vector<double> correctEffectConductivites{1.878064, 1};
    EXPECT_EQ(correctEffectConductivites.size(), effectiveLayerConductivities.size());
    for(size_t i = 0u; i < correctEffectConductivites.size(); ++i)
    {
        EXPECT_NEAR(correctEffectConductivites[i], effectiveLayerConductivities[i], 1e-6);
    }

    const auto systemKeff{
      aSystem->getEffectiveSystemConductivity(Tarcog::ISO15099::System::Uvalue)};
    EXPECT_NEAR(0.106427, systemKeff, 1e-6);

    const auto uval = aSystem->getUValue();
    EXPECT_NEAR(3.239692, uval, 1e-6);

    const auto heatflow =
      aSystem->getHeatFlow(Tarcog::ISO15099::System::Uvalue, Tarcog::ISO15099::Environment::Indoor);
    EXPECT_NEAR(126.347983, heatflow, 1e-6);
}
