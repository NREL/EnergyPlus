#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCEGases.hpp"
#include "WCETarcog.hpp"

// Unit test contains airflow calculation for the window that is 2 x 2 meters

class TestTripleShadeInside_UValue : public testing::Test
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

        auto windowWidth = 2.0;
        auto windowHeight = 2.0;

        auto solidLayerThickness = 0.005613400135;   // [m]
        auto solidLayerConductance = 0.996883;
        auto frontEmiss = 0.840000;
        auto frontTIR = 0.0;
        auto backEmiss = 0.038798544556;
        auto backTIR = 0.0;
        auto aLayer1 = Tarcog::ISO15099::Layers::solid(
          solidLayerThickness, solidLayerConductance, frontEmiss, frontTIR, backEmiss, backTIR);

        auto gapThickness = 0.0127;
        //const auto gapPressure = 101325.0;
        Gases::CGas gas1;
        gas1.addGasItem(0.1, Gases::GasDef::Air);
        gas1.addGasItem(0.9, Gases::GasDef::Argon);
        auto GapLayer1 = Tarcog::ISO15099::Layers::gap(gapThickness, gas1);

        solidLayerThickness = 0.005715;   // [m]
        solidLayerConductance = 1.0;
        frontEmiss = 0.840000;
        frontTIR = 0.0;
        backEmiss = 0.840000;
        backTIR = 0.0;
        auto aLayer2 = Tarcog::ISO15099::Layers::solid(
          solidLayerThickness, solidLayerConductance, frontEmiss, frontTIR, backEmiss, backTIR);

        gapThickness = 0.0127;
        auto GapLayer2 = Tarcog::ISO15099::Layers::gap(gapThickness);

        const auto shadeLayerConductance = 160.0;
        //const auto shadeThickness = 1.000000e-04;
        const auto shadeThickness = 0.0006;

        const auto dl{0.0};
        const auto dr{0.0};
        const auto dtop{0.0};
        const auto dbot{0.0};
        //const auto frontOpenness{0.9924940};
        const auto frontOpenness{0.9};

        EffectiveLayers::ShadeOpenness openness{frontOpenness, dl, dr, dtop, dbot};

        EffectiveLayers::EffectiveOpenness effOpenness{
          frontOpenness, dl, dr, dtop, dbot, frontOpenness};

        //const auto Ef = 7.379264e-01;
        //const auto Eb = 7.437828e-01;
        //const auto Tirf = 2.124426e-01;
        //const auto Tirb = 2.124426e-01;
        const auto Ef = 0.9;
        const auto Eb = 0.9;
        const auto Tirf = 0;
        const auto Tirb = 0;

        auto aLayer3 = Tarcog::ISO15099::Layers::shading(
          shadeThickness, shadeLayerConductance, effOpenness, Ef, Tirf, Eb, Tirb);

        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayers({aLayer1, GapLayer1, aLayer2, GapLayer2, aLayer3});

        /////////////////////////////////////////////////////////
        /// System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::unique_ptr<Tarcog::ISO15099::CSystem>(
          new Tarcog::ISO15099::CSystem(aIGU, Indoor, Outdoor));
    }

public:
    Tarcog::ISO15099::CSystem * GetSystem() const
    {
        return m_TarcogSystem.get();
    };
};

TEST_F(TestTripleShadeInside_UValue, Test1)
{
    SCOPED_TRACE("Begin Test: Outside venetian shade.");

    const auto aSystem = GetSystem();

    auto effectiveLayerConductivities{
      aSystem->getSolidEffectiveLayerConductivities(Tarcog::ISO15099::System::Uvalue)};

    const auto systemKeff{
      aSystem->getEffectiveSystemConductivity(Tarcog::ISO15099::System::Uvalue)};
    EXPECT_NEAR(0.039691, systemKeff, 1e-6);

    const auto uval = aSystem->getUValue();
    EXPECT_NEAR(1.196599, uval, 1e-6);

    const auto heatflow =
      aSystem->getHeatFlow(Tarcog::ISO15099::System::Uvalue, Tarcog::ISO15099::Environment::Indoor);
    EXPECT_NEAR(34.451676, heatflow, 1e-6);
}
