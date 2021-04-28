#include <gtest/gtest.h>
#include <memory>
#include <stdexcept>

#include "WCETarcog.hpp"

class TestSingleVisionWindow : public testing::Test
{
protected:
    void SetUp() override
    {}

    static std::shared_ptr<Tarcog::ISO15099::CSystem> getCOG()
    {
        /////////////////////////////////////////////////////////
        /// Outdoor
        /////////////////////////////////////////////////////////
        auto airTemperature = 300.0;   // Kelvins
        auto airSpeed = 5.5;           // meters per second
        auto tSky = 270.0;             // Kelvins
        auto solarRadiation = 789.0;

        auto Outdoor = Tarcog::ISO15099::Environments::outdoor(
          airTemperature, airSpeed, solarRadiation, tSky, Tarcog::ISO15099::SkyModel::AllSpecified);
        Outdoor->setHCoeffModel(Tarcog::ISO15099::BoundaryConditionsCoeffModel::CalculateH);

        /////////////////////////////////////////////////////////
        /// Indoor
        /////////////////////////////////////////////////////////

        auto roomTemperature = 294.15;
        auto Indoor = Tarcog::ISO15099::Environments::indoor(roomTemperature);

        /////////////////////////////////////////////////////////
        // IGU
        /////////////////////////////////////////////////////////
        auto solidLayerThickness = 0.003048;   // [m]
        auto solidLayerConductance = 1.0;

        auto aSolidLayer =
          Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        aSolidLayer->setSolarAbsorptance(0.094189159572, solarRadiation);

        auto windowWidth = 1.0;
        auto windowHeight = 1.0;
        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayer(aSolidLayer);

        /////////////////////////////////////////////////////////
        // System
        /////////////////////////////////////////////////////////
        return std::make_shared<Tarcog::ISO15099::CSystem>(aIGU, Indoor, Outdoor);
    }
};

TEST_F(TestSingleVisionWindow, PredefinedCOGValues)
{
    SCOPED_TRACE("Begin Test: Single vision window with predefined COG values.");

    const double uValue{2.134059};
    const double edgeUValue{2.251039};
    const double projectedFrameDimension{0.050813};
    const double wettedLength{0.05633282};
    const double absorptance{0.3};

    Tarcog::ISO15099::FrameData frameData{
      uValue, edgeUValue, projectedFrameDimension, wettedLength, absorptance};

    const auto width{1.2};
    const auto height{1.5};
    const auto iguUValue{1.667875};
    const auto shgc{0.430713};
    const auto tVis{0.638525};
    const auto tSol{0.3716};
    const auto hcout{15.0};

    auto window = Tarcog::ISO15099::WindowSingleVision(
      width,
      height,
      tVis,
      tSol,
      std::make_shared<Tarcog::ISO15099::SimpleIGU>(iguUValue, shgc, hcout));

    window.setFrameTop(frameData);
    window.setFrameBottom(frameData);
    window.setFrameLeft(frameData);
    window.setFrameRight(frameData);

    const double vt{window.vt()};
    EXPECT_NEAR(0.544853, vt, 1e-6);

    const double uvalue{window.uValue()};
    EXPECT_NEAR(1.833769, uvalue, 1e-6);

    const double windowSHGC{window.shgc()};
    EXPECT_NEAR(0.373175, windowSHGC, 1e-6);
}

TEST_F(TestSingleVisionWindow, CalculatedCOG)
{
    SCOPED_TRACE("Begin Test: Single vision window with calculated COG values.");

    const double uValue{2.134059};
    const double edgeUValue{2.251039};
    const double projectedFrameDimension{0.050813};
    const double wettedLength{0.05633282};
    const double absorptance{0.3};

    Tarcog::ISO15099::FrameData frameData{
      uValue, edgeUValue, projectedFrameDimension, wettedLength, absorptance};

    const auto width{1.2};
    const auto height{1.5};
    const auto tVis{0.638525};
    const auto tSol{0.3716};

    auto window = Tarcog::ISO15099::WindowSingleVision(width, height, tVis, tSol, getCOG());

    window.setFrameTop(frameData);
    window.setFrameBottom(frameData);
    window.setFrameLeft(frameData);
    window.setFrameRight(frameData);

    const double vt{window.vt()};
    EXPECT_NEAR(0.544853, vt, 1e-6);

    const double uvalue{window.uValue()};
    EXPECT_NEAR(4.377901, uvalue, 1e-6);

    const double windowSHGC{window.shgc()};
    EXPECT_NEAR(0.336182, windowSHGC, 1e-6);
}
