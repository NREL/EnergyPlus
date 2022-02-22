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
        auto airTemperature = 305.15;   // Kelvins
        auto airSpeed = 2.75;           // meters per second
        auto tSky = 305.15;             // Kelvins
        auto solarRadiation = 783.0;

        auto Outdoor = Tarcog::ISO15099::Environments::outdoor(
          airTemperature, airSpeed, solarRadiation, tSky, Tarcog::ISO15099::SkyModel::AllSpecified);
        Outdoor->setHCoeffModel(Tarcog::ISO15099::BoundaryConditionsCoeffModel::CalculateH);

        /////////////////////////////////////////////////////////
        /// Indoor
        /////////////////////////////////////////////////////////

        auto roomTemperature = 297.15;
        auto Indoor = Tarcog::ISO15099::Environments::indoor(roomTemperature);

        /////////////////////////////////////////////////////////
        // IGU
        /////////////////////////////////////////////////////////
        auto solidLayerThickness = 0.003048;   // [m]
        auto solidLayerConductance = 1.0;

        auto aSolidLayer =
          Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        aSolidLayer->setSolarAbsorptance(0.0914, solarRadiation);

        auto windowWidth = 2.0;
        auto windowHeight = 2.0;
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

    const double uValue{5.68};
    const double edgeUValue{5.575};
    const double projectedFrameDimension{0.05715};
    const double wettedLength{0.05715};
    const double absorptance{0.9};

    Tarcog::ISO15099::FrameData frameData{
      uValue, edgeUValue, projectedFrameDimension, wettedLength, absorptance};

    const auto width{2.0};
    const auto height{2.0};
    const auto iguUValue{5.575};
    const auto shgc{0.86};
    const auto tVis{0.899};
    const auto tSol{0.8338};
    const auto hout{20.42635};

    auto window = Tarcog::ISO15099::WindowSingleVision(
      width,
      height,
      tVis,
      tSol,
      std::make_shared<Tarcog::ISO15099::SimpleIGU>(iguUValue, shgc, hout));

    window.setFrameTop(frameData);
    window.setFrameBottom(frameData);
    window.setFrameLeft(frameData);
    window.setFrameRight(frameData);

    const double vt{window.vt()};
    EXPECT_NEAR(0.799180, vt, 1e-6);

    const double uvalue{window.uValue()};
    EXPECT_NEAR(5.586659, uvalue, 1e-6);

    const double windowSHGC{window.shgc()};
    EXPECT_NEAR(0.792299, windowSHGC, 1e-6);
}

TEST_F(TestSingleVisionWindow, CalculatedCOG)
{
    SCOPED_TRACE("Begin Test: Single vision window with calculated COG values.");

    const double uValue{5.68};
    const double edgeUValue{5.575};
    const double projectedFrameDimension{0.05715};
    const double wettedLength{0.05715};
    const double absorptance{0.9};

    Tarcog::ISO15099::FrameData frameData{
      uValue, edgeUValue, projectedFrameDimension, wettedLength, absorptance};

    const auto width{2.0};
    const auto height{2.0};
    const auto tVis{0.899};
    const auto tSol{0.8338};

    auto window = Tarcog::ISO15099::WindowSingleVision(width, height, tVis, tSol, getCOG());

    window.setFrameTop(frameData);
    window.setFrameBottom(frameData);
    window.setFrameLeft(frameData);
    window.setFrameRight(frameData);

    const double vt{window.vt()};
    EXPECT_NEAR(0.799181, vt, 1e-6);

    const double uvalue{window.uValue()};
    EXPECT_NEAR(5.255746, uvalue, 1e-6);

    const double windowSHGC{window.shgc()};
    EXPECT_NEAR(0.791895, windowSHGC, 1e-6);
}
