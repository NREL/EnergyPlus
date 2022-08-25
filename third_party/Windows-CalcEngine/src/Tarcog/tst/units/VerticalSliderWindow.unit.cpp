#include <gtest/gtest.h>

#include <memory>

#include "WCETarcog.hpp"

class TestVerticalSliderWindow : public testing::Test
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

TEST_F(TestVerticalSliderWindow, PredefinedCOGValues)
{
    SCOPED_TRACE("Begin Test: Vertical slider window with predefined COG values.");

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

    auto window = Tarcog::ISO15099::DualVisionVertical(
      width,
      height,
      tVis,
      tSol,
      std::make_shared<Tarcog::ISO15099::SimpleIGU>(iguUValue, shgc, hcout),
      tVis,
      tSol,
      std::make_shared<Tarcog::ISO15099::SimpleIGU>(iguUValue, shgc, hcout));

    window.setFrameTop(frameData);
    window.setFrameTopLeft(frameData);
    window.setFrameTopRight(frameData);
    window.setFrameMeetingRail(frameData);
    window.setFrameBottomLeft(frameData);
    window.setFrameBottomRight(frameData);
    window.setFrameBottom(frameData);

    const double vt{window.vt()};
    EXPECT_NEAR(0.525054, vt, 1e-6);

    const double uvalue{window.uValue()};
    EXPECT_NEAR(1.886101, uvalue, 1e-6);

    const double windowSHGC{window.shgc()};
    EXPECT_NEAR(0.361014, windowSHGC, 1e-6);
}

TEST_F(TestVerticalSliderWindow, CalculatedCOG)
{
    SCOPED_TRACE("Begin Test: Vertical slider window with calculated COG.");

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

    auto window = Tarcog::ISO15099::DualVisionVertical(
      width, height, tVis, tSol, getCOG(), tVis, tSol, getCOG());

    window.setFrameTop(frameData);
    window.setFrameTopLeft(frameData);
    window.setFrameTopRight(frameData);
    window.setFrameMeetingRail(frameData);
    window.setFrameBottomLeft(frameData);
    window.setFrameBottomRight(frameData);
    window.setFrameBottom(frameData);

    const double vt{window.vt()};
    EXPECT_NEAR(0.525054, vt, 1e-6);

    const double uvalue{window.uValue()};
    EXPECT_NEAR(4.074413, uvalue, 1e-6);

    const double windowSHGC{window.shgc()};
    EXPECT_NEAR(0.324160, windowSHGC, 1e-6);
}

TEST_F(TestVerticalSliderWindow, CalculatedSHGC01VT01)
{
    // This is example that was tested vs different WINDOW versions.

    // Bottom sill
    double uValue{3.123};
    double edgeUValue{1.241};
    double projectedFrameDimension{0.063824};
    double wettedLength{0.134467};
    double absorptance{0.3};

    Tarcog::ISO15099::FrameData bottomSill{
      uValue, edgeUValue, projectedFrameDimension, wettedLength, absorptance};

    // Top Sill
    uValue = 3.336;
    edgeUValue = 1.238;
    projectedFrameDimension = 0.063916;
    wettedLength = 0.098926;
    absorptance = 0.3;

    Tarcog::ISO15099::FrameData topSill{
      uValue, edgeUValue, projectedFrameDimension, wettedLength, absorptance};

    // Meeting rail
    uValue = 5.306;
    edgeUValue = 1.308;
    projectedFrameDimension = 0.036;
    wettedLength = 0.098123;
    absorptance = 0.3;

    Tarcog::ISO15099::FrameData meetingRail{
      uValue, edgeUValue, projectedFrameDimension, wettedLength, absorptance};

    // Lower jamb
    uValue = 2.753;
    edgeUValue = 1.269;
    projectedFrameDimension = 0.071711;
    wettedLength = 0.264335;
    absorptance = 0.3;

    Tarcog::ISO15099::FrameData lowerJamb{
      uValue, edgeUValue, projectedFrameDimension, wettedLength, absorptance};

    // Upper jamb
    uValue = 2.454;
    edgeUValue = 1.329;
    projectedFrameDimension = 0.083681;
    wettedLength = 0.11389;
    absorptance = 0.3;

    Tarcog::ISO15099::FrameData upperJamb{
      uValue, edgeUValue, projectedFrameDimension, wettedLength, absorptance};

    const auto width{1.2};
    const auto height{1.5};

    // VT0 and SHGC0
    auto iguUValue{1.154808};
    auto shgc{0.0};
    auto tVis{0.0};
    auto tSol{0.3716};
    auto hout{20.6077232};
    auto window = Tarcog::ISO15099::DualVisionVertical(
      width,
      height,
      tVis,
      tSol,
      std::make_shared<Tarcog::ISO15099::SimpleIGU>(iguUValue, shgc, hout),
      tVis,
      tSol,
      std::make_shared<Tarcog::ISO15099::SimpleIGU>(iguUValue, shgc, hout));

    window.setFrameTop(topSill);
    window.setFrameTopLeft(upperJamb);
    window.setFrameTopRight(upperJamb);
    window.setFrameMeetingRail(meetingRail);
    window.setFrameBottomLeft(lowerJamb);
    window.setFrameBottomRight(lowerJamb);
    window.setFrameBottom(bottomSill);

    const double VT0{window.vt()};
    EXPECT_NEAR(0.0, VT0, 1e-6);

    const double SHGC0{window.shgc()};
    EXPECT_NEAR(0.005074, SHGC0, 1e-6);

    // VT1 and SHGC1
    iguUValue = 1.154808;
    shgc = 1.0;
    tVis = 1.0;
    window = Tarcog::ISO15099::DualVisionVertical(
      width,
      height,
      tVis,
      tSol,
      std::make_shared<Tarcog::ISO15099::SimpleIGU>(iguUValue, shgc, hout),
      tVis,
      tSol,
      std::make_shared<Tarcog::ISO15099::SimpleIGU>(iguUValue, shgc, hout));

    window.setFrameTop(topSill);
    window.setFrameTopLeft(upperJamb);
    window.setFrameTopRight(upperJamb);
    window.setFrameMeetingRail(meetingRail);
    window.setFrameBottomLeft(lowerJamb);
    window.setFrameBottomRight(lowerJamb);
    window.setFrameBottom(bottomSill);

    const double VT1{window.vt()};
    EXPECT_NEAR(0.775483, VT1, 1e-6);

    const double SHGC1{window.shgc()};
    EXPECT_NEAR(0.780556, SHGC1, 1e-6);
}

TEST_F(TestVerticalSliderWindow, CalculatedSHGC01VT01GenericDividers)
{
    // This is example that was tested vs different WINDOW versions.

    // Bottom sill
    double uValue{3.123};
    double edgeUValue{1.241};
    double projectedFrameDimension{0.063824};
    double wettedLength{0.134467};
    double absorptance{0.3};

    Tarcog::ISO15099::FrameData bottomSill{
      uValue, edgeUValue, projectedFrameDimension, wettedLength, absorptance};

    // Top Sill
    uValue = 3.336;
    edgeUValue = 1.238;
    projectedFrameDimension = 0.063916;
    wettedLength = 0.098926;
    absorptance = 0.3;

    Tarcog::ISO15099::FrameData topSill{
      uValue, edgeUValue, projectedFrameDimension, wettedLength, absorptance};

    // Meeting rail
    uValue = 5.306;
    edgeUValue = 1.308;
    projectedFrameDimension = 0.036;
    wettedLength = 0.098123;
    absorptance = 0.3;

    Tarcog::ISO15099::FrameData meetingRail{
      uValue, edgeUValue, projectedFrameDimension, wettedLength, absorptance};

    // Lower jamb
    uValue = 2.753;
    edgeUValue = 1.269;
    projectedFrameDimension = 0.071711;
    wettedLength = 0.264335;
    absorptance = 0.3;

    Tarcog::ISO15099::FrameData lowerJamb{
      uValue, edgeUValue, projectedFrameDimension, wettedLength, absorptance};

    // Upper jamb
    uValue = 2.454;
    edgeUValue = 1.329;
    projectedFrameDimension = 0.083681;
    wettedLength = 0.11389;
    absorptance = 0.3;

    Tarcog::ISO15099::FrameData upperJamb{
      uValue, edgeUValue, projectedFrameDimension, wettedLength, absorptance};

    // Divider
    uValue = 2.2713050842285156;
    edgeUValue = 2.2713050842285156;
    projectedFrameDimension = 0.01905;
    wettedLength = 0.01905;
    absorptance = 0.3;

    Tarcog::ISO15099::FrameData divider{
      uValue, edgeUValue, projectedFrameDimension, wettedLength, absorptance};

    const auto width{1.2};
    const auto height{1.5};
    const auto nHorizontal{2u};
    const auto nVertical{3u};

    // VT0 and SHGC0
    auto iguUValue{1.154808};
    auto shgc{0.0};
    auto tVis{0.0};
    auto tSol{0.3716};
    auto hout{20.6077232};
    auto window = Tarcog::ISO15099::DualVisionVertical(
      width,
      height,
      tVis,
      tSol,
      std::make_shared<Tarcog::ISO15099::SimpleIGU>(iguUValue, shgc, hout),
      tVis,
      tSol,
      std::make_shared<Tarcog::ISO15099::SimpleIGU>(iguUValue, shgc, hout));

    window.setFrameTop(topSill);
    window.setFrameTopLeft(upperJamb);
    window.setFrameTopRight(upperJamb);
    window.setFrameMeetingRail(meetingRail);
    window.setFrameBottomLeft(lowerJamb);
    window.setFrameBottomRight(lowerJamb);
    window.setFrameBottom(bottomSill);
    window.setDividers(divider, nHorizontal, nVertical);

    const double VT0{window.vt()};
    EXPECT_NEAR(0.0, VT0, 1e-6);

    const double SHGC0{window.shgc()};
    EXPECT_NEAR(0.007859, SHGC0, 1e-6);

    // VT1 and SHGC1
    iguUValue = 1.154808;
    shgc = 1.0;
    tVis = 1.0;
    window = Tarcog::ISO15099::DualVisionVertical(
      width,
      height,
      tVis,
      tSol,
      std::make_shared<Tarcog::ISO15099::SimpleIGU>(iguUValue, shgc, hout),
      tVis,
      tSol,
      std::make_shared<Tarcog::ISO15099::SimpleIGU>(iguUValue, shgc, hout));

    window.setFrameTop(topSill);
    window.setFrameTopLeft(upperJamb);
    window.setFrameTopRight(upperJamb);
    window.setFrameMeetingRail(meetingRail);
    window.setFrameBottomLeft(lowerJamb);
    window.setFrameBottomRight(lowerJamb);
    window.setFrameBottom(bottomSill);
    window.setDividers(divider, nHorizontal, nVertical);

    const double VT1{window.vt()};
    EXPECT_NEAR(0.691254, VT1, 1e-6);

    const double SHGC1{window.shgc()};
    EXPECT_NEAR(0.699113, SHGC1, 1e-6);
}