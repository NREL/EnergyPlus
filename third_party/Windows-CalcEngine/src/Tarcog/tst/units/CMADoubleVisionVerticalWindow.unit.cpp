#include <gtest/gtest.h>

#include "WCETarcog.hpp"

class TestCMADoubleVisionVerticalWindow : public testing::Test
{
protected:
    void SetUp() override
    {}
};

TEST_F(TestCMADoubleVisionVerticalWindow, CMADualVerticalVision)
{
    SCOPED_TRACE("Begin Test: CMA test for double vision vertical window.");

    const Tarcog::ISO15099::FrameData frameDataBestBestHead{
      1.306919, 0.794668, 0.042875183, 0.110605026};
    const Tarcog::ISO15099::FrameData frameDataBestWorstHead{
      1.65724, 2.71409, 0.042875183, 0.110605026};
    const Tarcog::ISO15099::FrameData frameDataWorstBestHead{
      2.27964, 1.65214, 0.042875183, 0.110605026};
    const Tarcog::ISO15099::FrameData frameDataWorstWorstHead{
      2.32377, 3.19643, 0.042875183, 0.110605026};

    const CMA::CMAFrame cmaFrameHead{frameDataBestBestHead,
                                     frameDataBestWorstHead,
                                     frameDataWorstBestHead,
                                     frameDataWorstWorstHead};

    const Tarcog::ISO15099::FrameData frameDataBestBestJamb{
      1.25968, 0.76981, 0.042875183, 0.110605026};
    const Tarcog::ISO15099::FrameData frameDataBestWorstJamb{
      1.62145, 2.70202, 0.042875183, 0.110605026};
    const Tarcog::ISO15099::FrameData frameDataWorstBestJamb{
      2.26579, 1.64520, 0.042875183, 0.110605026};
    const Tarcog::ISO15099::FrameData frameDataWorstWorstJamb{
      2.30879, 3.18888, 0.042875183, 0.110605026};

    const CMA::CMAFrame cmaFrameJamb{frameDataBestBestJamb,
                                     frameDataBestWorstJamb,
                                     frameDataWorstBestJamb,
                                     frameDataWorstWorstJamb};

    const Tarcog::ISO15099::FrameData frameDataBestBestSill{
      1.30474, 0.79449, 0.042875183, 0.110605026};
    const Tarcog::ISO15099::FrameData frameDataBestWorstSill{
      1.64813, 2.71240, 0.042875183, 0.110605026};
    const Tarcog::ISO15099::FrameData frameDataWorstBestSill{
      2.27038, 1.64528, 0.042875183, 0.110605026};
    const Tarcog::ISO15099::FrameData frameDataWorstWorstSill{
      2.31302, 3.18880, 0.042875183, 0.110605026};

    const CMA::CMAFrame cmaFrameSill{frameDataBestBestSill,
                                     frameDataBestWorstSill,
                                     frameDataWorstBestSill,
                                     frameDataWorstWorstSill};

    const auto width{1.2};
    const auto height{1.5};

    auto window{CMA::CMAWindowDualVisionVertical(width, height)};

    window.setFrameTop(cmaFrameHead);
    window.setFrameBottom(cmaFrameSill);
    window.setFrameTopLeft(cmaFrameJamb);
    window.setFrameTopRight(cmaFrameJamb);
    window.setFrameBottomLeft(cmaFrameJamb);
    window.setFrameBottomRight(cmaFrameJamb);
    window.setFrameMeetingRail(cmaFrameJamb);

    const auto UvalueCOG{1.258};
    const auto SHGCCOG{0.341};
    const auto tVis{0.535};
    const auto spacerKeff{0.750454253};

    const double vt{window.vt(tVis)};
    EXPECT_NEAR(0.454171, vt, 1e-6);

    const double uvalue{window.uValue(UvalueCOG, spacerKeff)};
    EXPECT_NEAR(1.511768, uvalue, 1e-6);

    const double windowSHGC{window.shgc(SHGCCOG, spacerKeff)};
    EXPECT_NEAR(0.290800, windowSHGC, 1e-6);

    const auto iguDimensions{window.getIGUDimensions()};
    EXPECT_NEAR(1.114250, iguDimensions.width, 1e-6);
    EXPECT_NEAR(0.685687, iguDimensions.height, 1e-6);
}
