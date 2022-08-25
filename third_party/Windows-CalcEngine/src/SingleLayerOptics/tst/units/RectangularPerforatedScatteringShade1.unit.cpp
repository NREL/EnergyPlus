#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestRectangularPerforatedScatteringShade1 : public testing::Test
{
protected:
    virtual void SetUp()
    {}
};

TEST_F(TestRectangularPerforatedScatteringShade1, TestProperties)
{
    SCOPED_TRACE("Begin Test: Rectangular perforated cell - properties.");

    // make material
    const auto Tmat = 0.1;
    const auto Rfmat = 0.4;
    const auto Rbmat = 0.4;
    const auto minLambda = 0.3;
    const auto maxLambda = 2.5;
    const auto aMaterial =
      Material::singleBandMaterial(Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda);

    // make cell geometry
    const auto x = 20.0;          // mm
    const auto y = 25.0;          // mm
    const auto thickness = 7.0;   // mm
    const auto xHole = 5.0;       // mm
    const auto yHole = 8.0;       // mm

    auto shade =
      CScatteringLayer::createPerforatedRectangularLayer(aMaterial, x, y, thickness, xHole, yHole);

    const double tir = shade.getPropertySimple(
      minLambda, maxLambda, PropertySimple::T, Side::Front, Scattering::DiffuseDiffuse);

    EXPECT_NEAR(tir, 0.112482, 1e-6);

    const double rir = shade.getPropertySimple(
      minLambda, maxLambda, PropertySimple::R, Side::Front, Scattering::DiffuseDiffuse);
    EXPECT_NEAR(rir, 0.394452, 1e-6);


    auto irLayer = CScatteringLayerIR(shade);
    const double emiss = irLayer.emissivity(Side::Front);
    EXPECT_NEAR(emiss, 0.493065, 1e-6);
}

TEST_F(TestRectangularPerforatedScatteringShade1, TestHighEmissivity)
{
    SCOPED_TRACE("Begin Test: Rectangular perforated cell - properties.");

    // make material
    const auto Tmat = 0.1;
    const auto Rfmat = 0.01;
    const auto Rbmat = 0.01;
    const auto minLambda = 0.3;
    const auto maxLambda = 2.5;
    const auto aMaterial =
      Material::singleBandMaterial(Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda);

    // make cell geometry
    const auto x = 20.0;          // mm
    const auto y = 25.0;          // mm
    const auto thickness = 7.0;   // mm
    const auto xHole = 0.001;       // mm
    const auto yHole = 0.001;       // mm

    auto shade =
      CScatteringLayer::createPerforatedRectangularLayer(aMaterial, x, y, thickness, xHole, yHole);

    const double tir = shade.getPropertySimple(
      minLambda, maxLambda, PropertySimple::T, Side::Front, Scattering::DiffuseDiffuse);

    EXPECT_NEAR(tir, 0.1, 1e-6);

    const double rir = shade.getPropertySimple(
      minLambda, maxLambda, PropertySimple::R, Side::Front, Scattering::DiffuseDiffuse);
    EXPECT_NEAR(rir, 0.01, 1e-6);


    auto irLayer = CScatteringLayerIR(shade);
    const double emiss = irLayer.emissivity(Side::Front);
    EXPECT_NEAR(emiss, 0.89, 1e-6);
}
