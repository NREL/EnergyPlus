#include <memory>
#include <gtest/gtest.h>

#include "WCESpectralAveraging.hpp"
#include "WCECommon.hpp"


using namespace SpectralAveraging;
using namespace FenestrationCommon;

class TestAngularPropertiesUncoated : public testing::Test
{
protected:
    void SetUp() override
    {}
};

TEST_F(TestAngularPropertiesUncoated, Test1)
{
    SCOPED_TRACE("Begin Test: Uncoated properties - various angles.");

    Real64 aThickness = 0.005715;   // m
    Real64 lambda = 0.8e-6;         // m
    Real64 T0 = 0.722;
    Real64 R0 = 0.066;
    Real64 angle = 0.0;

    auto aAngularFactory = CAngularPropertiesFactory(T0, R0, aThickness);
    auto aProperties = aAngularFactory.getAngularProperties(SurfaceType::Uncoated);

    EXPECT_NEAR(0.722, aProperties->transmittance(angle, lambda), 1e-6);
    EXPECT_NEAR(0.066, aProperties->reflectance(angle, lambda), 1e-6);

    angle = 30;
    EXPECT_NEAR(0.70982055, aProperties->transmittance(angle, lambda), 1e-6);
    EXPECT_NEAR(0.067355436, aProperties->reflectance(angle, lambda), 1e-6);

    angle = 60;
    EXPECT_NEAR(0.625790657, aProperties->transmittance(angle, lambda), 1e-6);
    EXPECT_NEAR(0.126842853, aProperties->reflectance(angle, lambda), 1e-6);

    angle = 90;
    EXPECT_NEAR(0, aProperties->transmittance(angle, lambda), 1e-6);
    EXPECT_NEAR(1, aProperties->reflectance(angle, lambda), 1e-6);
}

TEST_F(TestAngularPropertiesUncoated, Test2)
{
    SCOPED_TRACE("Begin Test: Uncoated properties - zero normal transmittance.");

    Real64 aThickness = 0.005715;   // m
    Real64 lambda = 0.8e-6;         // m
    Real64 T0 = 0.0;
    Real64 R0 = 0.047;
    Real64 angle = 0.0;

    auto aAngularFactory = CAngularPropertiesFactory(T0, R0, aThickness);
    auto aProperties = aAngularFactory.getAngularProperties(SurfaceType::Uncoated);

    EXPECT_NEAR(0.0, aProperties->transmittance(angle, lambda), 1e-6);
    EXPECT_NEAR(0.047, aProperties->reflectance(angle, lambda), 1e-6);

    angle = 30;
    EXPECT_NEAR(0.0, aProperties->transmittance(angle, lambda), 1e-6);
    EXPECT_NEAR(0.048625638, aProperties->reflectance(angle, lambda), 1e-6);

    angle = 60;
    EXPECT_NEAR(0.0, aProperties->transmittance(angle, lambda), 1e-6);
    EXPECT_NEAR(0.097922095, aProperties->reflectance(angle, lambda), 1e-6);

    angle = 90;
    EXPECT_NEAR(0, aProperties->transmittance(angle, lambda), 1e-6);
    EXPECT_NEAR(1, aProperties->reflectance(angle, lambda), 1e-6);
}
