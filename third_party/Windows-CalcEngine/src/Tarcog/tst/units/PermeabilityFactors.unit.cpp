#include <memory>
#include <gtest/gtest.h>

#include <WCETarcog.hpp>

class TestPermeabilityFactors : public testing::Test
{
protected:
    void SetUp() override
    {}
};

TEST_F(TestPermeabilityFactors, TestVenetianPermeability)
{
    SCOPED_TRACE("Begin Test: Venetian layer thermal permeability.");

    const auto matThickness{0.0001};   // m
    const auto slatWidth{0.0148};      // m
    const auto slatSpacing{0.0127};    // m
    const auto slatTiltAngle{0.0};
    const auto curvatureRadius{0.03313057};   // m

    const auto permeabilityOpenness{ThermalPermeability::Venetian::openness(
      slatTiltAngle, slatSpacing, matThickness, curvatureRadius, slatWidth)};

    EXPECT_NEAR(0.9921875, permeabilityOpenness, 1e-6);
}

TEST_F(TestPermeabilityFactors, TestPerforatedCircularPermeability)
{
    SCOPED_TRACE("Begin Test: Circular perforated layer thermal permeability.");

    const auto perforatedType{ThermalPermeability::Perforated::Geometry::Circular};
    const auto diameter{0.00635};   // m
    const auto xSpacing{0.01905};   // m
    const auto ySpacing{0.01905};   // m
    const auto CellDimension{ThermalPermeability::Perforated::diameterToXYDimension(diameter)};

    EXPECT_NEAR(0.00635, CellDimension.x, 1e-6);
    EXPECT_NEAR(0.00635, CellDimension.y, 1e-6);

    const auto permeabilityOpenness{ThermalPermeability::Perforated::openness(
      perforatedType, xSpacing, ySpacing, CellDimension.x, CellDimension.y)};

    EXPECT_NEAR(0.087266, permeabilityOpenness, 1e-6);
}

TEST_F(TestPermeabilityFactors, TestPerforatedRectangularPermeability)
{
    SCOPED_TRACE("Begin Test: Rectangular perforated layer thermal permeability.");

    const auto perforatedType{ThermalPermeability::Perforated::Geometry::Rectangular};
    const auto width{0.00635};      // m
    const auto height{0.00635};     // m
    const auto xSpacing{0.01905};   // m
    const auto ySpacing{0.01905};   // m

    const auto permeabilityOpenness{
      ThermalPermeability::Perforated::openness(perforatedType, xSpacing, ySpacing, width, height)};

    EXPECT_NEAR(0.111111, permeabilityOpenness, 1e-6);
}

TEST_F(TestPermeabilityFactors, TestPerforatedSquarePermeability)
{
    SCOPED_TRACE("Begin Test: Square perforated layer thermal permeability.");

    const auto perforatedType{ThermalPermeability::Perforated::Geometry::Square};
    const auto width{0.00635};      // m
    const auto xSpacing{0.01905};   // m
    const auto ySpacing{0.01905};   // m

    const auto permeabilityOpenness{
      ThermalPermeability::Perforated::openness(perforatedType, xSpacing, ySpacing, width, width)};

    EXPECT_NEAR(0.111111, permeabilityOpenness, 1e-6);
}

TEST_F(TestPermeabilityFactors, TestWovenPermeability)
{
    SCOPED_TRACE("Begin Test: Woven layer thermal permeability.");

    const auto diameter{0.001};   // m
    const auto spacing{0.002};    // m

    const auto permeabilityOpenness{ThermalPermeability::Woven::openness(diameter, spacing)};

    EXPECT_NEAR(0.25, permeabilityOpenness, 1e-6);
}