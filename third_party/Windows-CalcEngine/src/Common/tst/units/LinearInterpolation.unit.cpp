#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"


class LinearInterpolationTest : public testing::Test
{
protected:
    void SetUp() override
    {}
};

TEST_F(LinearInterpolationTest, Test1)
{
    SCOPED_TRACE("Begin Test: Simple linear intepolation.");

    const auto x1{1.0};
    const auto x2{2.0};
    const auto y1{10.0};
    const auto y2{20.0};
    const auto x{1.5};
    const auto correctY{15.0};
    const auto evaluatedY{FenestrationCommon::linearInterpolation(x1, x2, y1, y2, x)};

    EXPECT_NEAR(correctY, evaluatedY, 1e-6);
}
