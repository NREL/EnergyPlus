#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace FenestrationCommon;

class TestMatrixCopyFrom : public testing::Test
{
protected:
    void SetUp() override
    {}
};

TEST_F(TestMatrixCopyFrom, Test1)
{
    SCOPED_TRACE("Begin Test: Test matrix addition operation.");

    SquareMatrix a{{1, 2}, {3, 4}};
    const SquareMatrix b{{2, 3}, {4, 5}};

    a = b;

    EXPECT_NEAR(2, a(0, 0), 1e-6);
    EXPECT_NEAR(3, a(0, 1), 1e-6);
    EXPECT_NEAR(4, a(1, 0), 1e-6);
    EXPECT_NEAR(5, a(1, 1), 1e-6);
}
