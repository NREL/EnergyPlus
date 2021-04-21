#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace FenestrationCommon;

class TestMatrixAddition : public testing::Test
{
protected:
    void SetUp() override
    {}
};

TEST_F(TestMatrixAddition, TestAddition)
{
    SCOPED_TRACE("Begin Test: Test matrix addition operation.");

    const SquareMatrix a{{1, 2}, {3, 4}};
    const SquareMatrix b{{2, 3}, {4, 5}};

    auto c = a + b;

    EXPECT_NEAR(3, c(0, 0), 1e-6);
    EXPECT_NEAR(5, c(0, 1), 1e-6);
    EXPECT_NEAR(7, c(1, 0), 1e-6);
    EXPECT_NEAR(9, c(1, 1), 1e-6);
}

TEST_F(TestMatrixAddition, TestAdditionException)
{
    SCOPED_TRACE("Begin Test: Test matrix addition exception.");

    const SquareMatrix a{{1, 2}, {3, 4}};
    const SquareMatrix b{{2, 3, 4}, {4, 5, 6}, {7, 5, 9}};

    try
    {
        auto c = a + b;
    } catch(const std::runtime_error & err) {
        EXPECT_EQ(err.what(), std::string("Matrices must be identical in size."));
    }
}

TEST_F(TestMatrixAddition, TestSubraction)
{
    SCOPED_TRACE("Begin Test: Test matrix subtraction operation.");

    const SquareMatrix a{{1, 2}, {3, 4}};
    const SquareMatrix b{{2, 3}, {4, 5}};

    auto c = a - b;

    EXPECT_NEAR(-1, c(0, 0), 1e-6);
    EXPECT_NEAR(-1, c(0, 1), 1e-6);
    EXPECT_NEAR(-1, c(1, 0), 1e-6);
    EXPECT_NEAR(-1, c(1, 1), 1e-6);
}

TEST_F(TestMatrixAddition, TestSubractionException)
{
    SCOPED_TRACE("Begin Test: Test matrix subraction exception.");

    const std::vector<std::vector<double>> aVec{{1, 2}, {3, 4}};
    const SquareMatrix a{std::move(aVec)};
    const SquareMatrix b{{2, 3, 4}, {4, 5, 6}, {7, 5, 9}};

    try
    {
        auto c = a - b;
    } catch(const std::runtime_error & err) {
        EXPECT_EQ(err.what(), std::string("Matrices must be identical in size."));
    }
}

TEST_F(TestMatrixAddition, TestAdditionWithEquality)
{
    SCOPED_TRACE("Begin Test: Test matrix += operation.");

    SquareMatrix a{{1, 2}, {3, 4}};
    const SquareMatrix b{{2, 3}, {4, 5}};

    a += b;

    EXPECT_NEAR(3, a(0, 0), 1e-6);
    EXPECT_NEAR(5, a(0, 1), 1e-6);
    EXPECT_NEAR(7, a(1, 0), 1e-6);
    EXPECT_NEAR(9, a(1, 1), 1e-6);
}

TEST_F(TestMatrixAddition, TestSubratctionWithEquality)
{
    SCOPED_TRACE("Begin Test: Test matrix -= operation.");

    SquareMatrix a{{1, 2}, {3, 4}};
    const SquareMatrix b{{2, 3}, {4, 5}};

    a -= b;

    EXPECT_NEAR(-1, a(0, 0), 1e-6);
    EXPECT_NEAR(-1, a(0, 1), 1e-6);
    EXPECT_NEAR(-1, a(1, 0), 1e-6);
    EXPECT_NEAR(-1, a(1, 1), 1e-6);
}
