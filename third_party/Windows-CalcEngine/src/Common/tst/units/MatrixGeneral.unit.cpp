#include <memory>
#include <stdexcept>
#include <vector>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace FenestrationCommon;

class TestMatrixGeneral : public testing::Test
{
protected:
    void SetUp() override
    {}
};

TEST_F(TestMatrixGeneral, TestSetDiagonal)
{
    SCOPED_TRACE("Begin Test: Test matrix set diagonal.");

    SquareMatrix a{{1, 2}, {3, 4}};
    const std::vector<double> b = {7, 8};

    a.setDiagonal(b);

    EXPECT_NEAR(7, a(0, 0), 1e-6);
    EXPECT_NEAR(0, a(0, 1), 1e-6);
    EXPECT_NEAR(0, a(1, 0), 1e-6);
    EXPECT_NEAR(8, a(1, 1), 1e-6);
}

TEST_F(TestMatrixGeneral, TestSetIdentity)
{
    SCOPED_TRACE("Begin Test: Test matrix set identity.");

    SquareMatrix a{{1, 2}, {3, 4}};

    a.setIdentity();

    EXPECT_NEAR(1, a(0, 0), 1e-6);
    EXPECT_NEAR(0, a(0, 1), 1e-6);
    EXPECT_NEAR(0, a(1, 0), 1e-6);
    EXPECT_NEAR(1, a(1, 1), 1e-6);
}

TEST_F(TestMatrixGeneral, TestSetDiagonalException)
{
    SCOPED_TRACE("Begin Test: Test matrix set diagonal exception.");

    SquareMatrix a{{1, 2}, {3, 4}};
    const std::vector<double> b = {7, 8, 9};

    try
    {
        a.setDiagonal(b);
    }
    catch(const std::runtime_error & err)
    {
        EXPECT_EQ(err.what(), std::string("Matrix and vector must be same size."));
    }
}