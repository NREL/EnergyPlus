#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace FenestrationCommon;

class TestMatrixMultiplication : public testing::Test
{
protected:
    void SetUp() override
    {}
};

TEST_F(TestMatrixMultiplication, Test1)
{
    SCOPED_TRACE("Begin Test: Test matrix multiplication (3 x 3).");

    const auto n = 3u;

    const SquareMatrix a{{4, 3, 9}, {8, 8, 4}, {4, 3, 7}};

    const SquareMatrix b{{6, 8, 5}, {3, 5, 6}, {1, 2, 3}};

    auto mult = a * b;

    EXPECT_EQ(n, mult.size());

    SquareMatrix multCorrect{{42, 65, 65}, {76, 112, 100}, {40, 61, 59}};

    for(size_t i = 0; i < n; ++i)
    {
        for(size_t j = 0; j < n; ++j)
        {
            EXPECT_NEAR(mult(i, j), multCorrect(i, j), 1e-6);
        }
    }
}

TEST_F(TestMatrixMultiplication, Test2)
{
    SCOPED_TRACE("Begin Test: Test matrix and std::vector multiplication (3 x 3) and (1 x 3).");

    const size_t n = 3;

    const SquareMatrix a{{4, 3, 9}, {8, 8, 4}, {4, 3, 7}};

    const std::vector<double> b = {8, 4, 6};

    auto mult = a * b;

    EXPECT_EQ(n, mult.size());

    std::vector<double> multCorrect = {98, 120, 86};

    for(size_t i = 0; i < n; ++i)
    {
        EXPECT_NEAR(mult[i], multCorrect[i], 1e-6);
    }
}

TEST_F(TestMatrixMultiplication, Test3)
{
    SCOPED_TRACE("Begin Test: Test matrix and std::vector multiplication (3 x 3) and (1 x 3).");

    const size_t n = 3;

    const SquareMatrix a{{4, 3, 9}, {8, 8, 4}, {4, 3, 7}};

    const std::vector<double> b = {8, 4, 6};

    auto mult = a * b;

    EXPECT_EQ(n, mult.size());

    std::vector<double> multCorrect1 = {98, 120, 86};

    for(size_t i = 0; i < n; ++i)
    {
        EXPECT_NEAR(mult[i], multCorrect1[i], 1e-6);
    }

    mult = b * a;

    std::vector<double> multCorrect2 = {88, 74, 130};

    for(size_t i = 0; i < n; ++i)
    {
        EXPECT_NEAR(mult[i], multCorrect2[i], 1e-6);
    }
}

TEST_F(TestMatrixMultiplication, Test4)
{
    SCOPED_TRACE("Begin Test: Test matrix multiplication (3 x 3).");

    const auto n = 3u;

    SquareMatrix a{{4, 3, 9}, {8, 8, 4}, {4, 3, 7}};

    const SquareMatrix b{{6, 8, 5}, {3, 5, 6}, {1, 2, 3}};

    a *= b;

    EXPECT_EQ(n, a.size());

    SquareMatrix multCorrect{{42, 65, 65}, {76, 112, 100}, {40, 61, 59}};

    for(size_t i = 0; i < n; ++i)
    {
        for(size_t j = 0; j < n; ++j)
        {
            EXPECT_NEAR(a(i, j), multCorrect(i, j), 1e-6);
        }
    }
}

TEST_F(TestMatrixMultiplication, Test5)
{
    SCOPED_TRACE("Begin Test: Test matrix mmultRow.");

    const size_t n = 3;

    SquareMatrix a{{4, 3, 9}, {8, 8, 4}, {4, 3, 7}};

    const std::vector<double> b = {8, 4, 6};

    SquareMatrix mult = a.mmultRows(b);

    EXPECT_EQ(n, mult.size());

    const SquareMatrix multCorrect{{32, 12, 54}, {64, 32, 24}, {32, 12, 42}};

    for(size_t i = 0; i < n; ++i)
    {
        for(size_t j = 0; j < n; ++j)
        {
            EXPECT_NEAR(mult(i, j), multCorrect(i, j), 1e-6);
        }
    }
}

TEST_F(TestMatrixMultiplication, Test6)
{
    SCOPED_TRACE("Begin Test: Test matrix and std::vector multiplication exception.");

    const SquareMatrix a{{4, 3, 9}, {8, 8, 4}, {4, 3, 7}};

    const std::vector<double> b = {8, 4};

    try
    {
        auto mult = a * b;
    }
    catch(const std::runtime_error & err)
    {
        EXPECT_EQ(err.what(), std::string("Vector and matrix do not have same size."));
    }
}

TEST_F(TestMatrixMultiplication, Test7)
{
    SCOPED_TRACE("Begin Test: Test matrix and std::vector multiplication exception.");

    const SquareMatrix a{{4, 3, 9}, {8, 8, 4}, {4, 3, 7}};

    const std::vector<double> b = {8, 4};

    try
    {
        auto mult = b * a;
    }
    catch(const std::runtime_error & err)
    {
        EXPECT_EQ(err.what(), std::string("Vector and matrix do not have same size."));
    }
}

TEST_F(TestMatrixMultiplication, Test8)
{
    SCOPED_TRACE("Begin Test: Test matrix multiplication exception.");

    const SquareMatrix a{{4, 3, 9}, {8, 8, 4}, {4, 3, 7}};

    const SquareMatrix b{{6, 8}, {3, 5}};

    try
    {
        auto mult = a * b;
    }
    catch(const std::runtime_error & err)
    {
        EXPECT_EQ(err.what(), std::string("Matrices must be identical in size."));
    }
}

TEST_F(TestMatrixMultiplication, TestMultRowsException)
{
    SCOPED_TRACE("Begin Test: Test matrix mmultRow exception.");

    SquareMatrix a{{4, 3, 9}, {8, 8, 4}, {4, 3, 7}};

    const std::vector<double> b = {8, 4};

    try
    {
        SquareMatrix mult = a.mmultRows(b);
    }
    catch(const std::runtime_error & err)
    {
        EXPECT_EQ(err.what(), std::string("Vector and matrix do not have same size."));
    }
}