#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace FenestrationCommon;

class TestLinearSolver1 : public testing::Test
{
protected:
    void SetUp() override
    {}
};

TEST_F(TestLinearSolver1, Test1)
{
    SCOPED_TRACE("Begin Test: Test Linear Solver (1) - Solving simple matrix.");

    SquareMatrix aMatrix{{2, 1, 3}, {2, 6, 8}, {6, 8, 18}};

    std::vector<double> aVector = {1, 3, 5};

    auto aSolution = CLinearSolver::solveSystem(aMatrix, aVector);

    EXPECT_NEAR(3.0 / 10.0, aSolution[0], 1e-6);
    EXPECT_NEAR(2.0 / 5.0, aSolution[1], 1e-6);
    EXPECT_NEAR(0.0, aSolution[2], 1e-6);
}

TEST_F(TestLinearSolver1, Test2)
{
    SCOPED_TRACE("Begin Test: Test Linear Solver (2) - Solving simple matrix.");

    SquareMatrix aMatrix{{32817.2867004354, 1, 0, -32808.3972386696},
                         {1.28054053432588, -1, 0, 0},
                         {0, 0, -1, 1.26433319889839},
                         {32808.3972386696, 0, -1, -32810.4664383299}};

    std::vector<double> aVector = {3163.241853, -73.479324, -67.913411, -1070.271453};

    auto aSolution = CLinearSolver::solveSystem(aMatrix, aVector);

    EXPECT_NEAR(303.040746, aSolution[0], 1e-6);
    EXPECT_NEAR(461.535283, aSolution[1], 1e-6);
    EXPECT_NEAR(451.057585, aSolution[2], 1e-6);
    EXPECT_NEAR(303.040507, aSolution[3], 1e-6);
}

TEST_F(TestLinearSolver1, TestSolverException)
{
    SCOPED_TRACE("Begin Test: Test Linear Solver - Test exception.");

    SquareMatrix aMatrix{{1, 2, 3}, {7, 2, 1}, {2, 4, 2}};

    std::vector<double> aVector = {1, 2};

    try
    {
        auto aSolution = CLinearSolver::solveSystem(aMatrix, aVector);
    }
    catch(const std::runtime_error & err)
    {
        EXPECT_EQ(
          err.what(),
          std::string("Matrix and vector for system of linear equations are not same size."));
    }
}
