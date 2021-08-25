#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"

class TestCMABestWorstUvalues : public testing::Test
{};

TEST_F(TestCMABestWorstUvalues, TestBestIGUUValue)
{
    SCOPED_TRACE("Begin Test: Test CMA Best Worst IGU U-value calculations");
    auto best{CMA::CreateBestWorstUFactorOption(CMA::Option::Best)};
    const auto uValue{best.uValue()};
    const auto correctUValue{0.454198};

    EXPECT_NEAR(correctUValue, uValue, 1e-5);
}

TEST_F(TestCMABestWorstUvalues, TestWorstIGUUValue)
{
    SCOPED_TRACE("Begin Test: Test CMA Best Worst IGU U-value calculations");
    auto worst{CMA::CreateBestWorstUFactorOption(CMA::Option::Worst)};
    const auto uValue{worst.uValue()};
    const auto correctUValue{2.839511};

    EXPECT_NEAR(correctUValue, uValue, 1e-5);
}
