#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"


using namespace FenestrationCommon;

class TestSeriesOperations : public testing::Test
{
private:
    CSeries m_Series1;
    CSeries m_Series2;

protected:
    void SetUp() override
    {
        m_Series1 = CSeries{{0.50, 3.3}, {0.51, 5.2}, {0.52, 8.9}, {0.53, 10.1}, {0.54, 11.4}};

        m_Series2 = CSeries{{0.50, 1.2}, {0.51, 6.1}, {0.52, 7.3}, {0.53, 9.5}, {0.54, 10.4}};
    }

public:
    CSeries getSeries1() const
    {
        return m_Series1;
    };

    CSeries getSeries2() const
    {
        return m_Series2;
    };
};

TEST_F(TestSeriesOperations, TestSeriesMultiplication)
{
    SCOPED_TRACE("Begin Test: Test multiplication over the range of data.");

    auto ser1 = getSeries1();
    auto ser2 = getSeries2();

    auto result = ser1 * ser2;

    std::vector<double> correctResults{3.96, 31.72, 64.97, 95.95, 118.56};

    EXPECT_EQ(result.size(), correctResults.size());

    for(size_t i = 0; i < result.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], result[i].value(), 1e-6);
    }
}

TEST_F(TestSeriesOperations, TestSeriesAddition)
{
    SCOPED_TRACE("Begin Test: Test addition over the range of data.");

    const auto ser1 = getSeries1();
    const auto ser2 = getSeries2();

    auto result = ser1 + ser2;

    std::vector<double> correctResults{4.5, 11.3, 16.2, 19.6, 21.8};

    EXPECT_EQ(result.size(), correctResults.size());

    for(size_t i = 0; i < result.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], result[i].value(), 1e-6);
    }
}

TEST_F(TestSeriesOperations, TestSeriesSubtraction)
{
    SCOPED_TRACE("Begin Test: Test subtraction over the range of data.");

    const auto ser1 = getSeries1();
    const auto ser2 = getSeries2();

    auto result = ser1 - ser2;

    std::vector<double> correctResults{2.1, -0.9, 1.6, 0.6, 1};

    EXPECT_EQ(result.size(), correctResults.size());

    for(size_t i = 0; i < result.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], result[i].value(), 1e-6);
    }
}

TEST_F(TestSeriesOperations, TestSeriesSubractionWithConstant)
{
    SCOPED_TRACE("Begin Test: Test subtraction over the range of data.");

    const double val{1.0};
    const auto ser2 = getSeries2();

    auto result = val - ser2;

    std::vector<double> correctResults{-0.2, -5.1, -6.3, -8.5, -9.4};

    EXPECT_EQ(result.size(), correctResults.size());

    for(size_t i = 0; i < result.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], result[i].value(), 1e-6);
    }
}

TEST_F(TestSeriesOperations, TestSeriesMultiplicationException)
{
    SCOPED_TRACE("Begin Test: Test multiplication with exception.");
    CSeries first;
    first.addProperty(1, 12);

    CSeries second;
    second.addProperty(2, 11);
    second.addProperty(5, 34);

    try
    {
        auto result = first * second;
    }
    catch(const std::runtime_error & err)
    {
        EXPECT_EQ(err.what(),
                  std::string(
                    "Wavelengths of two vectors are not the same. Cannot preform multiplication."));
    }
}