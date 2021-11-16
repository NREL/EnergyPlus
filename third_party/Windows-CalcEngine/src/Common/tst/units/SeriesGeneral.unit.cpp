#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"


using namespace FenestrationCommon;

class TestSeriesGeneral : public testing::Test
{
private:
    CSeries m_Series;

protected:
    void SetUp() override
    {
        const std::vector<std::pair<double, double>> vec{
          {0.5, 3.3}, {0.51, 5.2}, {0.52, 8.9}, {0.53, 10.1}, {0.54, 11.4}};

        m_Series = CSeries(vec);
    }

public:
    CSeries getSeries() const
    {
        return m_Series;
    };
};

TEST_F(TestSeriesGeneral, TestSeriesPoint)
{
    SCOPED_TRACE("Begin Test: Test simple series point.");

    const CSeriesPoint a(1, 5);
    CSeriesPoint b;
    b = a;

    EXPECT_EQ(a.value(), b.value());
    EXPECT_EQ(a.x(), b.x());

    b.value(7);

    EXPECT_EQ(b.value(), 7);

    const CSeriesPoint c(2, 5);

    EXPECT_TRUE(a < c);
}

TEST_F(TestSeriesGeneral, TestSeriesMultiplication)
{
    SCOPED_TRACE("Begin Test: Test multiplication over the range of data.");

    auto ser = getSeries();

    ser.insertToBeginning(0.55, 15.0);

    std::vector<double> correctResults{15.0, 3.3, 5.2, 8.9, 10.1, 11.4};

    EXPECT_EQ(ser.size(), correctResults.size());

    for(size_t i = 0; i < ser.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], ser[i].value(), 1e-6);
    }
}

TEST_F(TestSeriesGeneral, TestSeriesXValues)
{
    SCOPED_TRACE("Begin Test: Test getting x values from series.");

    auto ser = getSeries();

    const auto xValues = ser.getXArray();

    const std::vector<double> correctResults{0.50, 0.51, 0.52, 0.53, 0.54};

    EXPECT_EQ(ser.size(), correctResults.size());

    for(size_t i = 0; i < ser.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], xValues[i], 1e-6);
    }
}

TEST_F(TestSeriesGeneral, TestConstantSeries)
{
    SCOPED_TRACE("Begin Test: Test setting constant series.");

    const std::vector<double> x{1, 2, 3, 4, 5};

    CSeries test;
    test.setConstantValues(x, 12);

    const std::vector<double> correctResults{12, 12, 12, 12, 12};

    EXPECT_EQ(test.size(), correctResults.size());

    for(size_t i = 0; i < test.size(); ++i)
    {
        EXPECT_NEAR(correctResults[i], test[i].value(), 1e-6);
        EXPECT_NEAR(x[i], test[i].x(), 1e-6);
    }

    test.clear();
    EXPECT_EQ(test.size(), 0u);

    try
    {
        test[1];
    }
    catch(const std::out_of_range & err)
    {
        EXPECT_EQ(err.what(), std::string("Index out of range."));
    }
}