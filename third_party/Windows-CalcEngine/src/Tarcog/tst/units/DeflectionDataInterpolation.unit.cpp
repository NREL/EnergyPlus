#include <gtest/gtest.h>

#include <WCETarcog.hpp>
#include <WCECommon.hpp>

class TestDeflectionDataInterpolation : public testing::Test
{

protected:
    void SetUp() override
    {}

public:
};

TEST_F(TestDeflectionDataInterpolation, InterpolationAtMidPoint)
{
    const auto tbl{DeflectionData::getWNData()};
    const auto interpolationValue{2.5};
    const auto col{Table::columnInterpolation(tbl, interpolationValue)};

    const std::vector<double> correctX{-5, -2.6, -0.2, 2.2, 4.6, 7};
    const std::vector<double> correctY{-4.25694, -1.85765, 0.47293, 2.08248, 3.14455, 4.69143};

    EXPECT_EQ(6u, col.size());
    for(unsigned i = 0u; i < correctY.size(); ++i)
    {
        EXPECT_NEAR(col[i].x.value(), correctX[i], 1e-5);
        EXPECT_NEAR(col[i].y.value(), correctY[i], 1e-5);
    }
}

TEST_F(TestDeflectionDataInterpolation, InterpolationAtStartPoint)
{
    const auto tbl{DeflectionData::getWNData()};
    const auto interpolationValue{1.0};
    const auto col{Table::columnInterpolation(tbl, interpolationValue)};

    const std::vector<double> correctX{-5, -2.6, -0.2, 2.2, 4.6, 7};
    const std::vector<double> correctY{-5.296, -2.8966, -0.5569, 1.067, 2.1892, 3.2125};

    EXPECT_EQ(6u, col.size());
    for(unsigned i = 0u; i < correctY.size(); ++i)
    {
        EXPECT_NEAR(col[i].x.value(), correctX[i], 1e-5);
        EXPECT_NEAR(col[i].y.value(), correctY[i], 1e-5);
    }
}

TEST_F(TestDeflectionDataInterpolation, InterpolationAtEndPoint)
{
    const auto tbl{DeflectionData::getWNData()};
    const auto interpolationValue{10.0};
    const auto col{Table::columnInterpolation(tbl, interpolationValue)};

    const std::vector<double> correctX{-5, -2.6, -0.2, 2.2, 4.6, 7};
    const std::vector<double> correctY{-4.1207, -1.7207, 0.6846, 3.1262, 4.7056, 6.23315};

    EXPECT_EQ(6u, col.size());
    for(unsigned i = 0u; i < correctY.size(); ++i)
    {
        EXPECT_NEAR(col[i].x.value(), correctX[i], 1e-5);
        EXPECT_NEAR(col[i].y.value(), correctY[i], 1e-5);
    }
}