#include <gtest/gtest.h>

#include <WCECommon.hpp>

class TestTableInitialization : public testing::Test
{
protected:

    void SetUp() override
    {}
};

TEST_F(TestTableInitialization, TableInitalization)
{
    const Table::Table2D<double> tbl({2, 3}, {5, 8}, { {1, 2}, {3, 4}});
    const auto val{tbl(1,1)};

    EXPECT_EQ(4.0, val);
}
