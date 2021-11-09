#include <gtest/gtest.h>

#include <WCECommon.hpp>

class TestTableData : public testing::Test
{
private:
    Table::Table2D<double> table_{{2, 3}, {5, 8}, {{1, 2}, {3, 4}}};

protected:
    void SetUp() override
    {}

public:
    [[nodiscard]] Table::Table2D<double> getTable() const
    {
        return table_;
    }
};

TEST_F(TestTableData, AccessRow)
{
    const auto tbl{getTable()};
    const auto row{tbl.row(0u)};

    const std::vector<double> correct{1, 2};

    EXPECT_EQ(2u, row.size());
    for(unsigned i = 0u; i < correct.size(); ++i)
    {
        EXPECT_NEAR(row[i], correct[i], 1e-6);
    }
}

TEST_F(TestTableData, AccessColumn)
{
    const auto tbl{getTable()};
    const auto row{tbl.column(1u)};

    const std::vector<double> correct{2, 4};

    EXPECT_EQ(2u, row.size());
    for(unsigned i = 0u; i < correct.size(); ++i)
    {
        EXPECT_NEAR(row[i], correct[i], 1e-6);
    }
}