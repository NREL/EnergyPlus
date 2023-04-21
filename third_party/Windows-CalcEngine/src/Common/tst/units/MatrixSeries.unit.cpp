#include <memory>
#include <vector>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace FenestrationCommon;

class TestMatrixSeries : public testing::Test
{
private:
    std::shared_ptr<CMatrixSeries> m_MatrixSeries;

protected:
    void SetUp() override
    {
        // Create some number for matrix series to be tested
        std::vector<SquareMatrix> mat;
        std::vector<double> wl;
        mat.emplace_back(SquareMatrix{{2.8, 3.4}, {3.9, 7.5}});
        wl.push_back(0.45);

        mat.emplace_back(SquareMatrix{{7.4, 9.6}, {7.7, 1.3}});
        wl.push_back(0.50);

        mat.emplace_back(SquareMatrix{{8.3, 0.1}, {2.2, 3.6}});
        wl.push_back(0.55);

        mat.emplace_back(SquareMatrix{{1.5, 9.3}, {9.0, 7.4}});
        wl.push_back(0.60);

        // Fill up matrix series
        m_MatrixSeries = std::make_shared<CMatrixSeries>(2, 2);
        for(size_t i = 0; i < wl.size(); ++i)
        {
            m_MatrixSeries->addProperties(wl[i], mat[i]);
        }
    }

public:
    std::shared_ptr<CMatrixSeries> getMatrix() const
    {
        return m_MatrixSeries;
    }
};

TEST_F(TestMatrixSeries, Test1)
{
    SCOPED_TRACE("Begin Test: Test matrix series sum.");

    auto & aMat = *getMatrix();

    const double minLambda = 0.45;
    const double maxLambda = 0.65;
    const std::vector<double> scaleFactors = {1, 1, 1, 1};

    // Note that you need to increase maxLambda from 0.6 to 0.65 in order to include last range into
    // sum. Inserting 0.6 would mean that last range to be included into sum is from 0.55 to 0.6 and
    // last range is from 0.6 to infinity.
    auto mat = aMat.getSquaredMatrixSums(minLambda, maxLambda, scaleFactors);

    SquareMatrix correctResults{{20, 22.4}, {22.8, 19.8}};

    EXPECT_EQ(correctResults.size(), mat.size());

    for(size_t i = 0; i < mat.size(); ++i)
    {
        for(size_t j = 0; j < mat.size(); ++j)
        {
            EXPECT_NEAR(correctResults(i, j), mat(i, j), 1e-6);
        }
    }
}

TEST_F(TestMatrixSeries, Test2)
{
    SCOPED_TRACE("Begin Test: Test matrix series multiplication.");

    CMatrixSeries mat(*getMatrix());

    CSeries multiplier;
    multiplier.addProperty(0.45, 1.6);
    multiplier.addProperty(0.50, 3.8);
    multiplier.addProperty(0.55, 2.4);
    multiplier.addProperty(0.60, 8.3);

    mat.mMult(multiplier);

    std::vector<std::vector<double>> correctResults{{4.48, 28.12, 19.92, 12.45},
                                                    {5.44, 36.48, 0.24, 77.19},
                                                    {6.24, 29.26, 5.28, 74.7},
                                                    {12, 4.94, 8.64, 61.42}};

    std::vector<CSeries> matrixResults;

    for(size_t i = 0; i < mat.size1(); ++i)
    {
        for(size_t j = 0; j < mat.size2(); ++j)
        {
            auto aSeries = mat[i][j];
            matrixResults.push_back(aSeries);
        }
    }

    for(size_t i = 0; i < matrixResults.size(); ++i)
    {
        for(size_t k = 0; k < matrixResults[i].size(); ++k)
        {
            EXPECT_NEAR(correctResults[i][k], matrixResults[i][k].value(), 1e-6);
        }
    }
}
