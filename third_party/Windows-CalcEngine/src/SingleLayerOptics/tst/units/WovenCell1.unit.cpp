#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestWovenCell1 : public testing::Test
{
private:
    std::shared_ptr<CWovenCell> m_Cell;

protected:
    virtual void SetUp()
    {
        // create material
		const auto Tmat = 0.08;
		const auto Rfmat = 0.9;
		const auto Rbmat = 0.9;
		const auto minLambda = 0.3;
		const auto maxLambda = 2.5;
        const auto aMaterial =
          Material::singleBandMaterial(Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda);

        // make cell geometry
		const auto diameter = 6.35;   // mm
		const auto spacing = 19.05;   // mm
        std::shared_ptr<ICellDescription> aCell =
          std::make_shared<CWovenCellDescription>(diameter, spacing);

        m_Cell = std::make_shared<CWovenCell>(aMaterial, aCell);
    }

public:
    std::shared_ptr<CWovenCell> GetCell()
    {
        return m_Cell;
    };
};

TEST_F(TestWovenCell1, TestWoven1)
{
    SCOPED_TRACE("Begin Test: Woven cell (Theta = 0, Phi = 0).");

    std::shared_ptr<CWovenCell> aCell = GetCell();

    const double Theta{0}; // deg
    const double Phi{0};   // deg
    const Side aSide{Side::Front};

    CBeamDirection aDirection = CBeamDirection(Theta, Phi);

    const double Tdir_dir = aCell->T_dir_dir(aSide, aDirection);
    EXPECT_NEAR(0.444444444, Tdir_dir, 1e-6);

    const double Tdir_dif = aCell->T_dir_dif(aSide, aDirection);
    EXPECT_NEAR(0.045908, Tdir_dif, 1e-6);

    const double Rfdir_dif = aCell->R_dir_dif(aSide, aDirection);
    EXPECT_NEAR(0.478783, Rfdir_dif, 1e-6);

    const double Rbdir_dif = aCell->R_dir_dif(aSide, aDirection);
    EXPECT_NEAR(0.478783, Rbdir_dif, 1e-6);
}

TEST_F(TestWovenCell1, TestWoven2)
{
    SCOPED_TRACE("Begin Test: Woven cell (Theta = 45, Phi = 0).");

    std::shared_ptr<CWovenCell> aCell = GetCell();

    const double Theta{45}; // deg
    const double Phi{0};    // deg
    const Side aSide{Side::Front};

    CBeamDirection aDirection = CBeamDirection(Theta, Phi);

    const double Tdir_dir = aCell->T_dir_dir(aSide, aDirection);
    EXPECT_NEAR(0.352397, Tdir_dir, 1e-6);

    const double Tdir_dif = aCell->T_dir_dif(aSide, aDirection);
    EXPECT_NEAR(0.114759, Tdir_dif, 1e-6);

    const double Rfdir_dif = aCell->R_dir_dif(aSide, aDirection);
    EXPECT_NEAR(0.501635, Rfdir_dif, 1e-6);

    const double Rbdir_dif = aCell->R_dir_dif(aSide, aDirection);
    EXPECT_NEAR(0.501635, Rbdir_dif, 1e-6);
}

TEST_F(TestWovenCell1, TestWoven3)
{
    SCOPED_TRACE("Begin Test: Woven cell (Theta = 78, Phi = 45).");

    std::shared_ptr<CWovenCell> aCell = GetCell();

    const double Theta = 78; // deg
    const double Phi = 45;   // deg
    const Side aSide = Side::Front;

    const CBeamDirection aDirection = CBeamDirection(Theta, Phi);

    const double Tdir_dir = aCell->T_dir_dir(aSide, aDirection);
    EXPECT_NEAR(0.0, Tdir_dir, 1e-6);

    const double Tdir_dif = aCell->T_dir_dif(aSide, aDirection);
    EXPECT_NEAR(0.109392, Tdir_dif, 1e-6);

    const double Rfdir_dif = aCell->R_dir_dif(aSide, aDirection);
    EXPECT_NEAR(0.870608, Rfdir_dif, 1e-6);

    const double Rbdir_dif = aCell->R_dir_dif(aSide, aDirection);
    EXPECT_NEAR(0.870608, Rbdir_dif, 1e-6);
}

TEST_F(TestWovenCell1, TestWoven4)
{
    SCOPED_TRACE("Begin Test: Woven cell (Theta = 54, Phi = 270).");

    std::shared_ptr<CWovenCell> aCell = GetCell();

    const double Theta = 54; // deg
    const double Phi = 270;  // deg
    const Side aSide = Side::Front;

    const CBeamDirection aDirection = CBeamDirection(Theta, Phi);

    const double Tdir_dir = aCell->T_dir_dir(aSide, aDirection);
    EXPECT_NEAR(0.100838024, Tdir_dir, 1e-6);

    const double Tdir_dif = aCell->T_dir_dif(aSide, aDirection);
    EXPECT_NEAR(0.193195, Tdir_dif, 1e-6);

    const double Rfdir_dif = aCell->R_dir_dif(aSide, aDirection);
    EXPECT_NEAR(0.680730, Rfdir_dif, 1e-6);

    const double Rbdir_dif = aCell->R_dir_dif(aSide, aDirection);
    EXPECT_NEAR(0.680730, Rbdir_dif, 1e-6);
}
