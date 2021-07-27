#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestWovenCell2 : public testing::Test
{
private:
    std::shared_ptr<CWovenCell> m_Cell;

protected:
    virtual void SetUp()
    {
        // create material
        const auto Tmat = 0.15;
        const auto Rfmat = 0.8;
        const auto Rbmat = 0.6;
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

TEST_F(TestWovenCell2, TestWoven1)
{
    SCOPED_TRACE("Begin Test: Woven cell (Theta = 0, Phi = 0).");

    std::shared_ptr<CWovenCell> aCell = GetCell();

    const double Theta = 0;   // deg
    const double Phi = 0;     // deg
    const Side aFrontSide = Side::Front;
    const Side aBackSide = Side::Back;

    const CBeamDirection aDirection = CBeamDirection(Theta, Phi);

    double Tdir_dir = aCell->T_dir_dir(aFrontSide, aDirection);
    EXPECT_NEAR(0.444444444, Tdir_dir, 1e-6);

    Tdir_dir = aCell->T_dir_dir(aBackSide, aDirection);
    EXPECT_NEAR(0.444444444, Tdir_dir, 1e-6);

    double Tdir_dif = aCell->T_dir_dif(aFrontSide, aDirection);
    EXPECT_NEAR(0.055148, Tdir_dif, 1e-6);

    Tdir_dif = aCell->T_dir_dif(aBackSide, aDirection);
    EXPECT_NEAR(0.062702, Tdir_dif, 1e-6);

    double Rdir_dif = aCell->R_dir_dif(aFrontSide, aDirection);
    EXPECT_NEAR(0.435593, Rdir_dif, 1e-6);

    Rdir_dif = aCell->R_dir_dif(aBackSide, aDirection);
    EXPECT_NEAR(0.316928, Rdir_dif, 1e-6);
}

TEST_F(TestWovenCell2, TestWoven2)
{
    SCOPED_TRACE("Begin Test: Woven cell (Theta = 45, Phi = 0).");

    std::shared_ptr<CWovenCell> aCell = GetCell();

    const double Theta = 45;   // deg
    const double Phi = 0;      // deg
    const Side aFrontSide = Side::Front;
    const Side aBackSide = Side::Back;

    const CBeamDirection aDirection = CBeamDirection(Theta, Phi);

    double Tdir_dir = aCell->T_dir_dir(aFrontSide, aDirection);
    EXPECT_NEAR(0.352396986, Tdir_dir, 1e-6);

    Tdir_dir = aCell->T_dir_dir(aBackSide, aDirection);
    EXPECT_NEAR(0.352396986, Tdir_dir, 1e-6);

    double Tdir_dif = aCell->T_dir_dif(aFrontSide, aDirection);
    EXPECT_NEAR(0.110951, Tdir_dif, 1e-6);

    Tdir_dif = aCell->T_dir_dif(aBackSide, aDirection);
    EXPECT_NEAR(0.132270, Tdir_dif, 1e-6);

    double Rdir_dif = aCell->R_dir_dif(aFrontSide, aDirection);
    EXPECT_NEAR(0.470040, Rdir_dif, 1e-6);

    Rdir_dif = aCell->R_dir_dif(aBackSide, aDirection);
    EXPECT_NEAR(0.319200, Rdir_dif, 1e-6);
}

TEST_F(TestWovenCell2, TestWoven3)
{
    SCOPED_TRACE("Begin Test: Woven cell (Theta = 78, Phi = 45).");

    std::shared_ptr<CWovenCell> aCell = GetCell();

    const double Theta = 78;   // deg
    const double Phi = 45;     // deg
    const Side aFrontSide = Side::Front;
    const Side aBackSide = Side::Back;

    const CBeamDirection aDirection = CBeamDirection(Theta, Phi);

    double Tdir_dir = aCell->T_dir_dir(aFrontSide, aDirection);
    EXPECT_NEAR(0.0, Tdir_dir, 1e-6);

    Tdir_dir = aCell->T_dir_dir(aBackSide, aDirection);
    EXPECT_NEAR(0.0, Tdir_dir, 1e-6);

    double Tdir_dif = aCell->T_dir_dif(aFrontSide, aDirection);
    EXPECT_NEAR(0.168097, Tdir_dif, 1e-6);

    Tdir_dif = aCell->T_dir_dif(aBackSide, aDirection);
    EXPECT_NEAR(0.175433, Tdir_dif, 1e-6);

    double Rdir_dif = aCell->R_dir_dif(aFrontSide, aDirection);
    EXPECT_NEAR(0.781903, Rdir_dif, 1e-6);

    Rdir_dif = aCell->R_dir_dif(aBackSide, aDirection);
    EXPECT_NEAR(0.574567, Rdir_dif, 1e-6);
}
