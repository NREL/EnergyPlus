#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestRectangularPerforatedCell : public testing::Test
{
private:
    std::shared_ptr<CRectangularCellDescription> m_DescriptionCell;
    std::shared_ptr<CPerforatedCell> m_PerforatedCell;

protected:
    virtual void SetUp()
    {
        // create material
        const auto Tmat = 0.1;
        const auto Rfmat = 0.7;
        const auto Rbmat = 0.8;
        const auto minLambda = 0.3;
        const auto maxLambda = 2.5;
        const auto aMaterial =
          Material::singleBandMaterial(Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda);

        // make cell geometry
        const auto x = 10;          // mm
        const auto y = 10;          // mm
        const auto thickness = 1;   // mm
        const auto xHole = 5;       // mm
        const auto yHole = 5;       // mm
        m_DescriptionCell =
          std::make_shared<CRectangularCellDescription>(x, y, thickness, xHole, yHole);

        m_PerforatedCell = std::make_shared<CPerforatedCell>(aMaterial, m_DescriptionCell);
    }

public:
    std::shared_ptr<CPerforatedCell> GetCell()
    {
        return m_PerforatedCell;
    };

    std::shared_ptr<CRectangularCellDescription> GetDescription()
    {
        return m_DescriptionCell;
    };
};

TEST_F(TestRectangularPerforatedCell, TestRectangular1)
{
    SCOPED_TRACE("Begin Test: Rectangular perforated cell (Theta = 0, Phi = 0).");

    std::shared_ptr<CPerforatedCell> aCell = GetCell();
    std::shared_ptr<ICellDescription> aCellDescription = GetDescription();

    const auto Theta = 0;   // deg
    const auto Phi = 0;     // deg
    Side aFrontSide = Side::Front;
    Side aBackSide = Side::Back;

    CBeamDirection aDirection = CBeamDirection(Theta, Phi);

    double Tdir_dir = aCellDescription->T_dir_dir(aFrontSide, aDirection);
    EXPECT_NEAR(0.25, Tdir_dir, 1e-6);

    double Tdir_dif = aCell->T_dir_dif(aFrontSide, aDirection);
    EXPECT_NEAR(0.075, Tdir_dif, 1e-6);

    double Rfdir_dif = aCell->R_dir_dif(aFrontSide, aDirection);
    EXPECT_NEAR(0.525, Rfdir_dif, 1e-6);

    double Rbdir_dif = aCell->R_dir_dif(aBackSide, aDirection);
    EXPECT_NEAR(0.6, Rbdir_dif, 1e-6);
}

TEST_F(TestRectangularPerforatedCell, TestRectangular2)
{
    SCOPED_TRACE("Begin Test: Rectangular perforated cell (Theta = 45, Phi = 0).");

    std::shared_ptr<CPerforatedCell> aCell = GetCell();
    std::shared_ptr<ICellDescription> aCellDescription = GetDescription();

    double Theta = 45;   // deg
    double Phi = 0;      // deg
    Side aFrontSide = Side::Front;
    Side aBackSide = Side::Back;

    CBeamDirection aDirection = CBeamDirection(Theta, Phi);

    double Tdir_dir = aCellDescription->T_dir_dir(aFrontSide, aDirection);
    EXPECT_NEAR(0.2, Tdir_dir, 1e-6);

    double Tdir_dif = aCell->T_dir_dif(aFrontSide, aDirection);
    EXPECT_NEAR(0.08, Tdir_dif, 1e-6);

    double Rfdir_dif = aCell->R_dir_dif(aFrontSide, aDirection);
    EXPECT_NEAR(0.56, Rfdir_dif, 1e-6);

    double Rbdir_dif = aCell->R_dir_dif(aBackSide, aDirection);
    EXPECT_NEAR(0.64, Rbdir_dif, 1e-6);
}

TEST_F(TestRectangularPerforatedCell, TestRectangular3)
{
    SCOPED_TRACE("Begin Test: Rectangular perforated cell (Theta = 45, Phi = 45).");

    std::shared_ptr<CPerforatedCell> aCell = GetCell();
    std::shared_ptr<ICellDescription> aCellDescription = GetDescription();

    const auto Theta = 45;   // deg
    const auto Phi = 45;     // deg
    Side aFrontSide = Side::Front;
    Side aBackSide = Side::Back;

    CBeamDirection aDirection = CBeamDirection(Theta, Phi);

    double Tdir_dir = aCellDescription->T_dir_dir(aFrontSide, aDirection);
    EXPECT_NEAR(0.184289322, Tdir_dir, 1e-6);

    double Tdir_dif = aCell->T_dir_dif(aFrontSide, aDirection);
    EXPECT_NEAR(0.081571068, Tdir_dif, 1e-6);

    double Rfdir_dif = aCell->R_dir_dif(aFrontSide, aDirection);
    EXPECT_NEAR(0.570997475, Rfdir_dif, 1e-6);

    double Rbdir_dif = aCell->R_dir_dif(aBackSide, aDirection);
    EXPECT_NEAR(0.652568542, Rbdir_dif, 1e-6);
}
