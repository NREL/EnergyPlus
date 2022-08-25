#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestVenetianCellFlat45_3 : public testing::Test
{
private:
    std::shared_ptr<CVenetianCell> m_Cell;

protected:
    virtual void SetUp()
    {
        // create material
        const auto Tmat = 0.0;
        const auto Rfmat = 0.1;
        const auto Rbmat = 0.1;
        const auto minLambda = 0.3;
        const auto maxLambda = 2.5;
        const auto aMaterial =
          Material::singleBandMaterial(Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda);


        // make cell geometry
        const auto slatWidth = 0.016;     // m
        const auto slatSpacing = 0.012;   // m
        const auto slatTiltAngle = 0;
        const auto curvatureRadius = 0;
        const size_t numOfSlatSegments = 5;

        std::shared_ptr<CVenetianCellDescription> aCellDescription =
          std::make_shared<CVenetianCellDescription>(
            slatWidth, slatSpacing, slatTiltAngle, curvatureRadius, numOfSlatSegments);

        m_Cell = std::make_shared<CVenetianCell>(aMaterial, aCellDescription);
    }

public:
    std::shared_ptr<CVenetianCell> GetCell()
    {
        return m_Cell;
    };
};

TEST_F(TestVenetianCellFlat45_3, TestVenetian1)
{
    SCOPED_TRACE("Begin Test: Venetian cell (Flat, 45 degrees slats) - diffuse-diffuse.");

    std::shared_ptr<CVenetianCell> aCell = GetCell();

    // Front side
    Side aSide = Side::Front;
    double Tdif_dif = aCell->T_dif_dif(aSide);
    double Rdif_dif = aCell->R_dif_dif(aSide);

    EXPECT_NEAR(0.347602, Tdif_dif, 1e-6);
    EXPECT_NEAR(0.021039, Rdif_dif, 1e-6);

    // Back side
    aSide = Side::Back;
    Tdif_dif = aCell->T_dif_dif(aSide);
    Rdif_dif = aCell->R_dif_dif(aSide);

    EXPECT_NEAR(0.347602, Tdif_dif, 1e-6);
    EXPECT_NEAR(0.021039, Rdif_dif, 1e-6);
}

TEST_F(TestVenetianCellFlat45_3, TestVenetian2)
{
    SCOPED_TRACE("Begin Test: Venetian cell (Flat, 45 degrees slats) - direct-diffuse.");

    std::shared_ptr<CVenetianCell> aCell = GetCell();

    // Front side
    Side aSide = Side::Front;
    double Theta = 0;
    double Phi = 0;
    CBeamDirection aDirection = CBeamDirection(Theta, Phi);

    double Tdir_dir = aCell->T_dir_dir(aSide, aDirection);
    double Tdir_dif = aCell->T_dir_dif(aSide, aDirection);
    double Rdir_dif = aCell->R_dir_dif(aSide, aDirection);

    EXPECT_NEAR(1.0, Tdir_dir, 1e-6);
    EXPECT_NEAR(0.0, Tdir_dif, 1e-6);
    EXPECT_NEAR(0.0, Rdir_dif, 1e-6);

    // Back side
    aSide = Side::Back;
    Tdir_dir = aCell->T_dir_dir(aSide, aDirection);
    Tdir_dif = aCell->T_dir_dif(aSide, aDirection);
    Rdir_dif = aCell->R_dir_dif(aSide, aDirection);

    EXPECT_NEAR(1.0, Tdir_dir, 1e-6);
    EXPECT_NEAR(0.0, Tdir_dif, 1e-6);
    EXPECT_NEAR(0.0, Rdir_dif, 1e-6);
}
