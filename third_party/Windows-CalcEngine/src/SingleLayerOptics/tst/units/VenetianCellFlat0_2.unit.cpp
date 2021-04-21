#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestVenetianCellFlat0_2 : public testing::Test
{
private:
    std::shared_ptr<CVenetianCell> m_Cell;

protected:
    virtual void SetUp()
    {
        // create material
		const auto Tmat = 0.1;
		const auto Rfmat = 0.7;
		const auto Rbmat = 0.7;
		const auto minLambda = 0.3;
		const auto maxLambda = 2.5;
        const auto aMaterial =
          Material::singleBandMaterial(Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda);


        // make cell geometry
		const auto slatWidth = 0.016;     // m
		const auto slatSpacing = 0.010;   // m
		const auto slatTiltAngle = 0;
		const auto curvatureRadius = 0;
        const size_t numOfSlatSegments = 2;

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

TEST_F(TestVenetianCellFlat0_2, TestVenetian1)
{
    SCOPED_TRACE("Begin Test: Venetian cell (Flat, 0 degrees slats) - directional-diffuse.");

    std::shared_ptr<CVenetianCell> aCell = GetCell();

    // Front side
    Side aSide = Side::Front;
    double Theta = 18;
    double Phi = 45;
    CBeamDirection incomingDirection = CBeamDirection(Theta, Phi);

    Theta = 18;
    Phi = 90;
    CBeamDirection outgoingDirection = CBeamDirection(Theta, Phi);

    double Tdir_dif = aCell->T_dir_dif(aSide, incomingDirection, outgoingDirection);
    double Rdir_dif = aCell->R_dir_dif(aSide, incomingDirection, outgoingDirection);

    EXPECT_NEAR(0.11272685101443769, Tdir_dif, 1e-6);
    EXPECT_NEAR(0.11272685101443769, Rdir_dif, 1e-6);
}
