#include <memory>
#include <gtest/gtest.h>

#include <memory>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace SpectralAveraging;
using namespace FenestrationCommon;

// Creation of double range material with provided ratio
class TestDoubleRangeMaterialSolarRadiation : public testing::Test
{
private:
    std::shared_ptr<CMaterial> m_Material;

    CSeries loadSolarRadiation()
    {
        // Full ASTM E891-87 Table 1 (Solar radiation)
        CSeries aSolarRadiation(
          {{0.3000, 0.0},    {0.3050, 3.4},    {0.3100, 15.6},   {0.3150, 41.1},   {0.3200, 71.2},
           {0.3250, 100.2},  {0.3300, 152.4},  {0.3350, 155.6},  {0.3400, 179.4},  {0.3450, 186.7},
           {0.3500, 212.0},  {0.3600, 240.5},  {0.3700, 324.0},  {0.3800, 362.4},  {0.3900, 381.7},
           {0.4000, 556.0},  {0.4100, 656.3},  {0.4200, 690.8},  {0.4300, 641.9},  {0.4400, 798.5},
           {0.4500, 956.6},  {0.4600, 990.0},  {0.4700, 998.0},  {0.4800, 1046.1}, {0.4900, 1005.1},
           {0.5000, 1026.7}, {0.5100, 1066.7}, {0.5200, 1011.5}, {0.5300, 1084.9}, {0.5400, 1082.4},
           {0.5500, 1102.2}, {0.5700, 1087.4}, {0.5900, 1024.3}, {0.6100, 1088.8}, {0.6300, 1062.1},
           {0.6500, 1061.7}, {0.6700, 1046.2}, {0.6900, 859.2},  {0.7100, 1002.4}, {0.7180, 816.9},
           {0.7244, 842.8},  {0.7400, 971.0},  {0.7525, 956.3},  {0.7575, 942.2},  {0.7625, 524.8},
           {0.7675, 830.7},  {0.7800, 908.9},  {0.8000, 873.4},  {0.8160, 712.0},  {0.8237, 660.2},
           {0.8315, 765.5},  {0.8400, 799.8},  {0.8600, 815.2},  {0.8800, 778.3},  {0.9050, 630.4},
           {0.9150, 565.2},  {0.9250, 586.4},  {0.9300, 348.1},  {0.9370, 224.2},  {0.9480, 271.4},
           {0.9650, 451.2},  {0.9800, 549.7},  {0.9935, 630.1},  {1.0400, 582.9},  {1.0700, 539.7},
           {1.1000, 366.2},  {1.1200, 98.1},   {1.1300, 169.5},  {1.1370, 118.7},  {1.1610, 301.9},
           {1.1800, 406.8},  {1.2000, 375.2},  {1.2350, 423.6},  {1.2900, 365.7},  {1.3200, 223.4},
           {1.3500, 30.1},   {1.3950, 1.4},    {1.4425, 51.6},   {1.4625, 97.0},   {1.4770, 97.3},
           {1.4970, 167.1},  {1.5200, 239.3},  {1.5390, 248.8},  {1.5580, 249.3},  {1.5780, 222.3},
           {1.5920, 227.3},  {1.6100, 210.5},  {1.6300, 224.7},  {1.6460, 215.9},  {1.6780, 202.8},
           {1.7400, 158.2},  {1.8000, 28.6},   {1.8600, 1.8},    {1.9200, 1.1},    {1.9600, 19.7},
           {1.9850, 84.9},   {2.0050, 25.0},   {2.0350, 92.5},   {2.0650, 56.3},   {2.1000, 82.7},
           {2.1480, 76.2},   {2.1980, 66.4},   {2.2700, 65.0},   {2.3600, 57.6},   {2.4500, 19.8},
           {2.4940, 17.0},   {2.5370, 3.0},    {2.9410, 4.0},    {2.9730, 7.0},    {3.0050, 6.0},
           {3.0560, 3.0},    {3.1320, 5.0},    {3.1560, 18.0},   {3.2040, 1.2},    {3.2450, 3.0},
           {3.3170, 12.0},   {3.3440, 3.0},    {3.4500, 12.2},   {3.5730, 11.0},   {3.7650, 9.0},
           {4.0450, 6.9}

          });

        return aSolarRadiation;
    }

protected:
    virtual void SetUp()
    {
        // Solar range material
        const double Tsol = 0.1;
        const double Rfsol = 0.7;
        const double Rbsol = 0.7;

        // Visible range
        const double Tvis = 0.2;
        const double Rfvis = 0.6;
        const double Rbvis = 0.6;

        m_Material = Material::dualBandMaterial(
          Tsol, Tsol, Rfsol, Rbsol, Tvis, Tvis, Rfvis, Rbvis, loadSolarRadiation());
    }

public:
    std::shared_ptr<CMaterial> getMaterial()
    {
        return m_Material;
    };
};

TEST_F(TestDoubleRangeMaterialSolarRadiation, TestMaterialProperties)
{
    SCOPED_TRACE("Begin Test: Phi angles creation.");

    std::shared_ptr<CMaterial> aMaterial = getMaterial();

    double T = aMaterial->getProperty(Property::T, Side::Front);

    // Test for solar range first
    EXPECT_NEAR(0.1, T, 1e-6);

    double R = aMaterial->getProperty(Property::R, Side::Front);

    EXPECT_NEAR(0.7, R, 1e-6);

    size_t size = 4;

    std::vector<double> Transmittances = aMaterial->getBandProperties(Property::T, Side::Front);

    EXPECT_EQ(size, Transmittances.size());

    std::vector<double> correctResults;
    correctResults.push_back(0);
    correctResults.push_back(0.0021424777315234909);
    correctResults.push_back(0.2);
    correctResults.push_back(0.0021424777315234909);

    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], Transmittances[i], 1e-6);
    }

    std::vector<double> Reflectances = aMaterial->getBandProperties(Property::R, Side::Front);

    EXPECT_EQ(size, Reflectances.size());

    correctResults.clear();
    correctResults.push_back(0);
    correctResults.push_back(0.79785752226847650);
    correctResults.push_back(0.6);
    correctResults.push_back(0.79785752226847650);

    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], Reflectances[i], 1e-6);
    }
}
