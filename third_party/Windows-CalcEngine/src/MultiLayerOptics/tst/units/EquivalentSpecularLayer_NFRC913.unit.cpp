#include <memory>
#include <gtest/gtest.h>

#include "WCESpectralAveraging.hpp"
#include "WCEMultiLayerOptics.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace SingleLayerOptics;
using namespace FenestrationCommon;
using namespace SpectralAveraging;
using namespace MultiLayerOptics;

class EquivalentSpecularLayer_NFRC913 : public testing::Test
{
private:
    std::shared_ptr<CMultiPaneSpecular> m_Layer;

    CSeries loadSolarRadiationFile()
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

    std::shared_ptr<CSpectralSampleData> loadSampleData_NFRC_913()
    {
        auto aMeasurements_913 =
          CSpectralSampleData::create({{0.305, 0.0, 0.046, 0.046},   {0.31, 0.0, 0.046, 0.046},
                                       {0.315, 0.0, 0.047, 0.047},   {0.32, 0.0, 0.051, 0.051},
                                       {0.325, 0.0, 0.068, 0.068},   {0.33, 0.003, 0.084, 0.084},
                                       {0.335, 0.013, 0.078, 0.078}, {0.34, 0.048, 0.066, 0.066},
                                       {0.345, 0.121, 0.057, 0.057}, {0.35, 0.248, 0.054, 0.054},
                                       {0.36, 0.476, 0.055, 0.055},  {0.37, 0.588, 0.059, 0.059},
                                       {0.38, 0.548, 0.057, 0.057},  {0.39, 0.674, 0.063, 0.063},
                                       {0.4, 0.752, 0.067, 0.067},   {0.41, 0.759, 0.067, 0.067},
                                       {0.42, 0.756, 0.067, 0.067},  {0.43, 0.76, 0.066, 0.066},
                                       {0.44, 0.761, 0.066, 0.066},  {0.45, 0.781, 0.068, 0.068},
                                       {0.46, 0.803, 0.068, 0.068},  {0.47, 0.815, 0.068, 0.068},
                                       {0.48, 0.821, 0.069, 0.069},  {0.49, 0.825, 0.069, 0.069},
                                       {0.5, 0.828, 0.069, 0.069},   {0.51, 0.829, 0.069, 0.069},
                                       {0.52, 0.828, 0.069, 0.069},  {0.53, 0.825, 0.069, 0.069},
                                       {0.54, 0.82, 0.068, 0.068},   {0.55, 0.813, 0.068, 0.068},
                                       {0.57, 0.793, 0.067, 0.067},  {0.59, 0.767, 0.066, 0.066},
                                       {0.61, 0.734, 0.064, 0.064},  {0.63, 0.697, 0.062, 0.062},
                                       {0.65, 0.658, 0.06, 0.06},    {0.67, 0.616, 0.057, 0.057},
                                       {0.69, 0.571, 0.056, 0.056},  {0.71, 0.529, 0.054, 0.054},
                                       {0.718, 0.512, 0.054, 0.054}, {0.724, 0.498, 0.053, 0.053},
                                       {0.74, 0.468, 0.052, 0.052},  {0.752, 0.444, 0.051, 0.051},
                                       {0.758, 0.435, 0.051, 0.051}, {0.763, 0.427, 0.051, 0.051},
                                       {0.768, 0.418, 0.051, 0.051}, {0.78, 0.397, 0.05, 0.05},
                                       {0.8, 0.368, 0.05, 0.05},     {0.816, 0.347, 0.05, 0.05},
                                       {0.824, 0.339, 0.05, 0.05},   {0.832, 0.331, 0.05, 0.05},
                                       {0.84, 0.323, 0.049, 0.049},  {0.86, 0.305, 0.054, 0.054},
                                       {0.88, 0.29, 0.05, 0.05},     {0.905, 0.277, 0.05, 0.05},
                                       {0.915, 0.273, 0.052, 0.052}, {0.925, 0.267, 0.05, 0.05},
                                       {0.93, 0.265, 0.05, 0.05},    {0.937, 0.261, 0.05, 0.05},
                                       {0.948, 0.259, 0.049, 0.049}, {0.965, 0.253, 0.049, 0.049},
                                       {0.98, 0.248, 0.046, 0.046},  {0.994, 0.244, 0.047, 0.047},
                                       {1.04, 0.241, 0.049, 0.049},  {1.07, 0.24, 0.048, 0.048},
                                       {1.1, 0.237, 0.045, 0.045},   {1.12, 0.24, 0.047, 0.047},
                                       {1.13, 0.241, 0.048, 0.048},  {1.137, 0.242, 0.048, 0.048},
                                       {1.161, 0.245, 0.048, 0.048}, {1.18, 0.247, 0.047, 0.047},
                                       {1.2, 0.249, 0.045, 0.045},   {1.235, 0.26, 0.045, 0.045},
                                       {1.29, 0.28, 0.049, 0.049},   {1.32, 0.291, 0.046, 0.046},
                                       {1.35, 0.303, 0.038, 0.038},  {1.39, 0.328, 0.042, 0.042},
                                       {1.443, 0.368, 0.043, 0.043}, {1.463, 0.383, 0.042, 0.042},
                                       {1.477, 0.395, 0.042, 0.042}, {1.497, 0.41, 0.041, 0.041},
                                       {1.52, 0.426, 0.04, 0.04},    {1.539, 0.439, 0.04, 0.04},
                                       {1.559, 0.451, 0.04, 0.04},   {1.578, 0.462, 0.041, 0.041},
                                       {1.592, 0.47, 0.041, 0.041},  {1.61, 0.478, 0.041, 0.041},
                                       {1.63, 0.485, 0.041, 0.041},  {1.646, 0.491, 0.04, 0.04},
                                       {1.678, 0.497, 0.041, 0.041}, {1.74, 0.504, 0.041, 0.041},
                                       {1.8, 0.505, 0.037, 0.037},   {1.86, 0.503, 0.029, 0.029},
                                       {1.92, 0.506, 0.037, 0.037},  {1.961, 0.509, 0.044, 0.044},
                                       {1.985, 0.51, 0.048, 0.048},  {2.005, 0.512, 0.05, 0.05},
                                       {2.036, 0.517, 0.05, 0.05},   {2.065, 0.522, 0.05, 0.05},
                                       {2.1, 0.529, 0.049, 0.049},   {2.148, 0.536, 0.05, 0.05},
                                       {2.198, 0.534, 0.05, 0.05},   {2.27, 0.548, 0.05, 0.05},
                                       {2.36, 0.565, 0.051, 0.051},  {2.45, 0.565, 0.05, 0.05},
                                       {2.494, 0.561, 0.05, 0.05},   {2.537, 0.53, 0.049, 0.049},
                                       {2.942, 0.16, 0.04, 0.04},    {2.973, 0.127, 0.039, 0.039},
                                       {3.005, 0.097, 0.038, 0.038}, {3.056, 0.095, 0.038, 0.038},
                                       {3.132, 0.092, 0.038, 0.038}, {3.156, 0.091, 0.038, 0.038},
                                       {3.204, 0.089, 0.037, 0.037}, {3.245, 0.087, 0.037, 0.037},
                                       {3.317, 0.084, 0.037, 0.037}, {3.344, 0.083, 0.037, 0.037},
                                       {3.45, 0.078, 0.036, 0.036},  {3.573, 0.081, 0.036, 0.036},
                                       {3.765, 0.094, 0.035, 0.035}, {4.045, 0.1, 0.034, 0.034},
                                       {5.0, 0.0, 0.03, 0.03},       {6.0, 0.0, 0.023, 0.023},
                                       {7.0, 0.0, 0.012, 0.012},     {8.0, 0.0, 0.007, 0.007},
                                       {9.0, 0.0, 0.218, 0.218},     {10.0, 0.0, 0.235, 0.235},
                                       {11.0, 0.0, 0.14, 0.14},      {12.0, 0.0, 0.068, 0.068},
                                       {13.0, 0.0, 0.078, 0.078},    {14.0, 0.0, 0.065, 0.065},
                                       {15.0, 0.0, 0.053, 0.053},    {16.0, 0.0, 0.043, 0.043},
                                       {17.0, 0.0, 0.033, 0.033},    {18.0, 0.0, 0.025, 0.025},
                                       {19.0, 0.0, 0.068, 0.068},    {20.0, 0.0, 0.184, 0.184},
                                       {21.0, 0.0, 0.244, 0.244},    {22.0, 0.0, 0.24, 0.24},
                                       {23.0, 0.0, 0.21, 0.21},      {24.0, 0.003, 0.183, 0.183},
                                       {25.0, 0.0, 0.167, 0.167},    {26.0, 0.0, 0.153, 0.153},
                                       {27.0, 0.0, 0.143, 0.143},    {28.0, 0.0, 0.137, 0.137},
                                       {29.0, 0.0, 0.131, 0.131},    {30.0, 0.0, 0.127, 0.127},
                                       {31.0, 0.0, 0.125, 0.125},    {32.0, 0.0, 0.122, 0.122},
                                       {33.0, 0.0, 0.121, 0.121},    {34.0, 0.0, 0.12, 0.12},
                                       {35.0, 0.0, 0.12, 0.12},      {36.0, 0.0, 0.119, 0.119},
                                       {37.0, 0.0, 0.119, 0.119},    {38.0, 0.0, 0.119, 0.119},
                                       {39.0, 0.003, 0.119, 0.119},  {40.0, 0.005, 0.119, 0.119}});

        return aMeasurements_913;
    }

protected:
    virtual void SetUp()
    {
        const auto aSolarRadiation = loadSolarRadiationFile();

        double thickness = 4.7752e-3;   // [m]
        const auto aMaterial_913 = Material::nBandMaterial(
          loadSampleData_NFRC_913(), thickness, MaterialType::Monolithic, WavelengthRange::Solar);

        const auto layer913 = SpecularLayer::createLayer(aMaterial_913);

        m_Layer = CMultiPaneSpecular::create({layer913}, loadSolarRadiationFile());
    }

public:
    std::shared_ptr<CMultiPaneSpecular> getLayer() const
    {
        return m_Layer;
    };
};

TEST_F(EquivalentSpecularLayer_NFRC913, TestAngle0)
{
    SCOPED_TRACE("Begin Test: Specular MultiLayerOptics layer - angle = 0 deg.");

    const double angle = 0;

    CMultiPaneSpecular aLayer = *getLayer();

    const double T =
      aLayer.getPropertySimple(PropertySimple::T, Side::Front, Scattering::DirectDirect, angle, 0);
    EXPECT_NEAR(0.513367, T, 1e-6);

    const double Rf =
      aLayer.getPropertySimple(PropertySimple::R, Side::Front, Scattering::DirectDirect, angle, 0);
    EXPECT_NEAR(0.055074, Rf, 1e-6);

    const double Rb =
      aLayer.getPropertySimple(PropertySimple::R, Side::Back, Scattering::DirectDirect, angle, 0);
    EXPECT_NEAR(0.055074, Rb, 1e-6);

    const double Abs1 =
      aLayer.getAbsorptanceLayer(1, Side::Front, ScatteringSimple::Direct, angle, 0);
    EXPECT_NEAR(0.431559, Abs1, 1e-6);
}

TEST_F(EquivalentSpecularLayer_NFRC913, TestAngle10)
{
    SCOPED_TRACE("Begin Test: Specular MultiLayerOptics layer - angle = 10 deg.");

    const double angle = 10;

    CMultiPaneSpecular aLayer = *getLayer();

    const double T =
      aLayer.getPropertySimple(PropertySimple::T, Side::Front, Scattering::DirectDirect, angle, 0);
    EXPECT_NEAR(0.511677, T, 1e-6);

    const double Rf =
      aLayer.getPropertySimple(PropertySimple::R, Side::Front, Scattering::DirectDirect, angle, 0);
    EXPECT_NEAR(0.055026, Rf, 1e-6);

    const double Rb =
      aLayer.getPropertySimple(PropertySimple::R, Side::Back, Scattering::DirectDirect, angle, 0);
    EXPECT_NEAR(0.055026, Rb, 1e-6);

    const double Abs1 =
      aLayer.getAbsorptanceLayer(1, Side::Front, ScatteringSimple::Direct, angle, 0);
    EXPECT_NEAR(0.433297, Abs1, 1e-6);
}

TEST_F(EquivalentSpecularLayer_NFRC913, TestAngle20)
{
    SCOPED_TRACE("Begin Test: Specular MultiLayerOptics layer - angle = 20 deg.");

    const double angle = 20;

    CMultiPaneSpecular aLayer = *getLayer();

    const double T =
      aLayer.getPropertySimple(PropertySimple::T, Side::Front, Scattering::DirectDirect, angle, 0);
    EXPECT_NEAR(0.506483, T, 1e-6);

    const double Rf =
      aLayer.getPropertySimple(PropertySimple::R, Side::Front, Scattering::DirectDirect, angle, 0);
    EXPECT_NEAR(0.055132, Rf, 1e-6);

    const double Rb =
      aLayer.getPropertySimple(PropertySimple::R, Side::Back, Scattering::DirectDirect, angle, 0);
    EXPECT_NEAR(0.055132, Rb, 1e-6);

    const double Abs1 =
      aLayer.getAbsorptanceLayer(1, Side::Front, ScatteringSimple::Direct, angle, 0);
    EXPECT_NEAR(0.438385, Abs1, 1e-6);
}

TEST_F(EquivalentSpecularLayer_NFRC913, TestAngleHemispherical10)
{
    SCOPED_TRACE("Begin Test: Hemispherical to hemispherical with ten integration points.");

    const double minLambda = 0.3;
    const double maxLambda = 2.5;
    std::vector<double> aAngles{0, 10, 20, 30, 40, 50, 60, 70, 80, 90};

    CMultiPaneSpecular aLayer = *getLayer();

    double Tfhem =
      aLayer.getHemisphericalProperty(Side::Front, Property::T, aAngles, minLambda, maxLambda);
    EXPECT_NEAR(0.439701, Tfhem, 1e-6);

    double Tbhem =
      aLayer.getHemisphericalProperty(Side::Back, Property::T, aAngles, minLambda, maxLambda);
    EXPECT_NEAR(0.4397006, Tbhem, 1e-6);

    double Rfhem =
      aLayer.getHemisphericalProperty(Side::Front, Property::R, aAngles, minLambda, maxLambda);
    EXPECT_NEAR(0.106320, Rfhem, 1e-6);

    double Rbhem =
      aLayer.getHemisphericalProperty(Side::Back, Property::R, aAngles, minLambda, maxLambda);
    EXPECT_NEAR(0.106320, Rbhem, 1e-6);

    double Abs1 = aLayer.AbsHemispherical(1, aAngles, minLambda, maxLambda);
    EXPECT_NEAR(0.443805, Abs1, 1e-6);
}
