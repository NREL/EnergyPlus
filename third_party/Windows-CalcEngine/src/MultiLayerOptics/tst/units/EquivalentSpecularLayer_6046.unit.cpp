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

class EquivalentSpecularLayer_6046 : public testing::Test
{
private:
    std::shared_ptr<CMultiPaneSpecular> m_Layer;

    CSeries loadSolarRadiationFile() const
    {
        // Full ASTM E891-87 Table 1 (Solar radiation)
        auto aSolarRadiation = CSeries(
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

    std::shared_ptr<CSpectralSampleData> loadSampleData_NFRC_6046() const
    {
        auto aMeasurement = CSpectralSampleData::create(
          {{0.3, 0.0, 0.057, 0.322},     {0.305, 0.0, 0.056, 0.328},   {0.31, 0.0, 0.055, 0.336},
           {0.315, 0.0, 0.054, 0.35},    {0.32, 0.003, 0.047, 0.355},  {0.325, 0.008, 0.048, 0.355},
           {0.33, 0.018, 0.056, 0.362},  {0.335, 0.03, 0.057, 0.352},  {0.34, 0.042, 0.062, 0.341},
           {0.345, 0.055, 0.068, 0.332}, {0.35, 0.067, 0.075, 0.322},  {0.355, 0.084, 0.085, 0.314},
           {0.36, 0.108, 0.096, 0.31},   {0.365, 0.145, 0.112, 0.3},   {0.37, 0.195, 0.127, 0.282},
           {0.375, 0.255, 0.135, 0.261}, {0.38, 0.319, 0.133, 0.223},  {0.385, 0.395, 0.137, 0.187},
           {0.39, 0.467, 0.135, 0.155},  {0.395, 0.533, 0.128, 0.125}, {0.4, 0.587, 0.12, 0.101},
           {0.41, 0.66, 0.104, 0.068},   {0.42, 0.705, 0.087, 0.05},   {0.43, 0.734, 0.076, 0.043},
           {0.44, 0.748, 0.068, 0.039},  {0.45, 0.761, 0.06, 0.038},   {0.46, 0.766, 0.058, 0.041},
           {0.47, 0.776, 0.055, 0.043},  {0.48, 0.786, 0.054, 0.045},  {0.49, 0.791, 0.053, 0.046},
           {0.5, 0.799, 0.055, 0.049},   {0.51, 0.803, 0.057, 0.051},  {0.52, 0.806, 0.059, 0.052},
           {0.53, 0.807, 0.061, 0.052},  {0.54, 0.808, 0.062, 0.051},  {0.55, 0.806, 0.062, 0.05},
           {0.56, 0.799, 0.062, 0.048},  {0.57, 0.798, 0.06, 0.045},   {0.58, 0.796, 0.059, 0.042},
           {0.59, 0.788, 0.058, 0.04},   {0.6, 0.781, 0.057, 0.037},   {0.61, 0.771, 0.058, 0.037},
           {0.62, 0.76, 0.06, 0.038},    {0.63, 0.744, 0.063, 0.042},  {0.64, 0.728, 0.067, 0.047},
           {0.65, 0.711, 0.075, 0.058},  {0.66, 0.687, 0.086, 0.071},  {0.67, 0.663, 0.098, 0.088},
           {0.68, 0.637, 0.113, 0.11},   {0.69, 0.609, 0.128, 0.134},  {0.7, 0.577, 0.147, 0.162},
           {0.71, 0.543, 0.165, 0.191},  {0.72, 0.512, 0.187, 0.225},  {0.73, 0.481, 0.206, 0.26},
           {0.74, 0.448, 0.228, 0.294},  {0.75, 0.413, 0.247, 0.333},  {0.76, 0.385, 0.266, 0.371},
           {0.77, 0.354, 0.278, 0.401},  {0.78, 0.329, 0.297, 0.435},  {0.79, 0.302, 0.31, 0.462},
           {0.8, 0.28, 0.328, 0.502},    {0.81, 0.26, 0.338, 0.523},   {0.82, 0.241, 0.348, 0.559},
           {0.83, 0.218, 0.362, 0.591},  {0.84, 0.199, 0.36, 0.6},     {0.85, 0.187, 0.356, 0.613},
           {0.86, 0.162, 0.367, 0.647},  {0.87, 0.164, 0.387, 0.665},  {0.88, 0.148, 0.399, 0.684},
           {0.89, 0.141, 0.403, 0.704},  {0.9, 0.129, 0.407, 0.72},    {0.91, 0.115, 0.409, 0.729},
           {0.92, 0.113, 0.41, 0.741},   {0.93, 0.102, 0.413, 0.755},  {0.94, 0.099, 0.412, 0.762},
           {0.95, 0.09, 0.413, 0.767},   {0.96, 0.084, 0.42, 0.783},   {0.97, 0.082, 0.424, 0.793},
           {0.98, 0.075, 0.422, 0.793},  {0.99, 0.073, 0.431, 0.812},  {1.0, 0.066, 0.427, 0.81},
           {1.05, 0.053, 0.442, 0.842},  {1.1, 0.04, 0.454, 0.872},    {1.15, 0.033, 0.461, 0.881},
           {1.2, 0.028, 0.476, 0.893},   {1.25, 0.025, 0.491, 0.906},  {1.3, 0.022, 0.512, 0.92},
           {1.35, 0.016, 0.53, 0.919},   {1.4, 0.02, 0.555, 0.927},    {1.45, 0.015, 0.584, 0.932},
           {1.5, 0.015, 0.622, 0.943},   {1.55, 0.012, 0.64, 0.939},   {1.6, 0.012, 0.659, 0.936},
           {1.65, 0.01, 0.667, 0.936},   {1.7, 0.013, 0.677, 0.939},   {1.75, 0.006, 0.682, 0.948},
           {1.8, 0.01, 0.674, 0.943},    {1.85, 0.009, 0.671, 0.941},  {1.9, 0.006, 0.677, 0.946},
           {1.95, 0.009, 0.67, 0.949},   {2.0, 0.008, 0.677, 0.951},   {2.05, 0.005, 0.671, 0.947},
           {2.1, 0.006, 0.68, 0.953},    {2.15, 0.009, 0.672, 0.957},  {2.2, 0.007, 0.638, 0.953},
           {2.25, 0.0, 0.642, 0.958},    {2.3, 0.0, 0.65, 0.943},      {2.35, 0.008, 0.638, 0.942},
           {2.4, 0.0, 0.652, 0.932},     {2.45, 0.007, 0.631, 0.992},  {2.5, 0.009, 0.635, 0.966},
           {5.0, 0.0, 0.03, 0.964},      {6.0, 0.0, 0.025, 0.965},     {7.0, 0.0, 0.014, 0.966},
           {8.0, 0.0, 0.002, 0.967},     {9.0, 0.0, 0.204, 0.967},     {10.0, 0.0, 0.254, 0.967},
           {11.0, 0.0, 0.16, 0.967},     {12.0, 0.0, 0.074, 0.968},    {13.0, 0.0, 0.081, 0.969},
           {14.0, 0.0, 0.068, 0.969},    {15.0, 0.0, 0.056, 0.969},    {16.0, 0.0, 0.045, 0.97},
           {17.0, 0.0, 0.033, 0.971},    {18.0, 0.0, 0.026, 0.971},    {19.0, 0.0, 0.077, 0.972},
           {20.0, 0.0, 0.177, 0.972},    {21.0, 0.0, 0.231, 0.971},    {22.0, 0.0, 0.234, 0.971},
           {23.0, 0.0, 0.213, 0.971},    {24.0, 0.0, 0.189, 0.97},     {25.0, 0.0, 0.172, 0.971},
           {26.0, 0.0, 0.158, 0.971},    {27.0, 0.0, 0.147, 0.971},    {28.0, 0.0, 0.142, 0.971},
           {29.0, 0.0, 0.137, 0.97},     {30.0, 0.0, 0.133, 0.97},     {31.0, 0.0, 0.128, 0.97},
           {32.0, 0.0, 0.125, 0.97},     {33.0, 0.0, 0.126, 0.971},    {34.0, 0.0, 0.123, 0.971},
           {35.0, 0.0, 0.125, 0.97},     {36.0, 0.0, 0.127, 0.97},     {37.0, 0.0, 0.128, 0.97},
           {38.0, 0.0, 0.126, 0.967},    {39.0, 0.0, 0.125, 0.97},     {40.0, 0.0, 0.129, 0.965}});

        return aMeasurement;
    }

protected:
    virtual void SetUp()
    {
        const auto aSolarRadiation = loadSolarRadiationFile();

        // Wavelength data set according to NFRC 2003 standard is from solar radiation file
        const auto wl = aSolarRadiation.getXArray();

        double thickness = 5.66e-3;   // [m]
        const auto aMaterial_6046 = Material::nBandMaterial(
          loadSampleData_NFRC_6046(), thickness, MaterialType::Monolithic, WavelengthRange::Solar);

        aMaterial_6046->setBandWavelengths(wl);

        const auto layer102 = SpecularLayer::createLayer(aMaterial_6046);

        m_Layer = CMultiPaneSpecular::create({layer102}, loadSolarRadiationFile());
    }

public:
    std::shared_ptr<CMultiPaneSpecular> getLayer() const
    {
        return m_Layer;
    };
};

TEST_F(EquivalentSpecularLayer_6046, TestAngle0)
{
    SCOPED_TRACE("Begin Test: Specular MultiLayerOptics layer - angle = 0 deg.");

    const double angle = 0;

    CMultiPaneSpecular aLayer = *getLayer();

    const double T =
      aLayer.getPropertySimple(PropertySimple::T, Side::Front, Scattering::DirectDirect, angle, 0);
    EXPECT_NEAR(0.383209, T, 1e-6);

    const double Rf =
      aLayer.getPropertySimple(PropertySimple::R, Side::Front, Scattering::DirectDirect, angle, 0);
    EXPECT_NEAR(0.285971, Rf, 1e-6);

    const double Rb =
      aLayer.getPropertySimple(PropertySimple::R, Side::Back, Scattering::DirectDirect, angle, 0);
    EXPECT_NEAR(0.449239, Rb, 1e-6);

    const double Abs1 =
      aLayer.getAbsorptanceLayer(1, Side::Front, ScatteringSimple::Direct, angle, 0);
    EXPECT_NEAR(0.330818, Abs1, 1e-6);

    const double Them =
      aLayer.getPropertySimple(PropertySimple::T, Side::Front, Scattering::DiffuseDiffuse);
    EXPECT_NEAR(0.349505, Them, 1e-6);

    const double Rfhem =
      aLayer.getPropertySimple(PropertySimple::R, Side::Front, Scattering::DiffuseDiffuse);
    EXPECT_NEAR(0.303384, Rfhem, 1e-6);

    const double Rbhem =
      aLayer.getPropertySimple(PropertySimple::R, Side::Back, Scattering::DiffuseDiffuse);
    EXPECT_NEAR(0.458336, Rbhem, 1e-6);
}

TEST_F(EquivalentSpecularLayer_6046, TestAngle10)
{
    SCOPED_TRACE("Begin Test: Specular MultiLayerOptics layer - angle = 10 deg.");

    const double angle = 10;

    CMultiPaneSpecular aLayer = *getLayer();

    const double T =
            aLayer.getPropertySimple(PropertySimple::T, Side::Front, Scattering::DirectDirect, angle, 0);
    EXPECT_NEAR(0.382717, T, 1e-6);

    const double Rf =
            aLayer.getPropertySimple(PropertySimple::R, Side::Front, Scattering::DirectDirect, angle, 0);
    EXPECT_NEAR(0.285927, Rf, 1e-6);

    const double Rb =
            aLayer.getPropertySimple(PropertySimple::R, Side::Back, Scattering::DirectDirect, angle, 0);
    EXPECT_NEAR(0.449203, Rb, 1e-6);

    const double Abs1 =
            aLayer.getAbsorptanceLayer(1, Side::Front, ScatteringSimple::Direct, angle, 0);
    EXPECT_NEAR(0.331354, Abs1, 1e-6);
}

TEST_F(EquivalentSpecularLayer_6046, TestAngleHemispherical10)
{
    SCOPED_TRACE("Begin Test: Hemispherical to hemispherical with ten integration points.");

    const double minLambda = 0.3;
    const double maxLambda = 2.5;
    std::vector<double> aAngles{0, 10, 20, 30, 40, 50, 60, 70, 80, 90};

    CMultiPaneSpecular aLayer = *getLayer();

    double Tfhem =
      aLayer.getHemisphericalProperty(Side::Front, Property::T, aAngles, minLambda, maxLambda);
    EXPECT_NEAR(0.349505, Tfhem, 1e-6);

    double Tbhem =
      aLayer.getHemisphericalProperty(Side::Back, Property::T, aAngles, minLambda, maxLambda);
    EXPECT_NEAR(0.349505, Tbhem, 1e-6);

    double Rfhem =
      aLayer.getHemisphericalProperty(Side::Front, Property::R, aAngles, minLambda, maxLambda);
    EXPECT_NEAR(0.303384, Rfhem, 1e-6);

    double Rbhem =
      aLayer.getHemisphericalProperty(Side::Back, Property::R, aAngles, minLambda, maxLambda);
    EXPECT_NEAR(0.458336, Rbhem, 1e-6);

    double Abs1 = aLayer.AbsHemispherical(1, aAngles, minLambda, maxLambda);
    EXPECT_NEAR(0.336936, Abs1, 1e-6);
}
