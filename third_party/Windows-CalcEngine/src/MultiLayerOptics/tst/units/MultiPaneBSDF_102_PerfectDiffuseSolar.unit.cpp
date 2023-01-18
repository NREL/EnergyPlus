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

// Example on how to create multilayer BSDF from specular and venetian layers

class MultiPaneBSDF_102_PerfectDiffuseSolar : public testing::Test
{
private:
    std::unique_ptr<CMultiPaneBSDF> m_Layer;

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

    std::shared_ptr<CSpectralSampleData> loadSampleData_NFRC_102()
    {
        auto aMeasurements_102 = CSpectralSampleData::create(
          {{0.300, 0.0020, 0.0470, 0.0480}, {0.305, 0.0030, 0.0470, 0.0480},
           {0.310, 0.0090, 0.0470, 0.0480}, {0.315, 0.0350, 0.0470, 0.0480},
           {0.320, 0.1000, 0.0470, 0.0480}, {0.325, 0.2180, 0.0490, 0.0500},
           {0.330, 0.3560, 0.0530, 0.0540}, {0.335, 0.4980, 0.0600, 0.0610},
           {0.340, 0.6160, 0.0670, 0.0670}, {0.345, 0.7090, 0.0730, 0.0740},
           {0.350, 0.7740, 0.0780, 0.0790}, {0.355, 0.8180, 0.0820, 0.0820},
           {0.360, 0.8470, 0.0840, 0.0840}, {0.365, 0.8630, 0.0850, 0.0850},
           {0.370, 0.8690, 0.0850, 0.0860}, {0.375, 0.8610, 0.0850, 0.0850},
           {0.380, 0.8560, 0.0840, 0.0840}, {0.385, 0.8660, 0.0850, 0.0850},
           {0.390, 0.8810, 0.0860, 0.0860}, {0.395, 0.8890, 0.0860, 0.0860},
           {0.400, 0.8930, 0.0860, 0.0860}, {0.410, 0.8930, 0.0860, 0.0860},
           {0.420, 0.8920, 0.0860, 0.0860}, {0.430, 0.8920, 0.0850, 0.0850},
           {0.440, 0.8920, 0.0850, 0.0850}, {0.450, 0.8960, 0.0850, 0.0850},
           {0.460, 0.9000, 0.0850, 0.0850}, {0.470, 0.9020, 0.0840, 0.0840},
           {0.480, 0.9030, 0.0840, 0.0840}, {0.490, 0.9040, 0.0850, 0.0850},
           {0.500, 0.9050, 0.0840, 0.0840}, {0.510, 0.9050, 0.0840, 0.0840},
           {0.520, 0.9050, 0.0840, 0.0840}, {0.530, 0.9040, 0.0840, 0.0840},
           {0.540, 0.9040, 0.0830, 0.0830}, {0.550, 0.9030, 0.0830, 0.0830},
           {0.560, 0.9020, 0.0830, 0.0830}, {0.570, 0.9000, 0.0820, 0.0820},
           {0.580, 0.8980, 0.0820, 0.0820}, {0.590, 0.8960, 0.0810, 0.0810},
           {0.600, 0.8930, 0.0810, 0.0810}, {0.610, 0.8900, 0.0810, 0.0810},
           {0.620, 0.8860, 0.0800, 0.0800}, {0.630, 0.8830, 0.0800, 0.0800},
           {0.640, 0.8790, 0.0790, 0.0790}, {0.650, 0.8750, 0.0790, 0.0790},
           {0.660, 0.8720, 0.0790, 0.0790}, {0.670, 0.8680, 0.0780, 0.0780},
           {0.680, 0.8630, 0.0780, 0.0780}, {0.690, 0.8590, 0.0770, 0.0770},
           {0.700, 0.8540, 0.0760, 0.0770}, {0.710, 0.8500, 0.0760, 0.0760},
           {0.720, 0.8450, 0.0750, 0.0760}, {0.730, 0.8400, 0.0750, 0.0750},
           {0.740, 0.8350, 0.0750, 0.0750}, {0.750, 0.8310, 0.0740, 0.0740},
           {0.760, 0.8260, 0.0740, 0.0740}, {0.770, 0.8210, 0.0740, 0.0740},
           {0.780, 0.8160, 0.0730, 0.0730}, {0.790, 0.8120, 0.0730, 0.0730},
           {0.800, 0.8080, 0.0720, 0.0720}, {0.810, 0.8030, 0.0720, 0.0720},
           {0.820, 0.8000, 0.0720, 0.0720}, {0.830, 0.7960, 0.0710, 0.0710},
           {0.840, 0.7930, 0.0700, 0.0710}, {0.850, 0.7880, 0.0700, 0.0710},
           {0.860, 0.7860, 0.0700, 0.0700}, {0.870, 0.7820, 0.0740, 0.0740},
           {0.880, 0.7800, 0.0720, 0.0720}, {0.890, 0.7770, 0.0730, 0.0740},
           {0.900, 0.7760, 0.0720, 0.0720}, {0.910, 0.7730, 0.0720, 0.0720},
           {0.920, 0.7710, 0.0710, 0.0710}, {0.930, 0.7700, 0.0700, 0.0700},
           {0.940, 0.7680, 0.0690, 0.0690}, {0.950, 0.7660, 0.0680, 0.0680},
           {0.960, 0.7660, 0.0670, 0.0680}, {0.970, 0.7640, 0.0680, 0.0680},
           {0.980, 0.7630, 0.0680, 0.0680}, {0.990, 0.7620, 0.0670, 0.0670},
           {1.000, 0.7620, 0.0660, 0.0670}, {1.050, 0.7600, 0.0660, 0.0660},
           {1.100, 0.7590, 0.0660, 0.0660}, {1.150, 0.7610, 0.0660, 0.0660},
           {1.200, 0.7650, 0.0660, 0.0660}, {1.250, 0.7700, 0.0650, 0.0650},
           {1.300, 0.7770, 0.0670, 0.0670}, {1.350, 0.7860, 0.0660, 0.0670},
           {1.400, 0.7950, 0.0670, 0.0680}, {1.450, 0.8080, 0.0670, 0.0670},
           {1.500, 0.8190, 0.0690, 0.0690}, {1.550, 0.8290, 0.0690, 0.0690},
           {1.600, 0.8360, 0.0700, 0.0700}, {1.650, 0.8400, 0.0700, 0.0700},
           {1.700, 0.8420, 0.0690, 0.0700}, {1.750, 0.8420, 0.0690, 0.0700},
           {1.800, 0.8410, 0.0700, 0.0700}, {1.850, 0.8400, 0.0690, 0.0690},
           {1.900, 0.8390, 0.0680, 0.0680}, {1.950, 0.8390, 0.0710, 0.0710},
           {2.000, 0.8390, 0.0690, 0.0690}, {2.050, 0.8400, 0.0680, 0.0680},
           {2.100, 0.8410, 0.0680, 0.0680}, {2.150, 0.8390, 0.0690, 0.0690},
           {2.200, 0.8300, 0.0700, 0.0700}, {2.250, 0.8300, 0.0700, 0.0700},
           {2.300, 0.8320, 0.0690, 0.0690}, {2.350, 0.8320, 0.0690, 0.0700},
           {2.400, 0.8320, 0.0700, 0.0700}, {2.450, 0.8260, 0.0690, 0.0690},
           {2.500, 0.8220, 0.0680, 0.0680}});

        return aMeasurements_102;
    }

protected:
    virtual void SetUp()
    {
        const auto commonWL{loadSolarRadiationFile().getXArray()};
        auto thickness = 3.048e-3;   // [m]
        auto aMaterial_102 = SingleLayerOptics::Material::nBandMaterial(
          loadSampleData_NFRC_102(), thickness, MaterialType::Monolithic, WavelengthRange::Solar);

        aMaterial_102->setBandWavelengths(commonWL);

        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Small);

        auto Layer_102 = CBSDFLayerMaker::getSpecularLayer(aMaterial_102, aBSDF);

        // Layer_102->setBandWavelengths(commonWL);

        // Solar range
        const auto Tsol = 0.5;
        const auto Rfsol = 0.3;
        const auto Rbsol = 0.3;

        // Visible range
        const auto Tvis = 0.4;
        const auto Rfvis = 0.2;
        const auto Rbvis = 0.2;

        auto aMaterial = SingleLayerOptics::Material::dualBandMaterial(
          Tsol, Tsol, Rfsol, Rbsol, Tvis, Tvis, Rfvis, Rbvis);
        aMaterial->setBandWavelengths(commonWL);

        const auto diffuseLayer = CBSDFLayerMaker::getPerfectlyDiffuseLayer(aMaterial, aBSDF);

        // This is to test if results will remain the same even with empty detector spectrum.
        const CSeries detectorSpectrum;

        m_Layer = CMultiPaneBSDF::create(
          {Layer_102, diffuseLayer}, loadSolarRadiationFile(), detectorSpectrum);
    }

public:
    CMultiPaneBSDF & getLayer()
    {
        return *m_Layer;
    }
};

TEST_F(MultiPaneBSDF_102_PerfectDiffuseSolar, TestPerfectDiffuseBSDFSolar)
{
    SCOPED_TRACE("Begin Test: Perfectly diffusing IGU - BSDF.");

    const double minLambda = 0.3;
    const double maxLambda = 2.5;

    CMultiPaneBSDF & aLayer = getLayer();

    double tauDiff = aLayer.DiffDiff(minLambda, maxLambda, Side::Front, PropertySimple::T);
    EXPECT_NEAR(0.38807281612852856, tauDiff, 1e-6);

    double rhoDiff = aLayer.DiffDiff(minLambda, maxLambda, Side::Front, PropertySimple::R);
    EXPECT_NEAR(0.3192864301871256, rhoDiff, 1e-6);

    double absDiff1 = aLayer.AbsDiff(minLambda, maxLambda, Side::Front, 1);
    EXPECT_NEAR(0.12779473542404929, absDiff1, 1e-6);

    double absDiff2 = aLayer.AbsDiff(minLambda, maxLambda, Side::Front, 2);
    EXPECT_NEAR(0.16484601826029638, absDiff2, 1e-6);

    double theta = 0;
    double phi = 0;

    double tauHem = aLayer.DirHem(minLambda, maxLambda, Side::Front, PropertySimple::T, theta, phi);
    EXPECT_NEAR(0.43205231351437434, tauHem, 1e-6);

    double tauDir = aLayer.DirDir(minLambda, maxLambda, Side::Front, PropertySimple::T, theta, phi);
    EXPECT_NEAR(0.0055367364023813257, tauDir, 1e-6);

    double tauDirDiff =
      aLayer.DirDiff(minLambda, maxLambda, Side::Front, PropertySimple::T, theta, phi);
    EXPECT_NEAR(0.42651557711199301, tauDirDiff, 1e-6);

    double rhoHem = aLayer.DirHem(minLambda, maxLambda, Side::Front, PropertySimple::R, theta, phi);
    EXPECT_NEAR(0.26522929024908287, rhoHem, 1e-6);

    double rhoDir = aLayer.DirDir(minLambda, maxLambda, Side::Front, PropertySimple::R, theta, phi);
    EXPECT_NEAR(0.077483327932538468, rhoDir, 1e-6);

    double abs1 = aLayer.Abs(minLambda, maxLambda, Side::Front, 1, theta, phi);
    EXPECT_NEAR(0.12104831993041673, abs1, 1e-6);

    double abs2 = aLayer.Abs(minLambda, maxLambda, Side::Front, 2, theta, phi);
    EXPECT_NEAR(0.18167007630612608, abs2, 1e-6);

    theta = 45;
    phi = 78;

    tauHem = aLayer.DirHem(minLambda, maxLambda, Side::Front, PropertySimple::T, theta, phi);
    EXPECT_NEAR(0.42378838941894764, tauHem, 1e-6);

    tauDir = aLayer.DirDir(minLambda, maxLambda, Side::Front, PropertySimple::T, theta, phi);
    EXPECT_NEAR(0.093248419848833711, tauDir, 1e-6);

    rhoHem = aLayer.DirHem(minLambda, maxLambda, Side::Front, PropertySimple::R, theta, phi);
    EXPECT_NEAR(0.26832230792664158, rhoHem, 1e-6);

    rhoDir = aLayer.DirDir(minLambda, maxLambda, Side::Front, PropertySimple::R, theta, phi);
    EXPECT_NEAR(0.12643072493011953, rhoDir, 1e-6);

    abs1 = aLayer.Abs(minLambda, maxLambda, Side::Front, 1, theta, phi);
    EXPECT_NEAR(0.1285067589657867, abs1, 1e-6);

    abs2 = aLayer.Abs(minLambda, maxLambda, Side::Front, 2, theta, phi);
    EXPECT_NEAR(0.17938254368862436, abs2, 1e-6);
}
