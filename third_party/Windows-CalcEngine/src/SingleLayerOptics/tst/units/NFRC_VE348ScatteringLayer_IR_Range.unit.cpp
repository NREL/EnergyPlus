#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCESpectralAveraging.hpp"


using namespace SingleLayerOptics;
using namespace SpectralAveraging;
using namespace FenestrationCommon;

class TestVE345ScatteringLayer_IR_Range : public testing::Test
{
private:
    CScatteringLayer m_Layer;


    std::shared_ptr<CSpectralSampleData> loadSampleData_NFRC_VE348()
    {
        std::shared_ptr<CSpectralSampleData> aMeasurements_VE348 =
          std::make_shared<CSpectralSampleData>();

        aMeasurements_VE348->addRecord(0.300, 0.0001, 0.0609, 0.3251);
        aMeasurements_VE348->addRecord(0.305, 0.0000, 0.0598, 0.3202);
        aMeasurements_VE348->addRecord(0.310, 0.0000, 0.0589, 0.3136);
        aMeasurements_VE348->addRecord(0.315, 0.0000, 0.0580, 0.3052);
        aMeasurements_VE348->addRecord(0.320, 0.0001, 0.0572, 0.2937);
        aMeasurements_VE348->addRecord(0.325, 0.0001, 0.0562, 0.2781);
        aMeasurements_VE348->addRecord(0.330, 0.0000, 0.0549, 0.2589);
        aMeasurements_VE348->addRecord(0.335, 0.0006, 0.0535, 0.2352);
        aMeasurements_VE348->addRecord(0.340, 0.0039, 0.0532, 0.2151);
        aMeasurements_VE348->addRecord(0.345, 0.0137, 0.0545, 0.1994);
        aMeasurements_VE348->addRecord(0.350, 0.0338, 0.0555, 0.1805);
        aMeasurements_VE348->addRecord(0.355, 0.0629, 0.0570, 0.1608);
        aMeasurements_VE348->addRecord(0.360, 0.0990, 0.0597, 0.1420);
        aMeasurements_VE348->addRecord(0.365, 0.1381, 0.0638, 0.1247);
        aMeasurements_VE348->addRecord(0.370, 0.1738, 0.0686, 0.1089);
        aMeasurements_VE348->addRecord(0.375, 0.1927, 0.0716, 0.0950);
        aMeasurements_VE348->addRecord(0.380, 0.2041, 0.0728, 0.0810);
        aMeasurements_VE348->addRecord(0.385, 0.2386, 0.0792, 0.0711);
        aMeasurements_VE348->addRecord(0.390, 0.2828, 0.0890, 0.0634);
        aMeasurements_VE348->addRecord(0.395, 0.3155, 0.0968, 0.0571);
        aMeasurements_VE348->addRecord(0.400, 0.3347, 0.1009, 0.0514);
        aMeasurements_VE348->addRecord(0.410, 0.3413, 0.1008, 0.0414);
        aMeasurements_VE348->addRecord(0.420, 0.3295, 0.0955, 0.0332);
        aMeasurements_VE348->addRecord(0.430, 0.3229, 0.0923, 0.0269);
        aMeasurements_VE348->addRecord(0.440, 0.3133, 0.0886, 0.0218);
        aMeasurements_VE348->addRecord(0.450, 0.3101, 0.0867, 0.0181);
        aMeasurements_VE348->addRecord(0.460, 0.3068, 0.0850, 0.0154);
        aMeasurements_VE348->addRecord(0.470, 0.2978, 0.0824, 0.0138);
        aMeasurements_VE348->addRecord(0.480, 0.2854, 0.0795, 0.0132);
        aMeasurements_VE348->addRecord(0.490, 0.2737, 0.0768, 0.0135);
        aMeasurements_VE348->addRecord(0.500, 0.2661, 0.0747, 0.0144);
        aMeasurements_VE348->addRecord(0.510, 0.2581, 0.0728, 0.0159);
        aMeasurements_VE348->addRecord(0.520, 0.2534, 0.0719, 0.0181);
        aMeasurements_VE348->addRecord(0.530, 0.2530, 0.0716, 0.0208);
        aMeasurements_VE348->addRecord(0.540, 0.2637, 0.0736, 0.0240);
        aMeasurements_VE348->addRecord(0.550, 0.2826, 0.0772, 0.0277);
        aMeasurements_VE348->addRecord(0.560, 0.2925, 0.0790, 0.0312);
        aMeasurements_VE348->addRecord(0.570, 0.2813, 0.0763, 0.0341);
        aMeasurements_VE348->addRecord(0.580, 0.2565, 0.0714, 0.0367);
        aMeasurements_VE348->addRecord(0.590, 0.2360, 0.0676, 0.0396);
        aMeasurements_VE348->addRecord(0.600, 0.2367, 0.0676, 0.0431);
        aMeasurements_VE348->addRecord(0.610, 0.2416, 0.0683, 0.0468);
        aMeasurements_VE348->addRecord(0.620, 0.2431, 0.0685, 0.0505);
        aMeasurements_VE348->addRecord(0.630, 0.2391, 0.0678, 0.0541);
        aMeasurements_VE348->addRecord(0.640, 0.2325, 0.0668, 0.0578);
        aMeasurements_VE348->addRecord(0.650, 0.2304, 0.0666, 0.0620);
        aMeasurements_VE348->addRecord(0.660, 0.2368, 0.0679, 0.0662);
        aMeasurements_VE348->addRecord(0.670, 0.2516, 0.0708, 0.0711);
        aMeasurements_VE348->addRecord(0.680, 0.2737, 0.0757, 0.0763);
        aMeasurements_VE348->addRecord(0.690, 0.2961, 0.0812, 0.0817);
        aMeasurements_VE348->addRecord(0.700, 0.3096, 0.0854, 0.0872);
        aMeasurements_VE348->addRecord(0.710, 0.3127, 0.0874, 0.0924);
        aMeasurements_VE348->addRecord(0.720, 0.3096, 0.0878, 0.0977);
        aMeasurements_VE348->addRecord(0.730, 0.3035, 0.0876, 0.1031);
        aMeasurements_VE348->addRecord(0.740, 0.2954, 0.0869, 0.1085);
        aMeasurements_VE348->addRecord(0.750, 0.2872, 0.0862, 0.1141);
        aMeasurements_VE348->addRecord(0.760, 0.2787, 0.0855, 0.1200);
        aMeasurements_VE348->addRecord(0.770, 0.2705, 0.0846, 0.1256);
        aMeasurements_VE348->addRecord(0.780, 0.2625, 0.0841, 0.1317);
        aMeasurements_VE348->addRecord(0.790, 0.2545, 0.0835, 0.1378);
        aMeasurements_VE348->addRecord(0.800, 0.2474, 0.0832, 0.1443);
        aMeasurements_VE348->addRecord(0.810, 0.2405, 0.0826, 0.1510);
        aMeasurements_VE348->addRecord(0.820, 0.2340, 0.0822, 0.1568);
        aMeasurements_VE348->addRecord(0.830, 0.2276, 0.0817, 0.1634);
        aMeasurements_VE348->addRecord(0.840, 0.2204, 0.0818, 0.1715);
        aMeasurements_VE348->addRecord(0.850, 0.2148, 0.0822, 0.1789);
        aMeasurements_VE348->addRecord(0.860, 0.2100, 0.0821, 0.1861);
        aMeasurements_VE348->addRecord(0.870, 0.2052, 0.0824, 0.1933);
        aMeasurements_VE348->addRecord(0.880, 0.2006, 0.0824, 0.1999);
        aMeasurements_VE348->addRecord(0.890, 0.1964, 0.0824, 0.2067);
        aMeasurements_VE348->addRecord(0.900, 0.1924, 0.0826, 0.2140);
        aMeasurements_VE348->addRecord(0.910, 0.1886, 0.0830, 0.2212);
        aMeasurements_VE348->addRecord(0.920, 0.1848, 0.0834, 0.2287);
        aMeasurements_VE348->addRecord(0.930, 0.1815, 0.0839, 0.2361);
        aMeasurements_VE348->addRecord(0.940, 0.1781, 0.0844, 0.2435);
        aMeasurements_VE348->addRecord(0.950, 0.1749, 0.0850, 0.2512);
        aMeasurements_VE348->addRecord(0.960, 0.1718, 0.0854, 0.2589);
        aMeasurements_VE348->addRecord(0.970, 0.1690, 0.0862, 0.2665);
        aMeasurements_VE348->addRecord(0.980, 0.1662, 0.0868, 0.2745);
        aMeasurements_VE348->addRecord(0.990, 0.1636, 0.0873, 0.2816);
        aMeasurements_VE348->addRecord(1.000, 0.1611, 0.0883, 0.2898);
        aMeasurements_VE348->addRecord(1.050, 0.1494, 0.0920, 0.3279);
        aMeasurements_VE348->addRecord(1.100, 0.1386, 0.0964, 0.3661);
        aMeasurements_VE348->addRecord(1.150, 0.1289, 0.1004, 0.4017);
        aMeasurements_VE348->addRecord(1.200, 0.1207, 0.1055, 0.4363);
        aMeasurements_VE348->addRecord(1.250, 0.1158, 0.1133, 0.4685);
        aMeasurements_VE348->addRecord(1.300, 0.1135, 0.1250, 0.4982);
        aMeasurements_VE348->addRecord(1.350, 0.1127, 0.1406, 0.5264);
        aMeasurements_VE348->addRecord(1.400, 0.1111, 0.1570, 0.5519);
        aMeasurements_VE348->addRecord(1.450, 0.1105, 0.1750, 0.5740);
        aMeasurements_VE348->addRecord(1.500, 0.1099, 0.1967, 0.5965);
        aMeasurements_VE348->addRecord(1.550, 0.1094, 0.2198, 0.6145);
        aMeasurements_VE348->addRecord(1.600, 0.1077, 0.2422, 0.6333);
        aMeasurements_VE348->addRecord(1.650, 0.1047, 0.2582, 0.6494);
        aMeasurements_VE348->addRecord(1.700, 0.1001, 0.2686, 0.6650);
        aMeasurements_VE348->addRecord(1.750, 0.0950, 0.2749, 0.6788);
        aMeasurements_VE348->addRecord(1.800, 0.0902, 0.2811, 0.6929);
        aMeasurements_VE348->addRecord(1.850, 0.0861, 0.2884, 0.7057);
        aMeasurements_VE348->addRecord(1.900, 0.0824, 0.2975, 0.7172);
        aMeasurements_VE348->addRecord(1.950, 0.0791, 0.3070, 0.7277);
        aMeasurements_VE348->addRecord(2.000, 0.0762, 0.3172, 0.7372);
        aMeasurements_VE348->addRecord(2.050, 0.0737, 0.3291, 0.7470);
        aMeasurements_VE348->addRecord(2.100, 0.0712, 0.3405, 0.7546);
        aMeasurements_VE348->addRecord(2.150, 0.0686, 0.3514, 0.7644);
        aMeasurements_VE348->addRecord(2.200, 0.0645, 0.3478, 0.7712);
        aMeasurements_VE348->addRecord(2.250, 0.0624, 0.3584, 0.7789);
        aMeasurements_VE348->addRecord(2.300, 0.0611, 0.3743, 0.7841);
        aMeasurements_VE348->addRecord(2.350, 0.0589, 0.3862, 0.7917);
        aMeasurements_VE348->addRecord(2.400, 0.0564, 0.3905, 0.7966);
        aMeasurements_VE348->addRecord(2.450, 0.0516, 0.3863, 0.8030);
        aMeasurements_VE348->addRecord(2.500, 0.0378, 0.3755, 0.8007);
        aMeasurements_VE348->addRecord(5.000, 0.0000, 0.0300, 0.9045);
        aMeasurements_VE348->addRecord(6.000, 0.0000, 0.0250, 0.9133);
        aMeasurements_VE348->addRecord(7.000, 0.0000, 0.0140, 0.9173);
        aMeasurements_VE348->addRecord(8.000, 0.0000, 0.0020, 0.9200);
        aMeasurements_VE348->addRecord(9.000, 0.0000, 0.2040, 0.9257);
        aMeasurements_VE348->addRecord(10.000, 0.0000, 0.2540, 0.9263);
        aMeasurements_VE348->addRecord(11.000, 0.0000, 0.1600, 0.9238);
        aMeasurements_VE348->addRecord(12.000, 0.0000, 0.0740, 0.9245);
        aMeasurements_VE348->addRecord(13.000, 0.0000, 0.0810, 0.9267);
        aMeasurements_VE348->addRecord(14.000, 0.0000, 0.0680, 0.9267);
        aMeasurements_VE348->addRecord(15.000, 0.0000, 0.0560, 0.9234);
        aMeasurements_VE348->addRecord(16.000, 0.0000, 0.0450, 0.9291);
        aMeasurements_VE348->addRecord(17.000, 0.0000, 0.0330, 0.9296);
        aMeasurements_VE348->addRecord(18.000, 0.0000, 0.0260, 0.9304);
        aMeasurements_VE348->addRecord(19.000, 0.0000, 0.0770, 0.9316);
        aMeasurements_VE348->addRecord(20.000, 0.0000, 0.1770, 0.9331);
        aMeasurements_VE348->addRecord(21.000, 0.0000, 0.2310, 0.9332);
        aMeasurements_VE348->addRecord(22.000, 0.0000, 0.2340, 0.9333);
        aMeasurements_VE348->addRecord(23.000, 0.0000, 0.2130, 0.9338);
        aMeasurements_VE348->addRecord(24.000, 0.0000, 0.1890, 0.9327);
        aMeasurements_VE348->addRecord(25.000, 0.0000, 0.1720, 0.9349);
        aMeasurements_VE348->addRecord(26.000, 0.0000, 0.1580, 0.9324);
        aMeasurements_VE348->addRecord(27.000, 0.0000, 0.1470, 0.9327);
        aMeasurements_VE348->addRecord(28.000, 0.0000, 0.1420, 0.9279);
        aMeasurements_VE348->addRecord(29.000, 0.0000, 0.1370, 0.9326);
        aMeasurements_VE348->addRecord(30.000, 0.0000, 0.1330, 0.9297);
        aMeasurements_VE348->addRecord(31.000, 0.0000, 0.1280, 0.9305);
        aMeasurements_VE348->addRecord(32.000, 0.0000, 0.1250, 0.9331);
        aMeasurements_VE348->addRecord(33.000, 0.0000, 0.1260, 0.9354);
        aMeasurements_VE348->addRecord(34.000, 0.0000, 0.1230, 0.9324);
        aMeasurements_VE348->addRecord(35.000, 0.0000, 0.1250, 0.9337);
        aMeasurements_VE348->addRecord(36.000, 0.0000, 0.1270, 0.9373);
        aMeasurements_VE348->addRecord(37.000, 0.0000, 0.1280, 0.9303);
        aMeasurements_VE348->addRecord(38.000, 0.0000, 0.1260, 0.9339);
        aMeasurements_VE348->addRecord(39.000, 0.0000, 0.1250, 0.9314);
        aMeasurements_VE348->addRecord(40.000, 0.0000, 0.1290, 0.9274);

        return aMeasurements_VE348;
    }

protected:
    virtual void SetUp()
    {
        const double thickness = 5.66e-3;   // [m]
        const auto aMaterial = Material::nBandMaterial(loadSampleData_NFRC_VE348(),
                                                       thickness,
                                                       MaterialType::Coated,
                                                       WavelengthRange::IR,
                                                       IntegrationType::Trapezoidal);

        const double blackBodyTemperature = 300;
        m_Layer = CScatteringLayer::createSpecularLayer(aMaterial);
        m_Layer.setBlackBodySource(blackBodyTemperature);
    }

public:
    CScatteringLayer & getLayer()
    {
        return m_Layer;
    }
};

TEST_F(TestVE345ScatteringLayer_IR_Range, TestFrontIR)
{
    SCOPED_TRACE("Begin Test: VE345 scattering layer IR properties.");

    auto aLayer{CScatteringLayerIR(getLayer())};

    const double emiss = aLayer.emissivity(Side::Front, EmissivityPolynomials::NFRC_301_Uncoated);
    EXPECT_NEAR(0.840263, emiss, 1e-6);

    const double transmittance = aLayer.transmittance(Side::Front);
    EXPECT_NEAR(0.0, transmittance, 1e-6);
}

TEST_F(TestVE345ScatteringLayer_IR_Range, TestBackIR)
{
    SCOPED_TRACE("Begin Test: VE345 scattering layer IR properties.");

    auto aLayer{CScatteringLayerIR(getLayer())};

    const double emiss = aLayer.emissivity(Side::Back, EmissivityPolynomials::NFRC_301_Coated);
    EXPECT_NEAR(0.088879, emiss, 1e-6);

    const double transmittance = aLayer.transmittance(Side::Front);
    EXPECT_NEAR(0.0, transmittance, 1e-6);
}
