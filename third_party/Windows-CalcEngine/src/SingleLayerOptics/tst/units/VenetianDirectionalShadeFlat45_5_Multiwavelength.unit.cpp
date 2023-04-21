#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;
using namespace SpectralAveraging;

class TestVenetianDirectionalShadeFlat45_5_Multiwavelength : public testing::Test
{
private:
    std::shared_ptr<CBSDFLayer> m_Layer;

protected:
    virtual void SetUp()
    {
        // Solar range material
        const auto Tsol = 0.1;
        const auto Rfsol = 0.7;
        const auto Rbsol = 0.7;
        // Visible range
        const auto Tvis = 0.2;
        const auto Rfvis = 0.6;
        const auto Rbvis = 0.6;

        const auto aMaterial =
          Material::dualBandMaterial(Tsol, Tsol, Rfsol, Rbsol, Tvis, Tvis, Rfvis, Rbvis);

        // make cell geometry
        double slatWidth = 0.016;     // m
        double slatSpacing = 0.012;   // m
        double slatTiltAngle = 45;
        double curvatureRadius = 0;
        const size_t numOfSlatSegments = 5;

        // Method
        DistributionMethod aDistribution = DistributionMethod::DirectionalDiffuse;

        // create BSDF
        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Quarter);

        // make layer
        m_Layer = CBSDFLayerMaker::getVenetianLayer(aMaterial,
                                                    aBSDF,
                                                    slatWidth,
                                                    slatSpacing,
                                                    slatTiltAngle,
                                                    curvatureRadius,
                                                    numOfSlatSegments,
                                                    aDistribution,
                                                    true);
    }

public:
    std::shared_ptr<CBSDFLayer> getLayer()
    {
        return m_Layer;
    };
};

TEST_F(TestVenetianDirectionalShadeFlat45_5_Multiwavelength, TestVenetianMultiWavelength)
{
    SCOPED_TRACE("Begin Test: Venetian layer (multi range) - BSDF.");

    std::shared_ptr<CBSDFLayer> aLayer = getLayer();

    std::shared_ptr<std::vector<std::shared_ptr<CBSDFIntegrator>>> aResults =
      aLayer->getWavelengthResults();

    size_t correctSize = 5;

    EXPECT_EQ(correctSize, aResults->size());

    ///////////////////////////////////////////////////////////////////////
    //  Wavelength number 1
    ///////////////////////////////////////////////////////////////////////

    auto aT = (*aResults)[0]->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix
    size_t size = aT.size();

    std::vector<double> correctResults;
    correctResults.push_back(0.74389667783099223);
    correctResults.push_back(0.80180071560500688);
    correctResults.push_back(3.83865423418496250);
    correctResults.push_back(5.09656014852123370);
    correctResults.push_back(3.83865423418496250);
    correctResults.push_back(0.80180071560500688);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.74331014167182330);
    correctResults.push_back(5.19471881376132400);
    correctResults.push_back(8.45337612698354770);
    correctResults.push_back(9.64612748585082080);
    correctResults.push_back(8.45337612698354770);
    correctResults.push_back(5.19471881376132400);
    correctResults.push_back(0.74331014167182330);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.74331014167182419);
    correctResults.push_back(9.17617976833100530);
    correctResults.push_back(10.6445051622640850);
    correctResults.push_back(8.38492455592380990);
    correctResults.push_back(10.6445051622640850);
    correctResults.push_back(9.17617976833100530);
    correctResults.push_back(0.74331014167182419);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.70660053056312777);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.70660053056312777);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.00000000000000000);
    correctResults.push_back(0.00000000000000000);

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-6);
    }

    // Front reflectance
    auto aRf = (*aResults)[0]->getMatrix(Side::Front, PropertySimple::R);

    correctResults.clear();

    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);
    correctResults.push_back(0);

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-6);
    }

    ///////////////////////////////////////////////////////////////////////
    //  Wavelength number 2
    ///////////////////////////////////////////////////////////////////////

    aT = (*aResults)[1]->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix
    size = aT.size();

    correctResults.clear();

    correctResults.push_back(0.810985663669468670);
    correctResults.push_back(0.868889701443483320);
    correctResults.push_back(3.878456825501505300);
    correctResults.push_back(5.127134841249222100);
    correctResults.push_back(3.878456825501505300);
    correctResults.push_back(0.868889701443483320);
    correctResults.push_back(0.054756929401648835);
    correctResults.push_back(0.051389275815072119);
    correctResults.push_back(0.054756929401648835);
    correctResults.push_back(0.810399127510299740);
    correctResults.push_back(5.221918264087316100);
    correctResults.push_back(8.462600155511379900);
    correctResults.push_back(9.651144332384166800);
    correctResults.push_back(8.462600155511379900);
    correctResults.push_back(5.221918264087316100);
    correctResults.push_back(0.810399127510299740);
    correctResults.push_back(0.047001840695730615);
    correctResults.push_back(0.030441072745030544);
    correctResults.push_back(0.028524705634764967);
    correctResults.push_back(0.030441072745030544);
    correctResults.push_back(0.047001840695730615);
    correctResults.push_back(0.810399127510300630);
    correctResults.push_back(9.182702486745910200);
    correctResults.push_back(10.64697785943013200);
    correctResults.push_back(8.394428606409652700);
    correctResults.push_back(10.64697785943013200);
    correctResults.push_back(9.182702486745910200);
    correctResults.push_back(0.810399127510300630);
    correctResults.push_back(0.029105050579956087);
    correctResults.push_back(0.017559926372523890);
    correctResults.push_back(0.015982209312473284);
    correctResults.push_back(0.017559926372523890);
    correctResults.push_back(0.029105050579956087);
    correctResults.push_back(0.773689516401604220);
    correctResults.push_back(0.126076328808883390);
    correctResults.push_back(0.125697042689946390);
    correctResults.push_back(0.126076328808883390);
    correctResults.push_back(0.773689516401604220);
    correctResults.push_back(0.009367221424572298);
    correctResults.push_back(0.009210548260682570);
    correctResults.push_back(0.009367221424572298);

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-6);
    }

    // Front reflectance
    aRf = (*aResults)[1]->getMatrix(Side::Front, PropertySimple::R);

    correctResults.clear();

    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.149928590480610830);
    correctResults.push_back(0.137941160815866220);
    correctResults.push_back(0.149928590480610830);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.158391939325068450);
    correctResults.push_back(0.138042575478963450);
    correctResults.push_back(0.158391939325068450);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.126443022652177330);
    correctResults.push_back(0.069895870182664382);
    correctResults.push_back(0.053460518477694341);
    correctResults.push_back(0.069895870182664382);
    correctResults.push_back(0.126443022652177330);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.129940864327154710);
    correctResults.push_back(0.074626283497912074);
    correctResults.push_back(0.054754158084382533);
    correctResults.push_back(0.074626283497912074);
    correctResults.push_back(0.129940864327154710);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.059926558202154465);
    correctResults.push_back(0.006844552101026188);
    correctResults.push_back(0.013061745337847886);
    correctResults.push_back(0.006844552101026188);
    correctResults.push_back(0.059926558202154465);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.062559882884446152);
    correctResults.push_back(0.008237279967413455);
    correctResults.push_back(0.015225581075918348);
    correctResults.push_back(0.008237279967413455);
    correctResults.push_back(0.062559882884446152);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.038177194245758286);
    correctResults.push_back(0.051413733084179278);
    correctResults.push_back(0.038177194245758286);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.048262934352637205);
    correctResults.push_back(0.048740046603877221);
    correctResults.push_back(0.048262934352637205);

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-6);
    }

    ///////////////////////////////////////////////////////////////////////
    //  Wavelength number 3
    ///////////////////////////////////////////////////////////////////////

    aT = (*aResults)[2]->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix
    size = aT.size();

    correctResults.clear();

    correctResults.push_back(0.836275880754398400);
    correctResults.push_back(0.894179918528413050);
    correctResults.push_back(3.893461019742295200);
    correctResults.push_back(5.138660438286230200);
    correctResults.push_back(3.893461019742295200);
    correctResults.push_back(0.894179918528413050);
    correctResults.push_back(0.088417695248090525);
    correctResults.push_back(0.088474910851309277);
    correctResults.push_back(0.088417695248090525);
    correctResults.push_back(0.835689344595229480);
    correctResults.push_back(5.232171511989904800);
    correctResults.push_back(8.466077293836708200);
    correctResults.push_back(9.653035509249107800);
    correctResults.push_back(8.466077293836708200);
    correctResults.push_back(5.232171511989904800);
    correctResults.push_back(0.835689344595229480);
    correctResults.push_back(0.083366975858566325);
    correctResults.push_back(0.061722153521553885);
    correctResults.push_back(0.058886242955618555);
    correctResults.push_back(0.061722153521553885);
    correctResults.push_back(0.083366975858566325);
    correctResults.push_back(0.835689344595230370);
    correctResults.push_back(9.185161324998347300);
    correctResults.push_back(10.64790998036374100);
    correctResults.push_back(8.398011303285587200);
    correctResults.push_back(10.64790998036374100);
    correctResults.push_back(9.185161324998347300);
    correctResults.push_back(0.835689344595230370);
    correctResults.push_back(0.059735559077405055);
    correctResults.push_back(0.027508020796803857);
    correctResults.push_back(0.024511810701834273);
    correctResults.push_back(0.027508020796803857);
    correctResults.push_back(0.059735559077405055);
    correctResults.push_back(0.798979733486533950);
    correctResults.push_back(0.093292300851503973);
    correctResults.push_back(0.079357844380076134);
    correctResults.push_back(0.093292300851503973);
    correctResults.push_back(0.798979733486533950);
    correctResults.push_back(0.017310537462612144);
    correctResults.push_back(0.018209736057913754);
    correctResults.push_back(0.017310537462612144);

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-6);
    }

    // Front reflectance
    aRf = (*aResults)[2]->getMatrix(Side::Front, PropertySimple::R);

    correctResults.clear();

    correctResults.push_back(0.165903843794496520);
    correctResults.push_back(0.165903843794496520);
    correctResults.push_back(0.127956545925703070);
    correctResults.push_back(0.117843124975481240);
    correctResults.push_back(0.127956545925703070);
    correctResults.push_back(0.165903843794496520);
    correctResults.push_back(0.134906386909191520);
    correctResults.push_back(0.117926403568254980);
    correctResults.push_back(0.134906386909191520);
    correctResults.push_back(0.165903843794496520);
    correctResults.push_back(0.108225759077810250);
    correctResults.push_back(0.060355829056359361);
    correctResults.push_back(0.046206747713723524);
    correctResults.push_back(0.060355829056359361);
    correctResults.push_back(0.108225759077810250);
    correctResults.push_back(0.165903843794496520);
    correctResults.push_back(0.111117195342986560);
    correctResults.push_back(0.064266151443683694);
    correctResults.push_back(0.047276114717840656);
    correctResults.push_back(0.064266151443683694);
    correctResults.push_back(0.111117195342986560);
    correctResults.push_back(0.165903843794496520);
    correctResults.push_back(0.051783295867258577);
    correctResults.push_back(0.014108177373773591);
    correctResults.push_back(0.027952498113038276);
    correctResults.push_back(0.014108177373773591);
    correctResults.push_back(0.051783295867258577);
    correctResults.push_back(0.165903843794496520);
    correctResults.push_back(0.053960092644205690);
    correctResults.push_back(0.016014184991125564);
    correctResults.push_back(0.030913799813821443);
    correctResults.push_back(0.016014184991125564);
    correctResults.push_back(0.053960092644205690);
    correctResults.push_back(0.165903843794496520);
    correctResults.push_back(0.107196920174651770);
    correctResults.push_back(0.181881555366362770);
    correctResults.push_back(0.107196920174651770);
    correctResults.push_back(0.165903843794496520);
    correctResults.push_back(0.128611259539473550);
    correctResults.push_back(0.180142928161266060);
    correctResults.push_back(0.128611259539473550);

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-6);
    }

    ///////////////////////////////////////////////////////////////////////
    //  Wavelength number 4
    ///////////////////////////////////////////////////////////////////////

    aT = (*aResults)[3]->getMatrix(Side::Front, PropertySimple::T);

    // Test only diagonal of transmittance matrix
    size = aT.size();

    correctResults.clear();

    correctResults.push_back(0.810985663669468670);
    correctResults.push_back(0.868889701443483320);
    correctResults.push_back(3.878456825501505300);
    correctResults.push_back(5.127134841249222100);
    correctResults.push_back(3.878456825501505300);
    correctResults.push_back(0.868889701443483320);
    correctResults.push_back(0.054756929401648835);
    correctResults.push_back(0.051389275815072119);
    correctResults.push_back(0.054756929401648835);
    correctResults.push_back(0.810399127510299740);
    correctResults.push_back(5.221918264087316100);
    correctResults.push_back(8.462600155511379900);
    correctResults.push_back(9.651144332384166800);
    correctResults.push_back(8.462600155511379900);
    correctResults.push_back(5.221918264087316100);
    correctResults.push_back(0.810399127510299740);
    correctResults.push_back(0.047001840695730615);
    correctResults.push_back(0.030441072745030544);
    correctResults.push_back(0.028524705634764967);
    correctResults.push_back(0.030441072745030544);
    correctResults.push_back(0.047001840695730615);
    correctResults.push_back(0.810399127510300630);
    correctResults.push_back(9.182702486745910200);
    correctResults.push_back(10.64697785943013200);
    correctResults.push_back(8.394428606409652700);
    correctResults.push_back(10.64697785943013200);
    correctResults.push_back(9.182702486745910200);
    correctResults.push_back(0.810399127510300630);
    correctResults.push_back(0.029105050579956087);
    correctResults.push_back(0.017559926372523890);
    correctResults.push_back(0.015982209312473284);
    correctResults.push_back(0.017559926372523890);
    correctResults.push_back(0.029105050579956087);
    correctResults.push_back(0.773689516401604220);
    correctResults.push_back(0.126076328808883390);
    correctResults.push_back(0.125697042689946390);
    correctResults.push_back(0.126076328808883390);
    correctResults.push_back(0.773689516401604220);
    correctResults.push_back(0.009367221424572298);
    correctResults.push_back(0.009210548260682570);
    correctResults.push_back(0.009367221424572298);

    EXPECT_EQ(correctResults.size(), aT.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aT(i, i), 1e-6);
    }

    // Front reflectance
    aRf = (*aResults)[3]->getMatrix(Side::Front, PropertySimple::R);

    correctResults.clear();

    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.149928590480610830);
    correctResults.push_back(0.137941160815866220);
    correctResults.push_back(0.149928590480610830);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.158391939325068450);
    correctResults.push_back(0.138042575478963450);
    correctResults.push_back(0.158391939325068450);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.126443022652177330);
    correctResults.push_back(0.069895870182664382);
    correctResults.push_back(0.053460518477694341);
    correctResults.push_back(0.069895870182664382);
    correctResults.push_back(0.126443022652177330);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.129940864327154710);
    correctResults.push_back(0.074626283497912074);
    correctResults.push_back(0.054754158084382533);
    correctResults.push_back(0.074626283497912074);
    correctResults.push_back(0.129940864327154710);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.059926558202154465);
    correctResults.push_back(0.006844552101026188);
    correctResults.push_back(0.013061745337847886);
    correctResults.push_back(0.006844552101026188);
    correctResults.push_back(0.059926558202154465);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.062559882884446152);
    correctResults.push_back(0.008237279967413455);
    correctResults.push_back(0.015225581075918348);
    correctResults.push_back(0.008237279967413455);
    correctResults.push_back(0.062559882884446152);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.038177194245758286);
    correctResults.push_back(0.051413733084179278);
    correctResults.push_back(0.038177194245758286);
    correctResults.push_back(0.195811444604020860);
    correctResults.push_back(0.048262934352637205);
    correctResults.push_back(0.048740046603877221);
    correctResults.push_back(0.048262934352637205);

    EXPECT_EQ(correctResults.size(), aRf.size());
    for(size_t i = 0; i < size; ++i)
    {
        EXPECT_NEAR(correctResults[i], aRf(i, i), 1e-6);
    }
}
