#include <memory>
#include <gtest/gtest.h>

#include "WCESpectralAveraging.hpp"
#include "WCEMultiLayerOptics.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace FenestrationCommon;
using namespace SpectralAveraging;
using namespace SingleLayerOptics;
using namespace MultiLayerOptics;

// Example that tests interreflectance between two adjacent layers. This procedure will be used to
// calculate other multilayer properties
// Construction is NFRC=102 - NFRC=102
class TestDoubleLayerBSDFPerforated : public testing::Test {

private:
	std::shared_ptr< CBSDFDoubleLayer > m_DoubleLayer;

protected:
	virtual void SetUp() {

		// Create lambda matrix
		std::vector< CBSDFDefinition > aDefinitions;
		aDefinitions.push_back( CBSDFDefinition( 0, 1 ) );
		aDefinitions.push_back( CBSDFDefinition( 15, 1 ) );
		aDefinitions.push_back( CBSDFDefinition( 30, 1 ) );
		aDefinitions.push_back( CBSDFDefinition( 45, 1 ) );
		aDefinitions.push_back( CBSDFDefinition( 60, 1 ) );
		aDefinitions.push_back( CBSDFDefinition( 75, 1 ) );
		aDefinitions.push_back( CBSDFDefinition( 86.25, 1 ) );

		// Create BSDF out of previous definitions
		std::shared_ptr< CBSDFHemisphere > aBSDF = std::make_shared< CBSDFHemisphere >( aDefinitions );

		std::shared_ptr< CSeries > aSolarRadiation = std::make_shared< CSeries >();

		// Full ASTM E891-87 Table 1
		aSolarRadiation->addProperty( 0.3000, 0.0 );
		aSolarRadiation->addProperty( 0.3050, 3.4 );
		aSolarRadiation->addProperty( 0.3100, 15.6 );
		aSolarRadiation->addProperty( 0.3150, 41.1 );
		aSolarRadiation->addProperty( 0.3200, 71.2 );
		aSolarRadiation->addProperty( 0.3250, 100.2 );
		aSolarRadiation->addProperty( 0.3300, 152.4 );
		aSolarRadiation->addProperty( 0.3350, 155.6 );
		aSolarRadiation->addProperty( 0.3400, 179.4 );
		aSolarRadiation->addProperty( 0.3450, 186.7 );
		aSolarRadiation->addProperty( 0.3500, 212.0 );
		aSolarRadiation->addProperty( 0.3600, 240.5 );
		aSolarRadiation->addProperty( 0.3700, 324.0 );
		aSolarRadiation->addProperty( 0.3800, 362.4 );
		aSolarRadiation->addProperty( 0.3900, 381.7 );
		aSolarRadiation->addProperty( 0.4000, 556.0 );
		aSolarRadiation->addProperty( 0.4100, 656.3 );
		aSolarRadiation->addProperty( 0.4200, 690.8 );
		aSolarRadiation->addProperty( 0.4300, 641.9 );
		aSolarRadiation->addProperty( 0.4400, 798.5 );
		aSolarRadiation->addProperty( 0.4500, 956.6 );
		aSolarRadiation->addProperty( 0.4600, 990.0 );
		aSolarRadiation->addProperty( 0.4700, 998.0 );
		aSolarRadiation->addProperty( 0.4800, 1046.1 );
		aSolarRadiation->addProperty( 0.4900, 1005.1 );
		aSolarRadiation->addProperty( 0.5000, 1026.7 );
		aSolarRadiation->addProperty( 0.5100, 1066.7 );
		aSolarRadiation->addProperty( 0.5200, 1011.5 );
		aSolarRadiation->addProperty( 0.5300, 1084.9 );
		aSolarRadiation->addProperty( 0.5400, 1082.4 );
		aSolarRadiation->addProperty( 0.5500, 1102.2 );
		aSolarRadiation->addProperty( 0.5700, 1087.4 );
		aSolarRadiation->addProperty( 0.5900, 1024.3 );
		aSolarRadiation->addProperty( 0.6100, 1088.8 );
		aSolarRadiation->addProperty( 0.6300, 1062.1 );
		aSolarRadiation->addProperty( 0.6500, 1061.7 );
		aSolarRadiation->addProperty( 0.6700, 1046.2 );
		aSolarRadiation->addProperty( 0.6900, 859.2 );
		aSolarRadiation->addProperty( 0.7100, 1002.4 );
		aSolarRadiation->addProperty( 0.7180, 816.9 );
		aSolarRadiation->addProperty( 0.7244, 842.8 );
		aSolarRadiation->addProperty( 0.7400, 971.0 );
		aSolarRadiation->addProperty( 0.7525, 956.3 );
		aSolarRadiation->addProperty( 0.7575, 942.2 );
		aSolarRadiation->addProperty( 0.7625, 524.8 );
		aSolarRadiation->addProperty( 0.7675, 830.7 );
		aSolarRadiation->addProperty( 0.7800, 908.9 );
		aSolarRadiation->addProperty( 0.8000, 873.4 );
		aSolarRadiation->addProperty( 0.8160, 712.0 );
		aSolarRadiation->addProperty( 0.8237, 660.2 );
		aSolarRadiation->addProperty( 0.8315, 765.5 );
		aSolarRadiation->addProperty( 0.8400, 799.8 );
		aSolarRadiation->addProperty( 0.8600, 815.2 );
		aSolarRadiation->addProperty( 0.8800, 778.3 );
		aSolarRadiation->addProperty( 0.9050, 630.4 );
		aSolarRadiation->addProperty( 0.9150, 565.2 );
		aSolarRadiation->addProperty( 0.9250, 586.4 );
		aSolarRadiation->addProperty( 0.9300, 348.1 );
		aSolarRadiation->addProperty( 0.9370, 224.2 );
		aSolarRadiation->addProperty( 0.9480, 271.4 );
		aSolarRadiation->addProperty( 0.9650, 451.2 );
		aSolarRadiation->addProperty( 0.9800, 549.7 );
		aSolarRadiation->addProperty( 0.9935, 630.1 );
		aSolarRadiation->addProperty( 1.0400, 582.9 );
		aSolarRadiation->addProperty( 1.0700, 539.7 );
		aSolarRadiation->addProperty( 1.1000, 366.2 );
		aSolarRadiation->addProperty( 1.1200, 98.1 );
		aSolarRadiation->addProperty( 1.1300, 169.5 );
		aSolarRadiation->addProperty( 1.1370, 118.7 );
		aSolarRadiation->addProperty( 1.1610, 301.9 );
		aSolarRadiation->addProperty( 1.1800, 406.8 );
		aSolarRadiation->addProperty( 1.2000, 375.2 );
		aSolarRadiation->addProperty( 1.2350, 423.6 );
		aSolarRadiation->addProperty( 1.2900, 365.7 );
		aSolarRadiation->addProperty( 1.3200, 223.4 );
		aSolarRadiation->addProperty( 1.3500, 30.1 );
		aSolarRadiation->addProperty( 1.3950, 1.4 );
		aSolarRadiation->addProperty( 1.4425, 51.6 );
		aSolarRadiation->addProperty( 1.4625, 97.0 );
		aSolarRadiation->addProperty( 1.4770, 97.3 );
		aSolarRadiation->addProperty( 1.4970, 167.1 );
		aSolarRadiation->addProperty( 1.5200, 239.3 );
		aSolarRadiation->addProperty( 1.5390, 248.8 );
		aSolarRadiation->addProperty( 1.5580, 249.3 );
		aSolarRadiation->addProperty( 1.5780, 222.3 );
		aSolarRadiation->addProperty( 1.5920, 227.3 );
		aSolarRadiation->addProperty( 1.6100, 210.5 );
		aSolarRadiation->addProperty( 1.6300, 224.7 );
		aSolarRadiation->addProperty( 1.6460, 215.9 );
		aSolarRadiation->addProperty( 1.6780, 202.8 );
		aSolarRadiation->addProperty( 1.7400, 158.2 );
		aSolarRadiation->addProperty( 1.8000, 28.6 );
		aSolarRadiation->addProperty( 1.8600, 1.8 );
		aSolarRadiation->addProperty( 1.9200, 1.1 );
		aSolarRadiation->addProperty( 1.9600, 19.7 );
		aSolarRadiation->addProperty( 1.9850, 84.9 );
		aSolarRadiation->addProperty( 2.0050, 25.0 );
		aSolarRadiation->addProperty( 2.0350, 92.5 );
		aSolarRadiation->addProperty( 2.0650, 56.3 );
		aSolarRadiation->addProperty( 2.1000, 82.7 );
		aSolarRadiation->addProperty( 2.1480, 76.2 );
		aSolarRadiation->addProperty( 2.1980, 66.4 );
		aSolarRadiation->addProperty( 2.2700, 65.0 );
		aSolarRadiation->addProperty( 2.3600, 57.6 );
		aSolarRadiation->addProperty( 2.4500, 19.8 );
		aSolarRadiation->addProperty( 2.4940, 17.0 );
		aSolarRadiation->addProperty( 2.5370, 3.0 );
		aSolarRadiation->addProperty( 2.9410, 4.0 );
		aSolarRadiation->addProperty( 2.9730, 7.0 );
		aSolarRadiation->addProperty( 3.0050, 6.0 );
		aSolarRadiation->addProperty( 3.0560, 3.0 );
		aSolarRadiation->addProperty( 3.1320, 5.0 );
		aSolarRadiation->addProperty( 3.1560, 18.0 );
		aSolarRadiation->addProperty( 3.2040, 1.2 );
		aSolarRadiation->addProperty( 3.2450, 3.0 );
		aSolarRadiation->addProperty( 3.3170, 12.0 );
		aSolarRadiation->addProperty( 3.3440, 3.0 );
		aSolarRadiation->addProperty( 3.4500, 12.2 );
		aSolarRadiation->addProperty( 3.5730, 11.0 );
		aSolarRadiation->addProperty( 3.7650, 9.0 );
		aSolarRadiation->addProperty( 4.0450, 6.9 );

		std::shared_ptr< CSpectralSampleData > aMeasurements = std::make_shared< CSpectralSampleData >();

		aMeasurements->addRecord( 0.300, 0.0020, 0.0470, 0.0480 );
		aMeasurements->addRecord( 0.305, 0.0030, 0.0470, 0.0480 );
		aMeasurements->addRecord( 0.310, 0.0090, 0.0470, 0.0480 );
		aMeasurements->addRecord( 0.315, 0.0350, 0.0470, 0.0480 );
		aMeasurements->addRecord( 0.320, 0.1000, 0.0470, 0.0480 );
		aMeasurements->addRecord( 0.325, 0.2180, 0.0490, 0.0500 );
		aMeasurements->addRecord( 0.330, 0.3560, 0.0530, 0.0540 );
		aMeasurements->addRecord( 0.335, 0.4980, 0.0600, 0.0610 );
		aMeasurements->addRecord( 0.340, 0.6160, 0.0670, 0.0670 );
		aMeasurements->addRecord( 0.345, 0.7090, 0.0730, 0.0740 );
		aMeasurements->addRecord( 0.350, 0.7740, 0.0780, 0.0790 );
		aMeasurements->addRecord( 0.355, 0.8180, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.360, 0.8470, 0.0840, 0.0840 );
		aMeasurements->addRecord( 0.365, 0.8630, 0.0850, 0.0850 );
		aMeasurements->addRecord( 0.370, 0.8690, 0.0850, 0.0860 );
		aMeasurements->addRecord( 0.375, 0.8610, 0.0850, 0.0850 );
		aMeasurements->addRecord( 0.380, 0.8560, 0.0840, 0.0840 );
		aMeasurements->addRecord( 0.385, 0.8660, 0.0850, 0.0850 );
		aMeasurements->addRecord( 0.390, 0.8810, 0.0860, 0.0860 );
		aMeasurements->addRecord( 0.395, 0.8890, 0.0860, 0.0860 );
		aMeasurements->addRecord( 0.400, 0.8930, 0.0860, 0.0860 );
		aMeasurements->addRecord( 0.410, 0.8930, 0.0860, 0.0860 );
		aMeasurements->addRecord( 0.420, 0.8920, 0.0860, 0.0860 );
		aMeasurements->addRecord( 0.430, 0.8920, 0.0850, 0.0850 );
		aMeasurements->addRecord( 0.440, 0.8920, 0.0850, 0.0850 );
		aMeasurements->addRecord( 0.450, 0.8960, 0.0850, 0.0850 );
		aMeasurements->addRecord( 0.460, 0.9000, 0.0850, 0.0850 );
		aMeasurements->addRecord( 0.470, 0.9020, 0.0840, 0.0840 );
		aMeasurements->addRecord( 0.480, 0.9030, 0.0840, 0.0840 );
		aMeasurements->addRecord( 0.490, 0.9040, 0.0850, 0.0850 );
		aMeasurements->addRecord( 0.500, 0.9050, 0.0840, 0.0840 );
		aMeasurements->addRecord( 0.510, 0.9050, 0.0840, 0.0840 );
		aMeasurements->addRecord( 0.520, 0.9050, 0.0840, 0.0840 );
		aMeasurements->addRecord( 0.530, 0.9040, 0.0840, 0.0840 );
		aMeasurements->addRecord( 0.540, 0.9040, 0.0830, 0.0830 );
		aMeasurements->addRecord( 0.550, 0.9030, 0.0830, 0.0830 );
		aMeasurements->addRecord( 0.560, 0.9020, 0.0830, 0.0830 );
		aMeasurements->addRecord( 0.570, 0.9000, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.580, 0.8980, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.590, 0.8960, 0.0810, 0.0810 );
		aMeasurements->addRecord( 0.600, 0.8930, 0.0810, 0.0810 );
		aMeasurements->addRecord( 0.610, 0.8900, 0.0810, 0.0810 );
		aMeasurements->addRecord( 0.620, 0.8860, 0.0800, 0.0800 );
		aMeasurements->addRecord( 0.630, 0.8830, 0.0800, 0.0800 );
		aMeasurements->addRecord( 0.640, 0.8790, 0.0790, 0.0790 );
		aMeasurements->addRecord( 0.650, 0.8750, 0.0790, 0.0790 );
		aMeasurements->addRecord( 0.660, 0.8720, 0.0790, 0.0790 );
		aMeasurements->addRecord( 0.670, 0.8680, 0.0780, 0.0780 );
		aMeasurements->addRecord( 0.680, 0.8630, 0.0780, 0.0780 );
		aMeasurements->addRecord( 0.690, 0.8590, 0.0770, 0.0770 );
		aMeasurements->addRecord( 0.700, 0.8540, 0.0760, 0.0770 );
		aMeasurements->addRecord( 0.710, 0.8500, 0.0760, 0.0760 );
		aMeasurements->addRecord( 0.720, 0.8450, 0.0750, 0.0760 );
		aMeasurements->addRecord( 0.730, 0.8400, 0.0750, 0.0750 );
		aMeasurements->addRecord( 0.740, 0.8350, 0.0750, 0.0750 );
		aMeasurements->addRecord( 0.750, 0.8310, 0.0740, 0.0740 );
		aMeasurements->addRecord( 0.760, 0.8260, 0.0740, 0.0740 );
		aMeasurements->addRecord( 0.770, 0.8210, 0.0740, 0.0740 );
		aMeasurements->addRecord( 0.780, 0.8160, 0.0730, 0.0730 );
		aMeasurements->addRecord( 0.790, 0.8120, 0.0730, 0.0730 );
		aMeasurements->addRecord( 0.800, 0.8080, 0.0720, 0.0720 );
		aMeasurements->addRecord( 0.810, 0.8030, 0.0720, 0.0720 );
		aMeasurements->addRecord( 0.820, 0.8000, 0.0720, 0.0720 );
		aMeasurements->addRecord( 0.830, 0.7960, 0.0710, 0.0710 );
		aMeasurements->addRecord( 0.840, 0.7930, 0.0700, 0.0710 );
		aMeasurements->addRecord( 0.850, 0.7880, 0.0700, 0.0710 );
		aMeasurements->addRecord( 0.860, 0.7860, 0.0700, 0.0700 );
		aMeasurements->addRecord( 0.870, 0.7820, 0.0740, 0.0740 );
		aMeasurements->addRecord( 0.880, 0.7800, 0.0720, 0.0720 );
		aMeasurements->addRecord( 0.890, 0.7770, 0.0730, 0.0740 );
		aMeasurements->addRecord( 0.900, 0.7760, 0.0720, 0.0720 );
		aMeasurements->addRecord( 0.910, 0.7730, 0.0720, 0.0720 );
		aMeasurements->addRecord( 0.920, 0.7710, 0.0710, 0.0710 );
		aMeasurements->addRecord( 0.930, 0.7700, 0.0700, 0.0700 );
		aMeasurements->addRecord( 0.940, 0.7680, 0.0690, 0.0690 );
		aMeasurements->addRecord( 0.950, 0.7660, 0.0680, 0.0680 );
		aMeasurements->addRecord( 0.960, 0.7660, 0.0670, 0.0680 );
		aMeasurements->addRecord( 0.970, 0.7640, 0.0680, 0.0680 );
		aMeasurements->addRecord( 0.980, 0.7630, 0.0680, 0.0680 );
		aMeasurements->addRecord( 0.990, 0.7620, 0.0670, 0.0670 );
		aMeasurements->addRecord( 1.000, 0.7620, 0.0660, 0.0670 );
		aMeasurements->addRecord( 1.050, 0.7600, 0.0660, 0.0660 );
		aMeasurements->addRecord( 1.100, 0.7590, 0.0660, 0.0660 );
		aMeasurements->addRecord( 1.150, 0.7610, 0.0660, 0.0660 );
		aMeasurements->addRecord( 1.200, 0.7650, 0.0660, 0.0660 );
		aMeasurements->addRecord( 1.250, 0.7700, 0.0650, 0.0650 );
		aMeasurements->addRecord( 1.300, 0.7770, 0.0670, 0.0670 );
		aMeasurements->addRecord( 1.350, 0.7860, 0.0660, 0.0670 );
		aMeasurements->addRecord( 1.400, 0.7950, 0.0670, 0.0680 );
		aMeasurements->addRecord( 1.450, 0.8080, 0.0670, 0.0670 );
		aMeasurements->addRecord( 1.500, 0.8190, 0.0690, 0.0690 );
		aMeasurements->addRecord( 1.550, 0.8290, 0.0690, 0.0690 );
		aMeasurements->addRecord( 1.600, 0.8360, 0.0700, 0.0700 );
		aMeasurements->addRecord( 1.650, 0.8400, 0.0700, 0.0700 );
		aMeasurements->addRecord( 1.700, 0.8420, 0.0690, 0.0700 );
		aMeasurements->addRecord( 1.750, 0.8420, 0.0690, 0.0700 );
		aMeasurements->addRecord( 1.800, 0.8410, 0.0700, 0.0700 );
		aMeasurements->addRecord( 1.850, 0.8400, 0.0690, 0.0690 );
		aMeasurements->addRecord( 1.900, 0.8390, 0.0680, 0.0680 );
		aMeasurements->addRecord( 1.950, 0.8390, 0.0710, 0.0710 );
		aMeasurements->addRecord( 2.000, 0.8390, 0.0690, 0.0690 );
		aMeasurements->addRecord( 2.050, 0.8400, 0.0680, 0.0680 );
		aMeasurements->addRecord( 2.100, 0.8410, 0.0680, 0.0680 );
		aMeasurements->addRecord( 2.150, 0.8390, 0.0690, 0.0690 );
		aMeasurements->addRecord( 2.200, 0.8300, 0.0700, 0.0700 );
		aMeasurements->addRecord( 2.250, 0.8300, 0.0700, 0.0700 );
		aMeasurements->addRecord( 2.300, 0.8320, 0.0690, 0.0690 );
		aMeasurements->addRecord( 2.350, 0.8320, 0.0690, 0.0700 );
		aMeasurements->addRecord( 2.400, 0.8320, 0.0700, 0.0700 );
		aMeasurements->addRecord( 2.450, 0.8260, 0.0690, 0.0690 );
		aMeasurements->addRecord( 2.500, 0.8220, 0.0680, 0.0680 );

		std::shared_ptr< CSpectralSample > aSample = std::make_shared< CSpectralSample >( aMeasurements, aSolarRadiation );

		double thickness = 3.048e-3; // [m]
		MaterialType aType = MaterialType::Monolithic;
		double minLambda = 0.3;
		double maxLambda = 2.5;
		std::shared_ptr< CMaterial > aMaterial =
			std::make_shared< CMaterialSample >( aSample, thickness, aType, minLambda, maxLambda );

		// specular layer NFRC=102
		CBSDFLayerMaker aMaker102 = CBSDFLayerMaker( aMaterial, aBSDF );
		std::shared_ptr< CBSDFLayer > aLayer102 = aMaker102.getLayer();

		// Perforated cell
		// create material
		double Tmat = 0.2;
		double Rfmat = 0.75;
		double Rbmat = 0.66;
		std::shared_ptr< CMaterial > perfMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda );

		// make cell geometry
		double x = 22.5; // mm
		double y = 38.1; // mm
		thickness = 5; // mm
		double radius = 8.35; // mm
		std::shared_ptr< ICellDescription > perfCellDescription =
			std::make_shared< CCircularCellDescription >( x, y, thickness, radius );

		// get shading BSDF layer
		CBSDFLayerMaker aMakerVenetian = CBSDFLayerMaker( perfMaterial, aBSDF, perfCellDescription );
		std::shared_ptr< CBSDFLayer > aShade = aMakerVenetian.getLayer();

		CBSDFIntegrator aLayer1 = *aLayer102->getResults();
		CBSDFIntegrator aLayer2 = *aShade->getResults();

		m_DoubleLayer = std::make_shared< CBSDFDoubleLayer >( aLayer1, aLayer2 );

	}

public:
	std::shared_ptr< CBSDFDoubleLayer > getDoubleLayer() const
	{
		return m_DoubleLayer;
	}

};

TEST_F( TestDoubleLayerBSDFPerforated, TestDoubleLayerBSDF ) {
	SCOPED_TRACE( "Begin Test: Double Layer BSDF." );

	std::shared_ptr< CBSDFIntegrator > aLayer = getDoubleLayer()->value();

	// Front transmittance
	std::shared_ptr< CSquareMatrix > Tf = aLayer->getMatrix( Side::Front, PropertySimple::T );
	size_t matrixSize = Tf->getSize();

	// Test matrix
	size_t size = 7;

	EXPECT_EQ( size, matrixSize );

	CSquareMatrix correctResults = CSquareMatrix( size );
	correctResults[ 0 ] = { 4.02734229, 0.04807644, 0.04923533, 0.05014887, 0.04956643, 0.04042293, 0.01069836 };
	correctResults[ 1 ] = { 0.04660969, 0.52907808, 0.0489721, 0.04988075, 0.04930142, 0.04020681, 0.01064117 };
	correctResults[ 2 ] = { 0.04638265, 0.04758648, 0.29694739, 0.04963778, 0.04906128, 0.04001096, 0.01058933 };
	correctResults[ 3 ] = { 0.04633115, 0.04753364, 0.04867944, 0.22773067, 0.0490068, 0.03996653, 0.01057757 };
	correctResults[ 4 ] = { 0.04660871, 0.0478184, 0.04897107, 0.0498797, 0.18009053, 0.04020597, 0.01064094 };
	correctResults[ 5 ] = { 0.04372094, 0.04485568, 0.04593693, 0.04678926, 0.04624584, 0.03771489, 0.00998165 };
	correctResults[ 6 ] = { 0.04372094, 0.04485568, 0.04593693, 0.04678926, 0.04624584, 0.03771489, 0.00998165 };

	for ( size_t i = 0; i < size; ++i ) {
		for ( size_t j = 0; j < size; ++j ) {
			EXPECT_NEAR( correctResults[ i ][ j ], ( *Tf )[ i ][ j ], 1e-6 );
		}
	}

	// Front reflectance
	std::shared_ptr< CSquareMatrix > Rf = aLayer->getMatrix( Side::Front, PropertySimple::R );
	matrixSize = Rf->getSize();

	EXPECT_EQ( size, matrixSize );

	correctResults[ 0 ] = { 1.53453038, 0.14025367, 0.14363451, 0.14629956, 0.14460041, 0.11792603, 0.0312104 };
	correctResults[ 1 ] = { 0.13649545, 0.32414895, 0.14341371, 0.14607467, 0.14437812, 0.11774475, 0.03116242 };
	correctResults[ 2 ] = { 0.13559035, 0.13910948, 0.25137713, 0.14510605, 0.14342076, 0.11696399, 0.03095578 };
	correctResults[ 3 ] = { 0.13266706, 0.13611032, 0.13939128, 0.25120164, 0.14032865, 0.11444228, 0.03028838 };
	correctResults[ 4 ] = { 0.12275277, 0.12593872, 0.12897449, 0.13136754, 0.33299805, 0.10588994, 0.02802492 };
	correctResults[ 5 ] = { 0.08779424, 0.09007286, 0.09224408, 0.09395562, 0.0928644, 0.94925871, 0.02004375 };
	correctResults[ 6 ] = { 0.02323569, 0.02383875, 0.02441339, 0.02486637, 0.02457757, 0.02004375, 14.46404805 };

	for ( size_t i = 0; i < size; ++i ) {
		for ( size_t j = 0; j < size; ++j ) {
			EXPECT_NEAR( correctResults[ i ][ j ], ( *Rf )[ i ][ j ], 1e-6 );
		}
	}

	// Back Transmittance
	std::shared_ptr< CSquareMatrix > Tb = aLayer->getMatrix( Side::Back, PropertySimple::T );
	matrixSize = Tb->getSize();

	EXPECT_EQ( size, matrixSize );

	correctResults[ 0 ] = { 4.02734229, 0.04789303, 0.0491344, 0.05109201, 0.05490417, 0.05872637, 0.05872637 };
	correctResults[ 1 ] = { 0.04678819, 0.52907808, 0.04905886, 0.05101347, 0.05481977, 0.05863609, 0.05863609 };
	correctResults[ 2 ] = { 0.04647794, 0.04750232, 0.29694739, 0.0506752, 0.05445626, 0.05824728, 0.05824728 };
	correctResults[ 3 ] = { 0.04547589, 0.04647818, 0.04768287, 0.22773067, 0.0532822, 0.05699148, 0.05699148 };
	correctResults[ 4 ] = { 0.04207745, 0.04300484, 0.04411951, 0.04587732, 0.18009053, 0.05273248, 0.05273248 };
	correctResults[ 5 ] = { 0.03009429, 0.03075757, 0.03155479, 0.03281200, 0.03526022, 0.03771489, 0.03771489 };
	correctResults[ 6 ] = { 0.00796478, 0.00814032, 0.00835132, 0.00868405, 0.00933200, 0.00998165, 0.00998165 };

	for ( size_t i = 0; i < size; ++i ) {
		for ( size_t j = 0; j < size; ++j ) {
			EXPECT_NEAR( correctResults[ i ][ j ], ( *Tb )[ i ][ j ], 1e-6 );
		}
	}

	// Back Reflectance
	std::shared_ptr< CSquareMatrix > Rb = aLayer->getMatrix( Side::Back, PropertySimple::R );
	matrixSize = Rb->getSize();

	EXPECT_EQ( size, matrixSize );

	correctResults[ 0 ] = { 0.25123617, 0.16414142, 0.16913772, 0.17605257, 0.1881739, 0.21323834, 0.21323834 };
	correctResults[ 1 ] = { 0.1597431, 0.17423888, 0.16904768, 0.17595894, 0.18807328, 0.21313072, 0.21313072 };
	correctResults[ 2 ] = { 0.15966527, 0.16397411, 0.17383866, 0.17587408, 0.1879821, 0.21303319, 0.21303319 };
	correctResults[ 3 ] = { 0.15964762, 0.16395606, 0.16894756, 0.17935988, 0.18796142, 0.21301106, 0.21301106 };
	correctResults[ 4 ] = { 0.15974276, 0.1640533, 0.16904732, 0.17595857, 0.19114974, 0.2131303, 0.21313030 };
	correctResults[ 5 ] = { 0.15875288, 0.16304161, 0.16800941, 0.17487930, 0.18691309, 0.21188976, 0.21188976 };
	correctResults[ 6 ] = { 0.15875288, 0.16304161, 0.16800941, 0.17487930, 0.18691309, 0.21188976, 0.21188976 };

	for ( size_t i = 0; i < size; ++i ) {
		for ( size_t j = 0; j < size; ++j ) {
			EXPECT_NEAR( correctResults[ i ][ j ], ( *Rb )[ i ][ j ], 1e-6 );
		}
	}

}
