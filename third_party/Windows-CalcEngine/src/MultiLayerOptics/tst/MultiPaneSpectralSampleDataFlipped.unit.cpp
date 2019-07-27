#include <memory>
#include <gtest/gtest.h>

#include "WCEMultiLayerOptics.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCESpectralAveraging.hpp"
#include "WCECommon.hpp"


using namespace FenestrationCommon;
using namespace SpectralAveraging;
using namespace MultiLayerOptics;

class TestMultiLayerOpticsMeasuredSampleDataFlipped : public testing::Test {

private:
	std::shared_ptr< CMultiPaneSampleData > m_MultiLayerOptics;

protected:
	virtual void SetUp() {

		std::shared_ptr< CSpectralSampleData > sampleMeasurements1 = std::make_shared< CSpectralSampleData >();
		sampleMeasurements1->addRecord( 0.330, 0.0857, 0.0560, 0.2646 );
		sampleMeasurements1->addRecord( 0.335, 0.1280, 0.0623, 0.2664 );
		sampleMeasurements1->addRecord( 0.340, 0.1707, 0.0719, 0.2668 );
		sampleMeasurements1->addRecord( 0.345, 0.2125, 0.0840, 0.2680 );
		sampleMeasurements1->addRecord( 0.350, 0.2536, 0.0990, 0.2706 );
		sampleMeasurements1->addRecord( 0.355, 0.2953, 0.1165, 0.2735 );
		sampleMeasurements1->addRecord( 0.360, 0.3370, 0.1365, 0.2773 );
		sampleMeasurements1->addRecord( 0.365, 0.3774, 0.1579, 0.2809 );
		sampleMeasurements1->addRecord( 0.370, 0.4125, 0.1773, 0.2829 );
		sampleMeasurements1->addRecord( 0.375, 0.4414, 0.1931, 0.2836 );
		sampleMeasurements1->addRecord( 0.380, 0.4671, 0.2074, 0.2827 );
		sampleMeasurements1->addRecord( 0.385, 0.4953, 0.2244, 0.2814 );
		sampleMeasurements1->addRecord( 0.390, 0.5229, 0.2415, 0.2801 );
		sampleMeasurements1->addRecord( 0.395, 0.5455, 0.2553, 0.2781 );
		sampleMeasurements1->addRecord( 0.400, 0.5630, 0.2651, 0.2757 );

		std::shared_ptr< CSpectralSampleData > sampleMeasurements2 = std::make_shared< CSpectralSampleData >();
		sampleMeasurements2->addRecord( 0.330, 0.1600, 0.0450, 0.0470 );
		sampleMeasurements2->addRecord( 0.335, 0.2940, 0.0490, 0.0500 );
		sampleMeasurements2->addRecord( 0.340, 0.4370, 0.0550, 0.0560 );
		sampleMeasurements2->addRecord( 0.345, 0.5660, 0.0620, 0.0620 );
		sampleMeasurements2->addRecord( 0.350, 0.6710, 0.0690, 0.0690 );
		sampleMeasurements2->addRecord( 0.355, 0.7440, 0.0740, 0.0740 );
		sampleMeasurements2->addRecord( 0.360, 0.7930, 0.0780, 0.0780 );
		sampleMeasurements2->addRecord( 0.365, 0.8220, 0.0800, 0.0800 );
		sampleMeasurements2->addRecord( 0.370, 0.8320, 0.0810, 0.0810 );
		sampleMeasurements2->addRecord( 0.375, 0.8190, 0.0800, 0.0800 );
		sampleMeasurements2->addRecord( 0.380, 0.8090, 0.0790, 0.0790 );
		sampleMeasurements2->addRecord( 0.385, 0.8290, 0.0800, 0.0800 );
		sampleMeasurements2->addRecord( 0.390, 0.8530, 0.0820, 0.0820 );
		sampleMeasurements2->addRecord( 0.395, 0.8680, 0.0830, 0.0830 );
		sampleMeasurements2->addRecord( 0.400, 0.8750, 0.0830, 0.0830 );

		sampleMeasurements1->Filpped( true );

		m_MultiLayerOptics = std::make_shared< CMultiPaneSampleData >();
		m_MultiLayerOptics->addSample( sampleMeasurements1 );
		m_MultiLayerOptics->addSample( sampleMeasurements2 );
	}

public:
	std::shared_ptr< CMultiPaneSampleData > getMultiLayerOptics() {
		return m_MultiLayerOptics;
	};

};

TEST_F( TestMultiLayerOpticsMeasuredSampleDataFlipped, TestDoublePaneResultsFlipped ) {
	SCOPED_TRACE( "Begin Test: Test simple double pane calculations - Flipped (T, Rf, Rb and equivalent absorptances)." );

	CMultiPaneSampleData MultiLayerOptics = *getMultiLayerOptics();

	std::vector< double > correctT;
	correctT.push_back( 0.013746642 );
	correctT.push_back( 0.037747231 );
	correctT.push_back( 0.074892061 );
	correctT.push_back( 0.120904672 );
	correctT.push_back( 0.171335996 );
	correctT.push_back( 0.221613732 );
	correctT.push_back( 0.270116935 );
	correctT.push_back( 0.314191669 );
	correctT.push_back( 0.348200613 );
	correctT.push_back( 0.367178778 );
	correctT.push_back( 0.384178511 );
	correctT.push_back( 0.418109604 );
	correctT.push_back( 0.455044955 );
	correctT.push_back( 0.483744498 );
	correctT.push_back( 0.503708244 );

	CSeries transmittances = *MultiLayerOptics.properties( SampleData::T );

	EXPECT_EQ( transmittances.size(), correctT.size() );

	for ( size_t i = 0; i < transmittances.size(); ++i ) {
		EXPECT_NEAR( correctT[ i ], transmittances[ i ].value(), 1e-6 );
	}

	std::vector< double > correctRf;
	correctRf.push_back( 0.264931337 );
	correctRf.push_back( 0.267205274 );
	correctRf.push_back( 0.268408980 );
	correctRf.push_back( 0.270814345 );
	correctRf.push_back( 0.275068116 );
	correctRf.push_back( 0.280009069 );
	correctRf.push_back( 0.286253712 );
	correctRf.push_back( 0.292440237 );
	correctRf.push_back( 0.296883477 );
	correctRf.push_back( 0.299431278 );
	correctRf.push_back( 0.300223526 );
	correctRf.push_back( 0.301384529 );
	correctRf.push_back( 0.302973771 );
	correctRf.push_back( 0.303333016 );
	correctRf.push_back( 0.302600323 );

	CSeries Rf = *MultiLayerOptics.properties( SampleData::Rf );

	EXPECT_EQ( Rf.size(), correctRf.size() );

	for ( size_t i = 0; i < Rf.size(); ++i ) {
		EXPECT_NEAR( correctRf[ i ], Rf[ i ].value(), 1e-6 );
	}

	std::vector< double > correctRb;
	correctRb.push_back( 0.048437222 );
	correctRb.push_back( 0.055401452 );
	correctRb.push_back( 0.069785185 );
	correctRb.push_back( 0.089050784 );
	correctRb.push_back( 0.113880437 );
	correctRb.push_back( 0.139047720 );
	correctRb.push_back( 0.164761640 );
	correctRb.push_back( 0.188055460 );
	correctRb.push_back( 0.205519578 );
	correctRb.push_back( 0.211556230 );
	correctRb.push_back( 0.217000441 );
	correctRb.push_back( 0.237035991 );
	correctRb.push_back( 0.261267610 );
	correctRb.push_back( 0.279513243 );
	correctRb.push_back( 0.290533612 );

	CSeries Rb = *MultiLayerOptics.properties( SampleData::Rb );

	EXPECT_EQ( Rb.size(), correctRb.size() );

	for ( size_t i = 0; i < Rb.size(); ++i ) {
		EXPECT_NEAR( correctRb[ i ], Rb[ i ].value(), 1e-6 );
	}

	std::vector< double > correctAbs;
	correctAbs.push_back( 0.721322021 );
	correctAbs.push_back( 0.695047495 );
	correctAbs.push_back( 0.656698960 );
	correctAbs.push_back( 0.608280984 );
	correctAbs.push_back( 0.553595888 );
	correctAbs.push_back( 0.498377199 );
	correctAbs.push_back( 0.443629353 );
	correctAbs.push_back( 0.393368094 );
	correctAbs.push_back( 0.354915909 );
	correctAbs.push_back( 0.333389944 );
	correctAbs.push_back( 0.315597962 );
	correctAbs.push_back( 0.280505867 );
	correctAbs.push_back( 0.241981274 );
	correctAbs.push_back( 0.212922487 );
	correctAbs.push_back( 0.193691434 );

	CSeries Abs = *MultiLayerOptics.properties( SampleData::AbsF );

	EXPECT_EQ( Abs.size(), correctAbs.size() );

	for ( size_t i = 0; i < Abs.size(); ++i ) {
		EXPECT_NEAR( correctAbs[ i ], Abs[ i ].value(), 1e-6 );
	}


}

TEST_F( TestMultiLayerOpticsMeasuredSampleDataFlipped, TestDoublePaneAbsorptancesFlipped ) {
	SCOPED_TRACE( "Begin Test: Test layer absroptances - Flipped." );

	CMultiPaneSampleData MultiLayerOptics = *getMultiLayerOptics();

	std::vector< double > correctAbs;
	correctAbs.push_back( 0.653018396 );
	correctAbs.push_back( 0.610693989 );
	correctAbs.push_back( 0.569639081 );
	correctAbs.push_back( 0.528817136 );
	correctAbs.push_back( 0.487206381 );
	correctAbs.push_back( 0.444165237 );
	correctAbs.push_back( 0.399688515 );
	correctAbs.push_back( 0.355909720 );
	correctAbs.push_back( 0.318505509 );
	correctAbs.push_back( 0.288109045 );
	correctAbs.push_back( 0.262411321 );
	correctAbs.push_back( 0.234609638 );
	correctAbs.push_back( 0.207306101 );
	correctAbs.push_back( 0.185614330 );
	correctAbs.push_back( 0.169513438 );

	CSeries abs = *MultiLayerOptics.getLayerAbsorptances( 1 );

	EXPECT_EQ( abs.size(), correctAbs.size() );

	for ( size_t i = 0; i < abs.size(); ++i ) {
		EXPECT_NEAR( correctAbs[ i ], abs[ i ].value(), 1e-6 );
	}

	correctAbs.clear();
	correctAbs.push_back( 0.068303625 );
	correctAbs.push_back( 0.084353506 );
	correctAbs.push_back( 0.087059878 );
	correctAbs.push_back( 0.079463848 );
	correctAbs.push_back( 0.066389507 );
	correctAbs.push_back( 0.054211961 );
	correctAbs.push_back( 0.043940838 );
	correctAbs.push_back( 0.037458374 );
	correctAbs.push_back( 0.036410401 );
	correctAbs.push_back( 0.045280899 );
	correctAbs.push_back( 0.053186642 );
	correctAbs.push_back( 0.045896229 );
	correctAbs.push_back( 0.034675172 );
	correctAbs.push_back( 0.027308157 );
	correctAbs.push_back( 0.024177996 );

	abs = *MultiLayerOptics.getLayerAbsorptances( 2 );

	EXPECT_EQ( abs.size(), correctAbs.size() );

	for ( size_t i = 0; i < abs.size(); ++i ) {
		EXPECT_NEAR( correctAbs[ i ], abs[ i ].value(), 1e-6 );
	}
}
