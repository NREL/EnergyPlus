#include <memory>
#include <gtest/gtest.h>

#include "WCEMultiLayerOptics.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace FenestrationCommon;
using namespace SpectralAveraging;
using namespace MultiLayerOptics;

class TestMultiLayerOpticsMeasuredSampleData : public testing::Test {

private:
	std::shared_ptr< CMultiPaneSampleData > m_MultiLayerOptics;

protected:
	virtual void SetUp() {

		// This test (example) shows how to get multilayer sample data from two measurements. Results are calculated
		// at each wavelength

		std::shared_ptr< CSpectralSampleData > sampleMeasurements1 = make_shared< CSpectralSampleData >();
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

		std::shared_ptr< CSpectralSampleData > sampleMeasurements2 = make_shared< CSpectralSampleData >();
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

		m_MultiLayerOptics = make_shared< CMultiPaneSampleData >();
		m_MultiLayerOptics->addSample( sampleMeasurements1 );
		m_MultiLayerOptics->addSample( sampleMeasurements2 );
	}

public:
	std::shared_ptr< CMultiPaneSampleData > getMultiLayerOptics() {
		return m_MultiLayerOptics;
	};

};

TEST_F( TestMultiLayerOpticsMeasuredSampleData, TestDoublePaneResults ) {
	SCOPED_TRACE( "Begin Test: Test simple double pane calculations (T, Rf, Rb and equivalent absorptances)." );

	auto MultiLayerOptics = getMultiLayerOptics();

	vector< double > correctT;
	correctT.push_back( 0.013877236 );
	correctT.push_back( 0.038129730 );
	correctT.push_back( 0.075706822 );
	correctT.push_back( 0.122307257 );
	correctT.push_back( 0.173403282 );
	correctT.push_back( 0.224241626 );
	correctT.push_back( 0.273149050 );
	correctT.push_back( 0.317354388 );
	correctT.push_back( 0.351248832 );
	correctT.push_back( 0.369898865 );
	correctT.push_back( 0.386516080 );
	correctT.push_back( 0.420060093 );
	correctT.push_back( 0.456519123 );
	correctT.push_back( 0.484681565 );
	correctT.push_back( 0.504161785 );

	CSeries transmittances = *MultiLayerOptics->properties( SampleData::T );

	EXPECT_EQ( transmittances.size(), correctT.size() );

	for ( size_t i = 0; i < transmittances.size(); ++i ) {
		EXPECT_NEAR( correctT[ i ], transmittances[ i ].value(), 1e-6 );
	}

	vector< double > correctRf;
	correctRf.push_back( 0.056334485 );
	correctRf.push_back( 0.063113434 );
	correctRf.push_back( 0.073526484 );
	correctRf.push_back( 0.086846993 );
	correctRf.push_back( 0.103522027 );
	correctRf.push_back( 0.123086254 );
	correctRf.push_back( 0.145554219 );
	correctRf.push_back( 0.169556403 );
	correctRf.push_back( 0.191405891 );
	correctRf.push_back( 0.209048558 );
	correctRf.push_back( 0.225030150 );
	correctRf.push_back( 0.244477758 );
	correctRf.push_back( 0.264447873 );
	correctRf.push_back( 0.280581895 );
	correctRf.push_back( 0.292024544 );

	CSeries Rf = *MultiLayerOptics->properties( SampleData::Rf );

	EXPECT_EQ( Rf.size(), correctRf.size() );

	for ( size_t i = 0; i < Rf.size(); ++i ) {
		EXPECT_NEAR( correctRf[ i ], Rf[ i ].value(), 1e-6 );
	}

	vector< double > correctRb;
	correctRb.push_back( 0.053855387 );
	correctRb.push_back( 0.073331105 );
	correctRb.push_back( 0.107709312 );
	correctRb.push_back( 0.149306086 );
	correctRb.push_back( 0.193153331 );
	correctRb.push_back( 0.228519414 );
	correctRb.push_back( 0.256234943 );
	correctRb.push_back( 0.274162863 );
	correctRb.push_back( 0.281422839 );
	correctRb.push_back( 0.274643900 );
	correctRb.push_back( 0.268248318 );
	correctRb.push_back( 0.277843470 );
	correctRb.push_back( 0.290594317 );
	correctRb.push_back( 0.297477857 );
	correctRb.push_back( 0.299026161 );

	CSeries Rb = *MultiLayerOptics->properties( SampleData::Rb );

	EXPECT_EQ( Rb.size(), correctRb.size() );

	for ( size_t i = 0; i < Rb.size(); ++i ) {
		EXPECT_NEAR( correctRb[ i ], Rb[ i ].value(), 1e-6 );
	}

	vector< double > correctAbs;
	correctAbs.push_back( 0.929788279 );
	correctAbs.push_back( 0.898756836 );
	correctAbs.push_back( 0.850766694 );
	correctAbs.push_back( 0.790845749 );
	correctAbs.push_back( 0.723074691 );
	correctAbs.push_back( 0.652672120 );
	correctAbs.push_back( 0.581296731 );
	correctAbs.push_back( 0.513089209 );
	correctAbs.push_back( 0.457345277 );
	correctAbs.push_back( 0.421052577 );
	correctAbs.push_back( 0.388453771 );
	correctAbs.push_back( 0.335462150 );
	correctAbs.push_back( 0.279033005 );
	correctAbs.push_back( 0.234736540 );
	correctAbs.push_back( 0.203813671 );

	CSeries Abs = *MultiLayerOptics->properties( SampleData::AbsF );

	EXPECT_EQ( Abs.size(), correctAbs.size() );

	for ( size_t i = 0; i < Abs.size(); ++i ) {
		EXPECT_NEAR( correctAbs[ i ], Abs[ i ].value(), 1e-6 );
	}


}

TEST_F( TestMultiLayerOpticsMeasuredSampleData, TestDoublePaneAbsorptances ) {
	SCOPED_TRACE( "Begin Test: Test layer absroptances." );

	auto MultiLayerOptics = getMultiLayerOptics();

	vector< double > correctAbs;
	correctAbs.push_back( 0.860835761 );
	correctAbs.push_back( 0.813548561 );
	correctAbs.push_back( 0.762759679 );
	correctAbs.push_back( 0.710460061 );
	correctAbs.push_back( 0.655884150 );
	correctAbs.push_back( 0.597817313 );
	correctAbs.push_back( 0.536862648 );
	correctAbs.push_back( 0.475253771 );
	correctAbs.push_back( 0.420616132 );
	correctAbs.push_back( 0.375436233 );
	correctAbs.push_back( 0.334943510 );
	correctAbs.push_back( 0.289351814 );
	correctAbs.push_back( 0.244245498 );
	correctAbs.push_back( 0.207375484 );
	correctAbs.push_back( 0.179613906 );

	CSeries abs = *MultiLayerOptics->getLayerAbsorptances( 1 );

	EXPECT_EQ( abs.size(), correctAbs.size() );

	for ( size_t i = 0; i < abs.size(); ++i ) {
		EXPECT_NEAR( correctAbs[ i ], abs[ i ].value(), 1e-6 );
	}

	correctAbs.clear();
	correctAbs.push_back( 0.068952518 );
	correctAbs.push_back( 0.085208275 );
	correctAbs.push_back( 0.088007015 );
	correctAbs.push_back( 0.080385689 );
	correctAbs.push_back( 0.067190541 );
	correctAbs.push_back( 0.054854806 );
	correctAbs.push_back( 0.044434083 );
	correctAbs.push_back( 0.037835438 );
	correctAbs.push_back( 0.036729145 );
	correctAbs.push_back( 0.045616344 );
	correctAbs.push_back( 0.053510261 );
	correctAbs.push_back( 0.046110336 );
	correctAbs.push_back( 0.034787506 );
	correctAbs.push_back( 0.027361056 );
	correctAbs.push_back( 0.024199766 );

	abs = *MultiLayerOptics->getLayerAbsorptances( 2 );

	EXPECT_EQ( abs.size(), correctAbs.size() );

	for ( size_t i = 0; i < abs.size(); ++i ) {
		EXPECT_NEAR( correctAbs[ i ], abs[ i ].value(), 1e-6 );
	}
}
