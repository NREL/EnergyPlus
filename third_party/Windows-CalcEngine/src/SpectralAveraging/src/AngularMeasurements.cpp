#include <stdexcept>
#include <limits>
#include <cmath>

#include "SpectralSample.hpp"
#include "MeasuredSampleData.hpp"
#include "WCECommon.hpp"
#include "AngularMeasurements.hpp"


using namespace FenestrationCommon;

namespace SpectralAveraging {

	//////////////////////////////////////////////////////////////////////////////////////
	////  AngularMeasurement
	//////////////////////////////////////////////////////////////////////////////////////

	CSingleAngularMeasurement::CSingleAngularMeasurement( std::shared_ptr< CSpectralSample > const& t_Data,
	                                                      double const t_Angle ) : m_Data( t_Data ), m_Angle( t_Angle ) {
		if ( t_Data == nullptr ) {
			throw std::runtime_error( "Sample must have measured data in AngularMeasurement." );
		}

	}

	double CSingleAngularMeasurement::getAngle() const {
		return m_Angle;
	}

	std::shared_ptr< CSpectralSample > CSingleAngularMeasurement::getData() const {
		return m_Data;
	}

	std::vector< double > CSingleAngularMeasurement::getWavelengthsFromSample() const {
		return m_Data->getWavelengthsFromSample();
	}

	std::shared_ptr< CSpectralSample > CSingleAngularMeasurement::Interpolate(
		double const t_Angle, std::shared_ptr< CSpectralSample > const& t_Data1, double const t_Angle1,
		std::shared_ptr< CSpectralSample > const& t_Data2, double const t_Angle2 ) const {

		auto aData = std::make_shared< CSpectralSampleData >();
		auto wlv = t_Data1->getWavelengthsFromSample();
		auto trans1 = *t_Data1->getMeasuredData()->properties( SampleData::T );;
		auto trans2 = *t_Data2->getMeasuredData()->properties( SampleData::T );;
		auto reflef1 = *t_Data1->getMeasuredData()->properties( SampleData::Rf );;
		auto reflef2 = *t_Data2->getMeasuredData()->properties( SampleData::Rf );;
		auto refleb1 = *t_Data1->getMeasuredData()->properties( SampleData::Rb );;
		auto refleb2 = *t_Data2->getMeasuredData()->properties( SampleData::Rb );;
		auto frac = ( t_Angle - t_Angle1 ) / ( t_Angle2 - t_Angle1 );
		for ( size_t i = 0; i < wlv.size(); i++ ) {
			auto wl = wlv[ i ];
			auto t1 = trans1[ i ].value();
			auto t2 = trans2[ i ].value();
			auto rf1 = reflef1[ i ].value();
			auto rf2 = reflef2[ i ].value();
			auto rb1 = refleb1[ i ].value();
			auto rb2 = refleb2[ i ].value();
			auto t = t1 + frac * ( t2 - t1 );
			auto rf = rf1 + frac * ( rf2 - rf1 );
			auto rb = rb1 + frac * ( rb2 - rb1 );
			aData->addRecord( wl, t, rf, rb );
		}
		auto aSample = std::make_shared< CSpectralSample >( aData, t_Data1->getSourceData() );

		return aSample;
	}

	void CSingleAngularMeasurement::interpolate( std::vector< double > const& t_CommonWavelengths ) const {
		m_Data->getMeasuredData()->interpolate( t_CommonWavelengths );
	}

	//////////////////////////////////////////////////////////////////////////////////////
	////  CAngularMeasurements
	//////////////////////////////////////////////////////////////////////////////////////

	CAngularMeasurements::CAngularMeasurements( std::shared_ptr< CSingleAngularMeasurement > const& t_SignleMeasurement,
	                                            std::vector< double > const& t_CommonWavelengths ):
		m_SingleMeasurement( t_SignleMeasurement ), m_CommonWavelengths( t_CommonWavelengths ) {
		if ( m_SingleMeasurement == nullptr ) {
			throw std::runtime_error( "Sample must have measured data in AngularMeasurements." );
		}
		t_SignleMeasurement->interpolate( m_CommonWavelengths );
		m_Measurements.push_back( t_SignleMeasurement );
	}

	CAngularMeasurements::CAngularMeasurements( std::vector< std::shared_ptr< CSingleAngularMeasurement > > const& t_Measurements ): m_Measurements( t_Measurements ) {

	}

	// then you add function that will accept measurements at different angles
	void CAngularMeasurements::addMeasurement( std::shared_ptr< CSingleAngularMeasurement > const& t_SingleMeasurement ) {
		t_SingleMeasurement->interpolate( m_CommonWavelengths );
		m_Measurements.push_back( t_SingleMeasurement );
	}

	// Now insert very important function here
	std::shared_ptr< CSingleAngularMeasurement > CAngularMeasurements::getMeasurements( double const t_Angle ) {
		auto const angleTolerance = 1e-6;

		if ( m_Measurements.size() == 1 ) {
			throw std::runtime_error( "A single set is found. Spectral and angular sample must have 2 sets at least." );
		}

		for ( size_t i = 0; i < m_Measurements.size(); i++ ) {
			if ( fabs( m_Measurements[ i ]->getAngle() - t_Angle ) < angleTolerance ) {
				return m_Measurements[ i ];
			}
		}

		auto min1 = std::numeric_limits< double >::max();
		auto min2 = std::numeric_limits< double >::max();
		auto angle1 = 0.0;
		auto angle2 = 0.0;
		std::shared_ptr< CSpectralSample > sample1 = nullptr;
		std::shared_ptr< CSpectralSample > sample2 = nullptr;
		for ( size_t i = 0; i < m_Measurements.size(); i++ ) {
			const auto angle = m_Measurements[ i ]->getAngle();
			const auto diff = fabs( angle - t_Angle );
			if ( diff < min1 ) {
				sample1 = m_Measurements[ i ]->getData();
				angle1 = angle;
				min1 = diff;
			}
			else if ( diff < min2 ) {
				sample2 = m_Measurements[ i ]->getData();
				angle2 = angle;
				min2 = diff;
			}
		}

		// create sample3 (CSpectralSample) that is linear interploation between sample1 and sample2
		// 1. Get out SourceData
		// 2. Get out SpecralSampleData
		// 3. Do interpolation between two SpectralSampleData and create third SpectralSampleData
		// 4. Create SpectralSample from third SpectralSampleData and given SourceData

		std::shared_ptr< SpectralAveraging::CSpectralSample > sample3 = nullptr;
		sample3 = m_SingleMeasurement->Interpolate( t_Angle, sample1, angle1, sample2, angle2 );

		auto aAngular = std::make_shared< CSingleAngularMeasurement >( sample3, t_Angle );
		m_Measurements.push_back( aAngular );
		return aAngular;
	}

	void CAngularMeasurements::setSourceData( std::shared_ptr< CSeries > t_SourceData ) {
		for ( size_t i = 0; i < m_Measurements.size(); i++ ) {
			auto aAngular = m_Measurements[ i ];
			auto aSample = aAngular->getData();
			aSample->setSourceData( t_SourceData );
		}
	}


}
