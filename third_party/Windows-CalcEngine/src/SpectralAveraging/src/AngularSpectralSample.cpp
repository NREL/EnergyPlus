#include <vector>
#include <algorithm>
#include <cassert>
#include <math.h>

#include "AngularSpectralSample.hpp"
#include "MeasuredSampleData.hpp"
#include "SpectralSample.hpp"
#include "AngularProperties.hpp"
#include "WCECommon.hpp"


using namespace FenestrationCommon;

namespace SpectralAveraging {

	////////////////////////////////////////////////////////////////////////////////////////////////////
	//// CAngularSpectralProperties
	////////////////////////////////////////////////////////////////////////////////////////////////////

	CAngularSpectralProperties::CAngularSpectralProperties( std::shared_ptr< CSpectralSample > const& t_SpectralSample,
	                                                        double const t_Angle, MaterialType const t_Type, double const t_Thickness ) :
		m_Angle( t_Angle ), m_Thickness( t_Thickness ) {

		m_AngularData = std::make_shared< CSpectralSampleData >();
		calculateAngularProperties( t_SpectralSample, t_Type );
	}

	double CAngularSpectralProperties::angle() const {
		return m_Angle;
	}

	std::shared_ptr< CSpectralSampleData > CAngularSpectralProperties::properties() const {
		return m_AngularData;
	}

	void CAngularSpectralProperties::calculateAngularProperties( std::shared_ptr< CSpectralSample > const& t_SpectralSample,
	                                                             MaterialType const t_Type ) {

		assert( t_SpectralSample != nullptr );

		auto aMeasuredData = t_SpectralSample->getMeasuredData();

		if ( m_Angle != 0 ) {
			auto aSourceData = t_SpectralSample->getSourceData();

			auto aWavelengths = aMeasuredData->getWavelengths();
			auto aT = aMeasuredData->properties( SampleData::T );
			assert( aT->size() == aWavelengths.size() );

			auto aRf = aMeasuredData->properties( SampleData::Rf );
			assert( aRf->size() == aWavelengths.size() );

			auto aRb = aMeasuredData->properties( SampleData::Rb );
			assert( aRb->size() == aWavelengths.size() );

			auto lowLambda = 0.3;
			auto highLambda = 2.5;

			// TODO: Only one side is measured and it is considered that front properties are equal to back properties
			auto aTSolNorm = t_SpectralSample->getProperty( lowLambda, highLambda, Property::T, Side::Front );

			for ( size_t i = 0; i < aWavelengths.size(); ++i ) {
				auto ww = aWavelengths[ i ] * 1e-6;
				auto T = ( *aT )[ i ].value();
				auto Rf = ( *aRf )[ i ].value();
				auto Rb = ( *aRb )[ i ].value();

				auto aSurfaceType = coatingType.at( t_Type );

				auto aFrontFactory = CAngularPropertiesFactory( T, Rf, m_Thickness, aTSolNorm );
				auto aBackFactory = CAngularPropertiesFactory( T, Rb, m_Thickness, aTSolNorm );

				auto aFrontProperties = aFrontFactory.getAngularProperties( aSurfaceType );
				auto aBackProperties = aBackFactory.getAngularProperties( aSurfaceType );

				auto Tangle = aFrontProperties->transmittance( m_Angle, ww );
				auto Rfangle = aFrontProperties->reflectance( m_Angle, ww );
				auto Rbangle = aBackProperties->reflectance( m_Angle, ww );

				m_AngularData->addRecord( ww * 1e6, Tangle, Rfangle, Rbangle );
			}
		}
		else {
			m_AngularData = aMeasuredData;
		}

	}

	////////////////////////////////////////////////////////////////////////////////////////////////////
	//// CSpectralSampleAngle
	////////////////////////////////////////////////////////////////////////////////////////////////////

	CSpectralSampleAngle::CSpectralSampleAngle( std::shared_ptr< CSpectralSample > const& t_Sample,
	                                            double const t_Angle ) :
		m_Sample( t_Sample ), m_Angle( t_Angle ) {

	}

	double CSpectralSampleAngle::angle() const {
		return m_Angle;
	}

	std::shared_ptr< CSpectralSample > CSpectralSampleAngle::sample() const {
		return m_Sample;
	}

	////////////////////////////////////////////////////////////////////////////////////////////////////
	//// CAngularSpectralSample
	////////////////////////////////////////////////////////////////////////////////////////////////////

	CAngularSpectralSample::CAngularSpectralSample( std::shared_ptr< CSpectralSample > const& t_SpectralSample,
	                                                double const t_Thickness, FenestrationCommon::MaterialType const t_Type ) :
		m_SpectralSampleZero( t_SpectralSample ), m_Thickness( t_Thickness ), m_Type( t_Type ) {

	}

	void CAngularSpectralSample::setSourceData( std::shared_ptr< CSeries > t_SourceData ) {
		m_SpectralSampleZero->setSourceData( t_SourceData );
		m_SpectralProperties.clear();
	}

	double CAngularSpectralSample::getProperty( double const minLambda, double const maxLambda,
	                                            Property const t_Property, Side const t_Side, double const t_Angle ) {
		auto aSample = findSpectralSample( t_Angle );
		return aSample->getProperty( minLambda, maxLambda, t_Property, t_Side );
	}

	std::vector< double > CAngularSpectralSample::getWavelengthsProperty(
		double const minLambda, double const maxLambda,
		Property const t_Property, Side const t_Side, double const t_Angle ) {

		auto aSample = findSpectralSample( t_Angle );

		auto aProperties = aSample->getWavelengthsProperty( t_Property, t_Side );

		std::vector< double > aValues;

		if ( aProperties != nullptr ) {

			for ( auto& aProperty : *aProperties ) {
				if ( aProperty->x() >= minLambda && aProperty->x() <= maxLambda ) {
					aValues.push_back( aProperty->value() );
				}
			}

		}

		return aValues;
	}

	std::vector< double > CAngularSpectralSample::getBandWavelengths() const {
		return m_SpectralSampleZero->getWavelengthsFromSample();
	}

	std::shared_ptr< CSpectralSample > CAngularSpectralSample::findSpectralSample( double const t_Angle ) {
		std::shared_ptr< CSpectralSample > aSample = nullptr;

		std::vector< std::shared_ptr< CSpectralSampleAngle > >::iterator it = find_if( m_SpectralProperties.begin(), m_SpectralProperties.end(),
		                                                                     [ &t_Angle ]( std::shared_ptr< CSpectralSampleAngle > const& obj ) {
		                                                                     return fabs( obj->angle() - t_Angle ) < 1e-6;
	                                                                     } );

		if ( it != m_SpectralProperties.end() ) {
			aSample = ( *it )->sample();
		}
		else {
			auto aAngularData = CAngularSpectralProperties( m_SpectralSampleZero, t_Angle, m_Type, m_Thickness );

			aSample = std::make_shared< CSpectralSample >( aAngularData.properties(), m_SpectralSampleZero->getSourceData() );
			aSample->assignDetectorAndWavelengths( m_SpectralSampleZero );
			auto aSpectralSampleAngle = std::make_shared< CSpectralSampleAngle >( aSample, t_Angle );
			m_SpectralProperties.push_back( aSpectralSampleAngle );
		}

		return aSample;
	}

}
