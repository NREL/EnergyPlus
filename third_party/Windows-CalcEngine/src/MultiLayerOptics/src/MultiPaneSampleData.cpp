#include <vector>
#include <iterator>
#include <cassert>

#include "MultiPaneSampleData.hpp"
#include "EquivalentLayerSingleComponentMW.hpp"
#include "AbsorptancesMultiPane.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace FenestrationCommon;
using namespace SpectralAveraging;

namespace MultiLayerOptics {

	CMultiPaneSampleData::CMultiPaneSampleData() : CSpectralSampleData() {

	}

	vector< double > CMultiPaneSampleData::getWavelengths() const {
		CCommonWavelengths aWavelengths;

		for ( auto it = m_MeasuredSamples.begin(); it < m_MeasuredSamples.end(); ++it ) {
			aWavelengths.addWavelength( ( *it )->getWavelengths() );
		}

		// wavelengths will be combined into common one and no extrapolation will be done
		return aWavelengths.getCombinedWavelengths( Combine::Interpolate );
	}

	size_t CMultiPaneSampleData::numberOfLayers() const {
		return m_MeasuredSamples.size();
	}

	void CMultiPaneSampleData::addSample( const std::shared_ptr< CSpectralSampleData >& t_Sample ) {
		m_MeasuredSamples.push_back( t_Sample );
	}

	void CMultiPaneSampleData::calculateProperties() {
		if ( !m_absCalculated ) {
			calculateEquivalentProperties();
			m_absCalculated = true;
		}
	}

	std::shared_ptr< CSeries > CMultiPaneSampleData::getLayerAbsorptances( size_t const Index ) {
		calculateProperties();
		if ( ( Index - 1 ) > m_LayerAbsorptances.size() ) {
			throw runtime_error( "Index out of range. " );
		}
		return m_LayerAbsorptances[ Index - 1 ];
	}

	// Interpolate current sample data to new wavelengths set
	void CMultiPaneSampleData::interpolate( const std::vector< double >& t_Wavelengths ) {
		vector< std::shared_ptr< CSpectralSampleData > >::iterator it;
		for ( it = m_MeasuredSamples.begin(); it < m_MeasuredSamples.end(); ++it ) {
			( *it )->interpolate( t_Wavelengths );
		}

		CSpectralSampleData::interpolate( t_Wavelengths );
	}

	void CMultiPaneSampleData::calculateEquivalentProperties() {
		vector< double > wavelengths = getWavelengths();
		interpolate( wavelengths );

		assert( m_MeasuredSamples.size() != 0 );

		std::shared_ptr< CSeries > T = m_MeasuredSamples[ 0 ]->properties( SampleData::T );
		std::shared_ptr< CSeries > Rf = m_MeasuredSamples[ 0 ]->properties( SampleData::Rf );
		std::shared_ptr< CSeries > Rb = m_MeasuredSamples[ 0 ]->properties( SampleData::Rb );
		CEquivalentLayerSingleComponentMW aEqivalentLayer( T, T, Rf, Rb );
		CAbsorptancesMultiPane aAbsorptances( T, Rf, Rb );

		vector< std::shared_ptr< CSpectralSampleData > >::iterator it;
		for ( it = next( m_MeasuredSamples.begin() ); it < m_MeasuredSamples.end(); ++it ) {
			aEqivalentLayer.addLayer( ( *it )->properties( SampleData::T ), ( *it )->properties( SampleData::T ),
			                          ( *it )->properties( SampleData::Rf ), ( *it )->properties( SampleData::Rb ) );
			aAbsorptances.addLayer( ( *it )->properties( SampleData::T ),
			                        ( *it )->properties( SampleData::Rf ), ( *it )->properties( SampleData::Rb ) );
		}

		m_Transmittances = aEqivalentLayer.getProperties( Property::T, Side::Front );
		m_ReflectancesFront = aEqivalentLayer.getProperties( Property::R, Side::Front );
		m_ReflectancesBack = aEqivalentLayer.getProperties( Property::R, Side::Back );
		m_AbsorptancesFront = aEqivalentLayer.getProperties( Property::Abs, Side::Front );
		m_AbsorptancesBack = aEqivalentLayer.getProperties( Property::Abs, Side::Back );

		m_LayerAbsorptances.clear();
		size_t size = aAbsorptances.numOfLayers();
		for ( size_t i = 0; i < size; ++i ) {
			m_LayerAbsorptances.push_back( aAbsorptances.Abs( i ) );
		}

	}

}
