#include <cassert>
#include <algorithm>
#include <math.h>

#include "MultiPaneSpecular.hpp"
#include "AbsorptancesMultiPane.hpp"
#include "EquivalentLayerSingleComponentMW.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace FenestrationCommon;
using namespace SingleLayerOptics;

namespace MultiLayerOptics {

	////////////////////////////////////////////////////////////////////////////////////////////
	//  CEquivalentLayerSingleComponentMWAngle
	////////////////////////////////////////////////////////////////////////////////////////////
	CEquivalentLayerSingleComponentMWAngle::CEquivalentLayerSingleComponentMWAngle(
		const std::shared_ptr< CEquivalentLayerSingleComponentMW >& t_Layer,
		const std::shared_ptr< CAbsorptancesMultiPane >& t_Abs, const double t_Angle ) :
		m_Layer( t_Layer ), m_Abs( t_Abs ), m_Angle( t_Angle ) {

	}

	double CEquivalentLayerSingleComponentMWAngle::angle() const {
		return m_Angle;
	}

	std::shared_ptr< CEquivalentLayerSingleComponentMW > CEquivalentLayerSingleComponentMWAngle::layer() const {
		return m_Layer;
	}

	std::shared_ptr< CSeries > CEquivalentLayerSingleComponentMWAngle::getProperties( const Side t_Side, const Property t_Property ) {
		return m_Layer->getProperties( t_Property, t_Side );
	}

	std::shared_ptr< CSeries > CEquivalentLayerSingleComponentMWAngle::Abs( size_t const Index ) {
		return m_Abs->Abs( Index );
	}

	////////////////////////////////////////////////////////////////////////////////////////////
	//  CMultiPaneSpecular
	////////////////////////////////////////////////////////////////////////////////////////////
	CMultiPaneSpecular::CMultiPaneSpecular( std::vector< double > const& t_CommonWavelength,
	                                        const std::shared_ptr< CSeries >& t_SolarRadiation, const std::shared_ptr< CSpecularCell >& t_Layer ) :
		m_CommonWavelengths( t_CommonWavelength ), m_SolarRadiation( t_SolarRadiation ) {
		m_SolarRadiation = m_SolarRadiation->interpolate( m_CommonWavelengths );
		addLayer( t_Layer );
	}

	void CMultiPaneSpecular::addLayer( const std::shared_ptr< CSpecularCell >& t_Layer ) {
		t_Layer->setSourceData( m_SolarRadiation );
		m_Layers.push_back( t_Layer );
	}

	double CMultiPaneSpecular::getProperty( const Side t_Side, const Property t_Property, const double t_Angle,
	                                        const double minLambda, const double maxLambda, const IntegrationType t_IntegrationType ) {

		CEquivalentLayerSingleComponentMWAngle& aAngularProperties = *getAngular( t_Angle );

		auto aProperties = aAngularProperties.getProperties( t_Side, t_Property );

		auto aMult = aProperties->mMult( *m_SolarRadiation );

		auto iIntegrated = aMult->integrate( t_IntegrationType );
		
		double totalProperty = iIntegrated->sum( minLambda, maxLambda );
		double totalSolar = m_SolarRadiation->integrate( t_IntegrationType )->sum( minLambda, maxLambda );

		assert( totalSolar > 0 );

		return totalProperty / totalSolar;
	}

	double CMultiPaneSpecular::getHemisphericalProperty( const Side t_Side, const Property t_Property,
	                                                     const std::shared_ptr< const std::vector< double > >& t_Angles,
	                                                     const double minLambda, const double maxLambda, const IntegrationType t_IntegrationType ) {
		size_t size = t_Angles->size();
		std::shared_ptr< CSeries > aAngularProperties = make_shared< CSeries >();
		for ( size_t i = 0; i < size; ++i ) {
			double angle = ( *t_Angles )[ i ];
			double aProperty = getProperty( t_Side, t_Property, angle, minLambda, maxLambda, t_IntegrationType );
			aAngularProperties->addProperty( angle, aProperty );
		}
		CHemispherical2DIntegrator aIntegrator = CHemispherical2DIntegrator( *aAngularProperties, t_IntegrationType );
		return aIntegrator.value();
	}

	double CMultiPaneSpecular::Abs( size_t const Index, const double t_Angle,
	                                const double minLambda, const double maxLambda, const IntegrationType t_IntegrationType ) {
		CEquivalentLayerSingleComponentMWAngle& aAngularProperties = *getAngular( t_Angle );
		auto aProperties = aAngularProperties.Abs( Index - 1 );

		auto aMult = aProperties->mMult( *m_SolarRadiation );

		auto iIntegrated = aMult->integrate( t_IntegrationType );

		double totalProperty = iIntegrated->sum( minLambda, maxLambda );
		double totalSolar = m_SolarRadiation->integrate( t_IntegrationType )->sum( minLambda, maxLambda );

		assert( totalSolar > 0 );

		return totalProperty / totalSolar;
	}

	double CMultiPaneSpecular::AbsHemispherical( size_t const Index,
	                                             const std::shared_ptr< const std::vector< double > >& t_Angles, const double minLambda, const double maxLambda,
	                                             const IntegrationType t_IntegrationType ) {
		size_t size = t_Angles->size();
		std::shared_ptr< CSeries > aAngularProperties = make_shared< CSeries >();
		for ( size_t i = 0; i < size; ++i ) {
			double angle = ( *t_Angles )[ i ];
			double aAbs = Abs( Index, angle, minLambda, maxLambda, t_IntegrationType );
			aAngularProperties->addProperty( angle, aAbs );
		}

		CHemispherical2DIntegrator aIntegrator = CHemispherical2DIntegrator( *aAngularProperties, t_IntegrationType );
		return aIntegrator.value();
	}

	std::shared_ptr< CEquivalentLayerSingleComponentMWAngle > CMultiPaneSpecular::getAngular( const double t_Angle ) {
		std::shared_ptr< CEquivalentLayerSingleComponentMWAngle > aAngularProperties = nullptr;

		vector< std::shared_ptr< CEquivalentLayerSingleComponentMWAngle > >::iterator it;
		it = find_if( m_EquivalentAngle.begin(), m_EquivalentAngle.end(),
		              [ &t_Angle ]( const std::shared_ptr< CEquivalentLayerSingleComponentMWAngle >& obj ) {
		              return fabs( obj->angle() - t_Angle ) < 1e-6;
	              } );

		if ( it != m_EquivalentAngle.end() ) {
			aAngularProperties = ( *it );
		}
		else {
			aAngularProperties = createNewAngular( t_Angle );
		}

		return aAngularProperties;
	}

	std::shared_ptr< CEquivalentLayerSingleComponentMWAngle > CMultiPaneSpecular::createNewAngular( const double t_Angle ) {
		// Create direction for specular. It is irrelevant what is Phi angle and it is chosen to be zero in this case
		CBeamDirection aDirection = CBeamDirection( t_Angle, 0 );
		std::shared_ptr< CEquivalentLayerSingleComponentMW > aEqLayer = nullptr;
		std::shared_ptr< CAbsorptancesMultiPane > aAbs = nullptr;
		for ( size_t i = 0; i < m_Layers.size(); ++i ) {
			vector< double > wl = m_Layers[ i ]->getBandWavelengths();
			vector< double > Tv = m_Layers[ i ]->T_dir_dir_band( Side::Front, aDirection );
			vector< double > Rfv = m_Layers[ i ]->R_dir_dir_band( Side::Front, aDirection );
			vector< double > Rbv = m_Layers[ i ]->R_dir_dir_band( Side::Back, aDirection );
			std::shared_ptr< CSeries > T = make_shared< CSeries >();
			std::shared_ptr< CSeries > Rf = make_shared< CSeries >();
			std::shared_ptr< CSeries > Rb = make_shared< CSeries >();
			for ( size_t j = 0; j < wl.size(); ++j ) {
				T->addProperty( wl[ j ], Tv[ j ] );
				Rf->addProperty( wl[ j ], Rfv[ j ] );
				Rb->addProperty( wl[ j ], Rbv[ j ] );
			}
			T = T->interpolate( m_CommonWavelengths );
			Rf = Rf->interpolate( m_CommonWavelengths );
			Rb = Rb->interpolate( m_CommonWavelengths );
			if ( i == 0 ) {
				aEqLayer = make_shared< CEquivalentLayerSingleComponentMW >( T, T, Rf, Rb );
				aAbs = make_shared< CAbsorptancesMultiPane >( T, Rf, Rb );
			}
			else {
				assert( aEqLayer != nullptr );
				assert( aAbs != nullptr );
				aEqLayer->addLayer( T, T, Rf, Rb );
				aAbs->addLayer( T, Rf, Rb );
			}
		}
		assert( aEqLayer != nullptr );
		assert( aAbs != nullptr );

		std::shared_ptr< CEquivalentLayerSingleComponentMWAngle > newLayer = make_shared< CEquivalentLayerSingleComponentMWAngle >( aEqLayer,
		                                                                                                                       aAbs, t_Angle );

		m_EquivalentAngle.push_back( newLayer );

		return newLayer;

	}


}
