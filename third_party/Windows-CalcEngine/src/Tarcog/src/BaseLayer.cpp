#include "BaseLayer.hpp"


namespace Tarcog {

	CBaseLayer::CBaseLayer() : CState(), CLayerGeometry(), CLayerHeatFlow(),
	                           m_PreviousLayer( nullptr ), m_NextLayer( nullptr ) {

	}

	// CBaseLayer::CBaseLayer( CBaseLayer const& t_Layer ) :
	// 	CState( t_Layer ), CLayerGeometry( t_Layer ), CLayerHeatFlow( t_Layer ) {
	// 
	// }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wextra"

	CBaseLayer::CBaseLayer( CBaseLayer const& t_Layer ) : CState( t_Layer ),
														  CLayerGeometry( t_Layer ), CLayerHeatFlow( t_Layer ) {

	}

#pragma GCC diagnostic pop

	CBaseLayer & CBaseLayer::operator=( CBaseLayer const & t_BaseLayer ) {
		this->CState::operator=( t_BaseLayer );
		this->CLayerGeometry::operator=( t_BaseLayer );
		this->CLayerHeatFlow::operator=( t_BaseLayer );

		return *this;
	}

	std::shared_ptr< CBaseLayer > CBaseLayer::getPreviousLayer() const {
		return m_PreviousLayer;
	}

	std::shared_ptr< CBaseLayer > CBaseLayer::getNextLayer() const {
		return m_NextLayer;
	}

	void CBaseLayer::tearDownConnections() {
		m_PreviousLayer = nullptr;
		m_NextLayer = nullptr;
	}

	void CBaseLayer::connectToBackSide( std::shared_ptr< CBaseLayer > const& t_Layer ) {
		m_NextLayer = t_Layer;
		t_Layer->m_PreviousLayer = shared_from_this();
	}

	void CBaseLayer::calculateRadiationFlow() {

	}

}
