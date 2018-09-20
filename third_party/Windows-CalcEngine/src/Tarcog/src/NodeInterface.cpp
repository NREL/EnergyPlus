#include "NodeInterface.hpp"
#include "BaseLayer.hpp"



namespace Tarcog {

	//////////////////////////////////////////////////////////////////////////
	//      CLayerNode
	//////////////////////////////////////////////////////////////////////////

	CLayerNode::CLayerNode() {
		nullifyNodes();
	}

	void CLayerNode::tearDownConnections() {
		nullifyNodes();
	}

	void CLayerNode::nullifyNodes() {
		m_PreviousNode = nullptr;
		m_NextNode = nullptr;
	}

	std::shared_ptr< CLayerNode > CLayerNode::getNextNode() const {
		return m_NextNode;
	}

	std::shared_ptr< CLayerNode > CLayerNode::getPreviousNode() const {
		return m_PreviousNode;
	}

	void CLayerNode::connectToBackSide( std::shared_ptr< CLayerNode > const& t_Node ) {
		m_PreviousNode = t_Node->m_NextNode;
	}

	void CLayerNode::connectToFrontSide( std::shared_ptr< CLayerNode > const& t_Node ) {
		m_NextNode = t_Node->m_PreviousNode;
	}

	//////////////////////////////////////////////////////////////////////////
	//      CLayerNodes
	//////////////////////////////////////////////////////////////////////////

	CLayerNodes::CLayerNodes() {
	}

	CLayerNodes::~CLayerNodes() {
	}

	void CLayerNodes::addToFront( std::shared_ptr< CLayerNode > const& t_Node ) {
		auto aNode = m_Layers.front();
		m_Layers.push_front( t_Node );
		if ( aNode != nullptr ) {
			aNode->connectToFrontSide( t_Node );
			t_Node->connectToBackSide( aNode );
		}
	}

	void CLayerNodes::addToBack( std::shared_ptr< CLayerNode > const& t_Node ) {
		auto aNode = m_Layers.front();
		m_Layers.push_back( t_Node );
		if ( aNode != nullptr ) {
			aNode->connectToBackSide( t_Node );
			t_Node->connectToFrontSide( aNode );
		}
	}

	void CLayerNodes::tearDownConnections() {
		for ( auto& layer : m_Layers ) {
			layer->tearDownConnections();
		}
	}

	std::list< std::shared_ptr< CLayerNode > >::iterator CLayerNodes::iterator() {
		return m_Layers.begin();
	}

	//////////////////////////////////////////////////////////////////////////
	//      CThermalNode
	//////////////////////////////////////////////////////////////////////////

	CThermalNode::CThermalNode( std::shared_ptr< CBaseLayer > const& t_HeatFlowLayer ) :
		m_HeatFlowLayer( t_HeatFlowLayer ) {

	}

	std::shared_ptr< CBaseLayer > CThermalNode::getHeatFlowLayer() const {
		return m_HeatFlowLayer;
	}

	void CThermalNode::setHeatFlowLayer( std::shared_ptr< CBaseLayer > const& t_Layer ) {
		m_HeatFlowLayer = t_Layer;
	}

}
