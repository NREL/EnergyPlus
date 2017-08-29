#define _USE_MATH_DEFINES
#include <math.h>
#include <memory>

#include "WCETarcog.hpp"
#include "BaseIGULayer.hpp"



namespace Tarcog {


	////////////////////////////////////////////////////////////////////////////
	////  CSupportPillar
	////////////////////////////////////////////////////////////////////////////
	CSupportPillar::CSupportPillar( CIGUGapLayer const& t_Layer, double const t_Conductivity ) :
		CIGUGapLayer( t_Layer ), m_Conductivity( t_Conductivity ) {

	}

	CSupportPillar::CSupportPillar( CSupportPillar const& t_Pillar ) :
		CState( t_Pillar ), CIGUGapLayer( t_Pillar ), m_Conductivity( t_Pillar.m_Conductivity ) {

	}

	void CSupportPillar::calculateConvectionOrConductionFlow() {
		CIGUGapLayer::calculateConvectionOrConductionFlow();
		if ( !isCalculated() ) {
			m_ConductiveConvectiveCoeff += conductivityOfPillarArray();
		}
	}

	////////////////////////////////////////////////////////////////////////////
	////  CCircularPillar
	////////////////////////////////////////////////////////////////////////////
	CCircularPillar::CCircularPillar( CIGUGapLayer const& t_Gap,
	                                  double const t_Conductivity, double const t_Spacing, double const t_Radius ) :
		CSupportPillar( t_Gap, t_Conductivity ), m_Spacing( t_Spacing ), m_Radius( t_Radius ) {

	}

	CCircularPillar::CCircularPillar( CCircularPillar const& t_Pillar ) :
		CState( t_Pillar ), CSupportPillar( t_Pillar ) {

	}

	double CCircularPillar::conductivityOfPillarArray() {
		auto cond1 = std::dynamic_pointer_cast< CBaseIGULayer >( m_PreviousLayer )->getConductivity();
		auto cond2 = std::dynamic_pointer_cast< CBaseIGULayer >( m_NextLayer )->getConductivity();
		auto aveCond = ( cond1 + cond2 ) / 2;

		auto cond = 2 * aveCond * m_Radius / ( pow( m_Spacing, 2 ) );
		cond *= 1 / ( 1 + 2 * m_Thickness * aveCond / ( m_Conductivity * M_PI * m_Radius ) );

		return cond;
	}

}
