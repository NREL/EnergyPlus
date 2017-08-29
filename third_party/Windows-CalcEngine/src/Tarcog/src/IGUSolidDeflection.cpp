#define _USE_MATH_DEFINES
#include <math.h>

#include "IGUSolidDeflection.hpp"
#include "WCECommon.hpp"
#include "Surface.hpp"
#include "TarcogConstants.hpp"


using namespace FenestrationCommon;

namespace Tarcog {

	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//    CIGUSolidLayerDeflection
	////////////////////////////////////////////////////////////////////////////////////////////////////////////

	CIGUSolidLayerDeflection::CIGUSolidLayerDeflection( CIGUSolidLayer const& t_SolidLayer ) :
		CIGUSolidLayer( t_SolidLayer ), m_YoungsModulus( DeflectionConstants::YOUNGSMODULUS ),
		m_PoisonRatio( DeflectionConstants::POISONRATIO ) {
	}

	CIGUSolidLayerDeflection::CIGUSolidLayerDeflection( CIGUSolidLayer const& t_SolidLayer,
	                                                    double const t_YoungsModulus, double const t_PoisonRatio ) :
		CIGUSolidLayer( t_SolidLayer ),
		m_YoungsModulus( t_YoungsModulus ), m_PoisonRatio( t_PoisonRatio ) {
	}

	CIGUSolidLayerDeflection::CIGUSolidLayerDeflection(
		CIGUSolidLayerDeflection const& t_Layer ) : CState( t_Layer ),
		                                            CIGUSolidLayer( t_Layer ) {
		m_YoungsModulus = t_Layer.m_YoungsModulus;
		m_PoisonRatio = t_Layer.m_PoisonRatio;
	}

	void CIGUSolidLayerDeflection::calculateConvectionOrConductionFlow() {
		CIGUSolidLayer::calculateConvectionOrConductionFlow();

	}

	double CIGUSolidLayerDeflection::flexuralRigidity() const {
		return m_YoungsModulus * pow( m_Thickness, 3 ) / ( 12 * ( 1 - pow( m_PoisonRatio, 2 ) ) );
	}

	std::shared_ptr< CBaseLayer > CIGUSolidLayerDeflection::clone() const {
		return std::make_shared< CIGUSolidLayer >( *this );
	}

	double CIGUSolidLayerDeflection::pressureDifference() const {
		auto P1 = std::dynamic_pointer_cast< CGasLayer >( m_NextLayer )->getPressure();
		auto P2 = std::dynamic_pointer_cast< CGasLayer >( m_PreviousLayer )->getPressure();
		return P1 - P2;
	}

	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//    CIGUDeflectionTempAndPressure
	////////////////////////////////////////////////////////////////////////////////////////////////////////////

	CIGUDeflectionTempAndPressure::CIGUDeflectionTempAndPressure(
		std::shared_ptr< CIGUSolidLayerDeflection > const& t_SolidLayer,
		double const t_MaxDeflectionCoeff, double const t_MeanDeflectionCoeff ) :
		CIGUSolidLayerDeflection( *t_SolidLayer ),
		m_MaxCoeff( t_MaxDeflectionCoeff ), m_MeanCoeff( t_MeanDeflectionCoeff ) {

	}

	void CIGUDeflectionTempAndPressure::calculateConvectionOrConductionFlow() {
		CIGUSolidLayerDeflection::calculateConvectionOrConductionFlow();
		// Relaxation parameter is low because that will make possible solution to converge.
		// Instability in rest of equation is great if using higher relaxation parameter and 
		// it probaby does not matter what solver is used.
		auto const RelaxationParamter = 0.005;

		auto Dp = pressureDifference();
		auto D = flexuralRigidity();
		auto Ld = m_Surface[ Side::Front ]->getMeanDeflection();
		Ld += LdMean( Dp, D ) * RelaxationParamter;
		auto Ldmax = m_Surface[ Side::Front ]->getMaxDeflection();
		Ldmax += LdMax( Dp, D ) * RelaxationParamter;
		for ( auto aSide : EnumSide() ) {
			m_Surface[ aSide ]->applyDeflection( Ld, Ldmax );
		}
	}

	double CIGUDeflectionTempAndPressure::LdMean( double const t_P, double const t_D ) const {
		return m_MeanCoeff * t_P / t_D;
	}

	double CIGUDeflectionTempAndPressure::LdMax( double const t_P, double const t_D ) const {
		return m_MaxCoeff * t_P / t_D;
	}

	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//    CIGUDeflectionTempAndPressure
	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	CIGUDeflectionMeasuread::CIGUDeflectionMeasuread( std::shared_ptr< CIGUSolidLayerDeflection >& t_Layer,
	                                                  const double t_MeanDeflection, const double t_MaxDeflection ) :
		CIGUSolidLayerDeflection( *t_Layer ) {
		for ( Side aSide : EnumSide() ) {
			m_Surface[ aSide ]->applyDeflection( t_MeanDeflection, t_MaxDeflection );
		}
	}

}
