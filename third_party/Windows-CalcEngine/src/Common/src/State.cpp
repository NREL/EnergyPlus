#include "State.hpp"

namespace FenestrationCommon {

	//////////////////////////////////////////////////////////////////////////
	//      CState
	//////////////////////////////////////////////////////////////////////////

	CState::CState() : m_StateCalculated( false ) {

	}

	CState::CState( const CState& t_State ) {
		m_StateCalculated = t_State.m_StateCalculated;
	}

	void CState::resetCalculated() {
		m_StateCalculated = false;
		initializeStateVariables();
	}

	void CState::setCalculated() {
		m_StateCalculated = true;
	}

	bool CState::isCalculated() {
		return m_StateCalculated;
	}

	void CState::initializeStateVariables() {
		// Action will be needed in inherited classes (where calculation is performed).
	}

}
