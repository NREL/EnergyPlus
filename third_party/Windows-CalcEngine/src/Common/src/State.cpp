#include "State.hpp"

namespace FenestrationCommon {

	//////////////////////////////////////////////////////////////////////////
	//      CState
	//////////////////////////////////////////////////////////////////////////

	CState::CState() : m_StateCalculated( false ) {

	}

	CState::CState( CState const & t_State ) : m_StateCalculated( t_State.m_StateCalculated ) {

	}

	CState & CState::operator=( CState const & t_State ) {
		m_StateCalculated = t_State.m_StateCalculated;

		return *this;
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
