#ifndef FENESTRATIONCOMMONSTATE_H
#define FENESTRATIONCOMMONSTATE_H

namespace FenestrationCommon {

	// class CState is used to keep validity of object state. In some cases calculations do not need
	// to be performed before results is requested.
	class CState {
	public:
		virtual ~CState() = default;
		CState();
		CState( const CState& t_State );
		virtual void resetCalculated() final; // to reset state to non-calculated
		virtual void setCalculated() final; // calculations are up to date and set state to valid state
		virtual bool isCalculated() final; // check if state have valid results

	protected:
		// some intermediate state variables need to pick up new input parameters.
		virtual void initializeStateVariables();

	private:
		bool m_StateCalculated;

	};

}

#endif
