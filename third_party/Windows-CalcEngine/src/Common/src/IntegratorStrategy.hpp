#ifndef INTEGRATIONSTRATEGY_H
#define INTEGRATIONSTRATEGY_H

#include <memory>

namespace FenestrationCommon {

	enum class IntegrationType { Rectangular, Trapezoidal };

	class IIntegratorStrategy {
	public:
		virtual double integrate( double const x1, double const x2, double const y1, double const y2 ) = 0;
	protected:
		double dX( double const x1, double const x2 ) const;
	};

	class CIntegratorRectangular : public IIntegratorStrategy {
	public:
		double integrate( double const x1, double const x2, double const y1, double const y2 );
	};

	class CIntegratorTrapezoidal : public IIntegratorStrategy {
	public:
		double integrate( double const x1, double const x2, double const y1, double const y2 );
	};

	class CIntegratorFactory {
	public:
		std::shared_ptr< IIntegratorStrategy > getIntegrator( IntegrationType t_IntegratorType );
	};

}

#endif
