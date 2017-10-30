#ifndef TARIGUSOLIDLAYERDEFLECTION_H
#define TARIGUSOLIDLAYERDEFLECTION_H

#include <memory>

#include "IGUSolidLayer.hpp"

namespace Tarcog {

	////////////////////////////////////////////////////////////////////////////
	////    CIGUSolidLayerDeflection
	////////////////////////////////////////////////////////////////////////////
	class CIGUSolidLayerDeflection : public CIGUSolidLayer {
	public:
		explicit CIGUSolidLayerDeflection( CIGUSolidLayer const& t_SolidLayer );
		CIGUSolidLayerDeflection( CIGUSolidLayer const& t_SolidLayer,
		                          const double t_YoungsModulus, const double t_PoisonRatio );

		CIGUSolidLayerDeflection( CIGUSolidLayerDeflection const& t_Layer );
		CIGUSolidLayerDeflection & operator=( CIGUSolidLayerDeflection const & t_Layer );

		double flexuralRigidity() const;

		std::shared_ptr< CBaseLayer > clone() const override;

	protected:
		void calculateConvectionOrConductionFlow() override;
		double pressureDifference() const;

	private:
		double m_YoungsModulus;
		double m_PoisonRatio;
	};

	////////////////////////////////////////////////////////////////////////////
	////    CIGUDeflectionTempAndPressure
	////////////////////////////////////////////////////////////////////////////
	class CIGUDeflectionTempAndPressure : public CIGUSolidLayerDeflection {
	public:
		CIGUDeflectionTempAndPressure( std::shared_ptr< CIGUSolidLayerDeflection > const& t_SolidLayer,
		                               double const t_MaxDeflectionCoeff, double const t_MinDeflectionCoeff );

	protected:
		void calculateConvectionOrConductionFlow() override;

	private:
		double LdMean( double const t_P, double const t_D ) const;
		double LdMax( double const t_P, double const t_D ) const;

		double m_MaxCoeff;
		double m_MeanCoeff;
	};

	////////////////////////////////////////////////////////////////////////////
	////    CIGUDeflectionMeasuread
	////////////////////////////////////////////////////////////////////////////
	class CIGUDeflectionMeasuread : public CIGUSolidLayerDeflection {
	public:
		CIGUDeflectionMeasuread( std::shared_ptr< CIGUSolidLayerDeflection >& t_Layer,
		                         const double t_MeanDeflection, const double t_MaxDeflection );
	};
}

#endif
