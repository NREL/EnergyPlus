#ifndef TARIGUVENTILATEDGAPLAYER_H
#define TARIGUVENTILATEDGAPLAYER_H

#include <memory>

#include "IGUGapLayer.hpp"

namespace Gasses {

	class CGas;

}

namespace Tarcog {

	class CIGUVentilatedGapLayer : public CIGUGapLayer {
	public:
		explicit CIGUVentilatedGapLayer( std::shared_ptr< CIGUGapLayer > const& t_Layer );
		CIGUVentilatedGapLayer( CIGUVentilatedGapLayer const& t_Layer );

		virtual double layerTemperature();

		void setFlowGeometry( double const t_Atop, double const t_Abot, AirVerticalDirection const& t_Direction );
		void setFlowTemperatures( double const t_topTemp, double const t_botTemp,
		                          AirVerticalDirection const& t_Direction );
		void setFlowSpeed( double const t_speed );

		double getAirflowReferencePoint( double const t_GapTemperature );

		double bernoullyPressureTerm() const;
		double hagenPressureTerm() const;
		double pressureLossTerm() const;
		double betaCoeff();

		void smoothEnergyGain( double const qv1, double const qv2 );

	private:
		virtual void calculateConvectionOrConductionFlow();
		double characteristicHeight() const;
		double calcImpedance( double const t_A ) const;
		void ventilatedFlow();

		std::shared_ptr< CIGUGapLayer > m_Layer;
		std::shared_ptr< Gases::CGas > m_ReferenceGas;

		double m_inTemperature;
		double m_outTemperature;
		double m_Zin;
		double m_Zout;

	};

}

#endif
