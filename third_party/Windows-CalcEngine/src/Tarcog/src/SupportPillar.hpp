#ifndef SUPPORTPILLAR_H
#define SUPPORTPILLAR_H

#include "IGUGapLayer.hpp"

namespace Tarcog {

	class CSupportPillar : public CIGUGapLayer {
	public:
		CSupportPillar( CIGUGapLayer const& t_Layer, double const t_Conductivity );
		CSupportPillar( CSupportPillar const& t_Pillar );

	protected:
		void calculateConvectionOrConductionFlow() override;
		virtual double conductivityOfPillarArray() = 0;
		double m_Conductivity;
	};

	class CCircularPillar : public CSupportPillar {
	public:
		CCircularPillar( CIGUGapLayer const& t_Gap, double const t_Conductivity,
		                 double const t_Spacing, double const t_Radius );
		CCircularPillar( CCircularPillar const& t_Pillar );

	private:
		double conductivityOfPillarArray() override;
		double m_Spacing;
		double m_Radius;

	};

}

#endif
