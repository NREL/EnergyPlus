#ifndef BASEIGUTARCOGLAYER_H
#define BASEIGUTARCOGLAYER_H

#include <memory>
#include "BaseLayer.hpp"

namespace FenestrationCommon {

	enum class Side;

}

namespace Tarcog {

	class CBaseIGULayer : public CBaseLayer {
	public:
		explicit CBaseIGULayer( double const t_Thickness );
		CBaseIGULayer( CBaseIGULayer const& t_Layer );
		CBaseIGULayer& operator=( CBaseIGULayer const & t_BaseIGULayer );

		double getThickness() const;
		double getTemperature( FenestrationCommon::Side const t_Position ) const;
		double J( FenestrationCommon::Side const t_Position ) const;
		double getMaxDeflection() const;
		double getMeanDeflection() const;

		double getConductivity();

	protected:
		virtual double layerTemperature();

		double m_Thickness;
	};

}
#endif
