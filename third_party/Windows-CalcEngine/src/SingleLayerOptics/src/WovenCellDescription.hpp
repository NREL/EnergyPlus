#ifndef WOVENCELLDESCRIPTION_H
#define WOVENCELLDESCRIPTION_H

#include "CellDescription.hpp"

namespace SingleLayerOptics {

	class CWovenCellDescription : public ICellDescription {
	public:
		CWovenCellDescription( const double t_Diameter, const double t_Spacing );

		double gamma() const;

		double T_dir_dir( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );
		double R_dir_dir( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );

	private:
		double Tx( const CBeamDirection& t_Direction );
		double Ty( const CBeamDirection& t_Direction );

		double diameter() const;
		double spacing() const;
		double cutOffAngle() const;

		double m_Diameter;
		double m_Spacing;
	};

}

#endif
