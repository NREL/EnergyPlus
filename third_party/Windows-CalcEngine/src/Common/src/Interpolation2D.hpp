#ifndef INTERPOLATION2D_H
#define INTERPOLATION2D_H

#include <vector>

namespace FenestrationCommon {

	//////////////////////////////////////////////////////////////////////////////////////
	// IInterpolation2D
	//////////////////////////////////////////////////////////////////////////////////////

	// Interface for 2D interpolation curve
	class IInterpolation2D {
	public:
		explicit IInterpolation2D( std::vector< std::pair< double, double > > const& t_Points );

		virtual double getValue( double const t_Value ) const = 0;

	protected:
		std::vector< std::pair< double, double > > m_Points;
	};

	//////////////////////////////////////////////////////////////////////////////////////
	// CSPChipInterpolation2D
	//////////////////////////////////////////////////////////////////////////////////////

	class CSPChipInterpolation2D : public IInterpolation2D {
	public:
		explicit CSPChipInterpolation2D( std::vector< std::pair< double, double > > const& t_Points );

		double getValue( double const t_Value ) const;

	private:
		std::size_t getSubinterval( double const t_Value ) const;
		std::vector< double > calculateHs() const;
		std::vector< double > calculateDeltas() const;
		std::vector< double > calculateDerivatives() const;
		static double piecewiseCubicDerivative( double const delta_k, double const delta_k_minus_1, double const hk,
		                                        double const hk_minus_1 );
		double interpolate( double const h, double const s, double const y_k, double const y_k_plus_one,
		                    double const d_k, double const d_k_plus_one ) const;

		std::vector< double > m_Hs;
		std::vector< double > m_Deltas;
		std::vector< double > m_Derivatives;
	};

}

#endif
