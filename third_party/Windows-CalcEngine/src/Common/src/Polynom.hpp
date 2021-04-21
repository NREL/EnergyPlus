#ifndef WINDOWS_CALCENGINE_POLYNOM_HPP
#define WINDOWS_CALCENGINE_POLYNOM_HPP

#include <vector>

namespace FenestrationCommon {

	// Evaluate polynom value at certain point
	class Polynom {
	public:
		explicit Polynom( const std::vector< double > & t_Coeffs );

		double valueAt( double t_x ) const;

	private:
		std::vector< double > m_Coeffs;

	};

	// Keeps polynom tied to certain value point. For example, certain polynomial interpolation is
	// only valid for incoming angle Phi = 10 deg.
	class PolynomPoint {
	public:
		explicit PolynomPoint( double Value, Polynom const & t_Poly );
		double value() const;
		double valueAt( double t_X ) const;

	private:
		Polynom m_Polynom;
		double m_Value;

	};

	// Performs interpolation over polynomials for input range between 0 and 360 deg
	class PolynomialPoints360deg {
	public:
		PolynomialPoints360deg();

		void storePoint( double t_Value, const Polynom & t_Polynom );
		// Function to return point closest to given coordinate
		double valueAt( double t_PointValue, double t_Value );

	private:
		void sortPolynomials();

		bool isSorted;
		std::vector< PolynomPoint > m_Polynoms;
	};

}

#endif //WINDOWS_CALCENGINE_POLYNOM_HPP
