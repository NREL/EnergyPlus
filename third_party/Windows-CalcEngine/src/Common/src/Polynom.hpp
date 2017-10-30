#ifndef WINDOWS_CALCENGINE_POLYNOM_HPP
#define WINDOWS_CALCENGINE_POLYNOM_HPP

namespace FenestrationCommon {

	// Evaluate polynom value at certain point
	class Polynom {
	public:
		explicit Polynom( std::vector< double > const& t_Coeffs );

		double valueAt( double const t_x ) const;

	private:
		std::vector< double > m_Coeffs;

	};

	// Keeps polynom tied to certain value point. For example, certain polynomial interpolation is
	// only valid for incoming angle Phi = 10 deg.
	class PolynomPoint {
	public:
		explicit PolynomPoint( double const Value, Polynom const & t_Poly );
		double value() const;
		double valueAt( double const t_X ) const;

	private:
		Polynom m_Polynom;
		double m_Value;

	};

	// Performs interpolation over polynomials for input range between 0 and 360 deg
	class PolynomialPoints360deg {
	public:
		PolynomialPoints360deg();

		void storePoint( double const t_Value, Polynom const& t_Polynom );
		// Function to return point closest to given coordinate
		double valueAt( double const t_PointValue, double const t_Value );

	private:
		void sortPolynomials();

		bool isSorted;
		std::vector< PolynomPoint > m_Polynoms;
	};

}

#endif //WINDOWS_CALCENGINE_POLYNOM_HPP
