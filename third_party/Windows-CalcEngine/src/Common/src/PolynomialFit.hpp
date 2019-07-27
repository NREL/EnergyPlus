#ifndef POLYNOMIALFIT_CPOLYFIT_HPP
#define POLYNOMIALFIT_CPOLYFIT_HPP

#include <vector>

namespace FenestrationCommon {

	class PolynomialFit {
	public:
		explicit PolynomialFit( std::size_t const t_Order );

		// Get polynomial fit for given coefficients
		std::vector< double > getCoefficients( std::vector< std::pair< double, double > > t_Table ) const;

	private:
		std::size_t m_Order;
	};

}


#endif //POLYNOMIALFIT_CPOLYFIT_HPP
