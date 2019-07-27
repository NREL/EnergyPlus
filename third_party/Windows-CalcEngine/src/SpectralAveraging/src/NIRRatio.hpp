#ifndef NIRRATIO_H
#define NIRRATIO_H

#include <memory>

namespace FenestrationCommon {

	class CSeries;

}

// Calclates ratio of energy in sub-range range vs energy in solar range. When doing 
// spectral integration over entire solar range it is more precise to use visible properties
// in combination with calculated properties for the rest of the range. In order to calculate rest
// of the properties, this ratio is necessary to use
namespace SpectralAveraging {

	class CNIRRatio {
	public:
		CNIRRatio( std::shared_ptr< const FenestrationCommon::CSeries > const& t_SolarRadiation,
		           double const lowLambda, double const highLambda );
		double ratio() const;
	private:
		double m_Ratio;
	};

}

#endif
