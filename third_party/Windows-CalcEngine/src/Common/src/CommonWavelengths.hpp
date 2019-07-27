#ifndef COMMONWAVELENGTHS_H
#define COMMONWAVELENGTHS_H

#include <vector>

namespace FenestrationCommon {

	// When combining two wavelegth sets it is possible to do it in two ways.
	// Interpolate - this will create common wavelength set which minimum and maximum values will not exceed
	//               any wavelength set that is interpolated
	// Extrapolate - will create common wavelength set that minimum and maximum will exceed any wavelength set
	//               given for interpolation

	enum class Combine { Interpolate, Extrapolate };

	// Class to combine several wavelengths into one
	class CCommonWavelengths {
	public:
		CCommonWavelengths();

		// put additional wavelength
		void addWavelength( std::vector< double > const& t_wv );

		// getting combined wavelength
		std::vector< double > getCombinedWavelengths( Combine const t_Combination );

	private:
		// Combine two wavelengths into one by applyting correct strategy
		std::vector< double > combineWavelegths( std::vector< double > const& t_wv1,
		                                         std::vector< double > const& t_wv2, Combine const t_Combination ) const;

		std::vector< std::vector< double > > m_Wavelengths;
	};

}

#endif
