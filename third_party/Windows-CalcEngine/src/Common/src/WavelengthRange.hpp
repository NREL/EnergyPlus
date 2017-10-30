#ifndef WAVELENGTHRANGE_H
#define WAVELENGTHRANGE_H

namespace FenestrationCommon {

	enum class WavelengthRange;

	// To create wavelength range for certain pre-defined ranges
	class CWavelengthRange {
	public:
		explicit CWavelengthRange( const WavelengthRange t_Range );
		double minLambda() const;
		double maxLambda() const;
	private:
		void setWavelengthRange( const WavelengthRange t_Range );
		double m_MinLambda;
		double m_MaxLambda;
	};

}

#endif
