#ifndef THERMOCHROMICSURFACE_H
#define THERMOCHROMICSURFACE_H

#include <memory>
#include <vector>

#include "WCETarcog.hpp"

namespace FenestrationCommon {

	class IInterpolation2D;

}

namespace Chromogenics {

	class CThermochromicSurface : public Tarcog::ISurface {
	public:
		CThermochromicSurface( std::vector< std::pair< double, double > > const& t_Emissivity,
		                       std::vector< std::pair< double, double > > const& t_Transmittance );
		CThermochromicSurface( double const& t_Emissivity,
		                       std::vector< std::pair< double, double > > const& t_Transmittance );
		CThermochromicSurface( std::vector< std::pair< double, double > > const& t_Emissivity,
		                       double const& t_Transmittance );

		CThermochromicSurface( CThermochromicSurface const& t_Surface );
		CThermochromicSurface& operator=( CThermochromicSurface const& t_Surface );

		std::shared_ptr< Tarcog::ISurface > clone() const override;

		void setTemperature( double const t_Temperature ) override;

	private:
		std::shared_ptr< FenestrationCommon::IInterpolation2D > m_EmissivityInterpolator;
		std::shared_ptr< FenestrationCommon::IInterpolation2D > m_TransmittanceInterpolator;
	};

}

#endif
