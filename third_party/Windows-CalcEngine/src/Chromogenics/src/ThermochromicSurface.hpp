#ifndef THERMOCHROMICSURFACE_H
#define THERMOCHROMICSURFACE_H

#include <memory>
#include <vector>

#include "WCETarcog.hpp"

namespace FenestrationCommon
{
    class IInterpolation2D;
}

namespace Chromogenics
{
	namespace ISO15099 {
		class CThermochromicSurface : public Tarcog::ISO15099::ISurface {
		public:
			CThermochromicSurface( const std::vector< std::pair< double, double>> & t_Emissivity,
								   const std::vector< std::pair< double, double>> & t_Transmittance );
			CThermochromicSurface( double t_Emissivity,
								   const std::vector< std::pair< double, double>> & t_Transmittance );
			CThermochromicSurface( const std::vector< std::pair< double, double>> & t_Emissivity,
								   double t_Transmittance );

			CThermochromicSurface( const CThermochromicSurface & t_Surface );
			CThermochromicSurface & operator=( const CThermochromicSurface & t_Surface );

			std::shared_ptr< Tarcog::ISO15099::ISurface > clone() const override;

			void setTemperature( double t_Temperature ) override;

		private:
			std::shared_ptr< FenestrationCommon::IInterpolation2D > m_EmissivityInterpolator;
			std::shared_ptr< FenestrationCommon::IInterpolation2D > m_TransmittanceInterpolator;
		};

	}

}   // namespace Chromogenics

#endif
