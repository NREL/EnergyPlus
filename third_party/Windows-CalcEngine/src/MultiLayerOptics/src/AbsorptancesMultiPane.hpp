#ifndef AbsorptancesMultiPane_H
#define AbsorptancesMultiPane_H

#include <memory>
#include <vector>

namespace FenestrationCommon {

	class CSeries;

}

namespace MultiLayerOptics {

	// Calculate absorptances of multiplane layers for simple case (single incident angle)
	class CAbsorptancesMultiPane {
	public:
		CAbsorptancesMultiPane( const std::shared_ptr< const FenestrationCommon::CSeries >& t_T,
		                        const std::shared_ptr< const FenestrationCommon::CSeries >& t_Rf,
		                        const std::shared_ptr< const FenestrationCommon::CSeries >& t_Rb );

		void addLayer( const std::shared_ptr< const FenestrationCommon::CSeries >& t_T,
		               const std::shared_ptr< const FenestrationCommon::CSeries >& t_Rf,
		               const std::shared_ptr< const FenestrationCommon::CSeries >& t_Rb );

		std::shared_ptr< FenestrationCommon::CSeries > Abs( size_t const Index );
		size_t numOfLayers();

	private:
		void calculateState();

		std::shared_ptr< FenestrationCommon::CSeries > rCoeffs(
			const FenestrationCommon::CSeries& t_T,
			const FenestrationCommon::CSeries& t_Rf,
			const FenestrationCommon::CSeries& t_Rb,
			const FenestrationCommon::CSeries& t_RCoeffs );

		std::shared_ptr< FenestrationCommon::CSeries > tCoeffs(
			const FenestrationCommon::CSeries& t_T,
			const FenestrationCommon::CSeries& t_Rb,
			const FenestrationCommon::CSeries& t_RCoeffs );

		std::vector< std::shared_ptr< const FenestrationCommon::CSeries > > m_T;
		std::vector< std::shared_ptr< const FenestrationCommon::CSeries > > m_Rf;
		std::vector< std::shared_ptr< const FenestrationCommon::CSeries > > m_Rb;
		std::vector< std::shared_ptr< FenestrationCommon::CSeries > > m_Abs;

		std::vector< std::shared_ptr< FenestrationCommon::CSeries > > m_rCoeffs;
		std::vector< std::shared_ptr< FenestrationCommon::CSeries > > m_tCoeffs;

		bool m_StateCalculated;
	};
}

#endif
