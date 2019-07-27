#ifndef AbsorptancesMultiPaneBSDF_H
#define AbsorptancesMultiPaneBSDF_H

#include <memory>
#include <vector>

namespace FenestrationCommon {

	class CSquareMatrix;
	enum class Side;
	class CSeries;

}

namespace SingleLayerOptics {

	class CBSDFLayer;

}

namespace MultiLayerOptics {

	typedef std::vector< std::shared_ptr< FenestrationCommon::CSquareMatrix > > SquareMatrices;

	// Calculate BSDF absorptances of MultiLayerOptics layer.
	class CAbsorptancesMultiPaneBSDF {
	public:
		CAbsorptancesMultiPaneBSDF( FenestrationCommon::Side t_Side,
		                            const std::shared_ptr< std::vector< double > >& t_CommonWavelengths,
		                            const std::shared_ptr< FenestrationCommon::CSeries >& t_SolarRadiation,
		                            const std::shared_ptr< SingleLayerOptics::CBSDFLayer >& t_Layer );

		void addLayer( SingleLayerOptics::CBSDFLayer& t_Layer );

		std::shared_ptr< std::vector< double > > Abs( const double minLambda, const double maxLambda,
		                                              const size_t Index );

	private:
		void calculateState( const double minLambda, const double maxLambda );

		// Denominator term for t and r coefficients (absorptance calculations)
		std::shared_ptr< FenestrationCommon::CSquareMatrix > getDenomForRTCoeff(
			const FenestrationCommon::CSquareMatrix& t_Reflectance,
			const FenestrationCommon::CSquareMatrix& t_PreviousR );

		// Returns correct layer index. Depends if object is used to calculate forward or backward properties
		size_t layerIndex( const size_t Index ) const;

		std::shared_ptr< std::vector< double > > multVectors( const std::vector< double >& t_vec1,
		                                                      const std::vector< double >& t_vec2 );
		std::shared_ptr< std::vector< double > > divVectors( const std::vector< double >& t_vec1,
		                                                     const std::vector< double >& t_vec2 );
		std::shared_ptr< std::vector< double > > addVectors( const std::vector< double >& t_vec1,
		                                                     const std::vector< double >& t_vec2 );

		std::shared_ptr< const FenestrationCommon::CSquareMatrix > m_Lambda;
		std::shared_ptr< const std::vector< double > > m_LambdaVector;

		std::shared_ptr< FenestrationCommon::CSeries > m_SolarRadiation;

		// Layer by layer coefficients for each wavelength (layer, wavelength, direction)
		std::vector< std::shared_ptr< SquareMatrices > > m_TausF;
		std::vector< std::shared_ptr< SquareMatrices > > m_TausB;
		std::vector< std::shared_ptr< SquareMatrices > > m_RhosF;
		std::vector< std::shared_ptr< SquareMatrices > > m_RhosB;

		std::vector< std::shared_ptr< SquareMatrices > > m_rCoeffs;
		std::vector< std::shared_ptr< SquareMatrices > > m_tCoeffs;

		// Absorptances for each direction
		std::vector< std::shared_ptr< std::vector< double > > > m_Abs;

		std::shared_ptr< std::vector< double > > m_CommonWavelengths;
		bool m_StateCalculated;
		FenestrationCommon::Side m_Side;
		size_t m_NumOfLayers;

	};
}

#endif
