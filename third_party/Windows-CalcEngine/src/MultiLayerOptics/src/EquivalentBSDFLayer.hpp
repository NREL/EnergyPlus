#ifndef EQUIVALENTBSDFLAYERMULTIWL_H
#define EQUIVALENTBSDFLAYERMULTIWL_H

#include <memory>
#include <vector>
#include <map>

#include "AbsorptancesMultiPaneBSDF.hpp"

namespace FenestrationCommon {

	class CSquareMatrix;
	class CMatrixSeries;
	class CSeries;
	enum class Side;
	enum class PropertySimple;

}

namespace SingleLayerOptics {

	class CBSDFLayer;
	class CBSDFIntegrator;
	enum class BSDFHemisphere;
	class CBSDFDirections;

}

namespace MultiLayerOptics {

	class CEquivalentBSDFLayerSingleBand;

	// Calculates equivalent BSDF matrices for transmittances and reflectances
	class CEquivalentBSDFLayer {
	public:
		CEquivalentBSDFLayer( std::vector< double > const& t_CommonWavelengths,
		                      const std::shared_ptr< SingleLayerOptics::CBSDFLayer >& t_Layer );

		void addLayer( const std::shared_ptr< SingleLayerOptics::CBSDFLayer >& t_Layer );
		std::shared_ptr< const SingleLayerOptics::CBSDFDirections > getDirections( const SingleLayerOptics::BSDFHemisphere t_Side ) const;
		std::vector< double > getCommonWavelengths() const;

		// Absorptance wavelength by wavelength matrices
		std::shared_ptr< FenestrationCommon::CMatrixSeries > getTotalA( const FenestrationCommon::Side t_Side );

		// Transmittance and reflectance wavelength by wavelength matrices
		std::shared_ptr< FenestrationCommon::CMatrixSeries > getTotal( const FenestrationCommon::Side t_Side,
		                                                               const FenestrationCommon::PropertySimple t_Property );

		void setSolarRadiation( const std::shared_ptr< FenestrationCommon::CSeries >& t_SolarRadiation );

	private:

		void calculate();

		// Wavelength layer per layer calculations
		void calculateWavelengthProperties( size_t const t_NumOfLayers, size_t const t_Start, size_t const t_End ) const;

		void updateWavelengthLayers( const std::shared_ptr< SingleLayerOptics::CBSDFLayer >& t_Layer ) const;

		// std::vector of layer results over each wavelength
		std::shared_ptr< std::vector< std::shared_ptr< CEquivalentBSDFLayerSingleBand > > > m_LayersWL;

		// Layers that are added to the equivalent layer
		std::vector< std::shared_ptr< SingleLayerOptics::CBSDFLayer > > m_Layer;

		// Total absoprtance coefficients for every wavelength (does not include source data)
		std::map< FenestrationCommon::Side, std::shared_ptr< FenestrationCommon::CMatrixSeries > > m_TotA;

		// Total Transmittance and Reflectance values for every wavelength (does not include source data)
		std::map< std::pair< FenestrationCommon::Side, FenestrationCommon::PropertySimple >, std::shared_ptr< FenestrationCommon::CMatrixSeries > > m_Tot;

		std::shared_ptr< const FenestrationCommon::CSquareMatrix > m_Lambda;

		std::vector< double > m_CombinedLayerWavelengths;
		bool m_Calculated;
	};

}

#endif
