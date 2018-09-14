#ifndef BASEBSDFLAYERMULTIWL_H
#define BASEBSDFLAYERMULTIWL_H

#include <memory>
#include <vector>

namespace FenestrationCommon {

	enum class Side;
	class CSeries;

}

namespace MultiLayerOptics {

	class CEquivalentBSDFLayer;

}

namespace SingleLayerOptics {

	enum class BSDFHemisphere;
	class CBSDFHemisphere;
	class CBaseCell;
	class CBSDFIntegrator;
	class CBeamDirection;
	class CBSDFDirections;

	typedef std::vector< std::shared_ptr< CBSDFIntegrator > > BSDF_Results;

	// Base class for handling BSDF Layer
	class CBSDFLayer {
	public:
		CBSDFLayer( const std::shared_ptr< CBaseCell >& t_Cell,
		            const std::shared_ptr< const CBSDFHemisphere >& t_Directions );

		void setSourceData( std::shared_ptr< FenestrationCommon::CSeries > t_SourceData );

		// BSDF results for the enire spectrum range of the material in the cell
		std::shared_ptr< CBSDFIntegrator > getResults();

		std::shared_ptr< const CBSDFDirections > getDirections( const BSDFHemisphere t_Side ) const;

		// BSDF results for each wavelenght given in specular cell
		std::shared_ptr< BSDF_Results > getWavelengthResults();

		int getBandIndex( const double t_Wavelength );

		std::vector< double > getBandWavelengths() const;

	protected:
		// Diffuse calculation distribution will be calculated here. It will depend on base classes.
		// It can for example be uniform or directional. In case of specular layers there will be no any diffuse
		// distribution
		virtual void calcDiffuseDistribution( const FenestrationCommon::Side aSide,
		                                      const CBeamDirection& t_Direction,
		                                      const size_t t_DirectionIndex ) = 0;

		virtual void calcDiffuseDistribution_wv( const FenestrationCommon::Side aSide,
		                                         const CBeamDirection& t_Direction,
		                                         const size_t t_DirectionIndex ) = 0;

		// BSDF layer is not calculated by default because it is time consuming process and in some cases this call
		// is not necessary. However, refactoring is needed since there is no reason to create CBSDFLayer if
		// it will not be calculated
		void calculate();
		void calculate_wv();

		std::shared_ptr< const CBSDFHemisphere > m_BSDFHemisphere;
		std::shared_ptr< CBaseCell > m_Cell;
		std::shared_ptr< CBSDFIntegrator > m_Results;
		// Results over each wavelength
		std::shared_ptr< BSDF_Results > m_WVResults;

	private:
		void calc_dir_dir();
		void calc_dir_dif();
		void fillWLResultsFromMaterialCell();
		// Keeps state of the object. Calculations are not done by defult (in constructor)
		// becuase they are time consuming.
		bool m_Calculated;

		// Calculation of results over each wavelength
		void calc_dir_dir_wv();
		void calc_dir_dif_wv();
		// State to hold information of wavelength results are already calculated
		bool m_CalculatedWV;
	};

}

#endif
