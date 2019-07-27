#ifndef BSDFINTEGRATOR_H
#define BSDFINTEGRATOR_H

#include <memory>
#include <vector>
#include <map>
#include "../../Common/src/mmap.hpp"

namespace FenestrationCommon {

	class CSquareMatrix;
	enum class Side;
	enum class PropertySimple;

}

namespace SingleLayerOptics {

	class CBSDFDirections;

	typedef std::pair< FenestrationCommon::Side, FenestrationCommon::PropertySimple > pair_Side_PropertySimple;
	typedef std::shared_ptr< FenestrationCommon::CSquareMatrix > p_SquareMatrix;

	// Layer results from BSDF directions.
	class CBSDFIntegrator {
	public:
		explicit CBSDFIntegrator( const std::shared_ptr< const CBSDFIntegrator >& t_Integrator );
		explicit CBSDFIntegrator( const std::shared_ptr< const CBSDFDirections >& t_Directions );

		// Result matrices
		std::shared_ptr< FenestrationCommon::CSquareMatrix > getMatrix( const FenestrationCommon::Side t_Side,
		                                                                const FenestrationCommon::PropertySimple t_Property ) const;

		void setResultMatrices( const std::shared_ptr< FenestrationCommon::CSquareMatrix >& t_Tau,
		                        const std::shared_ptr< FenestrationCommon::CSquareMatrix >& t_Rho, FenestrationCommon::Side t_Side );

		// Direct-direct components
		double DirDir( const FenestrationCommon::Side t_Side, const FenestrationCommon::PropertySimple t_Property,
		               const double t_Theta, const double t_Phi );
		double DirDir( const FenestrationCommon::Side t_Side, const FenestrationCommon::PropertySimple t_Property,
		               const size_t Index );

		// Directional hemispherical results for every direction in BSDF definition
		std::shared_ptr< std::vector< double > > DirHem( const FenestrationCommon::Side t_Side,
		                                                 const FenestrationCommon::PropertySimple t_Property );
		std::shared_ptr< std::vector< double > > Abs( const FenestrationCommon::Side t_Side );

		// Directional hemispherical results for given Theta and Phi direction
		double DirHem( const FenestrationCommon::Side t_Side, const FenestrationCommon::PropertySimple t_Property,
		               const double t_Theta, const double t_Phi );
		double Abs( const FenestrationCommon::Side t_Side, const double t_Theta, const double t_Phi );
		double Abs( const FenestrationCommon::Side t_Side, const size_t Index );

		// std::shared_ptr< const CBSDFDirections > getDirections() const;

		double DiffDiff( const FenestrationCommon::Side t_Side,
		                 const FenestrationCommon::PropertySimple t_Property );

		// Lambda values for the layer.
		std::shared_ptr< const std::vector< double > > lambdaVector() const;
		std::shared_ptr< const FenestrationCommon::CSquareMatrix > lambdaMatrix() const;

		size_t getNearestBeamIndex( const double t_Theta, const double t_Phi ) const;

	protected:
		std::shared_ptr< const CBSDFDirections > m_Directions;
		size_t m_DimMatrices;

	private:
		// Hemispherical integration over m_Directions
		double integrate( FenestrationCommon::CSquareMatrix const& t_Matrix ) const;

		void calcDiffuseDiffuse();
		void calcHemispherical();

		std::map< pair_Side_PropertySimple, p_SquareMatrix > m_Matrix;
		std::map< pair_Side_PropertySimple, std::shared_ptr< std::vector< double > > > m_Hem;
		std::map< FenestrationCommon::Side, std::shared_ptr< std::vector< double > > > m_Abs;

		bool m_HemisphericalCalculated;
		bool m_DiffuseDiffuseCalculated;
		FenestrationCommon::mmap< double, FenestrationCommon::Side, FenestrationCommon::PropertySimple > m_MapDiffDiff;

	};

}

#endif
