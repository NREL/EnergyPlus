#ifndef MULTIBSDFLAYER_H
#define MULTIBSDFLAYER_H

#include <memory>
#include <vector>
#include <map>

namespace FenestrationCommon {

	class CSeries;
	class CSquareMatrix;
	enum class Side;
	enum class PropertySimple;

}

namespace SingleLayerOptics {

	class CBSDFIntegrator;

}

namespace MultiLayerOptics {

	class CEquivalentBSDFLayer;

	typedef std::shared_ptr< FenestrationCommon::CSeries > p_Series;
	typedef std::shared_ptr< std::vector< p_Series > > p_VectorSeries;

	class CMultiPaneBSDF {
	public:
		// t_SolarRadiation is spectra used for initialization of material properties in the layers
		// t_IncomingSpectra is solar radiation distribution used to calculate actual data.
		// If t_IncomingSpectra is missing then t_SolarRadiation is considered to be incoming spectra
		// for every direction
		CMultiPaneBSDF( const std::shared_ptr< CEquivalentBSDFLayer >& t_Layer,
		                const p_Series& t_SolarRadiation,
		                const p_VectorSeries& t_IncomingSpectra = nullptr );

		// Whole matrix results
		std::shared_ptr< FenestrationCommon::CSquareMatrix > getMatrix(
			const double minLambda, const double maxLambda,
			const FenestrationCommon::Side t_Side, const FenestrationCommon::PropertySimple t_Property );

		double DirDir( const double minLambda, const double maxLambda, const FenestrationCommon::Side t_Side,
		               const FenestrationCommon::PropertySimple t_Property, const double t_Theta, const double t_Phi );
		double DirDir( const double minLambda, const double maxLambda, const FenestrationCommon::Side t_Side,
		               const FenestrationCommon::PropertySimple t_Property, const size_t Index );

		// Vector of layer by layer absorptances for each incoming direction
		std::shared_ptr< std::vector< double > > Abs( const double minLambda, const double maxLambda,
		                                              const FenestrationCommon::Side t_Side, const size_t Index );

		// Hemispherical results for every direction
		std::shared_ptr< std::vector< double > > DirHem( const double minLambda, const double maxLambda,
		                                                 const FenestrationCommon::Side t_Side, const FenestrationCommon::PropertySimple t_Property );

		// Directional hemispherical results for given Theta and Phi direction
		double DirHem( const double minLambda, const double maxLambda,
		               const FenestrationCommon::Side t_Side, const FenestrationCommon::PropertySimple t_Property,
		               const double t_Theta, const double t_Phi );
		double DirHem( const double minLambda, const double maxLambda,
		               const FenestrationCommon::Side t_Side, const FenestrationCommon::PropertySimple t_Property,
		               const size_t Index );

		double Abs( const double minLambda, const double maxLambda,
		            const FenestrationCommon::Side t_Side, const size_t layerIndex, const double t_Theta, const double t_Phi );
		double Abs( const double minLambda, const double maxLambda,
		            const FenestrationCommon::Side t_Side, const size_t layerIndex, const size_t beamIndex );

		// Diffuse to diffuse properties
		double DiffDiff( const double minLambda, const double maxLambda,
		                 const FenestrationCommon::Side t_Side, const FenestrationCommon::PropertySimple t_Property );

		double AbsDiff( const double minLambda, const double maxLambda, const FenestrationCommon::Side t_Side,
		                const size_t t_LayerIndex );

		// Energy that gets transmitted or reflected from certain direction
		double energy( const double minLambda, const double maxLambda,
		               const FenestrationCommon::Side t_Side, const FenestrationCommon::PropertySimple t_Property,
		               const double t_Theta, const double t_Phi );

		double energyAbs( const double minLambda, const double maxLambda,
		                  const FenestrationCommon::Side t_Side, const size_t Index, const double t_Theta, const double t_Phi );

	private:
		void calculate( const double minLambda, const double maxLambda );

		void calcHemisphericalAbs( const FenestrationCommon::Side t_Side );

		std::shared_ptr< CEquivalentBSDFLayer > m_Layer;

		// Solar radiation for initialization
		std::shared_ptr< FenestrationCommon::CSeries > m_SolarRadiationInit;

		p_VectorSeries m_IncomingSpectra;
		std::vector< double > m_IncomingSolar;

		std::shared_ptr< SingleLayerOptics::CBSDFIntegrator > m_Results;

		std::map< FenestrationCommon::Side,
		          std::shared_ptr< std::vector< std::shared_ptr< std::vector< double > > > > > m_Abs;

		// Hemispherical absorptances for every layer
		std::map< FenestrationCommon::Side, std::shared_ptr< std::vector< double > > > m_AbsHem;

		bool m_Calculated;
		double m_MinLambdaCalculated;
		double m_MaxLambdaCalculated;

	};

}

#endif
