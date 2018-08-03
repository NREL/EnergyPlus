#ifndef MULTILAYERINTERREF_H
#define MULTILAYERINTERREF_H

#include <memory>
#include <vector>
#include <map>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

namespace FenestrationCommon {

	enum class Scattering;

}

namespace SingleLayerOptics {

	class CScatteringLayer;

}

namespace MultiLayerOptics {

	typedef std::vector< std::shared_ptr< SingleLayerOptics::CScatteringLayer > > CLayer_List;
	typedef std::shared_ptr< std::vector< double > > PtrToVectorOfDouble;

	class CSurfaceEnergy;
	class CInterRefSingleComponent;

	class CInterRef {
	public:
		CInterRef( const std::shared_ptr< SingleLayerOptics::CScatteringLayer >& t_Layer,
		           const double t_Theta = 0, const double t_Phi = 0 );

		void addLayer( const std::shared_ptr< SingleLayerOptics::CScatteringLayer >& t_Layer,
		               const FenestrationCommon::Side t_Side = FenestrationCommon::Side::Back,
		               const double t_Theta = 0, const double t_Phi = 0 );

		double getAbsorptance(
			const size_t Index, FenestrationCommon::Side t_Side,
			FenestrationCommon::ScatteringSimple t_Scattering,
			const double t_Theta = 0,
			const double t_Phi = 0 );

		double getEnergyToSurface(
			const size_t Index,
			const FenestrationCommon::Side t_SurfaceSide,
			const FenestrationCommon::EnergyFlow t_EnergyFlow,
			const FenestrationCommon::Scattering t_Scattering,
			const double t_Theta = 0,
			const double t_Phi = 0 );

		size_t size() const;

	private:
		void calculateEnergies( const double t_Theta, const double t_Phi );
		void createForwardLayers( const double t_Theta, const double t_Phi );
		void createBackwardLayers( const double t_Theta, const double t_Phi );

		// Function that calculate total diffuse energy that is leaving surface 
		// and that originates from direct beam
		std::shared_ptr< CSurfaceEnergy > calcDiffuseEnergy( const double t_Theta, const double t_Phi );

		// Calculate direct to diffuse component at each surface
		std::shared_ptr< CSurfaceEnergy > calcDirectToDiffuseComponent( const double t_Theta, const double t_Phi );

		void calculateAbsroptances( const double t_Theta, const double t_Phi );

		std::vector< std::shared_ptr< SingleLayerOptics::CScatteringLayer > > m_Layers;

		std::map< FenestrationCommon::Side, std::shared_ptr< CLayer_List > > m_StackedLayers;

		// for calculation of pure components (direct and diffuse)
		std::shared_ptr< CInterRefSingleComponent > m_DirectComponent;
		std::shared_ptr< CInterRefSingleComponent > m_DiffuseComponent;

		// Energy that is incoming at each surface. It contains three different components: 
		// 1. Direct beam energy component calculates how much of direct beam will be incoming at
		//   each surface.
		// 2. Diffuse component that originates from incoming direct beam.
		// 3. Diffuse component that originates from incoming diffuse.
		std::map< FenestrationCommon::Scattering, std::shared_ptr< CSurfaceEnergy > > m_Energy;

		// Absorptance for each layer comes in two different forms: Absrobed from diffuse and
		// absorbled from direct.
		std::map< std::pair< FenestrationCommon::Side,
		                     FenestrationCommon::ScatteringSimple >, PtrToVectorOfDouble > m_Abs;

		bool m_StateCalculated;
		double m_Theta;
		double m_Phi;
	};

}

#endif
