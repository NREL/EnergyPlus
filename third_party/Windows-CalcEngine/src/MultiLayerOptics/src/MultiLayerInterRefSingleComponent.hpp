#ifndef MULTILAYERINTERREFLECTANCESSINGLECOMPONENT_H
#define MULTILAYERINTERREFLECTANCESSINGLECOMPONENT_H

#include <vector>
#include <memory>
#include <map>

#include "WCECommon.hpp"

namespace SingleLayerOptics {

	class CLayerSingleComponent;

}

namespace MultiLayerOptics {

	class CSurfaceEnergy {
	public:
		CSurfaceEnergy();

		void addEnergy( const FenestrationCommon::Side t_Side,
		                const FenestrationCommon::EnergyFlow t_EnergyFlow, const double t_Value );

		double IEnergy( const size_t Index,
		                const FenestrationCommon::Side t_Side, const FenestrationCommon::EnergyFlow t_EnergyFlow );

	private:
		std::map< std::pair< FenestrationCommon::Side, FenestrationCommon::EnergyFlow >,
		          std::shared_ptr< std::vector< double > > > m_IEnergy;
	};

	// Calculates incoming energies to every surface of the layers in IGU considering only 
	// single component (direct or diffuse)
	class CInterRefSingleComponent {
	public:
		CInterRefSingleComponent( const double t_Tf, const double t_Rf, const double t_Tb, const double t_Rb );
		explicit CInterRefSingleComponent( const std::shared_ptr< const SingleLayerOptics::CLayerSingleComponent >& t_Layer );

		// Adding layer to the back or front side of the IGU composition
		void addLayer( const double t_Tf, const double t_Rf, const double t_Tb, const double t_Rb,
		               const FenestrationCommon::Side t_Side = FenestrationCommon::Side::Back );

		void addLayer( std::shared_ptr< const SingleLayerOptics::CLayerSingleComponent > t_Layer,
		               const FenestrationCommon::Side t_Side = FenestrationCommon::Side::Back );

		// Retrieves value of energy to the surface of given layer. Incoming energy can be outside or inside
		double getEnergyToSurface( const size_t Index, const FenestrationCommon::Side t_SurfaceSide,
		                           const FenestrationCommon::EnergyFlow t_EnergyFlow );

		std::shared_ptr< CSurfaceEnergy > getSurfaceEnergy();

		double getLayerAbsorptance( const size_t Index, const FenestrationCommon::Side t_Side );

	private:
		void initialize( const double t_Tf, const double t_Rf, const double t_Tb, const double t_Rb );
		void calculateEnergies();
		void calculateForwardLayers();
		void calculateBackwardLayers();

		std::vector< std::shared_ptr< SingleLayerOptics::CLayerSingleComponent > > m_Layers;

		// Keeping equivalent optical properties while adding layers. This is necessary
		// for calculation of interreflectances.
		// Forward layers is vector of layers that has been build by adding layers on the back side of
		// IGU and storing them into array. For example, three layer IGU will have forward layer built in this way -
		// First item in the vector is exterior environment layer with transmittance of one and reflectance of zero.
		// Second item in the vector is only first layer of the IGU.
		// Third item in the vector is IGU made of first and second layer of the IGU.
		// Fourth item in the vector is complete IGU. So in this case, number of items in forward layers vector
		// will be four
		std::vector< std::shared_ptr< SingleLayerOptics::CLayerSingleComponent > > m_ForwardLayers;

		// Backward layer vector is build in opposite way.
		// First item in the vector is interior environment (transmittance is one and reflectance is zero).
		// Second item is third layer from the IGU.
		// Third item is IGU made of second and third layer of the IGU
		// Fourth item is complete IGU
		std::vector< std::shared_ptr< SingleLayerOptics::CLayerSingleComponent > > m_BackwardLayers;

		// Results of interreflectances are calculated for beam incoming from inside and outside
		std::shared_ptr< CSurfaceEnergy > m_IEnergy;

		bool m_StateCalculated;

	};

}

#endif
