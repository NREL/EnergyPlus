#ifndef EQUIVALENTLAYERSINGLECOMPONENTMW_H
#define EQUIVALENTLAYERSINGLECOMPONENTMW_H

#include <memory>
#include <vector>
#include <map>

namespace FenestrationCommon {

	class CSeries;
	enum class Property;
	enum class Side;

}

namespace MultiLayerOptics {

	class CEquivalentLayerSingleComponent;

	///////////////////////////////////////////////////////////////////////////
	///   CSurfaceSeries
	///////////////////////////////////////////////////////////////////////////

	// Keeps series of data related to one surface
	class CSurfaceSeries {
	public:
		CSurfaceSeries( const std::shared_ptr< FenestrationCommon::CSeries >& t_T,
		                const std::shared_ptr< FenestrationCommon::CSeries >& t_R );

		std::shared_ptr< FenestrationCommon::CSeries > getProperties( const FenestrationCommon::Property t_Property ) const;

	private:
		std::map< FenestrationCommon::Property, std::shared_ptr< FenestrationCommon::CSeries > > m_Properties;
	};

	///////////////////////////////////////////////////////////////////////////
	///   CLayerSeries
	///////////////////////////////////////////////////////////////////////////

	class CLayerSeries {
	public:
		CLayerSeries( const std::shared_ptr< FenestrationCommon::CSeries >& t_Tf,
		              const std::shared_ptr< FenestrationCommon::CSeries >& t_Rf,
		              const std::shared_ptr< FenestrationCommon::CSeries >& t_Tb,
		              const std::shared_ptr< FenestrationCommon::CSeries >& t_Rb );

		std::shared_ptr< FenestrationCommon::CSeries > getProperties( const FenestrationCommon::Side t_Side,
		                                                              const FenestrationCommon::Property t_Property ) const;

	private:
		std::map< FenestrationCommon::Side, std::shared_ptr< CSurfaceSeries > > m_Surfaces;
	};

	///////////////////////////////////////////////////////////////////////////
	///   CEquivalentLayerSingleComponentMW
	///////////////////////////////////////////////////////////////////////////

	// Spectral properties of glazing system made up of layers with defined or measured spectral properties.
	// Single component means that ray will propagate through IGU in single state (as perfect beam or prefectly diffuse)
	class CEquivalentLayerSingleComponentMW {
	public:
		CEquivalentLayerSingleComponentMW( const std::shared_ptr< FenestrationCommon::CSeries >& t_Tf,
		                                   const std::shared_ptr< FenestrationCommon::CSeries >& t_Tb,
		                                   const std::shared_ptr< FenestrationCommon::CSeries >& t_Rf,
		                                   const std::shared_ptr< FenestrationCommon::CSeries >& t_Rb );

		void addLayer( const std::shared_ptr< FenestrationCommon::CSeries >& t_Tf,
		               const std::shared_ptr< FenestrationCommon::CSeries >& t_Tb,
		               const std::shared_ptr< FenestrationCommon::CSeries >& t_Rf,
		               const std::shared_ptr< FenestrationCommon::CSeries >& t_Rb );

		std::shared_ptr< FenestrationCommon::CSeries > getProperties( const FenestrationCommon::Property,
		                                                              const FenestrationCommon::Side ) const;

	private:
		std::shared_ptr< CLayerSeries > m_Layer;

		// Equivalent layers wavelength by wavelength
		std::vector< std::shared_ptr< CEquivalentLayerSingleComponent > > m_EqLayerBySeries;

	};

}

#endif
