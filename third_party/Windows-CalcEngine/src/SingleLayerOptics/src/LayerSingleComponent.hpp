#ifndef LAYERSINGLECOMPONENT_H
#define LAYERSINGLECOMPONENT_H

#include <memory>
#include <map>

namespace FenestrationCommon {

	enum class Side;
	enum class Property;

}

namespace SingleLayerOptics {

	class CSurface;

	// class to be used for description of single component of the light. By single component it is assumed that light 
	// will not change state (from beam to diffuse) during propagation through the layer
	class CLayerSingleComponent {
	public:
		CLayerSingleComponent( const double t_Tf, const double t_Rf, const double t_Tb, const double t_Rb );

		double getProperty( const FenestrationCommon::Property t_Property, const FenestrationCommon::Side t_Side ) const;

		std::shared_ptr< CSurface > getSurface( const FenestrationCommon::Side t_Side ) const;

	private:
		std::map< FenestrationCommon::Side, std::shared_ptr< CSurface > > m_Surface;

	};

}

#endif
