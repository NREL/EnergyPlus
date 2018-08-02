#ifndef TARIGU_H
#define TARIGU_H

#include <memory>
#include <vector>

namespace Tarcog {

	enum class Environment;
	class CBaseLayer;
	class CBaseIGULayer;
	class CIGUSolidLayer;
	class CIGUGapLayer;
	class CSurface;

	class CIGU {
	public:
		CIGU( double const t_Width, double const t_Height, double const t_Tilt = 90 );
		CIGU( CIGU const& t_IGU );
		CIGU & operator=( CIGU const & t_IGU );
		~CIGU();

		void addLayer( std::shared_ptr< CBaseIGULayer > const& t_Layer );

		std::vector< std::shared_ptr< CIGUSolidLayer > > getSolidLayers() const;
		std::vector< std::shared_ptr< CIGUGapLayer > > getGapLayers() const;
		std::vector< std::shared_ptr< CBaseIGULayer > > getLayers() const;

		void setTilt( double const t_Tilt );
		void setWidth( double const t_Width );
		void setHeight( double const t_Height );

		void setSolarRadiation( double const t_SolarRadiation ) const;

		std::shared_ptr< CBaseLayer > getLayer( Environment const t_Environment ) const;

		std::shared_ptr< std::vector< double > > getState() const;
		void setState( std::vector< double >& t_State ) const;

		std::shared_ptr< std::vector< double > > getTemperatures() const;
		std::shared_ptr< std::vector< double > > getRadiosities() const;
		std::shared_ptr< std::vector< double > > getMaxDeflections() const;
		std::shared_ptr< std::vector< double > > getMeanDeflections() const;

		double getTilt() const;
		double getWidth() const;
		double getHeight() const;
		double getThickness() const;

		size_t getNumOfLayers() const;

		double getVentilationFlow( Environment const t_Environment ) const;

		void setInitialGuess( std::vector< double > const& t_Temperatures ) const;

		void setDeflectionProperties( double const t_Tini, double const t_Pini );
		void setDeflectionProperties( std::vector< double > const& t_MeasuredDeflections );

	private:
		// Replces layer in existing construction and keeps correct connections in linked list
		void replaceLayer( std::shared_ptr< CBaseIGULayer > const& t_Original,
		                   std::shared_ptr< CBaseIGULayer > const& t_Replacement );

		// Check if layer needs to be decorated with another object
		void checkForLayerUpgrades( std::shared_ptr< CBaseIGULayer > const& t_Layer );

		std::vector< std::shared_ptr< CBaseIGULayer > > m_Layers;

		double m_Width; // meters
		double m_Height; // meters
		double m_Tilt; // degrees

		// Routines to calculate deflection coefficients
		double Ldmean() const;
		double Ldmax() const;

	};

}


#endif
