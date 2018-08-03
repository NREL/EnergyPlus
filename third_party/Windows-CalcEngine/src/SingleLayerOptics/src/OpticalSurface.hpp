#ifndef OPTICALSURFACE_H
#define OPTICALSURFACE_H

#include <map>

namespace FenestrationCommon {

	enum class Property;
	enum class PropertySimple;
	enum class Scattering;
	enum class ScatteringSimple;

}

namespace SingleLayerOptics {

	class CSurface {
	public:
		CSurface( const double t_T, const double t_R );
		double getProperty( const FenestrationCommon::Property t_Property );

	private:
		std::map< FenestrationCommon::Property, double > m_Property;

	};

	class CScatteringSurface {
	public:
		CScatteringSurface( const double T_dir_dir, const double R_dir_dir,
		                    double T_dir_dif, double R_dir_dif, const double T_dif_dif, const double R_dif_dif );

		// Simple property means only transmittance and reflectance
		double getPropertySimple( const FenestrationCommon::PropertySimple t_Property,
		                          const FenestrationCommon::Scattering t_Scattering ) const;

		void setPropertySimple( const FenestrationCommon::PropertySimple t_Property,
		                        const FenestrationCommon::Scattering t_Scattering, const double value );

		// In this case absroptance is different property from transmittance and reflectance
		double getAbsorptance( const FenestrationCommon::ScatteringSimple t_Scattering ) const;
		double getAbsorptance() const;

	private:
		std::map< std::pair< FenestrationCommon::PropertySimple, FenestrationCommon::Scattering >, double > m_PropertySimple;
		std::map< FenestrationCommon::ScatteringSimple, double > m_Absorptance;

	};

}


#endif
