#ifndef ANGULARPROPERTIES_H
#define ANGULARPROPERTIES_H

#include <memory>

namespace FenestrationCommon {

	enum class SurfaceType;

}

namespace SpectralAveraging {

	// Calculates angular propertes at given angle
	class CAngularProperties {
	public:
		virtual ~CAngularProperties() = default;
		CAngularProperties( double const t_TransmittanceZero, double const t_ReflectanceZero );

		virtual double transmittance( double const t_Angle, double const t_Wavelength = 0 ) = 0;
		virtual double reflectance( double const t_Angle, double const t_Wavelength = 0 ) = 0;

	protected:
		double cosAngle( double const t_Angle ) const;
		virtual void checkStateProperties( double const t_Angle, double const t_Wavelength );

		double m_Transmittance0;
		double m_Reflectance0;

		double m_Transmittance;
		double m_Reflectance;
		double m_StateAngle;
		double m_StateWavelength;
	};

	// Angular properties for uncoated type of glass layer
	class CAngularPropertiesUncoated : public CAngularProperties {
	public:
		CAngularPropertiesUncoated( const double t_Thicknes,
		                            const double t_TransmittanceZero, const double t_ReflectanceZero );

		double transmittance( const double t_Angle, const double t_Wavelength ) override;
		double reflectance( const double t_Angle, const double t_Wavelength ) override;

	protected:
		void checkStateProperties( const double t_Angle, const double t_Wavelength ) override;

	private:
		double m_Thickness;
		double m_Beta;
		double m_Rho0;

	};

	class CAngularPropertiesCoated : public CAngularProperties {
	public:
		CAngularPropertiesCoated( double const t_Transmittance, double const t_Reflectance,
		                          double const t_SolTransmittance0 );

		double transmittance( double const t_Angle, double const t_Wavelength = 0 ) override;
		double reflectance( double const t_Angle, double const t_Wavelength = 0 ) override;

	protected:
		void checkStateProperties( const double t_Angle, const double t_Wavelength ) override;

		double m_SolTransmittance0;
	};

	enum class CoatingProperty { T, R };

	enum class CoatingType { Clear, Bronze };

	class Coefficients {
	public:
		Coefficients( double const t_C0, double const t_C1, double const t_C2, double const t_C3,
		              double const t_C4 );
		double inerpolation( double const t_Value ) const;

	private:
		double C0;
		double C1;
		double C2;
		double C3;
		double C4;
	};

	// creates coating coefficients according to property and type.
	class CCoatingCoefficients {
	public:
		CCoatingCoefficients();
		std::shared_ptr< Coefficients > getCoefficients( CoatingProperty const t_Property, CoatingType const t_Type ) const;
	};

	class CAngularPropertiesFactory {
	public:
		CAngularPropertiesFactory( double const t_Transmittance0, double const t_Reflectance0,
		                           double const t_Thickness = 0, double const t_SolarTransmittance = 0 );

		std::shared_ptr< CAngularProperties >
		getAngularProperties( FenestrationCommon::SurfaceType const t_SurfaceType );

	private:
		double m_Thickness;
		double m_Transmittance0;
		double m_Reflectance0;
		double m_SolarTransmittance0;
	};

}

#endif
