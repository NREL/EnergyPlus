#ifndef ANGULARSPECTRALSAMPLE_H
#define ANGULARSPECTRALSAMPLE_H

#include <memory>
#include <vector>

namespace FenestrationCommon {

	enum class MaterialType;
	enum class Side;
	enum class Property;
	class CSeries;

}

namespace SpectralAveraging {

	class CSpectralSampleData;
	class CSpectralSample;

	// enum class SampleProperty;

	// Angular properties for each wavelength of measured data
	class CAngularSpectralProperties {
	public:
		CAngularSpectralProperties( std::shared_ptr< CSpectralSample > const& t_SpectralSample,
		                            double const t_Angle, FenestrationCommon::MaterialType const t_Type, double const t_Thickness );

		double angle() const;
		std::shared_ptr< CSpectralSampleData > properties() const;

	private:
		void calculateAngularProperties( std::shared_ptr< CSpectralSample > const& t_SpectralSample,
		                                 FenestrationCommon::MaterialType const t_Type );

		double m_Angle;
		double m_Thickness;
		std::shared_ptr< CSpectralSampleData > m_AngularData;
	};

	// Data for spectral sample under certain angle
	class CSpectralSampleAngle {
	public:
		CSpectralSampleAngle( std::shared_ptr< CSpectralSample > const& t_Sample, double const t_Angle );

		double angle() const;
		std::shared_ptr< CSpectralSample > sample() const;

	private:
		std::shared_ptr< CSpectralSample > m_Sample;
		double m_Angle;
	};

	class CAngularSpectralSample {
	public:
		CAngularSpectralSample( std::shared_ptr< CSpectralSample > const& t_SpectralSample,
		                        double const t_Thickness, FenestrationCommon::MaterialType const t_Type );

		void setSourceData( std::shared_ptr< FenestrationCommon::CSeries > t_SourceData );

		// Get averaged property over the wavelength
		double getProperty( double const minLambda, double const maxLambda,
		                    FenestrationCommon::Property const t_Property, FenestrationCommon::Side const t_Side, double const t_Angle );

		// Get property at each wavelength
		std::vector< double > getWavelengthsProperty( double const minLambda,
		                                              double const maxLambda, FenestrationCommon::Property const t_Property,
		                                              FenestrationCommon::Side const t_Side, double const t_Angle );

		std::vector< double > getBandWavelengths() const;

	private:
		// Finds spectral sample or creates new one if sample is not already created
		std::shared_ptr< CSpectralSample > findSpectralSample( double const t_Angle );
		std::vector< std::shared_ptr< CSpectralSampleAngle > > m_SpectralProperties;
		std::shared_ptr< CSpectralSample > m_SpectralSampleZero; // spectral sample as zero degrees
		double m_Thickness;
		FenestrationCommon::MaterialType m_Type;

	};

}

#endif
