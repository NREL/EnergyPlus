#ifndef SPECTRALSAMPLE_H
#define SPECTRALSAMPLE_H

#include <memory>
#include <vector>

namespace FenestrationCommon {

	enum class Property;
	enum class Side;
	enum class IntegrationType;
	class CSeries;

}

namespace SpectralAveraging {

	class CSpectralSampleData;
	class CSpectralAngleSample;
	class CAngularSpectralProperties;

	enum class WavelengthSet { Custom, Source, Data };

	// Base class for spectral sample data. Its base setup are spectral properties over certain range. It handles detector and source data.
	// Concrete sample data are handled in inherited classes and tha will depend on type of the sample data
	class CSample {
	public:
		virtual ~CSample() = default;
		explicit CSample( std::shared_ptr< FenestrationCommon::CSeries > const& t_SourceData );
		CSample();
		CSample( CSample const & t_Sample );
		CSample& operator=( CSample const & t_Sample );

		// Assigns detector and wavelengths from other sample. 
		void assignDetectorAndWavelengths( std::shared_ptr< CSample > const& t_Sample );

		// Gets source data. In case wavelengths are referenced to detector or custom 
		// wavelength set, it will perform interpolation according to desired settings.
		std::shared_ptr< FenestrationCommon::CSeries > getSourceData();
		void setSourceData( std::shared_ptr< FenestrationCommon::CSeries > t_SourceData );

		// Setting detector spectral properties for the sample
		void setDetectorData( std::shared_ptr< FenestrationCommon::CSeries > const& t_DetectorData );

		// Integrate sample property over the certain spectrum range
		double getProperty( double const minLambda, double const maxLambda,
		                    FenestrationCommon::Property const t_Property, FenestrationCommon::Side const t_Side );

		// Spectral properties over the wavelength range
		FenestrationCommon::CSeries* getEnergyProperties(
			FenestrationCommon::Property const t_Property, FenestrationCommon::Side const t_Side );

		// Defining the source of wavelengths to be used with the sample. Wavelengths can be used from measured sample,
		// detector data or can be custom provided.
		void setWavelengths( WavelengthSet const t_WavelengthSet,
		                     std::shared_ptr< std::vector< double > > const& t_Wavelenghts = nullptr );

		// For given incoming source and detector data, calculates energy (Transmitted, Reflected or Absorbed) for given
		// spectrum range.
		double getEnergy( double const minLambda, double const maxLambda,
		                  FenestrationCommon::Property const t_Property, FenestrationCommon::Side const t_Side );

		size_t getBandSize() const;

	protected:
		virtual void reset();
		virtual void calculateState();
		virtual void calculateProperties() = 0;

		// It can be single or multiple samples. In any case this should be seen as single set of wavelengts
		virtual std::vector< double > getWavelengthsFromSample() const = 0;

		std::shared_ptr< FenestrationCommon::CSeries > m_SourceData;
		std::shared_ptr< FenestrationCommon::CSeries > m_DetectorData;

		std::vector< double > m_Wavelengths;
		WavelengthSet m_WavelengthSet;

		// Keep energy for current state of the sample. Energy is calculated for each wavelength.
		std::unique_ptr< FenestrationCommon::CSeries > m_IncomingSource;
		std::unique_ptr< FenestrationCommon::CSeries > m_TransmittedSource;
		std::unique_ptr< FenestrationCommon::CSeries > m_ReflectedFrontSource;
		std::unique_ptr< FenestrationCommon::CSeries > m_ReflectedBackSource;
		std::unique_ptr< FenestrationCommon::CSeries > m_AbsorbedFrontSource;
		std::unique_ptr< FenestrationCommon::CSeries > m_AbsorbedBackSource;

		FenestrationCommon::IntegrationType m_IntegrationType;

		bool m_StateCalculated;
	};

	class CSpectralSample : public CSample {
	public:
		CSpectralSample( std::shared_ptr< CSpectralSampleData > const& t_SampleData,
		                 std::shared_ptr< FenestrationCommon::CSeries > const& t_SourceData );
		explicit CSpectralSample( std::shared_ptr< CSpectralSampleData > const& t_SampleData );

		// Before retreiving measured data from sample, function will do all necessary 
		// interpolations to desired wavelengths.
		std::shared_ptr< CSpectralSampleData > getMeasuredData();

		// Returns property at each wavelength
		std::shared_ptr< FenestrationCommon::CSeries > getWavelengthsProperty(
			FenestrationCommon::Property const t_Property, FenestrationCommon::Side const t_Side );

		std::vector< double > getWavelengthsFromSample() const override;

	protected:
		void calculateProperties() override;
		void calculateState() override;

		std::shared_ptr< CSpectralSampleData > m_SampleData;

		std::shared_ptr< FenestrationCommon::CSeries > m_Transmittance;
		std::shared_ptr< FenestrationCommon::CSeries > m_RefFront;
		std::shared_ptr< FenestrationCommon::CSeries > m_RefBack;
		std::shared_ptr< FenestrationCommon::CSeries > m_AbsFront;
		std::shared_ptr< FenestrationCommon::CSeries > m_AbsBack;
	};

	class CSpectralAngleSample {
		// Data for spectral sample under certain angle
	public:
		CSpectralAngleSample( std::shared_ptr< CSpectralSample > const& t_Sample, double const t_Angle );

		double angle() const;
		std::shared_ptr< CSpectralSample > sample() const;

	private:
		std::shared_ptr< CSpectralSample > m_Sample;
		double m_Angle;
	};


}

#endif
