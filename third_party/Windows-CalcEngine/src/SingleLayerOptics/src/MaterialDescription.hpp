#ifndef MATERIALDESCRIPTION_H
#define MATERIALDESCRIPTION_H

#include <memory>
#include <vector>
#include <map>

// Lixing
namespace FenestrationCommon {

	enum class Side;
	enum class Property;
	enum class MaterialType;
	enum class WavelengthRange;
	class CSeries;

}

namespace SpectralAveraging {

	// enum class SampleProperty;
	class CSpectralSample;
	class CAngularSpectralSample;
	class CSingleAngularMeasurement;
	class CAngularMeasurements;

}

namespace SingleLayerOptics {

	class CSurface;

	struct RMaterialProperties {
	public:
		RMaterialProperties( const double aTf, const double aTb, const double aRf, const double aRb );
		double getProperty( const FenestrationCommon::Property t_Property, const FenestrationCommon::Side t_Side ) const;

	private:
		std::map< FenestrationCommon::Side, std::shared_ptr< CSurface > > m_Surface;

	};

	//////////////////////////////////////////////////////////////////////////////////////////
	///   CMaterial
	//////////////////////////////////////////////////////////////////////////////////////////

	// Base virtual class for any material definition. It reprsents material properties over 
	// the certain wavelength range.
	// It also defines interface for angular dependency of material properties.
	class CMaterial {
	public:
		CMaterial( const double minLambda, const double maxLambda );
		explicit CMaterial( const FenestrationCommon::WavelengthRange t_Range );

		virtual void setSourceData( std::shared_ptr< FenestrationCommon::CSeries > t_SourceData );

		// Get certain material property over the entire range
		virtual double getProperty( const FenestrationCommon::Property t_Property,
		                            const FenestrationCommon::Side t_Side ) const = 0;

		virtual double getPropertyAtAngle( const FenestrationCommon::Property t_Property,
		                                   const FenestrationCommon::Side t_Side, const double t_Angle ) const;

		// Get properties for every band defined in the material
		virtual std::vector< double >
		getBandProperties( const FenestrationCommon::Property t_Property,
		                   const FenestrationCommon::Side t_Side ) const = 0;

		std::shared_ptr< std::vector< RMaterialProperties > > getBandProperties();

		std::shared_ptr< SpectralAveraging::CSpectralSample > getSpectralSample();

		virtual std::vector< double >
		getBandPropertiesAtAngle( const FenestrationCommon::Property t_Property,
		                          const FenestrationCommon::Side t_Side, const double t_Angle ) const;

		std::vector< double > getBandWavelengths();
		size_t getBandSize();
		// Return index of wavelength range for passed value. Returns -1 if index is out of range
		int getBandIndex( const double t_Wavelength );

		double getMinLambda() const;
		double getMaxLambda() const;

	protected:
		double m_MinLambda;
		double m_MaxLambda;

		// Set state in order not to calculate wavelengths every time
		virtual std::vector< double > calculateBandWavelengths() = 0;
		bool m_WavelengthsCalculated;
		std::vector< double > m_Wavelengths;

	};

	//////////////////////////////////////////////////////////////////////////////////////////
	///   CMaterialSingleBand
	//////////////////////////////////////////////////////////////////////////////////////////

	// Simple material with no angular dependence on reflection or transmittance. This is mainly used 
	// for shading device materials
	class CMaterialSingleBand : public CMaterial {
	public:
		CMaterialSingleBand( const double t_Tf, const double t_Tb, const double t_Rf, const double t_Rb,
		                     const double minLambda, const double maxLambda );
		CMaterialSingleBand( const double t_Tf, const double t_Tb, const double t_Rf, const double t_Rb,
		                     const FenestrationCommon::WavelengthRange t_Range );

		double getProperty( const FenestrationCommon::Property t_Property,
		                    const FenestrationCommon::Side t_Side ) const;

		std::vector< double >
		getBandProperties( const FenestrationCommon::Property t_Property, const FenestrationCommon::Side t_Side ) const;

	private:
		std::vector< double > calculateBandWavelengths();

	protected:
		std::map< FenestrationCommon::Side, std::shared_ptr< CSurface > > m_Property;

	};

	//////////////////////////////////////////////////////////////////////////////////////////
	///   CMaterialDualBand
	//////////////////////////////////////////////////////////////////////////////////////////

	// Material that for given solar and partial range (visible, uv) will calculate equivalent optical
	// properties for the entire range
	class CMaterialDualBand : public CMaterial {

	public:
		// ratio is calculated outside of the class and can be provided here.
		// TODO: Need to confirm with the team if we actually need this approach 
		// (ratio should be calculated and not quessed)
		CMaterialDualBand( const std::shared_ptr< CMaterial >& t_PartialRange,
		                   const std::shared_ptr< CMaterial >& t_SolarRange, const double t_Ratio );

		// ratio is calculated based on provided solar radiation values
		CMaterialDualBand( const std::shared_ptr< CMaterial >& t_PartialRange,
		                   const std::shared_ptr< CMaterial >& t_SolarRange,
		                   const std::shared_ptr< FenestrationCommon::CSeries >& t_SolarRadiation );

		CMaterialDualBand( const std::shared_ptr< CMaterial >& t_PartialRange,
		                   const std::shared_ptr< CMaterial >& t_SolarRange );

		virtual void setSourceData( std::shared_ptr< FenestrationCommon::CSeries > t_SourceData );

		double getProperty( const FenestrationCommon::Property t_Property,
		                    const FenestrationCommon::Side t_Side ) const;

		std::vector< double >
		getBandProperties( const FenestrationCommon::Property t_Property,
		                   const FenestrationCommon::Side t_Side ) const;

	private:
		std::vector< double > calculateBandWavelengths();
		// Checks if material is within valid range. Otherwise, algorithm is not valid.
		void checkIfMaterialWithingSolarRange( const CMaterial& t_Material ) const;
		void createUVRange();

		// Creates after UV range and stores data into m_Materials
		void createNIRRange( const std::shared_ptr< CMaterial >& t_PartialRange,
		                     const CMaterial& t_SolarRange, const double t_Fraction );

		// Properties over the rest of range will depend on partial range as well.
		// We do want to keep correct properties of partial range, but will want to update
		// properties for other partial ranges that are not provided by the user.
		double getModifiedProperty( const double t_Range, const double t_Solar, const double t_Fraction ) const;

		std::shared_ptr< CMaterial > m_MaterialFullRange;
		std::shared_ptr< CMaterial > m_MaterialPartialRange;

		std::vector< std::shared_ptr< CMaterial > > m_Materials;

	};

	//////////////////////////////////////////////////////////////////////////////////////////
	///   CMaterialSample
	//////////////////////////////////////////////////////////////////////////////////////////

	// Material that contains data measured over the range of wavelengths. It also provides material properties
	// at certain angle. Assumes that material properties at certain angle can be calculated by using coated and
	// uncoated algorithms
	class CMaterialSample : public CMaterial {
	public:
		CMaterialSample( const std::shared_ptr< SpectralAveraging::CSpectralSample >& t_SpectralSample,
		                 const double t_Thickness, const FenestrationCommon::MaterialType t_Type,
		                 const double minLambda, const double maxLambda );

		CMaterialSample( const std::shared_ptr< SpectralAveraging::CSpectralSample >& t_SpectralSample,
		                 const double t_Thickness, const FenestrationCommon::MaterialType t_Type,
		                 const FenestrationCommon::WavelengthRange t_Range );

		virtual void setSourceData( std::shared_ptr< FenestrationCommon::CSeries > t_SourceData );

		// In this case sample property is taken. Standard spectral data file contains T, Rf, Rb that is 
		// measured at certain wavelengths.
		double getPropertyAtAngle( const FenestrationCommon::Property t_Property,
		                           const FenestrationCommon::Side t_Side, const double t_Angle ) const;
		double getProperty( const FenestrationCommon::Property t_Property, const FenestrationCommon::Side t_Side ) const;

		// Get properties at each wavelength and at given incident angle
		std::vector< double >
		getBandPropertiesAtAngle( const FenestrationCommon::Property t_Property,
		                          const FenestrationCommon::Side t_Side, const double t_Angle ) const;

		std::vector< double >
		getBandProperties( const FenestrationCommon::Property t_Property, const FenestrationCommon::Side t_Side ) const;

	private:
		std::vector< double > calculateBandWavelengths();
		std::shared_ptr< SpectralAveraging::CAngularSpectralSample > m_AngularSample;

	};

	//////////////////////////////////////////////////////////////////////////////////////////
	///   CMaterialMeasured
	//////////////////////////////////////////////////////////////////////////////////////////

	// Material that contains data measured over the range of wavelengths. It also provides material properties
	// at certain angle. Assumes that material properties at certain angle can be calculated by using coated and
	// uncoated algorithms
	class CMaterialMeasured : public CMaterial {
	public:
		CMaterialMeasured( const std::shared_ptr< SpectralAveraging::CAngularMeasurements >& t_Measurements,
		                   const double minLambda, const double maxLambda );

		CMaterialMeasured( const std::shared_ptr< SpectralAveraging::CAngularMeasurements >& t_Measurements,
		                   const FenestrationCommon::WavelengthRange t_Range );

		virtual void setSourceData( std::shared_ptr< FenestrationCommon::CSeries > t_SourceData );

		// In this case sample property is taken. Standard spectral data file contains T, Rf, Rb that is 
		// measured at certain wavelengths.
		double getPropertyAtAngle( const FenestrationCommon::Property t_Property,
		                           const FenestrationCommon::Side t_Side, const double t_Angle ) const;
		double getProperty( const FenestrationCommon::Property t_Property, const FenestrationCommon::Side t_Side ) const;

		// Get properties at each wavelength and at given incident angle
		std::vector< double >
		getBandPropertiesAtAngle( const FenestrationCommon::Property t_Property,
		                          const FenestrationCommon::Side t_Side, const double t_Angle ) const;

		std::vector< double >
		getBandProperties( const FenestrationCommon::Property t_Property, const FenestrationCommon::Side t_Side ) const;

	private:
		std::vector< double > calculateBandWavelengths();
		std::shared_ptr< SpectralAveraging::CAngularMeasurements > m_AngularMeasurements;

	};
}

#endif
