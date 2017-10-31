#include <stdexcept>
#include <cassert>

#include "SpectralSample.hpp"
#include "MeasuredSampleData.hpp"
#include "WCECommon.hpp"


using namespace FenestrationCommon;

namespace SpectralAveraging {

	//////////////////////////////////////////////////////////////////////////////////////
	////  CSample
	//////////////////////////////////////////////////////////////////////////////////////

	CSample::CSample( std::shared_ptr< CSeries > const& t_SourceData ) : m_SourceData( t_SourceData ),
	                                                                m_WavelengthSet( WavelengthSet::Data ), m_IntegrationType( IntegrationType::Trapezoidal ),
	                                                                m_StateCalculated( false ) {
		m_DetectorData = nullptr;
		CSample::reset();
	}

	CSample::CSample() : m_SourceData( nullptr ), m_DetectorData( nullptr ),
	                     m_WavelengthSet( WavelengthSet::Data ), m_IntegrationType( IntegrationType::Trapezoidal ), 
						 m_StateCalculated( false ) {
		CSample::reset();
	}

	CSample &CSample::operator=( CSample const &t_Sample ) {
		m_StateCalculated = t_Sample.m_StateCalculated;
		m_WavelengthSet = t_Sample.m_WavelengthSet;
		m_IncomingSource = wce::make_unique< CSeries >( *t_Sample.m_IncomingSource );
		m_TransmittedSource = wce::make_unique< CSeries >( *t_Sample.m_TransmittedSource );
		m_ReflectedFrontSource = wce::make_unique< CSeries >( *t_Sample.m_ReflectedFrontSource );
		m_ReflectedBackSource = wce::make_unique< CSeries >( *t_Sample.m_ReflectedBackSource );
		m_AbsorbedFrontSource = wce::make_unique< CSeries >( *t_Sample.m_AbsorbedFrontSource );
		m_AbsorbedBackSource = wce::make_unique< CSeries >( *t_Sample.m_AbsorbedBackSource );

		return *this;
	}

	CSample::CSample( CSample const & t_Sample ) {
		operator=( t_Sample );
	}

	std::shared_ptr< CSeries > CSample::getSourceData() {
		calculateState(); // must interpolate data to same wavelengths
		return m_SourceData;
	}

	void CSample::setSourceData( std::shared_ptr< CSeries > t_SourceData ) {
		m_SourceData = t_SourceData;
		reset();
	}

	void CSample::setDetectorData( std::shared_ptr< CSeries > const& t_DetectorData ) {
		m_DetectorData = t_DetectorData;
		reset();
	}

	void CSample::assignDetectorAndWavelengths( std::shared_ptr< CSample > const& t_Sample ) {
		m_DetectorData = t_Sample->m_DetectorData;
		m_Wavelengths = t_Sample->m_Wavelengths;
		m_WavelengthSet = t_Sample->m_WavelengthSet;
	}

	void CSample::setWavelengths( WavelengthSet const t_WavelengthSet,
	                              std::shared_ptr< std::vector< double > > const& t_Wavelenghts ) {
		m_WavelengthSet = t_WavelengthSet;
		switch ( t_WavelengthSet ) {
		case WavelengthSet::Custom:
			if ( t_Wavelenghts == nullptr ) {
				throw std::runtime_error( "Need to provide custom wavelength set." );
			}
			m_Wavelengths = *t_Wavelenghts;
			break;
		case WavelengthSet::Source:
			if ( m_SourceData == nullptr ) {
				throw std::runtime_error( "Cannot extract wavelenghts from source. Source is empty." );
			}
			m_Wavelengths = m_SourceData->getXArray();
			break;
		case WavelengthSet::Data:
			m_Wavelengths = getWavelengthsFromSample();
			break;
		default:
			throw std::runtime_error( "Incorrect definition of wavelength set source." );
			break;
		}
		reset();
	}

	double CSample::getEnergy( double const minLambda, double const maxLambda, Property const t_Property,
	                           Side const t_Side ) {
		calculateState();
		auto Energy = 0.0;
		switch ( t_Property ) {
		case Property::T:
			Energy = m_TransmittedSource->sum( minLambda, maxLambda );
			break;
		case Property::R:
			switch ( t_Side ) {
			case Side::Front:
				Energy = m_ReflectedFrontSource->sum( minLambda, maxLambda );
				break;
			case Side::Back:
				Energy = m_ReflectedBackSource->sum( minLambda, maxLambda );
				break;
			default:
				assert("Incorrect selection of sample side.");
				break;
			}
			break;
		case Property::Abs:
			switch ( t_Side ) {
			case Side::Front:
				Energy = m_AbsorbedFrontSource->sum( minLambda, maxLambda );
				break;
			case Side::Back:
				Energy = m_AbsorbedBackSource->sum( minLambda, maxLambda );
				break;
			default:
				assert("Incorrect selection of sample side.");
				break;
			}
			break;
		default:
			assert("Incorrect selection of sample property.");
			break;
		}

		return Energy;
	}

	double CSample::getProperty( double const minLambda, double const maxLambda, Property const t_Property,
	                             Side const t_Side ) {
		calculateState();
		auto Prop = 0.0;
		// Incoming energy can be calculated only if user has defined incoming source.
		// Otherwise just assume zero property.
		if ( m_IncomingSource != nullptr ) {
			auto incomingEnergy = m_IncomingSource->sum( minLambda, maxLambda );
			double propertyEnergy = 0;
			switch ( t_Property ) {
			case Property::T:
				propertyEnergy = m_TransmittedSource->sum( minLambda, maxLambda );
				break;
			case Property::R:
				switch ( t_Side ) {
				case Side::Front:
					propertyEnergy = m_ReflectedFrontSource->sum( minLambda, maxLambda );
					break;
				case Side::Back:
					propertyEnergy = m_ReflectedBackSource->sum( minLambda, maxLambda );
					break;
				default:
					assert( "Incorrect selection of sample side." );
					break;
				}
				break;
			case Property::Abs:
				switch ( t_Side ) {
				case Side::Front:
					propertyEnergy = m_AbsorbedFrontSource->sum( minLambda, maxLambda );
					break;
				case Side::Back:
					propertyEnergy = m_AbsorbedBackSource->sum( minLambda, maxLambda );
					break;
				default:
					assert( "Incorrect selection of sample side." );
					break;
				}
				break;
			default:
				throw std::runtime_error( "Incorrect selection of sample property." );
				break;
			}

			Prop = propertyEnergy / incomingEnergy;
		}
		return Prop;
	}

	CSeries* CSample::getEnergyProperties( Property const t_Property, Side const t_Side ) {
		calculateState();

		CSeries* aProperty = nullptr;
		switch ( t_Property ) {
		case Property::T:
			aProperty = m_TransmittedSource.get();
			break;
		case Property::R:
			switch ( t_Side ) {
			case Side::Front:
				aProperty = m_ReflectedFrontSource.get();
				break;
			case Side::Back:
				aProperty = m_ReflectedBackSource.get();
				break;
			default:
				assert("Incorrect selection of sample side.");
				break;
			}
			break;
		case Property::Abs:
			switch ( t_Side ) {
			case Side::Front:
				aProperty = m_AbsorbedFrontSource.get();
				break;
			case Side::Back:
				aProperty = m_AbsorbedBackSource.get();
				break;
			default:
				assert("Incorrect selection of sample side.");
				break;
			}
			break;
		default:
			throw std::runtime_error( "Incorrect selection of sample property." );
			break;
		}

		return aProperty;
	}

	size_t CSample::getBandSize() const {
		return m_Wavelengths.size();
	}

	void CSample::reset() {
		m_StateCalculated = false;
		m_IncomingSource = nullptr;
		m_TransmittedSource = nullptr;
		m_ReflectedFrontSource = nullptr;
		m_ReflectedBackSource = nullptr;
		m_AbsorbedFrontSource = nullptr;
		m_AbsorbedBackSource = nullptr;
	}

	void CSample::calculateState() {
		if ( !m_StateCalculated ) {
			if ( m_WavelengthSet != WavelengthSet::Custom ) {
				setWavelengths( m_WavelengthSet );
			}

			// In case source data are set then apply solar radiation to the calculations.
			// Otherwise, just use measured data.
			if ( m_SourceData != nullptr ) {

				m_IncomingSource = m_SourceData->interpolate( m_Wavelengths );


				if ( m_DetectorData != nullptr ) {
					auto interpolatedDetector = *m_DetectorData->interpolate( m_Wavelengths );
					m_IncomingSource = m_IncomingSource->mMult( interpolatedDetector );
				}

				calculateProperties();

				m_IncomingSource = m_IncomingSource->integrate( m_IntegrationType );
				m_TransmittedSource = m_TransmittedSource->integrate( m_IntegrationType );
				m_ReflectedFrontSource = m_ReflectedFrontSource->integrate( m_IntegrationType );
				m_ReflectedBackSource = m_ReflectedBackSource->integrate( m_IntegrationType );
				m_AbsorbedFrontSource = m_AbsorbedFrontSource->integrate( m_IntegrationType );
				m_AbsorbedBackSource = m_AbsorbedBackSource->integrate( m_IntegrationType );

				m_StateCalculated = true;
			}
		}
	}

	//////////////////////////////////////////////////////////////////////////////////////
	////  CSpectralSample
	//////////////////////////////////////////////////////////////////////////////////////

	CSpectralSample::CSpectralSample( std::shared_ptr< CSpectralSampleData > const& t_SampleData,
	                                  std::shared_ptr< CSeries > const& t_SourceData ) :
		CSample( t_SourceData ), m_SampleData( t_SampleData ) {
		if ( t_SampleData == nullptr ) {
			throw std::runtime_error( "Sample must have measured data." );
		}
		setWavelengths( m_WavelengthSet );
		m_Transmittance = nullptr;
		m_RefFront = nullptr;
		m_RefBack = nullptr;
		m_AbsFront = nullptr;
		m_AbsBack = nullptr;
	}

	CSpectralSample::CSpectralSample( std::shared_ptr< CSpectralSampleData > const& t_SampleData ) :
		CSample(), m_SampleData( t_SampleData ) {
		if ( t_SampleData == nullptr ) {
			throw std::runtime_error( "Sample must have measured data." );
		}
		setWavelengths( m_WavelengthSet );
		m_Transmittance = nullptr;
		m_RefFront = nullptr;
		m_RefBack = nullptr;
		m_AbsFront = nullptr;
		m_AbsBack = nullptr;
	}

	std::shared_ptr< CSpectralSampleData > CSpectralSample::getMeasuredData() {
		calculateState(); // Interpolation is needed before returning the data
		return m_SampleData;
	}

	std::vector< double > CSpectralSample::getWavelengthsFromSample() const {
		return m_SampleData->getWavelengths();
	}

	std::shared_ptr< CSeries > CSpectralSample::getWavelengthsProperty( Property const t_Property, Side const t_Side ) {
		calculateState();
		std::shared_ptr< CSeries > aProperty = nullptr;
		switch ( t_Property ) {
		case Property::T:
			aProperty = m_Transmittance;
			break;
		case Property::R:
			switch ( t_Side ) {
			case Side::Front:
				aProperty = m_RefFront;
				break;
			case Side::Back:
				aProperty = m_RefBack;
				break;
			default:
				assert("Incorrect selection of Side.");
				break;
			}
			break;
		case Property::Abs:
			switch ( t_Side ) {
			case Side::Front:
				aProperty = m_AbsFront;
				break;
			case Side::Back:
				aProperty = m_AbsBack;
				break;
			default:
				assert("Incorrect selection of Side.");
				break;
			}
			break;
		default:
			assert("Non existent property requested.");
			break;
		}

		return aProperty;
	}

	void CSpectralSample::calculateProperties() {

		// No need to do interpolation if wavelength set is already from the data.
		if ( m_WavelengthSet == WavelengthSet::Data ) {
			m_Transmittance = m_SampleData->properties( SampleData::T );
			m_RefFront = m_SampleData->properties( SampleData::Rf );
			m_RefBack = m_SampleData->properties( SampleData::Rb );
			m_AbsFront = m_SampleData->properties( SampleData::AbsF );
			m_AbsBack = m_SampleData->properties( SampleData::AbsB );
		}
		else {
			m_Transmittance = m_SampleData->properties( SampleData::T )->interpolate( m_Wavelengths );
			m_RefFront = m_SampleData->properties( SampleData::Rf )->interpolate( m_Wavelengths );
			m_RefBack = m_SampleData->properties( SampleData::Rb )->interpolate( m_Wavelengths );
			m_AbsFront = m_SampleData->properties( SampleData::AbsF )->interpolate( m_Wavelengths );
			m_AbsBack = m_SampleData->properties( SampleData::AbsB )->interpolate( m_Wavelengths );
		}

		assert( m_IncomingSource != nullptr );

		// Calculation of energy balances
		m_TransmittedSource = m_Transmittance->mMult( *m_IncomingSource );
		m_ReflectedFrontSource = m_RefFront->mMult( *m_IncomingSource );
		m_ReflectedBackSource = m_RefBack->mMult( *m_IncomingSource );
		m_AbsorbedFrontSource = m_AbsFront->mMult( *m_IncomingSource );
		m_AbsorbedBackSource = m_AbsBack->mMult( *m_IncomingSource );
	}

	void CSpectralSample::calculateState() {
		CSample::calculateState();
		if ( m_SourceData == nullptr ) {
			// TODO: Make sure that interpolation is necessary here.
			// It slows down program for quite a bit

			//m_SampleData->interpolate( *m_Wavelengths );

			m_Transmittance = m_SampleData->properties( SampleData::T );
			m_RefFront = m_SampleData->properties( SampleData::Rf );
			m_RefBack = m_SampleData->properties( SampleData::Rb );
			m_AbsFront = m_SampleData->properties( SampleData::AbsF );
			m_AbsBack = m_SampleData->properties( SampleData::AbsB );

			m_StateCalculated = true;
		}
	}

	//////////////////////////////////////////////////////////////////////////////////////
	////  CSpectralAngleSample
	//////////////////////////////////////////////////////////////////////////////////////

	CSpectralAngleSample::CSpectralAngleSample( std::shared_ptr< CSpectralSample > const& t_Sample,
	                                            double const t_Angle ) :
		m_Sample( t_Sample ), m_Angle( t_Angle ) {

	}

	double CSpectralAngleSample::angle() const {
		return m_Angle;
	}

	std::shared_ptr< CSpectralSample > CSpectralAngleSample::sample() const {
		return m_Sample;
	}


}
