#include <math.h>
#include <stdexcept>
#include <vector>

#include "BaseLayer.hpp"
#include "Surface.hpp"
#include "WCEGases.hpp"



namespace Tarcog {

	//////////////////////////////////////////////////////////////////////////////
	// ISurface
	//////////////////////////////////////////////////////////////////////////////
	ISurface::ISurface() : m_Temperature( 273.15 ), m_J( 0 ),
	                       m_Emissivity( 0.84 ), m_Transmittance( 0 ),
	                       m_MeanDeflection( 0 ), m_MaxDeflection( 0 ) {
		calculateReflectance();
	}

	ISurface::ISurface( double const t_Emissivity, double const t_Transmittance ) :
		m_Temperature( 273.15 ), m_J( 0 ),
		m_Emissivity( t_Emissivity ), m_Transmittance( t_Transmittance ),
		m_MeanDeflection( 0 ), m_MaxDeflection( 0 ) {
		calculateReflectance();
	}

	ISurface::ISurface( ISurface const& t_Surface ) {
		operator=( t_Surface );
	}

	ISurface & ISurface::operator=( ISurface const & t_Surface ) {
		m_Emissivity = t_Surface.m_Emissivity;
		m_Transmittance = t_Surface.m_Transmittance;
		m_Temperature = t_Surface.m_Temperature;
		m_J = t_Surface.m_J;
		m_MaxDeflection = t_Surface.m_MaxDeflection;
		m_MeanDeflection = t_Surface.m_MeanDeflection;
		calculateReflectance();

		return *this;
	}

	void ISurface::setTemperature( double const t_Temperature ) {
		m_Temperature = t_Temperature;
	}

	void ISurface::setJ( double const t_J ) {
		m_J = t_J;
	}

	void ISurface::applyDeflection( double const t_MeanDeflection, double const t_MaxDeflection ) {
		m_MeanDeflection = t_MeanDeflection;
		m_MaxDeflection = t_MaxDeflection;
	}

	double ISurface::getTemperature() const {
		return m_Temperature;
	}

	double ISurface::getEmissivity() const {
		return m_Emissivity;
	}

	double ISurface::getReflectance() const {
		return m_Reflectance;
	}

	double ISurface::getTransmittance() const {
		return m_Transmittance;
	}

	double ISurface::J() const {
		return m_J;
	}

	double ISurface::getMeanDeflection() const {
		return m_MeanDeflection;
	}

	double ISurface::getMaxDeflection() const {
		return m_MaxDeflection;
	}

	double ISurface::emissivePowerTerm() const {
		using ConstantsData::STEFANBOLTZMANN;

		return STEFANBOLTZMANN * m_Emissivity * pow( m_Temperature, 3 );
	}

	void ISurface::calculateReflectance() {
		if ( m_Emissivity + m_Transmittance > 1 ) {
			throw std::runtime_error( "Sum of emittance and transmittance cannot be greater than one." );
		}
		else {
			m_Reflectance = 1 - m_Emissivity - m_Transmittance;
		}
	}

	void ISurface::initializeStart( double const t_Temperature ) {
		using ConstantsData::STEFANBOLTZMANN;

		m_Temperature = t_Temperature;
		m_J = STEFANBOLTZMANN * pow( m_Temperature, 4 );
	}

	void ISurface::initializeStart( double const t_Temperature, double const t_Radiation ) {
		m_Temperature = t_Temperature;
		m_J = t_Radiation;
	}


	//////////////////////////////////////////////////////////////////////////////
	// CSurface
	//////////////////////////////////////////////////////////////////////////////
	CSurface::CSurface( double const t_Emissivity, double const t_Transmittance ) :
		ISurface( t_Emissivity, t_Transmittance ) {

	}

	CSurface::CSurface( CSurface const& t_Surface ) : ISurface( t_Surface ) {

	}

	std::shared_ptr< ISurface > CSurface::clone() const {
		return std::make_shared< CSurface >( *this );
	}

	CSurface::CSurface() : ISurface() {

	}

}
