#include <math.h>
#include "Constants.hpp"
#include "GasProperties.hpp"

namespace Gases {

	///////////////////////////////////////////////////////////////////////////////////////////
	///  IntCoeff
	///////////////////////////////////////////////////////////////////////////////////////////
	CIntCoeff::CIntCoeff( double const t_A, double const t_B, double const t_C ) :
		m_A( t_A ), m_B( t_B ), m_C( t_C ) {

	}

	CIntCoeff::CIntCoeff() : m_A( 0 ), m_B( 0 ), m_C( 0 ) {
	}

	double CIntCoeff::interpolationValue( double const t_Temperature ) const {
		return m_A + m_B * t_Temperature + m_C * pow( t_Temperature, 2 );
	}

	CIntCoeff::CIntCoeff( CIntCoeff const & t_IntCoeff ) {
		operator=( t_IntCoeff );
	}

	CIntCoeff& CIntCoeff::operator=( CIntCoeff const& t_IntCoeff ) {
		m_A = t_IntCoeff.m_A;
		m_B = t_IntCoeff.m_B;
		m_C = t_IntCoeff.m_C;

		return *this;
	}

	////////////////////////////////////////////////////////////////////////////////////////////
	//   GasProperties
	////////////////////////////////////////////////////////////////////////////////////////////
	GasProperties& GasProperties::operator+( GasProperties const& t_A ) {
		m_ThermalConductivity += t_A.m_ThermalConductivity;
		m_Viscosity += t_A.m_Viscosity;
		m_SpecificHeat += t_A.m_SpecificHeat;
		m_Density += t_A.m_Density;
		m_MolecularWeight += t_A.m_MolecularWeight;
		calculateAlphaAndPrandl();

		return *this;
	}

	GasProperties::GasProperties( GasProperties const & t_GasProperties ) {
		operator=( t_GasProperties );
	}

	double GasProperties::getLambdaPrim() const {
		using ConstantsData::UNIVERSALGASCONSTANT;

		return 15.0 / 4.0 * UNIVERSALGASCONSTANT / m_MolecularWeight * m_Viscosity;
	}

	double GasProperties::getLambdaSecond() const {
		return m_ThermalConductivity - getLambdaPrim();
	}

	GasProperties& GasProperties::operator+=( GasProperties const& t_A ) {
		*this = *this + t_A;
		return *this;
	}

	GasProperties& GasProperties::operator=( GasProperties const& t_A ) {
		m_ThermalConductivity = t_A.m_ThermalConductivity;
		m_Viscosity = t_A.m_Viscosity;
		m_SpecificHeat = t_A.m_SpecificHeat;
		m_Density = t_A.m_Density;
		m_MolecularWeight = t_A.m_MolecularWeight;
		m_Alpha = t_A.m_Alpha;
		m_PrandlNumber = t_A.m_PrandlNumber;
		m_PropertiesCalculated = t_A.m_PropertiesCalculated;

		return *this;
	}

	void GasProperties::calculateAlphaAndPrandl() {
		m_Alpha = m_ThermalConductivity / ( m_SpecificHeat * m_Density );
		m_PrandlNumber = m_Viscosity / m_Density / m_Alpha;
	}


}
