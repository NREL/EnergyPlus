#include <stdexcept>

#include "GasProperties.hpp"
#include "GasData.hpp"



namespace Gases {
	CGasData::CGasData() : m_gasName( "Air" ), m_molWeight( 28.97 ), m_specificHeatRatio( 1.4 ) {
		m_Coefficients[ CoeffType::cCp ] = CIntCoeff( 1.002737e+03, 1.2324e-02, 0.0 );
		m_Coefficients[ CoeffType::cCond ] = CIntCoeff( 2.8733e-03, 7.76e-05, 0.0 );
		m_Coefficients[ CoeffType::cVisc ] = CIntCoeff( 3.7233e-06, 4.94e-08, 0.0 );
	}

	CGasData::CGasData( CGasData const& t_GasData ) {
		*this = t_GasData;
	}

	CGasData::CGasData(
		std::string const& t_Name,
		double const t_Wght,
		double const t_SpecHeatRatio,
		CIntCoeff const& t_Cp,
		CIntCoeff const& t_Con,
		CIntCoeff const& t_Visc ) :
		m_gasName( t_Name ), m_molWeight( t_Wght ), m_specificHeatRatio( t_SpecHeatRatio ) {
		m_Coefficients[ CoeffType::cCp ] = t_Cp;
		m_Coefficients[ CoeffType::cCond ] = t_Con;
		m_Coefficients[ CoeffType::cVisc ] = t_Visc;
	}

	CGasData& CGasData::operator=( CGasData const& t_GasData ) {
		m_gasName = t_GasData.m_gasName;
		m_molWeight = t_GasData.m_molWeight;
		m_specificHeatRatio = t_GasData.m_specificHeatRatio;
		m_Coefficients = t_GasData.m_Coefficients;

		return *this;
	}

	double CGasData::getPropertyValue( CoeffType const t_Type, double const t_Temperature ) const {
		return m_Coefficients.at( t_Type ).interpolationValue( t_Temperature );
	}

	double CGasData::getSpecificHeatRatio() const {
		return m_specificHeatRatio;
	}

	double CGasData::getMolecularWeight() const {
		return m_molWeight;
	}

}
