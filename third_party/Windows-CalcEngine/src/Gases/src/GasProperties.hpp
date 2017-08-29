#ifndef GASPROPERTIES_H
#define GASPROPERTIES_H

namespace Gases {

	enum class CoeffType { cCond, cVisc, cCp };

	class CIntCoeff {
	public:
		CIntCoeff();
		CIntCoeff( double const t_A, double const t_B, double const t_C );
		double interpolationValue( double const t_Temperature ) const;
		CIntCoeff& operator=( CIntCoeff const& t_IntCoeff );
	private:
		double m_A;
		double m_B;
		double m_C;
	};

	struct GasProperties {
		GasProperties() :
			m_ThermalConductivity( 0.0 ), m_Viscosity( 0.0 ), m_SpecificHeat( 0.0 ), m_Density( 0.0 ),
			m_MolecularWeight( 0.0 ), m_Alpha( 0.0 ), m_PrandlNumber( 0.0 ), m_PropertiesCalculated( false ) {
		};

		double getLambdaPrim() const;
		double getLambdaSecond() const;
		GasProperties& operator+( GasProperties const& t_A );
		GasProperties& operator+=( GasProperties const& t_A );
		GasProperties& operator=( GasProperties const& t_A );

		void calculateAlphaAndPrandl();
		double m_ThermalConductivity;
		double m_Viscosity;
		double m_SpecificHeat;
		double m_Density;
		double m_MolecularWeight;
		double m_Alpha;
		double m_PrandlNumber;
		bool m_PropertiesCalculated;
	};

}

#endif
