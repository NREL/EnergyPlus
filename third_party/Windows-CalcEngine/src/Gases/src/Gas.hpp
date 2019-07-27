#ifndef GAS_H
#define GAS_H

#include <memory>
#include <vector>

namespace Gases {

	class CGasItem;
	class CGasData;
	struct GasProperties;

	class CGas {
	public:
		CGas();
		CGas( const CGas& t_Gas );
		void addGasItem( CGasItem const& t_GasItem );
		double totalPercent();
		std::shared_ptr< GasProperties > getSimpleGasProperties();
		std::shared_ptr< GasProperties > getGasProperties();
		void setTemperatureAndPressure( double const t_Temperature, double const t_Pressure );

		CGas& operator=( CGas const& t_Gas );
		bool operator==( CGas const& t_Gas ) const;
		bool operator!=( CGas const& t_Gas ) const;

	private:

		std::shared_ptr< GasProperties > getStandardPressureGasProperties();
		std::shared_ptr< GasProperties > getVacuumPressureGasProperties();

		double viscTwoGases( GasProperties const& t_Gas1Properties, GasProperties const& t_Gas2Properties ) const;
		double viscDenomTwoGases( CGasItem& t_GasItem1, CGasItem& t_GasItem2 ) const;

		double lambdaPrimTwoGases( GasProperties const& t_Gas1Properties, GasProperties const& t_Gas2Properties ) const;
		double lambdaSecondTwoGases( GasProperties const& t_Gas1Properties, GasProperties const& t_Gas2Properties ) const;

		double lambdaPrimDenomTwoGases( CGasItem& t_GasItem1, CGasItem& t_GasItem2 ) const;
		double lambdaSecondDenomTwoGases( CGasItem& t_GasItem1, CGasItem& t_GasItem2 ) const;

		std::vector< CGasItem > m_GasItem;
		std::shared_ptr< GasProperties > m_SimpleProperties;
		std::shared_ptr< GasProperties > m_Properties;

		bool m_DefaultGas;
		double m_Pressure;
	};

}

#endif
