#ifndef TARINDOORENVIRONMENT_H
#define TARINDOORENVIRONMENT_H

#include "Environment.hpp"

namespace Tarcog {

	class CIndoorEnvironment : public CEnvironment {
	public:
		CIndoorEnvironment( double const t_AirTemperature, double const t_Pressure );
		CIndoorEnvironment( CIndoorEnvironment const& t_Indoor );
		CIndoorEnvironment & operator=( CIndoorEnvironment const & t_Environment );

		void connectToIGULayer( std::shared_ptr< CBaseLayer > const& t_IGULayer ) override;

		void setRoomRadiationTemperature( double const t_RadiationTemperature );

		std::shared_ptr< CBaseLayer > clone() const override;
		std::shared_ptr< CEnvironment > cloneEnvironment() const override;

	private:
		double getGasTemperature() override;
		double calculateIRFromVariables() override;
		void calculateConvectionOrConductionFlow() override;

		void calculateHc();
		double getHr() override;

		void setIRFromEnvironment( double const t_IR ) override;
		double getIRFromEnvironment() const override;

		double getRadiationTemperature() const override;

		double m_RoomRadiationTemperature;
	};

}

#endif
