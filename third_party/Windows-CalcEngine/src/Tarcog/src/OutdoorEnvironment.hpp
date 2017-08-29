#ifndef TAROUTDOORENVIRONEMENT_H
#define TAROUTDOORENVIRONEMENT_H

#include "TarcogConstants.hpp"
#include "Environment.hpp"

namespace Tarcog {

	enum SkyModel { AllSpecified, TSkySpecified, Swinbank };

	class COutdoorEnvironment : public CEnvironment {
	public:
		COutdoorEnvironment( double const t_Temperature, double const t_Pressure, double const t_AirSpeed,
		                     double const t_DirectSolarRadiation, AirHorizontalDirection const t_AirDirection,
		                     double const t_SkyTemperature, SkyModel const t_Model,
		                     double const t_FractClearSky = TarcogConstants::DEFAULT_FRACTION_OF_CLEAR_SKY );

		COutdoorEnvironment( COutdoorEnvironment const& t_Outdoor );

		void connectToIGULayer( std::shared_ptr< CBaseLayer > const& t_IGULayer ) override;

		std::shared_ptr< CBaseLayer > clone() const override;
		std::shared_ptr< CEnvironment > cloneEnvironment() const override;

		void setSolarRadiation( double const t_SolarRadiation );
		double getSolarRadiation() const;

	private:
		double getGasTemperature() override;
		double calculateIRFromVariables() override;
		void calculateConvectionOrConductionFlow() override;

		void calculateHc();
		double getHr() override;
		double getRadiationTemperature() const override;

		void setIRFromEnvironment( double const t_IR ) override;
		double getIRFromEnvironment() const override;

		double m_Tsky;
		double m_FractionOfClearSky;
		SkyModel m_SkyModel;
	};

}

#endif
