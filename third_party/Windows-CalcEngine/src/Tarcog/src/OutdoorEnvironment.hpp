#ifndef TAROUTDOORENVIRONEMENT_H
#define TAROUTDOORENVIRONEMENT_H

#include "TarcogConstants.hpp"
#include "Environment.hpp"

namespace Tarcog
{
    namespace ISO15099
    {
        enum class SkyModel
        {
            AllSpecified,
            TSkySpecified,
            Swinbank
        };

        class COutdoorEnvironment : public CEnvironment
        {
        public:
            COutdoorEnvironment(
              double t_AirTemperature,
              double t_AirSpeed,
              double t_DirectSolarRadiation,
              AirHorizontalDirection t_AirDirection,
              double t_SkyTemperature,
              SkyModel t_Model,
              double t_Pressure = 101325,
              double t_FractionClearSky = TarcogConstants::DEFAULT_FRACTION_OF_CLEAR_SKY);

            void connectToIGULayer(const std::shared_ptr<CBaseLayer> & t_IGULayer) override;

            std::shared_ptr<CBaseLayer> clone() const override;
            std::shared_ptr<CEnvironment> cloneEnvironment() const override;

            void setSolarRadiation(double t_SolarRadiation);
            double getSolarRadiation() const;

        private:
            double getGasTemperature() override;
            double calculateIRFromVariables() override;
            void calculateConvectionOrConductionFlow() override;

            void calculateHc();
            double getHr() override;
            double getRadiationTemperature() const override;

            void setIRFromEnvironment(double t_IR) override;
            double getIRFromEnvironment() const override;

            double m_Tsky;
            double m_FractionOfClearSky;
            SkyModel m_SkyModel;
        };
    }   // namespace ISO15099

}   // namespace Tarcog

#endif
