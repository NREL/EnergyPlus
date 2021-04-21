#ifndef TARIGUSOLIDLAYER_H
#define TARIGUSOLIDLAYER_H

#include <memory>
#include "BaseIGULayer.hpp"

namespace FenestrationCommon
{
    enum class Side;
}

namespace Tarcog
{
    namespace ISO15099
    {
        class CIGUSolidLayer : public CBaseIGULayer
        {
        public:
            CIGUSolidLayer(double t_Thickness,
                           double t_Conductivity,
                           const std::shared_ptr<Tarcog::ISO15099::ISurface> & t_FrontSurface = nullptr,
                           const std::shared_ptr<Tarcog::ISO15099::ISurface> & t_BackSurface = nullptr);

            CIGUSolidLayer(double t_Thickness,
                           double t_Conductivity,
                           double t_FrontEmissivity,
                           double t_FrontIRTransmittance,
                           double t_BackEmissivity,
                           double t_BackIRTransmittance);

            void connectToBackSide(const std::shared_ptr<CBaseLayer> & t_Layer) override;

            double getConductance() const;
            double getSolarAbsorptance() const;

            void setLayerState(double t_Tf, double t_Tb, double t_Jf, double t_Jb);
            void setSolarRadiation(double t_SolarRadiation);
            void setSolarAbsorptance(double t_SolarAbsorptance, double t_SolarRadiation);

            // Radiation flow in solid layer should be eliminated
            double getRadiationFlow() override;

            virtual bool isDeflected() const;

            std::shared_ptr<CBaseLayer> clone() const override;

        protected:
            virtual void calculateConvectionOrConductionFlow() override;

            double m_Conductivity;

        private:
            void setSurfaceState(double t_Temperature,
                                 double t_J,
                                 FenestrationCommon::Side t_Position);
            
            double m_SolarAbsorptance;
        };

    }   // namespace ISO15099

}   // namespace Tarcog

#endif
