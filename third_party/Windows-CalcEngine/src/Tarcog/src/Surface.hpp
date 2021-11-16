#ifndef SURFACE_H
#define SURFACE_H

#include <memory>
#include <vector>

namespace Tarcog
{
    namespace ISO15099
    {
        class ISurface
        {
        public:
            virtual ~ISurface() = default;
            ISurface();
            ISurface(double t_Emissivity, double t_Transmittance);

            ISurface(ISurface const & t_Surface);
            ISurface & operator=(ISurface const & t_Surface);

            virtual std::shared_ptr<ISurface> clone() const = 0;

            virtual void setTemperature(double t_Temperature);
            virtual void setJ(double t_J);

            void applyDeflection(double t_MeanDeflection, double t_MaxDeflection);
            virtual double getTemperature() const final;
            virtual double getEmissivity() const final;
            virtual double getReflectance() const final;
            virtual double getTransmittance() const final;
            virtual double J() const final;
            virtual double emissivePowerTerm() const final;
            virtual double getMeanDeflection() const final;
            virtual double getMaxDeflection() const final;

            void initializeStart(double t_Temperature);
            void initializeStart(double t_Temperature, double t_Radiation);

        protected:
            void calculateReflectance();

            double m_Temperature;
            double m_J;

            double m_Emissivity;
            double m_Reflectance;
            double m_Transmittance;

            // Value for deflection. Positive deflection is surface curved towards left side and
            // negative deflection vice-versa.
            double m_MeanDeflection;
            double m_MaxDeflection;
        };

        class CSurface : public ISurface
        {
        public:
            CSurface();
            CSurface(double t_Emissivity, double t_Transmittance);
            CSurface(const CSurface & t_Surface);

            std::shared_ptr<ISurface> clone() const override;
        };
    }   // namespace ISO15099

}   // namespace Tarcog

#endif
