#pragma once

#include "IGUConfigurations.hpp"


namespace Tarcog
{
    namespace ISO15099
    {
        class SimpleIGU : public IIGUSystem
        {
        public:
            SimpleIGU(double uValue, double shgc, double h);

            double getUValue() override;
            double getSHGC(double t_TotSol) override;
            double getH(System system, Environment environment) const override;

            void setWidth(double width) override;
            void setHeight(double height) override;
            void setTilt(double tilt) override;
            void setWidthAndHeight(double width, double height) override;
            void setInteriorAndExteriorSurfacesHeight(double height) override;

        private:
            double m_UValue;
            double m_SHGC;
            double m_H;
        };
    }   // namespace ISO15099
}   // namespace Tarcog
