#include "SimpleIGU.hpp"

namespace Tarcog
{
    namespace ISO15099
    {
        SimpleIGU::SimpleIGU(double uValue, double shgc, double hc) :
          m_UValue(uValue), m_SHGC(shgc), m_Hc(hc)
        {}

        double SimpleIGU::getUValue()
        {
            return m_UValue;
        }

        double SimpleIGU::getSHGC(double)
        {
            return m_SHGC;
        }

        double SimpleIGU::getHc(System, Environment) const
        {
            return m_Hc;
        }

        void SimpleIGU::setWidth(double)
        {}

        void SimpleIGU::setHeight(double)
        {}

        void SimpleIGU::setWidthAndHeight(double, double)
        {}

        void SimpleIGU::setInteriorAndExteriorSurfacesHeight(double)
        {}
    }   // namespace ISO15099
}   // namespace Tarcog
