#include "SimpleIGU.hpp"

namespace Tarcog
{
    namespace ISO15099
    {
        SimpleIGU::SimpleIGU(double uValue, double shgc, double h) :
            m_UValue(uValue),
            m_SHGC(shgc),
            m_H(h)
        {}

        double SimpleIGU::getUValue()
        {
            return m_UValue;
        }

        double SimpleIGU::getSHGC(double)
        {
            return m_SHGC;
        }

        double SimpleIGU::getH(System, Environment) const
        {
            return m_H;
        }

        void SimpleIGU::setWidth(double)
        {}

        void SimpleIGU::setHeight(double)
        {}

        void SimpleIGU::setWidthAndHeight(double, double)
        {}

        void SimpleIGU::setInteriorAndExteriorSurfacesHeight(double)
        {}

        void SimpleIGU::setTilt(double)
        {}
    }   // namespace ISO15099
}   // namespace Tarcog
