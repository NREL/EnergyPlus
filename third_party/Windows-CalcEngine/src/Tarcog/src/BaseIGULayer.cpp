#include <memory>
#include <cassert>

#include "BaseIGULayer.hpp"
#include "Surface.hpp"

using FenestrationCommon::Side;

namespace Tarcog
{
    namespace ISO15099
    {
        CBaseIGULayer::CBaseIGULayer(double const t_Thickness) : m_Thickness(t_Thickness)
        {}

        double CBaseIGULayer::layerTemperature()
        {
            return (getTemperature(Side::Front) + getTemperature(Side::Back)) / 2;
        }

        double CBaseIGULayer::getThickness() const
        {
            return m_Thickness + getSurface(Side::Front)->getMeanDeflection()
                   - getSurface(Side::Back)->getMeanDeflection();
        }

        double CBaseIGULayer::getTemperature(Side const t_Position) const
        {
            return getSurface(t_Position)->getTemperature();
        }

        double CBaseIGULayer::J(Side const t_Position) const
        {
            return getSurface(t_Position)->J();
        }

        double CBaseIGULayer::getMaxDeflection() const
        {
            assert(getSurface(Side::Front)->getMaxDeflection()
                   == getSurface(Side::Back)->getMaxDeflection());
            return getSurface(Side::Front)->getMaxDeflection();
        }

        double CBaseIGULayer::getMeanDeflection() const
        {
            assert(getSurface(Side::Front)->getMeanDeflection()
                   == getSurface(Side::Back)->getMeanDeflection());
            return getSurface(Side::Front)->getMeanDeflection();
        }

        double CBaseIGULayer::getConductivity()
        {
            return getConductionConvectionCoefficient() * m_Thickness;
        }

        double CBaseIGULayer::getEffectiveThermalConductivity()
        {
            return std::abs(getHeatFlow() * m_Thickness
                            / (m_Surface.at(FenestrationCommon::Side::Front)->getTemperature()
                               - m_Surface.at(FenestrationCommon::Side::Back)->getTemperature()));
        }

    }   // namespace ISO15099

}   // namespace Tarcog
