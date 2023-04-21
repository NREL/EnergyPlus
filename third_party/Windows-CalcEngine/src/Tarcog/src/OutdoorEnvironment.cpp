
#include <cmath>
#include <stdexcept>
#include <cassert>

#include "WCEGases.hpp"
#include "Surface.hpp"
#include "TarcogConstants.hpp"
#include "OutdoorEnvironment.hpp"
#include "WCECommon.hpp"

using FenestrationCommon::Side;

namespace Tarcog
{
    namespace ISO15099
    {
        COutdoorEnvironment::COutdoorEnvironment(double t_AirTemperature,
                                                 double t_AirSpeed,
                                                 double t_DirectSolarRadiation,
                                                 AirHorizontalDirection t_AirDirection,
                                                 double t_SkyTemperature,
                                                 SkyModel t_Model,
                                                 double t_Pressure,
                                                 double t_FractionClearSky) :
            CEnvironment(t_Pressure, t_AirSpeed, t_AirDirection),
            m_Tsky(t_SkyTemperature),
            m_FractionOfClearSky(t_FractionClearSky),
            m_SkyModel(t_Model)
        {
            m_Surface[Side::Front] = std::make_shared<CSurface>();
            m_Surface.at(Side::Front)->setTemperature(t_AirTemperature);
            m_DirectSolarRadiation = t_DirectSolarRadiation;
        }

        double COutdoorEnvironment::calculateIRFromVariables()
        {
            auto aEmissivity = 0.0;
            switch(m_SkyModel)
            {
                case SkyModel::AllSpecified:
                    aEmissivity = m_Emissivity * pow(m_Tsky, 4) / pow(getAirTemperature(), 4);
                    break;
                case SkyModel::TSkySpecified:
                    aEmissivity = pow(m_Tsky, 4) / pow(getAirTemperature(), 4);
                    break;
                case SkyModel::Swinbank:
                    aEmissivity = 5.31e-13 * pow(getAirTemperature(), 6)
                                  / (ConstantsData::STEFANBOLTZMANN * pow(getAirTemperature(), 4));
                    break;
                default:
                    throw std::runtime_error("Incorrect sky model specified.");
            }

            auto radiationTemperature = 0.0;
            if(m_HCoefficientModel == BoundaryConditionsCoeffModel::HPrescribed)
            {
                radiationTemperature = getAirTemperature();
            }
            else
            {
                using ConstantsData::WCE_PI;

                auto fSky = (1 + cos(m_Tilt * WCE_PI / 180)) / 2;
                auto fGround = 1 - fSky;
                auto eZero = fGround + (1 - m_FractionOfClearSky) * fSky
                             + fSky * m_FractionOfClearSky * aEmissivity;
                radiationTemperature = getAirTemperature() * pow(eZero, 0.25);
            }

            return ConstantsData::STEFANBOLTZMANN * pow(radiationTemperature, 4);
        }

        void COutdoorEnvironment::connectToIGULayer(std::shared_ptr<CBaseLayer> const & t_IGULayer)
        {
            this->connectToBackSide(t_IGULayer);
            m_Surface[Side::Back] = t_IGULayer->getSurface(Side::Front);
        }

        std::shared_ptr<CBaseLayer> COutdoorEnvironment::clone() const
        {
            return cloneEnvironment();
        }

        std::shared_ptr<CEnvironment> COutdoorEnvironment::cloneEnvironment() const
        {
            return std::make_shared<COutdoorEnvironment>(*this);
        }

        void COutdoorEnvironment::setSolarRadiation(double const t_SolarRadiation)
        {
            m_DirectSolarRadiation = t_SolarRadiation;
        }

        double COutdoorEnvironment::getSolarRadiation() const
        {
            return m_DirectSolarRadiation;
        }

        double COutdoorEnvironment::getGasTemperature()
        {
            assert(m_Surface.at(Side::Front) != nullptr);
            return m_Surface.at(Side::Front)->getTemperature();
        }

        void COutdoorEnvironment::calculateConvectionOrConductionFlow()
        {
            switch(m_HCoefficientModel)
            {
                case BoundaryConditionsCoeffModel::CalculateH:
                {
                    calculateHc();
                    break;
                }
                case BoundaryConditionsCoeffModel::HPrescribed:
                {
                    auto hr = getHr();
                    m_ConductiveConvectiveCoeff = m_HInput - hr;
                    break;
                }
                case BoundaryConditionsCoeffModel::HcPrescribed:
                {
                    m_ConductiveConvectiveCoeff = m_HInput;
                    break;
                }
                default:
                {
                    throw std::runtime_error(
                      "Incorrect definition for convection model (Outdoor environment).");
                }
            }
        }

        void COutdoorEnvironment::calculateHc()
        {
            m_ConductiveConvectiveCoeff = 4 + 4 * m_AirSpeed;
        }

        double COutdoorEnvironment::getHr()
        {
            assert(m_Surface.at(Side::Back) != nullptr);
            assert(m_Surface.at(Side::Front) != nullptr);
            return getRadiationFlow()
                   / (m_Surface.at(Side::Back)->getTemperature() - getRadiationTemperature());
        }

        double COutdoorEnvironment::getRadiationTemperature() const
        {
            assert(m_Surface.at(Side::Front) != nullptr);
            return pow(m_Surface.at(Side::Front)->J() / ConstantsData::STEFANBOLTZMANN, 0.25);
        }

        void COutdoorEnvironment::setIRFromEnvironment(double const t_IR)
        {
            assert(m_Surface.at(Side::Front) != nullptr);
            m_Surface.at(Side::Front)->setJ(t_IR);
        }

        double COutdoorEnvironment::getIRFromEnvironment() const
        {
            assert(m_Surface.at(Side::Front) != nullptr);
            return m_Surface.at(Side::Front)->J();
        }

    }   // namespace ISO15099

}   // namespace Tarcog
