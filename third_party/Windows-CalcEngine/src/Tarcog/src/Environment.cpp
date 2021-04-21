#include "Environment.hpp"
#include "TarcogConstants.hpp"


namespace Tarcog
{
    namespace ISO15099
    {

        CEnvironment::CEnvironment(double t_Pressure,
                                   double t_AirSpeed,
                                   AirHorizontalDirection t_AirDirection) :
            CGasLayer(t_Pressure, t_AirSpeed, t_AirDirection),
            m_DirectSolarRadiation(0),
            m_Emissivity(TarcogConstants::DEFAULT_ENV_EMISSIVITY),
            m_HInput(0),
            m_HCoefficientModel(BoundaryConditionsCoeffModel::CalculateH),
            m_IRCalculatedOutside(false)
        {
            m_ForcedVentilation =
              ForcedVentilation();   // Creates forced ventilation with zero values
        }

        CEnvironment::CEnvironment(const CEnvironment & t_Environment) :
            CState(t_Environment),
            CBaseLayer(t_Environment),
            CGasLayer(t_Environment)
        {
            operator=(t_Environment);
        }

        CEnvironment & CEnvironment::operator=(CEnvironment const & t_Environment)
        {
            CState::operator=(t_Environment);
            CBaseLayer::operator=(t_Environment);
            CGasLayer::operator=(t_Environment);
            m_DirectSolarRadiation = t_Environment.m_DirectSolarRadiation;
            m_Emissivity = t_Environment.m_Emissivity;
            m_HInput = t_Environment.m_HInput;
            m_HCoefficientModel = t_Environment.m_HCoefficientModel;
            m_IRCalculatedOutside = t_Environment.m_IRCalculatedOutside;

            return *this;
        }

        CEnvironment::~CEnvironment()
        {
            tearDownConnections();
        }

        void CEnvironment::setHCoeffModel(BoundaryConditionsCoeffModel const t_BCModel,
                                          double const t_HCoeff)
        {
            m_HCoefficientModel = t_BCModel;
            m_HInput = t_HCoeff;
            resetCalculated();
        }

        void CEnvironment::setForcedVentilation(ForcedVentilation const & t_ForcedVentilation)
        {
            m_ForcedVentilation = t_ForcedVentilation;
            resetCalculated();
        }

        void CEnvironment::setEnvironmentIR(double const t_InfraRed)
        {
            setIRFromEnvironment(t_InfraRed);
            m_IRCalculatedOutside = true;
            resetCalculated();
        }

        void CEnvironment::setEmissivity(double const t_Emissivity)
        {
            m_Emissivity = t_Emissivity;
            resetCalculated();
        }

        double CEnvironment::getEnvironmentIR()
        {
            calculateLayerHeatFlow();
            return getIRFromEnvironment();
        }

        double CEnvironment::getHc()
        {
            return getConductionConvectionCoefficient();
        }

        double CEnvironment::getAirTemperature()
        {
            return getGasTemperature();
        }

        double CEnvironment::getAmbientTemperature()
        {
            double hc = getHc();
            double hr = getHr();
            return (hc * getAirTemperature() + hr * getRadiationTemperature()) / (hc + hr);
        }

        double CEnvironment::getDirectSolarRadiation() const
        {
            return m_DirectSolarRadiation;
        }

        void CEnvironment::connectToIGULayer(std::shared_ptr<CBaseLayer> const &)
        {
            //
        }

        void CEnvironment::initializeStateVariables()
        {
            CGasLayer::initializeStateVariables();
        }

        void CEnvironment::calculateRadiationFlow()
        {
            // In case of environments, there is no need to calculate radiation
            // if radiation is provided from outside calculations
            if(!m_IRCalculatedOutside)
            {
                setIRFromEnvironment(calculateIRFromVariables());
            }
        }

    }   // namespace ISO15099

}   // namespace Tarcog
