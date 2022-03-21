#include <cassert>
#include <stdexcept>
#include <cmath>

#include "BaseShade.hpp"
#include "Surface.hpp"
#include "IGUGapLayer.hpp"
#include "Environment.hpp"
#include "TarcogConstants.hpp"
#include "IGUVentilatedGapLayer.hpp"


namespace Tarcog
{
    namespace ISO15099
    {
        auto const OPENING_TOLERANCE = 1e-6;

        ////////////////////////////////////////////////////////////////////////////////////////////////
        /// CShadeOpenings
        ////////////////////////////////////////////////////////////////////////////////////////////////

        CShadeOpenings::CShadeOpenings(double const t_Atop,
                                       double const t_Abot,
                                       double const t_Aleft,
                                       double const t_Aright,
                                       double const t_Afront,
                                       double const t_FrontPorosity) :
            m_Atop(t_Atop),
            m_Abot(t_Abot),
            m_Aleft(t_Aleft),
            m_Aright(t_Aright),
            m_Afront(t_Afront),
            m_FrontPorosity(t_FrontPorosity)
        {
            initialize();
        }

        CShadeOpenings::CShadeOpenings() :
            m_Atop(0),
            m_Abot(0),
            m_Aleft(0),
            m_Aright(0),
            m_Afront(0)
        {
            initialize();
        }

        void CShadeOpenings::initialize()
        {
            if(m_Atop == 0)
            {
                m_Atop = OPENING_TOLERANCE;
            }

            if(m_Abot == 0)
            {
                m_Abot = OPENING_TOLERANCE;
            }
        }

        double CShadeOpenings::openingMultiplier()
        {
            return (m_Aleft + m_Aright + m_Afront) / (m_Abot + m_Atop);
        }

        double CShadeOpenings::Aeq_bot()
        {
            return m_Abot + 0.5 * m_Atop * openingMultiplier();
        }

        double CShadeOpenings::Aeq_top()
        {
            return m_Atop + 0.5 * m_Abot * openingMultiplier();
        }

        double CShadeOpenings::frontPorositiy() const
        {
            return m_FrontPorosity;
        }

        bool CShadeOpenings::isOpen() const
        {
            return m_Abot > 0 || m_Atop > 0 || m_Aleft > 0 || m_Aright > 0 || m_Afront > 0;
        }

        ////////////////////////////////////////////////////////////////////////////////////////////////
        /// CIGUShadeLayer
        ////////////////////////////////////////////////////////////////////////////////////////////////

        CIGUShadeLayer::CIGUShadeLayer(
          const double t_Thickness,
          const double t_Conductivity,
          const std::shared_ptr<CShadeOpenings> & t_ShadeOpenings,
          const std::shared_ptr<Tarcog::ISO15099::ISurface> & t_FrontSurface,
          const std::shared_ptr<Tarcog::ISO15099::ISurface> & t_BackSurface) :
            CIGUSolidLayer(t_Thickness, t_Conductivity, t_FrontSurface, t_BackSurface),
            m_ShadeOpenings(t_ShadeOpenings),
            m_MaterialConductivity(t_Conductivity)
        {}

        CIGUShadeLayer::CIGUShadeLayer(std::shared_ptr<CIGUSolidLayer> & t_Layer,
                                       std::shared_ptr<CShadeOpenings> & t_ShadeOpenings) :
            CIGUSolidLayer(*t_Layer),
            m_ShadeOpenings(t_ShadeOpenings),
            m_MaterialConductivity(t_Layer->getConductance())
        {}

        CIGUShadeLayer::CIGUShadeLayer(double t_Thickness, double t_Conductivity) :
            CIGUSolidLayer(t_Thickness, t_Conductivity),
            m_ShadeOpenings(std::make_shared<CShadeOpenings>()),
            m_MaterialConductivity(t_Conductivity)
        {}

        std::shared_ptr<CBaseLayer> CIGUShadeLayer::clone() const
        {
            return std::make_shared<CIGUShadeLayer>(*this);
        }

        bool CIGUShadeLayer::isPermeable() const
        {
            return m_ShadeOpenings->isOpen();
        }

        void CIGUShadeLayer::calculateConvectionOrConductionFlow()
        {
            m_Conductivity =
              equivalentConductivity(m_MaterialConductivity, m_ShadeOpenings->frontPorositiy());
            CIGUSolidLayer::calculateConvectionOrConductionFlow();
            assert(m_NextLayer != nullptr);
            assert(m_PreviousLayer != nullptr);

            // This must be set here or gap will be constantly calling this routine back throughout
            // nextLayer property.
            setCalculated();

            if(std::dynamic_pointer_cast<CIGUGapLayer>(m_PreviousLayer) != nullptr
               && std::dynamic_pointer_cast<CIGUGapLayer>(m_NextLayer) != nullptr)
            {
                calcInBetweenShadeFlow(
                  std::dynamic_pointer_cast<CIGUVentilatedGapLayer>(m_PreviousLayer),
                  std::dynamic_pointer_cast<CIGUVentilatedGapLayer>(m_NextLayer));
            }
            else if(std::dynamic_pointer_cast<CEnvironment>(m_PreviousLayer) != nullptr
                    && std::dynamic_pointer_cast<CIGUVentilatedGapLayer>(m_NextLayer) != nullptr)
            {
                calcEdgeShadeFlow(std::dynamic_pointer_cast<CEnvironment>(m_PreviousLayer),
                                  std::dynamic_pointer_cast<CIGUVentilatedGapLayer>(m_NextLayer));
            }
            else if(std::dynamic_pointer_cast<CIGUVentilatedGapLayer>(m_PreviousLayer) != nullptr
                    && std::dynamic_pointer_cast<CEnvironment>(m_NextLayer) != nullptr)
            {
                calcEdgeShadeFlow(
                  std::dynamic_pointer_cast<CEnvironment>(m_NextLayer),
                  std::dynamic_pointer_cast<CIGUVentilatedGapLayer>(m_PreviousLayer));
            }
        }

        void CIGUShadeLayer::calcInBetweenShadeFlow(std::shared_ptr<CIGUVentilatedGapLayer> t_Gap1,
                                                    std::shared_ptr<CIGUVentilatedGapLayer> t_Gap2)
        {
            double Tup = t_Gap1->layerTemperature();
            double Tdown = t_Gap2->layerTemperature();
            double RelaxationParameter = IterationConstants::RELAXATION_PARAMETER_AIRFLOW;
            bool converged = false;
            size_t iterationStep = 0;

            while(!converged)
            {
                double tempGap1 = t_Gap1->layerTemperature();
                double tempGap2 = t_Gap2->layerTemperature();
                double Tav1 = t_Gap1->averageTemperature();
                double Tav2 = t_Gap2->averageTemperature();
                if(tempGap1 > tempGap2)
                {
                    t_Gap1->setFlowGeometry(m_ShadeOpenings->Aeq_bot(),
                                            m_ShadeOpenings->Aeq_top(),
                                            AirVerticalDirection::Up);
                    t_Gap2->setFlowGeometry(m_ShadeOpenings->Aeq_top(),
                                            m_ShadeOpenings->Aeq_bot(),
                                            AirVerticalDirection::Down);
                }
                else
                {
                    t_Gap1->setFlowGeometry(m_ShadeOpenings->Aeq_top(),
                                            m_ShadeOpenings->Aeq_bot(),
                                            AirVerticalDirection::Down);
                    t_Gap2->setFlowGeometry(m_ShadeOpenings->Aeq_bot(),
                                            m_ShadeOpenings->Aeq_top(),
                                            AirVerticalDirection::Up);
                }
                double drivingPressure = t_Gap1->getAirflowReferencePoint(tempGap2);
                double ratio = t_Gap1->getThickness() / t_Gap2->getThickness();
                double A1 = t_Gap1->bernoullyPressureTerm() + t_Gap1->pressureLossTerm();
                double A2 = t_Gap2->bernoullyPressureTerm() + t_Gap2->pressureLossTerm();
                double B1 = t_Gap1->hagenPressureTerm();
                double B2 = t_Gap2->hagenPressureTerm();
                double A = A1 + pow(ratio, 2) * A2;
                double B = B1 + ratio * B2;
                double speed1 =
                  (sqrt(std::abs(pow(B, 2.0) + 4 * A * drivingPressure)) - B) / (2.0 * A);
                double speed2 = speed1 / ratio;
                t_Gap1->setFlowSpeed(speed1);
                t_Gap2->setFlowSpeed(speed2);

                double beta1 = t_Gap1->betaCoeff();
                double beta2 = t_Gap2->betaCoeff();
                double alpha1 = 1 - beta1;
                double alpha2 = 1 - beta2;

                double TupOld = Tup;
                double TdownOld = Tdown;

                if(tempGap1 > tempGap2)
                {
                    Tup = (alpha1 * Tav1 + beta1 * alpha2 * Tav2) / (1 - beta1 * beta2);
                    Tdown = alpha2 * Tav2 + beta2 * Tup;
                }
                else
                {
                    Tdown = (alpha1 * Tav1 + beta1 * alpha2 * Tav2) / (1 - beta1 * beta2);
                    Tup = alpha2 * Tav2 + beta2 * Tdown;
                }

                Tup = RelaxationParameter * Tup + (1 - RelaxationParameter) * TupOld;
                Tdown = RelaxationParameter * Tdown + (1 - RelaxationParameter) * TdownOld;

                AirVerticalDirection gap1Direction = AirVerticalDirection::None;
                AirVerticalDirection gap2Direction = AirVerticalDirection::None;
                if(tempGap1 > tempGap2)
                {
                    gap1Direction = AirVerticalDirection::Up;
                    gap2Direction = AirVerticalDirection::Down;
                }
                else
                {
                    gap1Direction = AirVerticalDirection::Down;
                    gap2Direction = AirVerticalDirection::Up;
                }

                converged =
                  std::abs(Tup - TupOld) < IterationConstants::CONVERGENCE_TOLERANCE_AIRFLOW;
                converged =
                  converged
                  && std::abs(Tdown - TdownOld) < IterationConstants::CONVERGENCE_TOLERANCE_AIRFLOW;

                t_Gap1->setFlowTemperatures(Tup, Tdown, gap1Direction);
                t_Gap2->setFlowTemperatures(Tup, Tdown, gap2Direction);

                ++iterationStep;
                if(iterationStep > IterationConstants::NUMBER_OF_STEPS)
                {
                    converged = true;
                    throw std::runtime_error("Airflow iterations fail to converge. Maximum number "
                                             "of iteration steps reached.");
                }
            }

            double qv1 = t_Gap1->getGainFlow();
            double qv2 = t_Gap2->getGainFlow();
            t_Gap1->smoothEnergyGain(qv1, qv2);
            t_Gap2->smoothEnergyGain(qv1, qv2);
        }

        void CIGUShadeLayer::calcEdgeShadeFlow(std::shared_ptr<CEnvironment> t_Environment,
                                               std::shared_ptr<CIGUVentilatedGapLayer> t_Gap)
        {
            double TgapOut = t_Gap->layerTemperature();
            double RelaxationParameter = IterationConstants::RELAXATION_PARAMETER_AIRFLOW;
            bool converged = false;
            size_t iterationStep = 0;

            double tempGap = t_Gap->layerTemperature();
            while(!converged)
            {
                double tempEnvironment = t_Environment->getGasTemperature();
                double TavGap = t_Gap->averageTemperature();
                if(tempGap > tempEnvironment)
                {
                    t_Gap->setFlowGeometry(m_ShadeOpenings->Aeq_bot(),
                                           m_ShadeOpenings->Aeq_top(),
                                           AirVerticalDirection::Up);
                }
                else
                {
                    t_Gap->setFlowGeometry(m_ShadeOpenings->Aeq_top(),
                                           m_ShadeOpenings->Aeq_bot(),
                                           AirVerticalDirection::Down);
                }
                double drivingPressure = t_Gap->getAirflowReferencePoint(tempEnvironment);
                double A = t_Gap->bernoullyPressureTerm() + t_Gap->pressureLossTerm();
                double B = t_Gap->hagenPressureTerm();
                double speed = (sqrt(std::abs(pow(B, 2) + 4 * A * drivingPressure)) - B) / (2 * A);
                t_Gap->setFlowSpeed(speed);
                double beta = t_Gap->betaCoeff();
                double alpha = 1 - beta;

                double TgapOutOld = TgapOut;

                TgapOut = alpha * TavGap + beta * tempEnvironment;

                AirVerticalDirection gapDirection = AirVerticalDirection::None;
                if(TgapOut > tempEnvironment)
                {
                    gapDirection = AirVerticalDirection::Up;
                    t_Gap->setFlowTemperatures(TgapOut, tempEnvironment, gapDirection);
                }
                else
                {
                    gapDirection = AirVerticalDirection::Down;
                    t_Gap->setFlowTemperatures(tempEnvironment, TgapOut, gapDirection);
                }

                tempGap = t_Gap->layerTemperature();

                TgapOut = RelaxationParameter * tempGap + (1 - RelaxationParameter) * TgapOutOld;

                converged = std::abs(TgapOut - TgapOutOld)
                            < IterationConstants::CONVERGENCE_TOLERANCE_AIRFLOW;

                ++iterationStep;
                if(iterationStep > IterationConstants::NUMBER_OF_STEPS)
                {
                    RelaxationParameter -= IterationConstants::RELAXATION_PARAMETER_AIRFLOW_STEP;
                    iterationStep = 0;
                    if(RelaxationParameter == IterationConstants::RELAXATION_PARAMETER_AIRFLOW_MIN)
                    {
                        converged = true;
                        throw std::runtime_error("Airflow iterations fail to converge. "
                                                 "Maximum number of iteration steps reached.");
                    }
                }
            }
        }

        double CIGUShadeLayer::equivalentConductivity(const double t_Conductivity,
                                                      const double permeabilityFactor)
        {
            const auto standardPressure{101325.0};   // Pa
            const auto Tf{m_Surface.at(FenestrationCommon::Side::Front)->getTemperature()};
            const auto Tb{m_Surface.at(FenestrationCommon::Side::Back)->getTemperature()};
            Gases::CGas air;
            air.setTemperatureAndPressure((Tf + Tb) / 2, standardPressure);
            const auto airThermalConductivity = air.getGasProperties().m_ThermalConductivity;
            return airThermalConductivity * permeabilityFactor
                   + (1 - permeabilityFactor) * t_Conductivity;
        }
    }   // namespace ISO15099

}   // namespace Tarcog
