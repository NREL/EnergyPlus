#include "System.hpp"
#include "IGU.hpp"
#include "Environment.hpp"
#include "SingleSystem.hpp"


namespace Tarcog
{
    namespace ISO15099
    {
        CSystem::CSystem(CIGU & t_IGU,
                         std::shared_ptr<CEnvironment> const & t_Indoor,
                         std::shared_ptr<CEnvironment> const & t_Outdoor)
        {
            m_System[System::SHGC] = std::make_shared<CSingleSystem>(t_IGU, t_Indoor, t_Outdoor);
            m_System[System::Uvalue] = std::make_shared<CSingleSystem>(
              t_IGU, t_Indoor->cloneEnvironment(), t_Outdoor->cloneEnvironment());
            m_System.at(System::Uvalue)->setSolarRadiation(0);

            solve();
        }

        std::vector<double> CSystem::getTemperatures(System const t_System)
        {
            checkSolved();
            return m_System.at(t_System)->getTemperatures();
        }

        std::vector<double> CSystem::getRadiosities(System const t_System)
        {
            checkSolved();
            return m_System.at(t_System)->getRadiosities();
        }

        std::vector<double> CSystem::getMaxDeflections(System const t_System)
        {
            checkSolved();
            return m_System.at(t_System)->getMaxDeflections();
        }

        std::vector<double> CSystem::getMeanDeflections(System const t_System)
        {
            checkSolved();
            return m_System.at(t_System)->getMeanDeflections();
        }

        std::vector<std::shared_ptr<CIGUSolidLayer>>
          CSystem::getSolidLayers(System const t_System) const
        {
            return m_System.at(t_System)->getSolidLayers();
        }

        double CSystem::getHeatFlow(System const t_System, Environment const t_Environment)
        {
            checkSolved();
            return m_System.at(t_System)->getHeatFlow(t_Environment);
        }

        double CSystem::getUValue()
        {
            checkSolved();
            return m_System.at(System::Uvalue)->getUValue();
        }

        double CSystem::getSHGC(double const t_TotSol)
        {
            checkSolved();
            const auto ventilatedFlowSHGC{
              m_System.at(System::SHGC)->getVentilationFlow(Environment::Indoor)};
            const auto ventilatedFlowU{
              m_System.at(System::Uvalue)->getVentilationFlow(Environment::Indoor)};

            const auto indoorFlowSHGC{m_System.at(System::SHGC)->getHeatFlow(Environment::Indoor)
                                      + ventilatedFlowSHGC};
            const auto indoorFlowU{m_System.at(System::Uvalue)->getHeatFlow(Environment::Indoor)
                                   + ventilatedFlowU};

            auto result{m_System.at(System::SHGC)->getSolarRadiation() != 0.0
                          ? t_TotSol
                              - (indoorFlowSHGC - indoorFlowU)
                                  / m_System.at(System::SHGC)->getSolarRadiation()
                          : 0};

            return result;
        }

        size_t CSystem::getNumberOfIterations(System const t_System)
        {
            checkSolved();
            return m_System.at(t_System)->getNumberOfIterations();
        }

        std::vector<double>
          CSystem::getSolidEffectiveLayerConductivities(const System t_System)
        {
            checkSolved();
            return m_System.at(t_System)->getSolidEffectiveLayerConductivities();
        }

        std::vector<double> CSystem::getGapEffectiveLayerConductivities(const System t_System)
        {
            checkSolved();
            return m_System.at(t_System)->getGapEffectiveLayerConductivities();
        }

        double CSystem::getEffectiveSystemConductivity(const System t_System)
        {
            checkSolved();
            return m_System.at(t_System)->EffectiveConductivity();
        }

        double CSystem::thickness(const System t_System) const
        {
            return m_System.at(t_System)->thickness();
        }

        double CSystem::relativeHeatGain(const double Tsol)
        {
            return getUValue() * 7.78 + getSHGC(Tsol) / 0.87 * 630.9;
        }

        void CSystem::setAbsorptances(const std::vector<double> & absorptances)
        {
            m_System.at(System::SHGC)->setAbsorptances(absorptances);
            m_Solved = false;
        }

        void CSystem::setWidth(double width)
        {
            for(auto & [key, aSystem] : m_System)
            {
                aSystem->setWidth(width);
            }
            m_Solved = false;
        }

        void CSystem::setHeight(double height)
        {
            for(auto & [key, aSystem] : m_System)
            {
                aSystem->setHeight(height);
            }
            m_Solved = false;
        }

        void CSystem::setWidthAndHeight(double width, double height)
        {
            for(auto & [key, aSystem] : m_System)
            {
                aSystem->setWidth(width);
                aSystem->setHeight(height);
            }
            m_Solved = false;
        }

        void CSystem::setInteriorAndExteriorSurfacesHeight(double height)
        {
            for(auto & [key, aSystem] : m_System)
            {
                aSystem->setInteriorAndExteriorSurfacesHeight(height);                
            }
            m_Solved = false;
        }

        void CSystem::solve()
        {
            for(auto & [key, aSystem] : m_System)
            {
                aSystem->solve();
            }
            m_Solved = true;
        }

        void CSystem::checkSolved()
        {
            if(!m_Solved)
            {
                solve();
                m_Solved = true;
            }
        }

        double CSystem::getHc(System sys, Environment environment) const
        {
            return m_System.at(sys)->getHc(environment);
        }

    }   // namespace ISO15099

}   // namespace Tarcog
