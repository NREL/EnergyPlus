#pragma once

#include <memory>
#include <vector>
#include <map>
#include "IGUConfigurations.hpp"

namespace Tarcog
{
    namespace ISO15099
    {
        enum class Environment;

        class CIGU;

        class CEnvironment;

        class CSingleSystem;

        class CIGUSolidLayer;

        class CSystem : public IIGUSystem
        {
        public:
            CSystem(CIGU & t_IGU,
                    const std::shared_ptr<CEnvironment> & t_Indoor,
                    const std::shared_ptr<CEnvironment> & t_Outdoor);

            [[nodiscard]] std::vector<double> getTemperatures(System t_System);
            [[nodiscard]] std::vector<double> getRadiosities(System t_System);

            [[nodiscard]] std::vector<double> getMaxDeflections(System t_System);
            [[nodiscard]] std::vector<double> getMeanDeflections(System t_System);
            [[nodiscard]] std::vector<double> getPanesLoad(System t_System);
            void setAppliedLoad(const std::vector<double> & load);

            [[nodiscard]] std::vector<std::shared_ptr<CIGUSolidLayer>>
              getSolidLayers(System t_System) const;

            [[nodiscard]] std::vector<double>
              getSolidEffectiveLayerConductivities(const System t_System);
            [[nodiscard]] std::vector<double>
              getGapEffectiveLayerConductivities(const System t_System);
            [[nodiscard]] double getEffectiveSystemConductivity(const System t_System);
            [[nodiscard]] double thickness(const System t_System) const;

            [[nodiscard]] double getHeatFlow(System t_System, Environment t_Environment);
            [[nodiscard]] double getUValue() override;
            [[nodiscard]] double getSHGC(double t_TotSol) override;
            [[nodiscard]] double getH(System sys, Environment environment) const override;
            [[nodiscard]] size_t getNumberOfIterations(System t_System);

            [[nodiscard]] double relativeHeatGain(double Tsol);

            void setAbsorptances(const std::vector<double> & absorptances);

            void setWidth(double width) override;
            void setHeight(double height) override;
            void setTilt(double tilt) override;
            void setWidthAndHeight(double width, double height) override;
            void setInteriorAndExteriorSurfacesHeight(double height) override;

            void setDeflectionProperties(double t_Tini, double t_Pini);
            void clearDeflection();

        private:
            void solve();
            void checkSolved();

            std::map<System, std::shared_ptr<CSingleSystem>> m_System;

            bool m_Solved{false};
        };

    }   // namespace ISO15099

}   // namespace Tarcog
