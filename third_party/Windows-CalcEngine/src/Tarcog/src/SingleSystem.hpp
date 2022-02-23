#ifndef TARCOGSINGLESYSTEM_H
#define TARCOGSINGLESYSTEM_H

#include <memory>
#include <map>
#include <vector>

#include "IGU.hpp"

namespace Tarcog
{
    namespace ISO15099
    {
        enum class Environment;

        class CBaseIGULayer;

        class CIGUSolidLayer;

        class CIGUGapLayer;

        class CEnvironment;

        class CNonLinearSolver;

        class CSingleSystem
        {
        public:
            CSingleSystem(CIGU & t_IGU,
                          const std::shared_ptr<CEnvironment> & t_Indoor,
                          const std::shared_ptr<CEnvironment> & t_Outdoor);

            CSingleSystem(const CSingleSystem & t_SingleSystem);
            CSingleSystem & operator=(const CSingleSystem & t_SingleSystem);

            [[nodiscard]] std::vector<std::shared_ptr<CIGUSolidLayer>> getSolidLayers() const;
            [[nodiscard]] std::vector<std::shared_ptr<CIGUGapLayer>> getGapLayers() const;

            [[nodiscard]] std::vector<double> getSolidEffectiveLayerConductivities() const;
            [[nodiscard]] std::vector<double> getGapEffectiveLayerConductivities() const;

            [[nodiscard]] std::vector<double> getTemperatures() const;
            [[nodiscard]] std::vector<double> getRadiosities() const;

            [[nodiscard]] std::vector<double> getMaxDeflections() const;
            [[nodiscard]] std::vector<double> getMeanDeflections() const;
            [[nodiscard]] std::vector<double> getPanesLoad() const;
            void setAppliedLoad(std::vector<double> load);

            [[nodiscard]] std::shared_ptr<CSingleSystem> clone() const;

            [[nodiscard]] double getHeatFlow(Environment t_Environment) const;
            [[nodiscard]] double getConvectiveHeatFlow(Environment t_Environment) const;
            [[nodiscard]] double getRadiationHeatFlow(Environment t_Environment) const;
            [[nodiscard]] double getHc(Environment t_Environment) const;
            [[nodiscard]] double getHr(Environment t_Environment) const;
            [[nodiscard]] double getH(Environment t_Environment) const;
            [[nodiscard]] double getAirTemperature(Environment t_Environment) const;

            // If interior layer have openings, this will return heat flow from airflow
            [[nodiscard]] double getVentilationFlow(Environment t_Environment) const;
            [[nodiscard]] double getUValue() const;
            [[nodiscard]] size_t getNumberOfIterations() const;
            [[nodiscard]] double solutionTolarance() const;
            [[nodiscard]] bool isToleranceAchieved() const;

            [[nodiscard]] double EffectiveConductivity() const;

            // Set solution tolerance
            void setTolerance(double t_Tolerance) const;
            // Set intial guess for solution.
            void setInitialGuess(const std::vector<double> & t_Temperatures) const;

            void setSolarRadiation(double t_SolarRadiation);
            [[nodiscard]] double getSolarRadiation() const;

            void solve() const;

            [[nodiscard]] double thickness() const;

            void setAbsorptances(const std::vector<double> & absorptances);

            void setWidth(double width);
            void setHeight(double height);
            void setTilt(double tilt);

            //! If IGU is part of the window then frame will still count in surface height.
            void setInteriorAndExteriorSurfacesHeight(double height);

            void setDeflectionProperties(double t_Tini, double t_Pini);

            void clearDeflection();

        private:
            CIGU m_IGU;
            std::map<Environment, std::shared_ptr<CEnvironment>> m_Environment;
            std::shared_ptr<CNonLinearSolver> m_NonLinearSolver;
            void initializeStartValues();
        };

    }   // namespace ISO15099

}   // namespace Tarcog

#endif
