#ifndef TARIGU_H
#define TARIGU_H

#include <memory>
#include <vector>

namespace Tarcog
{
    namespace ISO15099
    {
        enum class Environment;
        class CBaseIGULayer;
        class CIGUSolidLayer;
        class CIGUGapLayer;
        class CBaseLayer;
        class CSurface;


        class CIGU
        {
        public:
            CIGU(double t_Width = 1, double t_Height = 1, double t_Tilt = 90);
            CIGU(CIGU const & t_IGU);
            CIGU & operator=(CIGU const & t_IGU);
            ~CIGU();

            void addLayer(const std::shared_ptr<CBaseLayer> & t_Layer);
            void addLayers(const std::initializer_list<std::shared_ptr<CBaseIGULayer>> & layers);

            void setAbsorptances(const std::vector<double> & absorptances, double solarRadiation);

            [[nodiscard]] std::vector<std::shared_ptr<CIGUSolidLayer>> getSolidLayers() const;
            [[nodiscard]] std::vector<std::shared_ptr<CIGUGapLayer>> getGapLayers() const;
            [[nodiscard]] std::vector<std::shared_ptr<CBaseLayer>> getLayers() const;

            void setTilt(double t_Tilt);
            void setWidth(double t_Width);
            void setHeight(double t_Height);

            void setSolarRadiation(double t_SolarRadiation) const;

            [[nodiscard]] std::shared_ptr<CBaseLayer>
              getEnvironment(Environment t_Environment) const;

            [[nodiscard]] std::vector<double> getState() const;
            void setState(const std::vector<double> & t_State) const;

            [[nodiscard]] std::vector<double> getTemperatures() const;
            [[nodiscard]] std::vector<double> getRadiosities() const;
            [[nodiscard]] std::vector<double> getMaxDeflections() const;
            [[nodiscard]] std::vector<double> getMeanDeflections() const;

            [[nodiscard]] double getTilt() const;
            [[nodiscard]] double getWidth() const;
            [[nodiscard]] double getHeight() const;
            [[nodiscard]] double getThickness() const;

            [[nodiscard]] size_t getNumOfLayers() const;

            [[nodiscard]] double getVentilationFlow(Environment t_Environment) const;

            void setInitialGuess(const std::vector<double> & t_Guess) const;

            void setDeflectionProperties(double t_Tini, double t_Pini);
            void setDeflectionProperties(const std::vector<double> & t_MeasuredDeflections);

        private:
            // Replces layer in existing construction and keeps correct connections in linked list
            void replaceLayer(const std::shared_ptr<CBaseIGULayer> & t_Original,
                              const std::shared_ptr<CBaseIGULayer> & t_Replacement);

            // Check if layer needs to be decorated with another object
            void checkForLayerUpgrades(const std::shared_ptr<CBaseLayer> & t_Layer);

            std::vector<std::shared_ptr<CBaseLayer>> m_Layers;

            double m_Width;    // meters
            double m_Height;   // meters
            double m_Tilt;     // degrees

            // Routines to calculate deflection coefficients
            [[nodiscard]] double Ldmean() const;
            [[nodiscard]] double Ldmax() const;
        };

    }   // namespace ISO15099

}   // namespace Tarcog


#endif
