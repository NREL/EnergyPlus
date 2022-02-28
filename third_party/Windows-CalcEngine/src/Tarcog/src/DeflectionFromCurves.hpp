#pragma once

#include <vector>
#include <WCECommon.hpp>

#include "TarcogConstants.hpp"

namespace Deflection
{
    const double defaultGlassDensity = Tarcog::MaterialConstants::GLASSDENSITY;                                                // kg/m3
    const double defaultPressure = 101325;                                                  // Pa
    const double defaultModulusOfElasticity = Tarcog::DeflectionConstants::YOUNGSMODULUS;   // Pa

    struct LayerData
    {
        //! Expected input data are in standard SI units. This structure will convert them in what
        //! is needed for the calculation.
        //!
        //! \param thickness Layer thickness in meters
        //! \param density Layer material density in kg/m3
        //! \param modulusOfElasticity Modulus of elasticity in Pa
        LayerData(double thickness,
                  double density = defaultGlassDensity,
                  double modulusOfElasticity = defaultModulusOfElasticity);

        double thickness;             // mm
        double density;               // kg/m3
        double modulusOfElasticity;   // KPa
    };

    struct GapData
    {
        //! Expected input data are in standard SI units. This structure will convert them in what
        //! is needed for the calculation.
        //!
        //! \param thickness Gap thickness in meters
        //! \param initialTemperature Initial gap temperature in Kelvins
        //! \param initialPressure Initial gap pressure in Pascals
        GapData(double thickness,
                double initialTemperature,
                double initialPressure = defaultPressure);

        double thickness;            // mm
        double initialTemperature;   // Kelvin
        double initialPressure;      // KPa
    };

    struct DeflectionResults
    {
        DeflectionResults() = default;
        DeflectionResults(std::optional<double> error,
                          const std::vector<double> & deflection,
                          const std::vector<double> & pressureDifference);
        std::optional<double> error{0.0};
        std::vector<double> deflection;
        std::vector<double> paneLoad;
    };

    //! Class that does deflection calculations based on E1300 curves. It is working only for
    //! Poisson's ratio 0.22
    class DeflectionE1300
    {
    public:
        DeflectionE1300(double width,
                        double height,
                        std::vector<LayerData> layer,
                        std::vector<GapData> gap);

        [[maybe_unused]] void setLoadTemperatures(std::vector<double> loadTemperature);

        [[maybe_unused]] void setExteriorPressure(double pressure);
        [[maybe_unused]] void setInteriorPressure(double pressure);

        [[maybe_unused]] void setIGUTilt(double theta);
        [[maybe_unused]] void setDimensions(double width, double height);

        //! Adding applied load to the layers
        //!
        //! \param appliedLoad Vector of applied loads to the each of the layers [Pa]
        [[maybe_unused]] void setAppliedLoad(std::vector<double> appliedLoad);

        DeflectionResults results();

    private:
        static std::vector<double> getPsWeight(const std::vector<LayerData> & layer, double theta);
        [[nodiscard]] std::vector<double> getPsLoaded(const std::vector<LayerData> & layer,
                                                      double theta);

        double m_ExteriorPressure{defaultPressure / 1000};   // KPa
        double m_InteriorPressure{defaultPressure / 1000};   // KPa
        double m_LongDimension;                              // mm
        double m_ShortDimension;                             // mm
        double m_Theta{90};                                  // degrees
        std::vector<LayerData> m_Layer;
        std::vector<GapData> m_Gap;
        std::vector<double> m_LoadTemperature;
        std::vector<double> m_AppliedLoad;
        std::vector<double> m_SelfWeight;
        std::vector<double> m_PsLoaded;

        static std::vector<double> calcPcs(double shortDimension,
                                           const std::vector<LayerData> & layer);
        std::vector<double> m_Pcs;

        static std::vector<double> calcVcs(double shortDimension,
                                           const std::vector<LayerData> & layer);
        std::vector<double> m_Vcs;

        std::vector<Table::point> m_PnVns;
        std::vector<Table::point> m_PnWns;

        [[nodiscard]] static double DP1pGuess(double Pdiff, const std::vector<LayerData> & layer);

        [[nodiscard]] DeflectionResults nIGU_Li(size_t index, double PasiLoaded, double dpCoeff);

        bool m_ResultsCalculated{false};
        DeflectionResults m_DeflectionResults;

        DeflectionResults calculateResults();
    };
}   // namespace Deflection