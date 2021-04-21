#pragma once

#include "CMAInterface.hpp"

namespace CMA
{

    ///////////////////////////////////////////////////
    //  CMABestWorstUFactors
    ///////////////////////////////////////////////////

    //! Will be used to calculate best and worst IGU options for CMA calculations.
    //! It contains default variables that usually do not need to be changed by the user.
    class CMABestWorstUFactors
    {
    public:
        CMABestWorstUFactors() = default;
        CMABestWorstUFactors(double hci, double hco, double gapConductance);

        CMABestWorstUFactors(double hci,
                             double hco,
                             double gapConductance,
                             double interiorGlassThickness,
                             double interiorGlassConductivity,
                             double interiorGlassSurfaceEmissivity,
                             double exteriorGlassThickness,
                             double exteriorGlassConductivity,
                             double exteriorGlassSurfaceEmissivity,
                             double insideAirTemperature,
                             double outsideAirTemperature);

        [[nodiscard]] double uValue();
        [[nodiscard]] double hcout();

    private:
        [[nodiscard]] double heatFlow(double interiorRadiationFilmCoefficient, double exteriorRadiationFilmCoefficient) const;
        [[nodiscard]] double hrout(double surfaceTemperature) const;
        [[nodiscard]] double hrin(double surfaceTemperature) const;
        [[nodiscard]] double insideSurfaceTemperature(double interiorRadiationFilmCoefficient) const;
        [[nodiscard]] double outsideSurfaceTemperature(double exteriorRadiationFilmCoefficient) const;
        void caluculate();

        double m_Hci{0};
        double m_Hco{0};
        double m_GapConductance{0};
        double m_InteriorGlassThickness{0.006};
        double m_InteriorGlassConductivity{1};
        double m_InteriorGlassSurfaceEmissivity{0.84};
        double m_ExteriorGlassThickness{0.006};
        double m_ExteriorGlassConductivity{1};
        double m_ExteriorGlassSurfaceEmissivity{0.84};
        double m_InsideAirTemperature{21};
        double m_OutsideAirTemperature{-18};

        //! These two needs to be calculated through iterations 
        double m_Hri{0};
        double m_Hro{0};

        bool m_Calculated{false};
    };

    //! Creates built in values for CMA U-factors
    CMABestWorstUFactors CreateBestWorstUFactorOption(Option option);
}
