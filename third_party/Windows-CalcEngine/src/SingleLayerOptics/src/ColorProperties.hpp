#ifndef WINDOWS_CALCENGINE_COLORPROPERTIES_HPP
#define WINDOWS_CALCENGINE_COLORPROPERTIES_HPP

#include <memory>
#include <vector>

#include "WCECommon.hpp"
#include "ScatteringLayer.hpp"

namespace SingleLayerOptics
{
    struct Trichromatic
    {
        Trichromatic(double X, double Y, double Z);

        double X;
        double Y;
        double Z;
    };

    struct aRGB
    {
        aRGB(int R, int G, int B);

        int R;
        int G;
        int B;
    };

    struct CIE_LAB
    {
        CIE_LAB(double L, double A, double B);

        double L;
        double a;
        double b;
    };

    class ColorProperties
    {
    public:
        /// For BSDF layers make additional constructor
        ColorProperties(std::unique_ptr<IScatteringLayer> && layerX,
                        std::unique_ptr<IScatteringLayer> && layerY,
                        std::unique_ptr<IScatteringLayer> && layerZ,
                        const FenestrationCommon::CSeries & t_Source,
                        const FenestrationCommon::CSeries & t_DetectorX,
                        const FenestrationCommon::CSeries & t_DetectorY,
                        const FenestrationCommon::CSeries & t_DetectorZ,
                        const std::vector<double> & t_wavelengths = {});

        Trichromatic getTrichromatic(const FenestrationCommon::PropertySimple t_Property,
                                     const FenestrationCommon::Side t_Side,
                                     const FenestrationCommon::Scattering t_Scattering,
                                     const double t_Theta = 0,
                                     const double t_Phi = 0);

        aRGB getRGB(const FenestrationCommon::PropertySimple t_Property,
                    const FenestrationCommon::Side t_Side,
                    const FenestrationCommon::Scattering t_Scattering,
                    const double t_Theta = 0,
                    const double t_Phi = 0);

        CIE_LAB getCIE_Lab(const FenestrationCommon::PropertySimple t_Property,
                           const FenestrationCommon::Side t_Side,
                           const FenestrationCommon::Scattering t_Scattering,
                           const double t_Theta = 0,
                           const double t_Phi = 0);

    private:
        std::unique_ptr<IScatteringLayer> m_LayerX;
        std::unique_ptr<IScatteringLayer> m_LayerY;
        std::unique_ptr<IScatteringLayer> m_LayerZ;
        double m_SDx;
        double m_SDy;
        double m_SDz;
    };

}   // namespace SingleLayerOptics


#endif   // WINDOWS_CALCENGINE_COLORPROPERTIES_HPP
