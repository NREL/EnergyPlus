#ifndef EQUIVALENTBSDFLAYERSINGLEBAND_H
#define EQUIVALENTBSDFLAYERSINGLEBAND_H

#include <memory>
#include <vector>
#include <map>

#include "WCECommon.hpp"

namespace SingleLayerOptics
{
    class CBSDFIntegrator;
}

namespace MultiLayerOptics
{
    // Matrix will store absorptances for each layer at every direction
    typedef std::vector<std::vector<double>> Abs_Matrix;

    // Class to handle interreflectance calculations
    class CInterReflectance
    {
    public:
        CInterReflectance(const FenestrationCommon::SquareMatrix & t_Lambda,
                          const FenestrationCommon::SquareMatrix & t_Rb,
                          const FenestrationCommon::SquareMatrix & t_Rf);

        FenestrationCommon::SquareMatrix value() const;

    private:
        FenestrationCommon::SquareMatrix m_InterRefl;
    };

    // Class to calculate equivalent BSDF transmittance and reflectances. This will be used by
    // multilayer routines to calculate properties for any number of layers.
    class CBSDFDoubleLayer
    {
    public:
        CBSDFDoubleLayer(const SingleLayerOptics::CBSDFIntegrator & t_FrontLayer,
                         const SingleLayerOptics::CBSDFIntegrator & t_BackLayer);

        std::shared_ptr<SingleLayerOptics::CBSDFIntegrator> value() const;

    private:
        static FenestrationCommon::SquareMatrix
          equivalentT(const FenestrationCommon::SquareMatrix & t_Tf2,
                      const FenestrationCommon::SquareMatrix & t_InterRefl,
                      const FenestrationCommon::SquareMatrix & t_Lambda,
                      const FenestrationCommon::SquareMatrix & t_Tf1);

        static FenestrationCommon::SquareMatrix
          equivalentR(const FenestrationCommon::SquareMatrix & t_Rf1,
                      const FenestrationCommon::SquareMatrix & t_Tf1,
                      const FenestrationCommon::SquareMatrix & t_Tb1,
                      const FenestrationCommon::SquareMatrix & t_Rf2,
                      const FenestrationCommon::SquareMatrix & t_InterRefl,
                      const FenestrationCommon::SquareMatrix & t_Lambda);

        std::shared_ptr<SingleLayerOptics::CBSDFIntegrator> m_Results;

        FenestrationCommon::SquareMatrix m_Tf;
        FenestrationCommon::SquareMatrix m_Tb;
        FenestrationCommon::SquareMatrix m_Rf;
        FenestrationCommon::SquareMatrix m_Rb;
    };

    // Class for equivalent BSDF layer for single material properties (or single wavelength)
    class CEquivalentBSDFLayerSingleBand
    {
    public:
        explicit CEquivalentBSDFLayerSingleBand(
          const std::shared_ptr<SingleLayerOptics::CBSDFIntegrator> & t_Layer);
        void addLayer(const std::shared_ptr<SingleLayerOptics::CBSDFIntegrator> & t_Layer);

        FenestrationCommon::SquareMatrix getMatrix(FenestrationCommon::Side t_Side,
                                                   FenestrationCommon::PropertySimple t_Property);

        FenestrationCommon::SquareMatrix getProperty(FenestrationCommon::Side t_Side,
                                                     FenestrationCommon::PropertySimple t_Property);

        std::vector<double> getLayerAbsorptances(size_t Index, FenestrationCommon::Side t_Side);

        size_t getNumberOfLayers() const;

    private:
        void calcEquivalentProperties();

        std::vector<double> absTerm1(const std::vector<double> & t_Alpha,
                                     const FenestrationCommon::SquareMatrix & t_InterRefl,
                                     const FenestrationCommon::SquareMatrix & t_T) const;

        std::vector<double> absTerm2(const std::vector<double> & t_Alpha,
                                     const FenestrationCommon::SquareMatrix & t_InterRefl,
                                     const FenestrationCommon::SquareMatrix & t_R,
                                     const FenestrationCommon::SquareMatrix & t_T) const;

        std::shared_ptr<SingleLayerOptics::CBSDFIntegrator> m_EquivalentLayer;
        std::vector<std::shared_ptr<SingleLayerOptics::CBSDFIntegrator>> m_Layers;

        // Forward and backward layers are used for calculation of equivalent absorptances
        std::vector<std::shared_ptr<SingleLayerOptics::CBSDFIntegrator>> m_Forward;
        std::vector<std::shared_ptr<SingleLayerOptics::CBSDFIntegrator>> m_Backward;

        // Abs_Matrix m_Af;
        // Abs_Matrix m_Ab;
        std::map<FenestrationCommon::Side, Abs_Matrix> m_A;

        bool m_PropertiesCalculated;

        FenestrationCommon::SquareMatrix m_Lambda;
    };

}   // namespace MultiLayerOptics

#endif
