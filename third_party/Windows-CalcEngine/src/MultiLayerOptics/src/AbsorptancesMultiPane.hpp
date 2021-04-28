#ifndef AbsorptancesMultiPane_H
#define AbsorptancesMultiPane_H

#include <memory>
#include <vector>
#include <WCECommon.hpp>

namespace MultiLayerOptics
{
    //! \brief Calculate absorptances of multiplane layers for simple case (single incident angle)
    class CAbsorptancesMultiPane
    {
    public:
        CAbsorptancesMultiPane(const FenestrationCommon::CSeries & t_T,
                               const FenestrationCommon::CSeries & t_Rf,
                               const FenestrationCommon::CSeries & t_Rb);

        void addLayer(const FenestrationCommon::CSeries & t_T,
                      const FenestrationCommon::CSeries & t_Rf,
                      const FenestrationCommon::CSeries & t_Rb);

        FenestrationCommon::CSeries Abs(size_t Index);
        FenestrationCommon::CSeries Abs(size_t Index, FenestrationCommon::Side side);
        size_t numOfLayers();

    private:
        void calculateState();

        FenestrationCommon::CSeries rCoeffs(const FenestrationCommon::CSeries & t_T,
                                            const FenestrationCommon::CSeries & t_Rf,
                                            const FenestrationCommon::CSeries & t_Rb,
                                            const FenestrationCommon::CSeries & t_RCoeffs);

        FenestrationCommon::CSeries tCoeffs(const FenestrationCommon::CSeries & t_T,
                                            const FenestrationCommon::CSeries & t_Rb,
                                            const FenestrationCommon::CSeries & t_RCoeffs);

        std::vector<FenestrationCommon::CSeries> m_T;
        std::vector<FenestrationCommon::CSeries> m_Rf;
        std::vector<FenestrationCommon::CSeries> m_Rb;
        std::vector<FenestrationCommon::CSeries> m_Abs;

        //! \brief Keeps data on how much of absorptance is coming from front and back sides.
        //! These data are only important for photovoltaic calculations.
        std::map<FenestrationCommon::Side, std::vector<FenestrationCommon::CSeries>> m_AbsBySide;

        std::vector<FenestrationCommon::CSeries> m_rCoeffs;
        std::vector<FenestrationCommon::CSeries> m_tCoeffs;

        bool m_StateCalculated;
    };
}   // namespace MultiLayerOptics

#endif
