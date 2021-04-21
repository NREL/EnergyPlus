#ifndef MULTILAYER_H
#define MULTILAYER_H

#include <memory>
#include <vector>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"

namespace SingleLayerOptics
{
    class CLayerSingleComponent;
    class CScatteringLayer;

}   // namespace SingleLayerOptics

namespace MultiLayerOptics
{
    class CInterRef;
    class CEquivalentScatteringLayer;

    // Handles equivalent layer properties of multilayer IGU that is made of
    // any type of layer (specular or diffuse)
    class CMultiLayerScattered : public SingleLayerOptics::IScatteringLayer
    {
    public:
        CMultiLayerScattered(double t_Tf_dir_dir = 0,
                             double t_Rf_dir_dir = 0,
                             double t_Tb_dir_dir = 0,
                             double t_Rb_dir_dir = 0,
                             double t_Tf_dir_dif = 0,
                             double t_Rf_dir_dif = 0,
                             double t_Tb_dir_dif = 0,
                             double t_Rb_dir_dif = 0,
                             double t_Tf_dif_dif = 0,
                             double t_Rf_dif_dif = 0,
                             double t_Tb_dif_dif = 0,
                             double t_Rb_dif_dif = 0);

        static std::unique_ptr<CMultiLayerScattered>
          create(const SingleLayerOptics::CScatteringLayer & t_Layer);

        static std::unique_ptr<CMultiLayerScattered>
          create(const std::vector<SingleLayerOptics::CScatteringLayer> & layers);

        void addLayer(double t_Tf_dir_dir,
                      double t_Rf_dir_dir,
                      double t_Tb_dir_dir,
                      double t_Rb_dir_dir,
                      double t_Tf_dir_dif,
                      double t_Rf_dir_dif,
                      double t_Tb_dir_dif,
                      double t_Rb_dir_dif,
                      double t_Tf_dif_dif,
                      double t_Rf_dif_dif,
                      double t_Tb_dif_dif,
                      double t_Rb_dif_dif,
                      FenestrationCommon::Side t_Side = FenestrationCommon::Side::Back);

        void addLayer(const SingleLayerOptics::CScatteringLayer & t_Layer,
                      FenestrationCommon::Side t_Side = FenestrationCommon::Side::Back);

        void setSourceData(FenestrationCommon::CSeries & t_SourceData);

        size_t getNumOfLayers() const;

        double getPropertySimple(double minLambda,
                                 double maxLambda,
                                 FenestrationCommon::PropertySimple t_Property,
                                 FenestrationCommon::Side t_Side,
                                 FenestrationCommon::Scattering t_Scattering,
                                 double t_Theta = 0,
                                 double t_Phi = 0) override;

        double getAbsorptanceLayer(size_t Index,
                                   FenestrationCommon::Side t_Side,
                                   FenestrationCommon::ScatteringSimple t_Scattering,
                                   double t_Theta = 0,
                                   double t_Phi = 0);

        double getAbsorptanceLayer(double minLambda,
                                   double maxLambda,
                                   size_t Index,
                                   FenestrationCommon::Side t_Side,
                                   FenestrationCommon::ScatteringSimple t_Scattering,
                                   double t_Theta = 0,
                                   double t_Phi = 0);

        std::vector<double> getAbsorptanceLayers(double minLambda,
                                                 double maxLambda,
                                                 FenestrationCommon::Side side,
                                                 FenestrationCommon::ScatteringSimple scattering,
                                                 double theta = 0,
                                                 double phi = 0) override;

        double getAbsorptance(FenestrationCommon::Side t_Side,
                              FenestrationCommon::ScatteringSimple t_Scattering,
                              double t_Theta = 0,
                              double t_Phi = 0);

        std::vector<double> getWavelengths() const override;

        double getMinLambda() const override;
        double getMaxLambda() const override;

    private:
        explicit CMultiLayerScattered(const SingleLayerOptics::CScatteringLayer & t_Layer);
        CMultiLayerScattered(const std::vector<SingleLayerOptics::CScatteringLayer> & layers);

        void initialize(const SingleLayerOptics::CScatteringLayer & t_Layer);

        void calculateState(double t_Theta, double t_Phi);

        std::shared_ptr<CInterRef> m_InterRef;
        std::shared_ptr<CEquivalentScatteringLayer> m_Layer;
        std::vector<SingleLayerOptics::CScatteringLayer> m_Layers;

        bool m_Calculated;
        double m_Theta;
        double m_Phi;
    };
}   // namespace MultiLayerOptics

#endif
