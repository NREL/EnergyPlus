#pragma once

#include "SpecularCell.hpp"

namespace SingleLayerOptics
{
    // Needed for dynamic_cast between two layer types
    class BaseLayer
    {
    public:
        virtual ~BaseLayer() = default;
        virtual void Flipped(bool flipped);
    };

    class SpecularLayer : public BaseLayer
    {
    public:
        static std::shared_ptr<SpecularLayer> createLayer(const std::shared_ptr<CMaterial> & t_Material);

        // Transmittance averaged over entire wavelength spectrum
        double T_dir_dir(FenestrationCommon::Side t_Side, const CBeamDirection & t_Direction);

        // Reflectance averaged over entire wavelength spectrum
        double R_dir_dir(FenestrationCommon::Side t_Side, const CBeamDirection & t_Direction);

        // Transmittance of specular material for entire wavelength spectrum
        std::vector<double> T_dir_dir_band(FenestrationCommon::Side t_Side,
                                           const CBeamDirection & t_Direction);

        // Reflectance of specular material over entire wavelength spectrum
        std::vector<double> R_dir_dir_band(FenestrationCommon::Side t_Side,
                                           const CBeamDirection & t_Direction);

        std::vector<double> getBandWavelengths() const;
        void setSourceData(FenestrationCommon::CSeries &t_SourceData);

        double getMinLambda() const;
        double getMaxLambda() const;

        void Flipped(bool flipped) override;

        explicit SpecularLayer(const CSpecularCell & m_Cell);

    protected:
        CSpecularCell m_Cell;
    };

}   // namespace SingleLayerOptics
