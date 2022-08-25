#ifndef WOVENCELL_H
#define WOVENCELL_H

#include <memory>

#include "UniformDiffuseCell.hpp"

namespace SingleLayerOptics
{
    class CWovenCellDescription;
    class ICellDescription;
    class CBeamDirection;

    class CWovenBase : public CUniformDiffuseCell
    {
    public:
        CWovenBase(const std::shared_ptr<CMaterial> & t_MaterialProperties,
                   const std::shared_ptr<ICellDescription> & t_Cell);

    protected:
        std::shared_ptr<CWovenCellDescription> getCellAsWoven() const;
    };

    class CWovenCell : public CWovenBase
    {
    public:
        CWovenCell(const std::shared_ptr<CMaterial> & t_MaterialProperties,
                   const std::shared_ptr<ICellDescription> & t_Cell);

        double T_dir_dir(FenestrationCommon::Side t_Side,
                         const CBeamDirection & t_Direction) override;
        double T_dir_dif(FenestrationCommon::Side t_Side,
                         const CBeamDirection & t_Direction) override;
        double R_dir_dif(FenestrationCommon::Side t_Side,
                         const CBeamDirection & t_Direction) override;

        std::vector<double> T_dir_dir_band(FenestrationCommon::Side t_Side,
                                           const CBeamDirection & t_Direction) override;
        std::vector<double> T_dir_dif_band(FenestrationCommon::Side t_Side,
                                           const CBeamDirection & t_Direction) override;
        std::vector<double> R_dir_dif_band(FenestrationCommon::Side t_Side,
                                           const CBeamDirection & t_Direction) override;

    private:
        double Tscatter_single(FenestrationCommon::Side t_Side,
                               const CBeamDirection & t_Direction);
        std::vector<double> Tscatter_range(FenestrationCommon::Side t_Side,
                                           const CBeamDirection & t_Direction);

        // Calculates scattered part of reflection from woven
        double Tscatter(const CBeamDirection & t_Direction, double Rmat) const;
    };

}   // namespace SingleLayerOptics

#endif
