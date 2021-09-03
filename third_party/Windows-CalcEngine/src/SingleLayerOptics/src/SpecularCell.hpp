#ifndef SPECULARCELL_H
#define SPECULARCELL_H

#include <memory>
#include <vector>

#include "BaseCell.hpp"

namespace FenestrationCommon
{
    enum class Side;
}

namespace SingleLayerOptics
{
    class CSpecularCellDescription;
    class ICellDescription;
    class CBeamDirection;

    // Calculates spectral properties of specular layer over the given wavelength range and it also
    // calculates spectral properties over given wavelengths
    class CSpecularCell : public CBaseCell
    {
    public:
        CSpecularCell(const std::shared_ptr<CMaterial> & t_MaterialProperties,
                      const std::shared_ptr<ICellDescription> & t_Cell);

        explicit CSpecularCell(const std::shared_ptr<CMaterial> & t_MaterialProperties);

        // Transmittance averaged over entire wavelength spectrum
        double T_dir_dir(FenestrationCommon::Side t_Side,
                         const CBeamDirection & t_Direction) override;

        // Reflectance averaged over entire wavelength spectrum
        double R_dir_dir(FenestrationCommon::Side t_Side,
                         const CBeamDirection & t_Direction) override;

        // Transmittance of specular material for enire wavelength spectrum
        std::vector<double> T_dir_dir_band(FenestrationCommon::Side t_Side,
                                           const CBeamDirection & t_Direction) override;

        // Reflectance of specular material over entire wavelength spectrum
        std::vector<double> R_dir_dir_band(FenestrationCommon::Side t_Side,
                                           const CBeamDirection & t_Direction) override;

    protected:
        std::shared_ptr<CSpecularCellDescription> getCellAsSpecular() const;
    };

}   // namespace SingleLayerOptics

#endif
