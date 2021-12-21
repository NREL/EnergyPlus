#include "FlatCellDescription.hpp"

using namespace FenestrationCommon;

namespace SingleLayerOptics
{
    CFlatCellDescription::CFlatCellDescription() : ICellDescription()
    {}

    double CFlatCellDescription::T_dir_dir(const Side, const CBeamDirection &)
    {
        return 0;
    }

    double CFlatCellDescription::R_dir_dir(const Side, const CBeamDirection &)
    {
        return 0;
    }

}   // namespace SingleLayerOptics
