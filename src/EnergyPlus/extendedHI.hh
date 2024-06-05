
#ifndef extendedHI_hh_INCLUDED
#define extendedHI_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace extendedHI {

    Real64 pvstar(Real64 T);
    Real64 Le(Real64 T);
    Real64 Qv(Real64 Ta, Real64 Pa);
    Real64 Zs(Real64 Rs);
    Real64 Ra(Real64 Ts, Real64 Ta);
    Real64 Ra_bar(Real64 Tf, Real64 Ta);
    Real64 Ra_un(Real64 Ts, Real64 Ta);
    std::tuple<std::string, double, double, double, double> find_eqvar(EnergyPlusData &state, double Ta, double RH);
    std::pair<Real64, std::string> find_T(EnergyPlusData &state, std::string eqvar_name, Real64 eqvar);
    Real64 heatindex(EnergyPlusData &state, Real64 Ta, Real64 RH, bool show_info);

} // namespace extendedHI
} // namespace EnergyPlus

#endif