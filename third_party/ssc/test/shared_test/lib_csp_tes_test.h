#ifndef __LIB_CSP_TES_TEST_H__
#define __LIB_CSP_TES_TEST_H__

#include <gtest/gtest.h>
#include "../tcs/storage_hx.h"

using Tank = Storage_HX;

namespace csp_common
{
    struct TankSpecifications;          // forward declaration
    struct TankState;
    struct TankExternalConditions;

    const double kErrorToleranceLo = 0.001;    // 0.1%
    const double kErrorToleranceHi = 0.01;     // 1.0%

    class TankFactory {
    public:
        TankFactory() {};
        virtual std::unique_ptr<Tank> MakeTank(TankSpecifications* tank_specifications) const = 0;
        virtual std::unique_ptr<TankSpecifications> MakeSpecifications() const = 0;
        virtual TankState MakeTankState() const = 0;
        virtual TankExternalConditions MakeExternalConditions() const = 0;
    };

    class DefaultTankFactory : public TankFactory {
    public:
        DefaultTankFactory() {};
        virtual std::unique_ptr<Tank> MakeTank(TankSpecifications* tank_specifications) const;
        virtual std::unique_ptr<TankSpecifications> MakeSpecifications() const;
        virtual TankState MakeTankState() const;
        virtual TankExternalConditions MakeExternalConditions() const;
    };

    struct TankSpecifications
    {
        int field_fluid;
        int store_fluid;
        HTFProperties fluid_field;
        HTFProperties fluid_store;
        bool is_direct;
        int config;
        double duty_des;
        double vol_des;
        double h_des;
        double u_des;
        double tank_pairs_des;
        double hot_htr_set_point_des;
        double cold_htr_set_point_des;
        double max_q_htr_cold;
        double max_q_htr_hot;
        double dt_hot_des;
        double dt_cold_des;
        double T_h_in_des;
        double T_h_out_des;
    };

    struct TankState
    {
        double m_prev;
        double T_prev;
    };

    struct TankExternalConditions
    {
        double m_dot_in;
        double m_dot_out;
        double T_in;
        double T_amb;
    };
}

#endif
