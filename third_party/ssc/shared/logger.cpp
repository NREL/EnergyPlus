#include "logger.h"

#include "lib_util.h"
#include "lib_battery_capacity.h"
#include "lib_battery_voltage.h"
#include "lib_battery_lifetime_calendar_cycle.h"
#include "lib_battery.h"

/**
* Helper fx
*/

template<typename T>
std::ostream& operator<<(std::ostream& s, std::vector<T> t) {
    s.precision(2);
    s << "[";
    for (std::size_t i = 0; i < t.size(); i++) {
        s << t[i] << (i == t.size() - 1 ? " " : ", ");
    }
    return s << "]";
}

template<typename T>
std::ostream& operator<<(std::ostream& s, util::matrix_t<T> t) {
    size_t nr = t.nrows();
    size_t nc = t.ncols();

    s.precision(2);
    s << "[";
    for (std::size_t i = 0; i < nr; i++) {
        s << "[";
        for (std::size_t j = 0; j < nc; j++) {
            s << t.at(i, j) << (j == nc - 1 ? "" : ", ");
        }
        s << "]" << (i == nr - 1 ? "" : ", ");;

    }
    return s << "]";
}

std::ostream &operator<<(std::ostream &os, const voltage_params &p) {
    char buf[1024];
    sprintf(buf, "\"voltage_params\": { \"voltage_choice\": %d, \"num_cells_series\": %d, \"num_strings\": %d, "
                 "\"Vnom_default\": %.3f, \"resistance\": %.3f, \"dt_hr\": %.3f, "
                 "\"dynamic\": { \"Vfull\": %.3f, \"Vexp\": %.3f, \"Vnom\": %.3f, "
                 "\"Qfull\": %.3f, \"Qexp\": %.3f, \"Qnom\": %.3f, \"C_rate\": %.3f } }", p.voltage_choice, p.num_cells_series,
            p.num_strings,
            p.Vnom_default, p.resistance, p.dt_hr,
            p.dynamic.Vfull, p.dynamic.Vexp, p.dynamic.Vnom,
            p.dynamic.Qfull, p.dynamic.Qexp, p.dynamic.Qnom, p.dynamic.C_rate);
    os << buf;
    return os;
}

std::ostream &operator<<(std::ostream &os, const voltage_state &p) {
    char buf[128];
    sprintf(buf, R"("voltage_state": { "cell_voltage": %.3f })",
            p.cell_voltage);
    os << buf;
    return os;
}

std::ostream &operator<<(std::ostream &os, const capacity_state &p) {
    char buf[1024];
    sprintf(buf, "\"capacity_state\": { \"q0\": %.3f, \"qmax_lifetime\": %.3f, \"qmax_thermal\": %.3f, \"cell_current\": %.3f, "
                 "\"I_loss\": %.3f, \"SOC\": %.3f, \"SOC_prev\": %.3f, "
                 "\"charge_mode\": %d, \"prev_charge\": %d, \"chargeChange\": %d, "
                 "\"leadacid\": { \"q1_0\": %.3f, \"q2_0\": %.3f, \"qn\": %.3f, \"q2\": %.3f } }",
            p.q0, p.qmax_lifetime, p.qmax_thermal, p.cell_current,
            p.I_loss, p.SOC, p.SOC_prev,
            p.charge_mode, p.prev_charge, p.chargeChange,
            p.leadacid.q1_0, p.leadacid.q2_0, p.leadacid.q1, p.leadacid.q2);
    os << buf;
    return os;
}

std::ostream &operator<<(std::ostream &os, const capacity_params &p) {
    char buf[1024];
    sprintf(buf, "\"capacity_params\": { \"qmax_init\": %.3f, \"initial_SOC\": %.3f, \"maximum_SOC\": %.3f, "
                 "\"minimum_SOC\": %.3f, \"dt_hr\": %.3f, "
                 "\"leadacid\": { \"tn\": %.3f, \"t2\": %.3f, \"F1\": %.3f, \"F2\": %.3f, "
                 "\"q10\": %.3f, \"q20\": %.3f, \"I20\": %.3f} }", p.qmax_init, p.initial_SOC, p.maximum_SOC,
            p.minimum_SOC, p.dt_hr, p.leadacid.tn, p.leadacid.t2, p.leadacid.F1, p.leadacid.F2,
            p.leadacid.q10, p.leadacid.q20, p.leadacid.I20);
    os << buf;
    return os;
}

std::ostream &operator<<(std::ostream &os, const cycle_state &p) {
    char buf[1024];
    sprintf(buf, "\"cycle_state\": { \"q_relative_cycle\": %.3f, "
                 "\"rainflow_Xlt\": %.3f, \"rainflow_Ylt\": %.3f, \"rainflow_jlt\": %d, \"rainflow_peaks\": ",
            p.q_relative_cycle,
            p.rainflow_Xlt, p.rainflow_Ylt, p.rainflow_jlt);
    os << buf << p.rainflow_peaks << " }";
    return os;
}

std::ostream &operator<<(std::ostream &os, const calendar_state &p) {
    char buf[1024];
    sprintf(buf, "\"calendar_state\": { \"q_relative_calendar\": %.3f, "
                 "\"dq_relative_calendar_old\": %.3f }",
            p.q_relative_calendar, p.dq_relative_calendar_old);
    os << buf;
    return os;
}

std::ostream& operator<<(std::ostream& os, const lifetime_nmc_state& p) {
    char buf[1024];
    sprintf(buf, "\"lifetime_nmc_state\": { \"q_relative_li\": %.3f, "
        "\"q_relative_neg\": %.3f, \"dq_relative_li_old\": %.3f, \"dq_relative_neg_old\": %.3f, "
        "\"DOD_max\": %f, \"n_cycles_prev_day\": %d, \"cum_dt\": %.3f, \"b1_dt\": %.3f, "
        "\"b2_dt\": %.3f, \"b3_dt\": %.3f, \"c0_dt\": %.3f, \"c2_dt\": %.3f }",
        p.q_relative_li, p.q_relative_neg, p.dq_relative_li_old, p.dq_relative_neg_old, p.DOD_max, p.n_cycles_prev_day,
        p.cum_dt, p.b1_dt, p.b2_dt, p.b3_dt, p.c0_dt, p.c2_dt);
    os << buf ;
    return os;
}

std::ostream &operator<<(std::ostream &os, const lifetime_state &p) {
    os.precision(3);
    char buf[1024];
    sprintf(buf, R"("lifetime_state": { "q_relative": %f, "n_cycles": %d, "range": %.3f, "average_range": %.3f,
                 "day_age_of_battery": %.3f, )",
            p.q_relative, p.n_cycles, p.range, p.average_range, p.day_age_of_battery);
    os << buf << *p.cycle << ", " << *p.calendar << ", " << *p.nmc_li_neg << " }";
    return os;
}

std::ostream &operator<<(std::ostream &os, const calendar_cycle_params &p) {
    os << R"("calendar_cycle_params": { "cycling_matrix": )" << p.cycling_matrix;

    char buf[1024];
    sprintf(buf, ", \"calendar_choice\": %d, \"calendar_q0\": %.3f, "
                 "\"calendar_a\": %.3f, \"calendar_b\": %.3f, "
                 "\"calendar_c\": %.3f, ", p.calendar_choice, p.calendar_q0,
            p.calendar_a, p.calendar_b, p.calendar_c);
    os << buf;
    os << R"("calendar_matrix": )" << p.calendar_matrix << " }";
    return os;
}

std::ostream &operator<<(std::ostream &os, const lifetime_params &p) {
    os.precision(3);
    char buf[1024];
    sprintf(buf, R"("lifetime_params": { "dt_hr": %.3f, "model_choice": %d, )", p.dt_hr, p.model_choice);
    os << buf;
    os << *p.cal_cyc << " }";
    return os;
}

std::ostream &operator<<(std::ostream &os, const replacement_state &p) {
    char buf[256];
    sprintf(buf, R"("replacement_state": { "n_replacements": %d, "indices_replaced": )", p.n_replacements);
    os << buf << p.indices_replaced << " }";
    return os;
}

std::ostream &operator<<(std::ostream &os, const replacement_params &p) {
    char buf[256];
    sprintf(buf, R"("replacement_params": {"replacement_option": %d, "replacement_capacity": %.3f, )", p.replacement_option, p.replacement_capacity);
    os << buf;
    os << R"("replacement_schedule_percent": )" << p.replacement_schedule_percent;
    os << " }";
    return os;
}

std::ostream &operator<<(std::ostream &os, const thermal_state &p) {
    char buf[256];
    sprintf(buf, R"("thermal_state": { "q_relative_thermal": %.3f, "T_batt": %.3f, "T_room": %.3f, "heat_dissipated": %.3f, "T_batt_prev": %.3f })",
            p.q_relative_thermal, p.T_batt, p.T_room, p.heat_dissipated, p.T_batt_prev);
    os << buf;
    return os;
}

std::ostream &operator<<(std::ostream &os, const thermal_params &p) {
    char buf[1024];
    sprintf(buf, "\"thermal_params\": { \"dt_hr\": %.3f, \"mass\": %.3f, \"surface_area\": %.3f, "
                 "\"Cp\": %.3f, \"h\": %.3f, \"resistance\": %.3e, \"en_cap_vs_temp\": %d, \"cap_vs_temp\": ",
            p.dt_hr, p.mass, p.surface_area,
            p.Cp, p.h, p.resistance, p.en_cap_vs_temp);
    os << buf << p.cap_vs_temp;
    os.precision(3);
    os << R"(, "option": )" << p.option;
    os << R"(, "T_room_schedule": )" << p.T_room_schedule << " }";
    return os;
}

std::ostream &operator<<(std::ostream& os, const losses_state &p) {
    char buf[256];
    sprintf(buf, R"("losses_state": { "loss_percent": %.3f })", p.loss_kw);
    os << buf;
    return os;
}

std::ostream &operator<<(std::ostream& os, const losses_params &p) {
    os.precision(3);
    os << R"("losses_params": { "loss_choice": )" << p.loss_choice << ", ";
    os << R"("monthly_charge_loss": )" << p.monthly_charge_loss << ", ";
    os << R"("monthly_discharge_loss": )" << p.monthly_discharge_loss << ", ";
    os << R"("monthly_idle_loss": )" << p.monthly_idle_loss << ", ";
    os << R"("schedule_loss": )" << p.schedule_loss << " }";
    return os;
}

std::ostream &operator<<(std::ostream &os, const battery_state &p) {
    char buf[1024];
    sprintf(buf, "\"battery_state\": { \"last_idx\": %zu, \"V\": %.3f, \"Q\": %.3f, \"Q_max\": %.3f, \"I\": %.3f, "
                 "\"I_dischargeable\" %.3f, \"I_chargeable\" %.3f, "
                 "\"P\": %.3f, \"P_dischargeable\": %.3f, \"P_chargeable\": %.3f, ",
            p.last_idx, p.V, p.Q, p.Q_max, p.I,
            p.I_dischargeable, p.I_chargeable,
            p.P, p.P_dischargeable, p.P_chargeable);
    os << buf << ", ";
    os << *p.capacity << ", ";
    os << *p.voltage << ", ";
    os << *p.thermal << ", ";
    os << *p.lifetime << ", ";
    os << *p.losses << ", ";
    os << *p.replacement << " }";
    return os;
}

std::ostream &operator<<(std::ostream &os, const battery_params &p) {
    char buf[1024];
    sprintf(buf, R"("battery_params": { "chem": %u, "dt_hr": %.3f, "nominal_voltage": %.3f, "nominal_energy": %.3f)",
            p.chem, p.dt_hr, p.nominal_voltage, p.nominal_energy);
    os << buf << ", ";
    os << *p.capacity << ", ";
    os << *p.voltage << ", ";
    os << *p.thermal << ", ";
    os << *p.lifetime << ", ";
    os << *p.losses << ", ";
    os << *p.replacement << " }";
    return os;
}
