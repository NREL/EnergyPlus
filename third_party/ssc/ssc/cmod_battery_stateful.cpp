/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <lib_battery_capacity.h>
#include "common.h"
#include "vartab.h"
#include "core.h"
#include "cmod_battery_stateful.h"

var_info vtab_battery_stateful_inputs[] = {
        /*   VARTYPE           DATATYPE         NAME                                            LABEL                                                   UNITS      META                   GROUP           REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
        { SSC_INPUT,        SSC_NUMBER,      "control_mode",                               "Control using current (0) or power (1)",                  "0/1",      "",   "Controls",       "*",                           "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "dt_hr",                                      "Time step in hours",                                      "hr",      "",   "Controls",       "*",                           "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "input_current",                              "Current at which to run battery",                         "A",       "",   "Controls",       "control_mode=0",              "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "input_power",                                "Power at which to run battery",                           "kW",      "",   "Controls",       "control_mode=1",              "",                              "" },

        { SSC_INPUT,        SSC_NUMBER,      "chem",                                       "Lead Acid (0), Li Ion (1), Vanadium Redox (2), Iron Flow (3)","0/1/2/3","",   "ParamsCell",       "*",                           "",                              "" },
        { SSC_INOUT,        SSC_NUMBER,      "nominal_energy",                             "Nominal installed energy",                                "kWh",     "",                     "ParamsPack",       "*",                           "",                              "" },
        { SSC_INOUT,        SSC_NUMBER,      "nominal_voltage",                            "Nominal DC voltage",                                      "V",       "",                     "ParamsPack",       "*",                           "",                              "" },

        // capacity
        { SSC_INPUT,        SSC_NUMBER,      "initial_SOC",		                          "Initial state-of-charge",                                 "%",       "",                     "ParamsCell",       "*",                           "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "minimum_SOC",		                          "Minimum allowed state-of-charge",                         "%",       "",                     "ParamsCell",       "*",                           "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "maximum_SOC",                                "Maximum allowed state-of-charge",                         "%",       "",                     "ParamsCell",       "*",                           "",                              "" },
        { SSC_INPUT,		SSC_NUMBER,		 "leadacid_q20",	                              "Capacity at 20-hour discharge rate",                      "Ah",       "",                     "ParamsCell",       "chem=0",                           "",                             "" },
        { SSC_INPUT,		SSC_NUMBER,		 "leadacid_q10",	                              "Capacity at 10-hour discharge rate",                      "Ah",       "",                     "ParamsCell",       "chem=0",                           "",                             "" },
        { SSC_INPUT,		SSC_NUMBER,		 "leadacid_qn",	                              "Capacity at discharge rate for n-hour rate",              "Ah",       "",                     "ParamsCell",       "chem=0",                           "",                             "" },
        { SSC_INPUT,		SSC_NUMBER,		 "leadacid_tn",	                              "Hours to discharge for qn rate",                          "h",        "",                     "ParamsCell",       "chem=0",                           "",                             "" },

        // Voltage discharge curve
        { SSC_INPUT,        SSC_NUMBER,      "voltage_choice",                             "Battery voltage input option",                            "0/1",     "0=Model,1=Table",      "ParamsCell",       "?=0",                        "",                             "" },
        { SSC_INPUT,		SSC_MATRIX,      "voltage_matrix",                             "Table with depth-of-discharge % and Voltage as columns",  "[[%, V]]","",                     "ParamsCell",       "voltage_choice=1",      "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "Vnom_default",                               "Default nominal cell voltage",                            "V",       "",                     "ParamsCell",       "*",                          "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "resistance",                                 "Internal resistance",                                     "Ohm",     "",                     "ParamsCell",       "*",                           "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "Qfull",                                      "Fully charged cell capacity",                             "Ah",      "",                     "ParamsCell",       "*",                       "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "Qexp",                                       "Cell capacity at end of exponential zone",                "Ah",      "",                     "ParamsCell",       "voltage_choice=0&chem~2", "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "Qnom",                                       "Cell capacity at end of nominal zone",                    "Ah",      "",                     "ParamsCell",       "voltage_choice=0&chem~2", "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "Vfull",                                      "Fully charged cell voltage",                              "V",       "",                     "ParamsCell",       "voltage_choice=0&chem~2", "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "Vexp",                                       "Cell voltage at end of exponential zone",                 "V",       "",                     "ParamsCell",       "voltage_choice=0&chem~2",  "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "Vnom",                                       "Cell voltage at end of nominal zone",                     "V",       "",                     "ParamsCell",       "voltage_choice=0&chem~2", "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "C_rate",                                     "Rate at which voltage vs. capacity curve input",          "",        "",                     "ParamsCell",       "voltage_choice=0&chem~2", "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "Qfull_flow",                                 "Fully charged flow battery capacity",                     "Ah",      "",                     "ParamsCell",       "voltage_choice=0&chem=3", "",                              "" },

        // thermal inputs
        { SSC_INPUT,        SSC_NUMBER,      "mass",                                       "Battery mass",                                            "kg",       "",                     "ParamsPack",       "*",                           "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "surface_area",                               "Battery surface area",                                    "m^2",      "",                     "ParamsPack",       "*",                           "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "Cp",                                         "Battery specific heat capacity",                          "J/KgK",    "",                     "ParamsPack",       "*",                           "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "h",                                          "Heat transfer between battery and environment",           "W/m2K",    "",                     "ParamsPack",       "*",                           "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "T_room_init",                                "Temperature of storage room",                             "C",        "",                     "ParamsPack",       "*",                           "",                             "" },
        { SSC_INPUT,        SSC_MATRIX,      "cap_vs_temp",                                "Table with Temperature and Capacity % as columns",        "[[C,%]]",  "",                     "ParamsPack",       "life_model=0",                "",                             "" },

        // lifetime inputs
        { SSC_INPUT,		SSC_NUMBER,      "life_model",                                 "Battery life model specifier",                            "0/1",      "0=calendar/cycle,1=NMC", "ParamsCell",       "*",                                   "",                             "" },
        { SSC_INPUT,		SSC_MATRIX,      "cycling_matrix",                             "Table with DOD %, Cycle #, and Capacity % columns",       "[[%, #, %]]","",                     "ParamsCell",       "life_model=0",                        "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "calendar_choice",                            "Calendar life degradation input option",                  "0/1/2",    "0=None,1=LithiomIonModel,2=InputLossTable",  "ParamsCell",       "life_model=0",    "",                             "" },
        { SSC_INPUT,        SSC_MATRIX,      "calendar_matrix",                            "Table with Day # and Capacity % columns",                 "[[#, %]]", "",                     "ParamsCell",       "life_model=0&calendar_choice=2",        "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "calendar_q0",                                "Calendar life model initial capacity cofficient",         "",         "",                     "ParamsCell",       "life_model=0&calendar_choice=1",        "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "calendar_a",                                 "Calendar life model coefficient",                         "1/sqrt(day)","",                   "ParamsCell",       "life_model=0&calendar_choice=1",        "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "calendar_b",                                 "Calendar life model coefficient",                         "K",        "",                     "ParamsCell",       "life_model=0&calendar_choice=1",        "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "calendar_c",                                 "Calendar life model coefficient",                         "K",        "",                     "ParamsCell",       "life_model=0&calendar_choice=1",        "",                             "" },

        // losses
        { SSC_INPUT,        SSC_NUMBER,      "loss_choice",                                "Loss power input option",                                 "0/1",        "0=Monthly,1=TimeSeries", "ParamsPack",       "?=0",                        "",                             "" },
        { SSC_INPUT,        SSC_ARRAY,       "monthly_charge_loss",                        "Battery system losses when charging",                     "[kW]",       "",                     "ParamsPack",       "?=0",                        "",                             "" },
        { SSC_INPUT,        SSC_ARRAY,       "monthly_discharge_loss",                     "Battery system losses when discharging",                  "[kW]",       "",                     "ParamsPack",       "?=0",                        "",                             "" },
        { SSC_INPUT,        SSC_ARRAY,       "monthly_idle_loss",                          "Battery system losses when idle",                         "[kW]",       "",                     "ParamsPack",       "?=0",                        "",                             "" },
        { SSC_INPUT,        SSC_ARRAY,       "schedule_loss",                              "Battery system losses at each timestep",                  "[kW]",       "",                     "ParamsPack",       "?=0",                        "",                             "" },

        // replacement inputs
        { SSC_INPUT,        SSC_NUMBER,      "replacement_option",                         "Replacements: none (0), by capacity (1), or schedule (2)", "0=none,1=capacity limit,2=yearly schedule", "", "ParamsPack", "?=0",                  "INTEGER,MIN=0,MAX=2",          "" },
        { SSC_INPUT,        SSC_NUMBER,      "replacement_capacity",                       "Capacity degradation at which to replace battery",       "%",        "",                     "ParamsPack",       "replacement_option=1",                           "",                             "" },
        { SSC_INPUT,        SSC_ARRAY,       "replacement_schedule_percent",               "Percentage of battery capacity to replace in each year", "[%/year]","length <= analysis_period",                  "ParamsPack",      "replacement_option=2",   "",                             "" },
        var_info_invalid
};

var_info vtab_battery_state[] = {
        // battery pack
        { SSC_INOUT,        SSC_NUMBER,     "last_idx",                  "Last index (lifetime)",                                    "",          "",                     "StatePack",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "V",                         "Voltage",                                                  "V",         "",                     "StatePack",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "P",                         "Power",                                                    "kW",        "",                     "StatePack",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "Q",                         "Capacity",                                                 "Ah",        "",                     "StatePack",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "Q_max",                     "Max Capacity",                                             "Ah",        "",                     "StatePack",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "I",                         "Current",                                                  "A",         "",                     "StatePack",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "I_dischargeable",           "Estimated max dischargeable current",                      "A",         "",                     "StatePack",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "I_chargeable",              "Estimated max chargeable current",                         "A",         "",                     "StatePack",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "P_dischargeable",           "Estimated max dischargeable power",                        "kW",        "",                     "StatePack",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "P_chargeable",              "Estimated max chargeable power ",                          "kW",        "",                     "StatePack",       "",                           "",                               ""  },

        // capacity
        { SSC_INOUT,        SSC_NUMBER,     "SOC",                       "State of Charge",                                          "%",         "",                     "StatePack",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "q0",                        "Cell capacity at timestep",                                "Ah",        "",                     "StateCell",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "qmax_lifetime",             "Maximum possible cell capacity",                           "Ah",        "",                     "StateCell",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "qmax_thermal",              "Maximum cell capacity adjusted for temperature effects",   "Ah",        "",                     "StateCell",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "cell_current",              "Cell current",                                             "A",         "",                     "StateCell",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "I_loss",                    "Lifetime and thermal losses",                              "A",         "",                     "StateCell",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "charge_mode",               "Charge (0), Idle (1), Discharge (2)",                      "0/1/2",     "",                     "StateCell",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "SOC_prev",                  "State of Charge of last time step",                        "%",         "",                     "StateCell",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "prev_charge",               "Charge mode of last time step",                            "0/1/2",     "",                     "StateCell",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "chargeChange",              "Whether Charge mode changed since last step",              "0/1",       "",                     "StateCell",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "q1_0",                      "Lead acid - Cell charge available",                        "Ah",        "",                     "StateCell",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "q2_0",                      "Lead acid - Cell charge bound",                            "Ah",        "",                     "StateCell",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "qn",                        "Lead acid - Cell capacity at n-hr discharge rate",         "Ah",        "",                     "StateCell",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "q2",                        "Lead acid - Cell capacity at 10-hr discharge rate",        "Ah",        "",                     "StateCell",       "",                           "",                               ""  },

        // voltage
        { SSC_INOUT,        SSC_NUMBER,     "cell_voltage",              "Cell voltage",                                             "V",         "",                     "StateCell",        "",                           "",                               ""  },

        // thermal
        { SSC_INOUT,        SSC_NUMBER,     "q_relative_thermal",        "Relative capacity due to thermal effects",                 "Ah",        "",                     "StateCell",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "T_batt",                    "Battery temperature averaged over time step",              "C",         "",                     "StatePack",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "T_room",                    "Room temperature",                                         "C",         "",                     "StatePack",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "heat_dissipated",           "Heat dissipated due to flux",                              "kW",        "",                     "StatePack",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "T_batt_prev",               "Battery temperature at end of last time step",             "C",         "",                     "StateCell",        "",                           "",                               ""  },

        // lifetime
        { SSC_INOUT,        SSC_NUMBER,     "q_relative",                "Overall relative capacity due to lifetime effects",        "Ah",        "",                     "StateCell",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "q_relative_cycle",          "Relative capacity due to cycling effects",                 "%",         "",                     "StateCell",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "n_cycles",                  "Number of cycles",                                         "",          "",                     "StateCell",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "range",                     "Cycle range",                                              "%",         "",                     "StateCell",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "average_range",             "Average cycle range",                                      "%",         "",                     "StateCell",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "rainflow_Xlt",              "Rainflow range of second to last half cycle",              "%",         "",                     "StateCell",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "rainflow_Ylt",              "Rainflow range of last half cycle",                        "%",         "",                     "StateCell",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "rainflow_jlt",              "Rainflow number of turning points",                        "",          "",                     "StateCell",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_ARRAY,      "rainflow_peaks",            "Rainflow peaks of DOD",                                    "[%]",       "",                     "StateCell",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "q_relative_calendar",       "Relative capacity due to calendar effects",                "%",         "",                     "StateCell",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "day_age_of_battery",        "Day age of battery",                                       "day",       "",                     "StateCell",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "dq_relative_calendar_old",  "Change in capacity of last time step",                     "%",         "",                     "StateCell",        "",                           "",                               ""  },

        // losses
        { SSC_INOUT,        SSC_NUMBER,     "loss_kw",                   "Ancillary power loss (kW DC for DC connected, AC for AC connected)", "kW", "",                  "StatePack",          "",                           "",                               ""  },

        // replacements
        { SSC_INOUT,        SSC_NUMBER,     "n_replacements",            "Number of replacements at current year",                   "",         "",                      "StatePack",     "",                           "",                               ""  },
        { SSC_INOUT,        SSC_ARRAY,      "indices_replaced",          "Lifetime indices of replacement occurrences",              "",         "",                      "StatePack",     "",                           "",                               ""  },

        var_info_invalid };

void write_battery_state(const battery_state& state, var_table* vt) {
    vt->assign_match_case("last_idx", (int)state.last_idx);
    vt->assign_match_case("V", state.V);
    vt->assign_match_case("Q", state.Q);
    vt->assign_match_case("Q_max", state.Q_max);
    vt->assign_match_case("I", state.I);
    vt->assign_match_case("I_dischargeable", state.I_dischargeable);
    vt->assign_match_case("I_chargeable", state.I_chargeable);
    vt->assign_match_case("P", state.P);
    vt->assign_match_case("P_dischargeable", state.P_dischargeable);
    vt->assign_match_case("P_chargeable", state.P_chargeable);

    auto cap = state.capacity;
    vt->assign_match_case("q0", cap->q0);
    vt->assign_match_case("qmax_lifetime", cap->qmax_lifetime);
    vt->assign_match_case("qmax_thermal", cap->qmax_thermal);
    vt->assign_match_case("cell_current", cap->cell_current);
    vt->assign_match_case("I_loss", cap->I_loss);
    vt->assign_match_case("SOC", cap->SOC);
    vt->assign_match_case("SOC_prev", cap->SOC_prev);
    vt->assign_match_case("charge_mode", cap->charge_mode);
    vt->assign_match_case("prev_charge", cap->prev_charge);
    vt->assign_match_case("chargeChange", cap->chargeChange);

    int choice;
    vt_get_int(vt, "chem", &choice);
    if (choice == battery_params::CHEM::LEAD_ACID) {
        vt->assign_match_case("q1_0", cap->leadacid.q1_0);
        vt->assign_match_case("q2_0", cap->leadacid.q2_0);
        vt->assign_match_case("qn", cap->leadacid.q1);
        vt->assign_match_case("q2", cap->leadacid.q2);
    }

    vt->assign_match_case("cell_voltage", state.voltage->cell_voltage);

    auto thermal = state.thermal;
    vt->assign_match_case("q_relative_thermal", thermal->q_relative_thermal);
    vt->assign_match_case("T_batt", thermal->T_batt);
    vt->assign_match_case("T_room", thermal->T_room);
    vt->assign_match_case("heat_dissipated", thermal->heat_dissipated);
    vt->assign_match_case("T_batt_prev", thermal->T_batt_prev);

    auto lifetime = state.lifetime;
    vt->assign_match_case("q_relative", lifetime->q_relative);
    vt->assign_match_case("n_cycles", lifetime->n_cycles);
    vt->assign_match_case("range", lifetime->range);
    vt->assign_match_case("average_range", lifetime->average_range);
    vt->assign_match_case("day_age_of_battery", lifetime->day_age_of_battery);

    vt_get_int(vt, "life_model", &choice);
    vt->assign_match_case("q_relative_cycle", lifetime->cycle->q_relative_cycle);
    if (!lifetime->cycle->rainflow_peaks.empty()) {
        vt->assign_match_case("rainflow_peaks", lifetime->cycle->rainflow_peaks);
    }
    vt->assign_match_case("rainflow_Xlt", lifetime->cycle->rainflow_Xlt);
    vt->assign_match_case("rainflow_Ylt", lifetime->cycle->rainflow_Ylt);
    vt->assign_match_case("rainflow_jlt", lifetime->cycle->rainflow_jlt);
    if (choice == lifetime_params::CALCYC) {
        vt->assign_match_case("q_relative_calendar", lifetime->calendar->q_relative_calendar);
        vt->assign_match_case("dq_relative_calendar_old", lifetime->calendar->dq_relative_calendar_old);
    }
    else if (choice == lifetime_params::NMCNREL) {
        vt->assign_match_case("q_relative_li", lifetime->nmc_li_neg->q_relative_li);
        vt->assign_match_case("q_relative_neg", lifetime->nmc_li_neg->q_relative_neg);
        vt->assign_match_case("dq_relative_li_old", lifetime->nmc_li_neg->dq_relative_li_old);
        vt->assign_match_case("dq_relative_neg_old", lifetime->nmc_li_neg->dq_relative_neg_old);
        vt->assign_match_case("DOD_max", lifetime->nmc_li_neg->DOD_max);
        vt->assign_match_case("n_cycles_prev_day", lifetime->nmc_li_neg->n_cycles_prev_day);
        vt->assign_match_case("cum_dt", lifetime->nmc_li_neg->cum_dt);
        vt->assign_match_case("b1_dt", lifetime->nmc_li_neg->b1_dt);
        vt->assign_match_case("b2_dt", lifetime->nmc_li_neg->b2_dt);
        vt->assign_match_case("b3_dt", lifetime->nmc_li_neg->b3_dt);
        vt->assign_match_case("c0_dt", lifetime->nmc_li_neg->c0_dt);
        vt->assign_match_case("c2_dt", lifetime->nmc_li_neg->c2_dt);
    }

    vt->assign_match_case("loss_kw", state.losses->loss_kw);

    vt->assign_match_case("n_replacements", state.replacement->n_replacements);
    vt->assign_match_case( "indices_replaced", state.replacement->indices_replaced);
}

void read_battery_state(battery_state& state, var_table* vt) {
    vt_get_uint(vt, "last_idx", &state.last_idx);
    vt_get_number(vt, "I", &state.I);
    vt_get_number(vt, "I_dischargeable", &state.I_dischargeable);
    vt_get_number(vt, "I_chargeable", &state.I_chargeable);
    vt_get_number(vt, "V", &state.V);
    vt_get_number(vt, "P", &state.P);
    vt_get_number(vt, "Q", &state.Q);
    vt_get_number(vt, "Q_max", &state.Q_max);
    vt_get_number(vt, "P_dischargeable", &state.P_dischargeable);
    vt_get_number(vt, "P_chargeable", &state.P_chargeable);

    auto cap = state.capacity;
    vt_get_number(vt, "q0", &cap->q0);
    vt_get_number(vt, "qmax_lifetime", &cap->qmax_lifetime);
    vt_get_number(vt, "qmax_thermal", &cap->qmax_thermal);
    vt_get_number(vt, "cell_current", &cap->cell_current);
    vt_get_number(vt, "I_loss", &cap->I_loss);
    vt_get_number(vt, "SOC", &cap->SOC);
    vt_get_number(vt, "SOC_prev", &cap->SOC_prev);
    vt_get_int(vt, "charge_mode", &cap->charge_mode);
    vt_get_int(vt, "prev_charge", &cap->prev_charge);
    vt_get_bool(vt, "chargeChange", &cap->chargeChange);

    int choice;
    vt_get_int(vt, "chem", &choice);
    if (choice == battery_params::CHEM::LEAD_ACID) {
        vt_get_number(vt, "q1_0", &cap->leadacid.q1_0);
        vt_get_number(vt, "q2_0", &cap->leadacid.q2_0);
        vt_get_number(vt, "qn", &cap->leadacid.q1);
        vt_get_number(vt, "q2", &cap->leadacid.q2);
    }

    vt_get_number(vt, "cell_voltage", &state.voltage->cell_voltage);

    auto thermal = state.thermal;
    vt_get_number(vt, "q_relative_thermal", &thermal->q_relative_thermal);
    vt_get_number(vt, "T_batt", &thermal->T_batt);
    vt_get_number(vt, "T_room", &thermal->T_room);
    vt_get_number(vt, "heat_dissipated", &thermal->heat_dissipated);
    vt_get_number(vt, "T_batt_prev", &thermal->T_batt_prev);

    auto lifetime = state.lifetime;
    vt_get_number(vt, "q_relative", &lifetime->q_relative);
    vt_get_number(vt, "q_relative_cycle", &lifetime->cycle->q_relative_cycle);
    vt_get_int(vt, "n_cycles", &lifetime->n_cycles);
    vt_get_number(vt, "range", &lifetime->range);
    vt_get_number(vt, "average_range", &lifetime->average_range);
    vt_get_number(vt, "day_age_of_battery", &lifetime->day_age_of_battery);

    vt_get_int(vt, "life_model", &choice);
    vt_get_number(vt, "rainflow_Xlt", &lifetime->cycle->rainflow_Xlt);
    vt_get_number(vt, "rainflow_Ylt", &lifetime->cycle->rainflow_Ylt);
    vt_get_int(vt, "rainflow_jlt", &lifetime->cycle->rainflow_jlt);
    if (vt->is_assigned("rainflow_peaks"))
    {
        // If not assigned, leave empty
        vt_get_array_vec(vt, "rainflow_peaks", lifetime->cycle->rainflow_peaks);
    }
    if (choice == lifetime_params::CALCYC) {
        vt_get_number(vt, "q_relative_calendar", &lifetime->calendar->q_relative_calendar);
        vt_get_number(vt, "dq_relative_calendar_old", &lifetime->calendar->dq_relative_calendar_old);
    }
    else {
        vt_get_number(vt, "q_relative_li", &lifetime->nmc_li_neg->q_relative_li);
        vt_get_number(vt, "q_relative_neg", &lifetime->nmc_li_neg->q_relative_neg);
        vt_get_number(vt, "dq_relative_li_old", &lifetime->nmc_li_neg->dq_relative_li_old);
        vt_get_number(vt, "dq_relative_neg_old", &lifetime->nmc_li_neg->dq_relative_neg_old);
        vt_get_number(vt, "DOD_max", &lifetime->nmc_li_neg->DOD_max);
        vt_get_int(vt, "n_cycles_prev_day", &lifetime->nmc_li_neg->n_cycles_prev_day);
        vt_get_number(vt, "cum_dt", &lifetime->nmc_li_neg->cum_dt);
        vt_get_number(vt, "b1_dt", &lifetime->nmc_li_neg->b1_dt);
        vt_get_number(vt, "b2_dt", &lifetime->nmc_li_neg->b2_dt);
        vt_get_number(vt, "b3_dt", &lifetime->nmc_li_neg->b3_dt);
        vt_get_number(vt, "c0_dt", &lifetime->nmc_li_neg->c0_dt);
        vt_get_number(vt, "c2_dt", &lifetime->nmc_li_neg->c2_dt);
    }

    vt_get_number(vt, "loss_kw", &state.losses->loss_kw);

    vt_get_int(vt, "n_replacements", &state.replacement->n_replacements);
    vt_get_array_vec(vt, "indices_replaced", state.replacement->indices_replaced);
}

std::shared_ptr<battery_params> create_battery_params(var_table *vt, double dt_hr) {
    auto params = std::make_shared<battery_params>();
    int chem;
    vt_get_int(vt, "chem", &chem);
    params->chem = static_cast<battery_params::CHEM>(chem);
    params->dt_hr = dt_hr;

    // voltage
    auto voltage = params->voltage;
    int choice;
    vt_get_int(vt, "voltage_choice", &choice);
    voltage->voltage_choice = static_cast<voltage_params::MODE>(choice);
    voltage->dt_hr = dt_hr;
    vt_get_number(vt, "Vnom_default", &voltage->Vnom_default);
    vt_get_number(vt, "Qfull", &voltage->dynamic.Qfull);
    vt_get_number(vt, "resistance", &voltage->resistance);

    double nominal_e, nominal_v;
    vt_get_number(vt, "nominal_energy", &nominal_e);
    vt_get_number(vt, "nominal_voltage", &nominal_v);
    voltage->num_cells_series = (int)std::ceil(nominal_v / voltage->Vnom_default);
    voltage->num_strings = (int)std::round((nominal_e * 1000.) / (voltage->dynamic.Qfull * voltage->num_cells_series * voltage->Vnom_default));
    params->nominal_voltage = voltage->Vnom_default * voltage->num_cells_series;
    params->nominal_energy = params->nominal_voltage * voltage->num_strings * voltage->dynamic.Qfull * 1e-3;

    if (voltage->voltage_choice == voltage_params::TABLE || params->chem == battery_params::IRON_FLOW) {
        vt_get_matrix_vec(vt, "voltage_matrix", voltage->voltage_table);
    }
    else {
        if (params->chem == battery_params::LEAD_ACID  || params->chem == battery_params::LITHIUM_ION) {
            vt_get_number(vt, "Vfull", &voltage->dynamic.Vfull);
            vt_get_number(vt, "Vexp", &voltage->dynamic.Vexp);
            vt_get_number(vt, "Vnom", &voltage->dynamic.Vnom);
            vt_get_number(vt, "Qfull", &voltage->dynamic.Qfull);
            vt_get_number(vt, "Qexp", &voltage->dynamic.Qexp);
            vt_get_number(vt, "Qnom", &voltage->dynamic.Qnom);
            vt_get_number(vt, "C_rate", &voltage->dynamic.C_rate);
        }
    }

    // capacity
    auto capacity = params->capacity;
    vt_get_number(vt, "initial_SOC", &capacity->initial_SOC);
    vt_get_number(vt, "maximum_soc", &capacity->maximum_SOC);
    vt_get_number(vt, "minimum_soc", &capacity->minimum_SOC);
    capacity->dt_hr = dt_hr;
    if (params->chem == battery_params::LEAD_ACID) {
        vt_get_number(vt, "leadacid_tn", &capacity->leadacid.tn);
        vt_get_number(vt, "leadacid_qn", &capacity->leadacid.qn);
        vt_get_number(vt, "leadacid_q10", &capacity->leadacid.q10);
        vt_get_number(vt, "leadacid_q20", &capacity->leadacid.q20);
    }
    else if (chem == battery_params::LITHIUM_ION)
    {
        capacity->qmax_init = voltage->dynamic.Qfull * voltage->num_strings;
    }
    else if (chem == battery_params::VANADIUM_REDOX || chem == battery_params::IRON_FLOW)
    {
        capacity->qmax_init = voltage->dynamic.Qfull;
    }

    // lifetime
    auto lifetime = params->lifetime;
    vt_get_number(vt, "dt_hr", &lifetime->dt_hr);
    vt_get_int(vt, "life_model", &choice);
    lifetime->model_choice = static_cast<lifetime_params::MODEL_CHOICE>(choice);

    if (lifetime->model_choice == lifetime_params::CALCYC) {
        vt_get_int(vt, "calendar_choice", &choice);
        lifetime->cal_cyc->calendar_choice = static_cast<calendar_cycle_params::CALENDAR_CHOICE>(choice);
        lifetime->dt_hr = dt_hr;
        vt_get_matrix(vt, "cycling_matrix", lifetime->cal_cyc->cycling_matrix);
        if (lifetime->cal_cyc->calendar_choice == calendar_cycle_params::CALENDAR_CHOICE::MODEL) {
            vt_get_number(vt, "calendar_q0", &lifetime->cal_cyc->calendar_q0);
            vt_get_number(vt, "calendar_a", &lifetime->cal_cyc->calendar_a);
            vt_get_number(vt, "calendar_b", &lifetime->cal_cyc->calendar_b);
            vt_get_number(vt, "calendar_c", &lifetime->cal_cyc->calendar_c);
        }
        else if (lifetime->cal_cyc->calendar_choice == calendar_cycle_params::CALENDAR_CHOICE::TABLE) {
            vt_get_matrix(vt, "calendar_matrix", lifetime->cal_cyc->calendar_matrix);
        }
    }
    else if (lifetime->model_choice == lifetime_params::NMCNREL) {
        if (chem != battery_params::LITHIUM_ION)
            throw exec_error("battery_stateful", "NMC life model (life_model=1) can only be used with Li-Ion chemistries (batt_chem=1).");
    }

    // thermal
    auto thermal = params->thermal;
    thermal->dt_hr = dt_hr;
    thermal->option = thermal_params::VALUE;
    thermal->resistance = params->voltage->resistance;
    vt_get_number(vt, "mass", &thermal->mass);
    vt_get_number(vt, "surface_area", &thermal->surface_area);
    vt_get_number(vt, "Cp", &thermal->Cp);
    vt_get_number(vt, "h", &thermal->h);
    vt_get_number(vt, "T_room_init", &thermal->T_room_init);
    if (lifetime->model_choice == lifetime_params::CALCYC) {
        vt_get_matrix(vt, "cap_vs_temp", thermal->cap_vs_temp);
        thermal->en_cap_vs_temp = true;
    }
    else {
        thermal->en_cap_vs_temp = false;
    }

    // losses
    auto losses = params->losses;
    vt_get_int(vt, "loss_choice", &choice);
    losses->loss_choice = static_cast<losses_params::OPTIONS>(choice);
    if (losses->loss_choice == losses_params::MONTHLY) {
        vt_get_array_vec(vt, "monthly_charge_loss", losses->monthly_charge_loss);
        vt_get_array_vec(vt, "monthly_discharge_loss", losses->monthly_discharge_loss);
        vt_get_array_vec(vt, "monthly_idle_loss", losses->monthly_idle_loss);
    }
    else if (losses->loss_choice == losses_params::SCHEDULE) {
        vt_get_array_vec(vt, "schedule_loss", losses->schedule_loss);
    }

    // replacements
    auto replacements = params->replacement;
    vt_get_int(vt, "replacement_option", &choice);
    replacements->replacement_option = static_cast<replacement_params::OPTIONS>(choice);
    if (replacements->replacement_option == replacement_params::SCHEDULE) {
        vt_get_array_vec(vt, "replacement_schedule_percent", replacements->replacement_schedule_percent);
    }
    else if (replacements->replacement_option == replacement_params::CAPACITY_PERCENT) {
        vt_get_number(vt, "replacement_capacity", &replacements->replacement_capacity);
    }

    return params;
}

cm_battery_stateful::cm_battery_stateful():
        dt_hr(0),
        control_mode(0){
    add_var_info(vtab_battery_stateful_inputs);
    add_var_info(vtab_battery_state);
}

cm_battery_stateful::cm_battery_stateful(var_table* vt) :
        cm_battery_stateful() {
    m_vartab = vt;
    try {
        if (!compute_module::verify("precheck input", SSC_INPUT))
            throw exec_error("battery_stateful", log(0)->text);
        dt_hr = as_number("dt_hr");
        control_mode = as_integer("control_mode");
        params = create_battery_params(m_vartab, dt_hr);
        battery = std::unique_ptr<battery_t>(new battery_t(params));
        write_battery_state(battery->get_state(), m_vartab);
    }
    catch (general_error& e) {
        throw std::runtime_error(e.err_text);
    }
}

void cm_battery_stateful::exec() {
    if (!battery)
        throw exec_error("battery_stateful", "Battery model must be initialized first.");

    // Update state
    battery_state state;
    try {
        read_battery_state(state, m_vartab);
        battery->set_state(state);
    }
    catch (std::exception& e) {
        std::string err = "battery_stateful error: Could not read state. ";
        err += e.what();
        throw runtime_error(err);
    }

    // Update controls
    control_mode = static_cast<MODE>(as_integer("control_mode"));
    double control_dt_hr = as_float("dt_hr");
    if (fabs(control_dt_hr - dt_hr) > 1e-7) {
        dt_hr = control_dt_hr;
        battery->ChangeTimestep(dt_hr);
    }

    // Simulate
    if (static_cast<MODE>(as_integer("control_mode")) == MODE::CURRENT) {
        double I = as_number("input_current");
        battery->runCurrent(I);
    }
    else {
        double P = as_number("input_power");
        battery->runPower(P);
    }

    write_battery_state(battery->get_state(), m_vartab);
}

DEFINE_STATEFUL_MODULE_ENTRY(battery_stateful, "Battery management system model with state", 1)
