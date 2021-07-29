#include <algorithm>
#include <set>

#include "vartab.h"
#include "cmod_utilityrate5_eqns.h"

bool try_get_rate_schedule(var_table* vt, const std::string& ssc_name, util::matrix_t<double>& schedule_matrix){
    schedule_matrix.clear();
    auto vd = vt->lookup(ssc_name);
    if (!vd) return false;

    auto mat = vd->num;
    schedule_matrix.copy(mat);
    for (size_t i = 0; i < mat.nrows(); i++){
        for (size_t j = 0; j < mat.ncols(); j++ ) {
            schedule_matrix.at(i, j) -= 1;
        }
    }
    return true;
}

bool try_get_rate_structure(var_table* vt, const std::string& ssc_name, bool power_units,
                            std::vector<std::vector<var_data>>& rate_structure) {
    rate_structure.clear();
    auto vd = vt->lookup(ssc_name);
    if (!vd) return false;

    std::vector<std::vector<double>> rate_matrix = vd->matrix_vector();
    size_t n_periods = (size_t)(*(std::max_element(rate_matrix.begin(), rate_matrix.end(),
                                                   [] (const std::vector<double>& lhs, const std::vector<double>& rhs) {
                                                       return lhs[0] < rhs[0];
                                                   })))[0];
    rate_structure.resize(n_periods);

    // sort by tiers then by period
    std::sort(rate_matrix.begin(), rate_matrix.end(),
              [] (const std::vector<double>& lhs, const std::vector<double>& rhs) {
                  return lhs[1] < rhs[1];
              });
    std::sort(rate_matrix.begin(), rate_matrix.end(),
              [] (const std::vector<double>& lhs, const std::vector<double>& rhs) {
                  return lhs[0] < rhs[0];
              });

    if (power_units) {
        vt->assign("demandrateunit", var_data("kW"));
    }

    for (const auto& row : rate_matrix) {
        int period = row[0];
        double max = row[2];
        double buy;
        var_data rate_data;
        rate_data.type = SSC_TABLE;
        if (!power_units){
            int unit_type = row[3];
            buy = row[4];
            double sell = row[5];
            if (unit_type == 0 || ((unit_type == -1 && max > 1e36)))
                rate_data.table.assign("unit", var_data("kWh"));
            else if (unit_type == 2)
                rate_data.table.assign("unit", var_data("kWh daily"));
            else
                throw(std::runtime_error("ElectricityRates_format_as_URDBv7 error. Unit type in " + ssc_name + " not allowed."));
            rate_data.table.assign("sell", sell);
        }
        else{
            buy = row[3];
        }
        rate_data.table.assign("max", max);
        rate_data.table.assign("rate", buy);

        rate_structure[period - 1].push_back(rate_data);
    }
    return true;
}

SSCEXPORT void ElectricityRates_format_as_URDBv7(ssc_data_t data) {
    auto vt = static_cast<var_table*>(data);
    if (!vt){
        throw std::runtime_error("ssc_data_t data invalid");
    }
    auto urdb_data = var_table();
    std::string log;

    int net_metering;
    vt_get_int(vt, "ur_metering_option", &net_metering);
    std::string dgrules;
    switch(net_metering) {
        case 0:
            dgrules = "Net Metering";
            break;
        case 1:
        case 3:
            throw(std::runtime_error("ElectricityRates_format_as_URDBv7 error. ur_net_metering_option not available in URDBv7."));
        case 2:
            dgrules = "Net Billing Hourly";
            break;
        case 4:
            dgrules = "Buy All Sell All";
            break;
        default:
            throw(std::runtime_error("ElectricityRates_format_as_URDBv7 error. ur_net_metering_option not recognized."));
    }
    urdb_data.assign("dgrules", dgrules);

    double monthly_fixed, monthly_min;
    vt_get_number(vt, "ur_monthly_fixed_charge", &monthly_fixed);
    urdb_data.assign("fixedmonthlycharge", monthly_fixed);
    vt_get_number(vt, "ur_monthly_min_charge", &monthly_min);
    urdb_data.assign("minmonthlycharge", monthly_min);

    try{
        double annual_min;
        vt_get_number(vt, "ur_annual_min_charge", &annual_min);
        urdb_data.assign("annualmincharge", annual_min);
    }
    catch(std::exception&){}

    // energy rates
    util::matrix_t<double> sched_matrix;
    if (try_get_rate_schedule(vt, "ur_ec_sched_weekday", sched_matrix))
        urdb_data.assign("energyweekdayschedule", sched_matrix);
    if (try_get_rate_schedule(vt, "ur_ec_sched_weekend", sched_matrix))
        urdb_data.assign("energyweekendschedule", sched_matrix);

    std::vector<std::vector<var_data>> rate_structure;
    if (try_get_rate_structure(vt, "ur_ec_tou_mat", false, rate_structure))
        urdb_data.assign("energyratestructure", rate_structure);

    // flat demand structure
    sched_matrix.clear();
    if (vt->is_assigned("ur_dc_flat_mat")){
        sched_matrix = vt->lookup("ur_dc_flat_mat")->num;

        size_t n_rows = sched_matrix.nrows();
        std::vector<std::vector<double>> flatdemand;
        for (size_t i = 0; i < n_rows; i++){
            std::vector<double> row;
            row.push_back(sched_matrix.at(i, 0));
            row.push_back(sched_matrix.at(i, 1));
            row.push_back(sched_matrix.at(i, 2));
            row.push_back(sched_matrix.at(i, 3));
            flatdemand.emplace_back(row);
        }

        std::vector<std::vector<var_data>> flat_demand_structure;
        std::vector<double> flat_demand_months;
        flat_demand_months.resize(12);

        // pull out first tier of periods
        for (size_t i = 0; i < n_rows; i++){
            double tier = sched_matrix.at(i, 1);

            if (tier != 1)
                continue;

            double month = sched_matrix.at(i, 0);
            double max = sched_matrix.at(i, 2);
            double charge = sched_matrix.at(i, 3);
            std::vector<var_data> row;

            // see if a period with a matching first tier exists
            size_t period = -1;
            for (size_t j = 0; j < flat_demand_structure.size(); j++){
                double j_max = flat_demand_structure[j][0].table.lookup("max")->num[0];
                double j_charge = flat_demand_structure[j][0].table.lookup("rate")->num[0];
                if (abs(max - j_max) < 1e-3 && abs(charge - j_charge) < 1e-3){
                    period = j;
                    break;
                }
            }
            // doesn't exist so add it
            if (period == -1){
                var_data rate_data;
                rate_data.type = SSC_TABLE;
                rate_data.table.assign("max", max);
                rate_data.table.assign("rate", charge);
                row.emplace_back(rate_data);
                flat_demand_structure.emplace_back(row);
                period = flat_demand_structure.size() - 1;
            }
            flat_demand_months[month] = period;
        }
        // do other tiers
        for (size_t i = 0; i < n_rows; i++){
            double tier = sched_matrix.at(i, 1);

            if (tier == 1)
                continue;

            double month = sched_matrix.at(i, 0);
            double max = sched_matrix.at(i, 2);
            double charge = sched_matrix.at(i, 3);

            // see if a period with a matching first tier exists
            double period = flat_demand_months[month];
            var_data rate_data;
            rate_data.type = SSC_TABLE;
            rate_data.table.assign("max", max);
            rate_data.table.assign("rate", charge);
            flat_demand_structure[period].emplace_back(rate_data);
        }
        urdb_data.assign("flatdemandstructure", flat_demand_structure);
        urdb_data.assign("flatdemandmonths", flat_demand_months);
    }

    // tou
    if (try_get_rate_schedule(vt, "ur_dc_sched_weekday", sched_matrix))
        urdb_data.assign("demandweekdayschedule", sched_matrix);
    if (try_get_rate_schedule(vt, "ur_dc_sched_weekend", sched_matrix))
        urdb_data.assign("demandweekendschedule", sched_matrix);

    if (try_get_rate_structure(vt, "ur_dc_tou_mat", true, rate_structure))
        urdb_data.assign("demandratestructure", rate_structure);

    vt->assign("urdb_data", urdb_data);
}
