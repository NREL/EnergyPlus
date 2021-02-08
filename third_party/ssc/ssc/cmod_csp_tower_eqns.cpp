#include "cmod_csp_tower_eqns.h"
#include "cmod_csp_common_eqns.h"
#include "vartab.h"
#include <cmath>

#pragma warning(disable: 4297)  // ignore warning: 'function assumed not to throw an exception but does'


void MSPT_System_Design_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }
    double P_ref, gross_net_conversion_factor, nameplate, design_eff, solarm, q_pb_design, q_rec_des, tshours, tshours_sf;

    // nameplate
    ssc_data_t_get_number(data, "P_ref", &P_ref);
    ssc_data_t_get_number(data, "gross_net_conversion_factor", &gross_net_conversion_factor);
    nameplate = Nameplate(P_ref, gross_net_conversion_factor);
    ssc_data_t_set_number(data, "nameplate", nameplate);

    // q_pb_design
    ssc_data_t_get_number(data, "P_ref", &P_ref);                         // repeat assessors because the values may have changed
    ssc_data_t_get_number(data, "design_eff", &design_eff);
    q_pb_design = Q_pb_design(P_ref, design_eff);
    ssc_data_t_set_number(data, "q_pb_design", q_pb_design);

    // q_rec_des
    ssc_data_t_get_number(data, "solarm", &solarm);
    ssc_data_t_get_number(data, "q_pb_design", &q_pb_design);
    q_rec_des = Q_rec_des(solarm, q_pb_design);
    ssc_data_t_set_number(data, "q_rec_des", q_rec_des);

    // tshours_sf
    ssc_data_t_get_number(data, "tshours", &tshours);
    ssc_data_t_get_number(data, "solarm", &solarm);
    tshours_sf = Tshours_sf(tshours, solarm);
    ssc_data_t_set_number(data, "tshours_sf", tshours_sf);
}

void Tower_SolarPilot_Solar_Field_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }
    double land_max, h_tower, land_max_calc, helio_height, helio_width, dens_mirror, csp_pt_sf_heliostat_area,
        land_min, land_min_calc, csp_pt_sf_fixed_land_area, land_area_base,
        csp_pt_sf_land_overhead_factor, csp_pt_sf_total_land_area, a_sf_ui, helio_area_tot, csp_pt_sf_tower_height,
        c_atm_0, c_atm_1, c_atm_2, c_atm_3, c_atm_info, helio_optical_error_mrad, error_equiv, field_model_type,
        q_rec_des, q_design, dni_des, dni_des_calc, opt_flux_penalty,
        n_hel, override_opt, is_optimize, override_layout, opt_algorithm;

    util::matrix_t<double> helio_positions;
    bool success;

    // land_max_calc
    ssc_data_t_get_number(data, "land_max", &land_max);
    ssc_data_t_get_number(data, "h_tower", &h_tower);
    land_max_calc = Land_max_calc(land_max, h_tower);
    ssc_data_t_set_number(data, "land_max_calc", land_max_calc);

    // n_hel
    ssc_data_t_get_matrix(vt, "helio_positions", helio_positions);
    n_hel = N_hel(helio_positions);
    ssc_data_t_set_number(data, "n_hel", n_hel);

    // csp_pt_sf_heliostat_area
    ssc_data_t_get_number(data, "helio_height", &helio_height);
    ssc_data_t_get_number(data, "helio_width", &helio_width);
    ssc_data_t_get_number(data, "dens_mirror", &dens_mirror);
    csp_pt_sf_heliostat_area = Csp_pt_sf_heliostat_area(helio_height, helio_width, dens_mirror);
    ssc_data_t_set_number(data, "csp.pt.sf.heliostat_area", csp_pt_sf_heliostat_area);
    
    //  This one is not being read in the UI
    //// csp_pt_sf_total_reflective_area
    //double csp_pt_sf_total_reflective_area;
    //ssc_data_t_get_number(data, "n_hel", &n_hel);
    //ssc_data_t_get_number(data, "csp_pt_sf_heliostat_area", &csp_pt_sf_heliostat_area);
    //csp_pt_sf_total_reflective_area = Csp_pt_sf_total_reflective_area(n_hel, csp_pt_sf_heliostat_area);
    //ssc_data_t_set_number(data, "csp_pt_sf_total_reflective_area", csp_pt_sf_total_reflective_area);

    // land_min_calc
    ssc_data_t_get_number(data, "land_min", &land_min);
    ssc_data_t_get_number(data, "h_tower", &h_tower);
    land_min_calc = Land_min_calc(land_min, h_tower);
    ssc_data_t_set_number(data, "land_min_calc", land_min_calc);

    // csp_pt_sf_total_land_area
    ssc_data_t_get_number(data, "csp.pt.sf.fixed_land_area", &csp_pt_sf_fixed_land_area);
    ssc_data_t_get_number(data, "land_area_base", &land_area_base);
    ssc_data_t_get_number(data, "csp.pt.sf.land_overhead_factor", &csp_pt_sf_land_overhead_factor);
    csp_pt_sf_total_land_area = Csp_pt_sf_total_land_area(csp_pt_sf_fixed_land_area, land_area_base, csp_pt_sf_land_overhead_factor);
    ssc_data_t_set_number(data, "csp.pt.sf.total_land_area", csp_pt_sf_total_land_area);

    // a_sf_ui
    ssc_data_t_get_number(data, "helio_width", &helio_width);
    ssc_data_t_get_number(data, "helio_height", &helio_height);
    ssc_data_t_get_number(data, "dens_mirror", &dens_mirror);
    ssc_data_t_get_number(data, "n_hel", &n_hel);
    a_sf_ui = A_sf_UI(helio_width, helio_height, dens_mirror, n_hel);
    ssc_data_t_set_number(data, "a_sf_ui", a_sf_ui);

    // helio_area_tot
    ssc_data_t_get_number(data, "a_sf_ui", &a_sf_ui);
    helio_area_tot = Helio_area_tot(a_sf_ui);
    ssc_data_t_set_number(data, "helio_area_tot", helio_area_tot);
    
    // csp_pt_sf_tower_height
    ssc_data_t_get_number(data, "h_tower", &h_tower);
    csp_pt_sf_tower_height = Csp_pt_sf_tower_height(h_tower);
    ssc_data_t_set_number(data, "csp.pt.sf.tower_height", csp_pt_sf_tower_height);

    // c_atm_info
    //ssc_data_t_get_matrix(vt, "helio_positions", helio_positions);        // THIS IS A PROBLEM: (getting the same ssc_data_t matrix data must also be changing it)
    ssc_data_t_get_number(data, "c_atm_0", &c_atm_0);
    ssc_data_t_get_number(data, "c_atm_1", &c_atm_1);
    ssc_data_t_get_number(data, "c_atm_2", &c_atm_2);
    ssc_data_t_get_number(data, "c_atm_3", &c_atm_3);
    ssc_data_t_get_number(data, "h_tower", &h_tower);
    c_atm_info = C_atm_info(helio_positions, c_atm_0, c_atm_1, c_atm_2, c_atm_3, h_tower);
    ssc_data_t_set_number(data, "c_atm_info", c_atm_info);

    // error_equiv
    ssc_data_t_get_number(data, "helio_optical_error_mrad", &helio_optical_error_mrad);
    error_equiv = Error_equiv(helio_optical_error_mrad);
    ssc_data_t_set_number(data, "error_equiv", error_equiv);

    // is_optimize
    success = ssc_data_t_get_number(data, "override_opt", &override_opt);
    if (!success) { override_opt = 0.; }
    is_optimize = Is_optimize(override_opt);
    ssc_data_t_set_number(data, "is_optimize", is_optimize);

    // field_model_type
    success = ssc_data_t_get_number(data, "is_optimize", &is_optimize);
    if (!success) { is_optimize = 0.; }
    success = ssc_data_t_get_number(data, "override_layout", &override_layout);
    if (!success) { override_layout = 0.; }
    double assigned_field_model_type;
    success = ssc_data_t_get_number(data, "field_model_type", &assigned_field_model_type);
    if (!success) { assigned_field_model_type = -1.; }
    field_model_type = Field_model_type(is_optimize, override_layout, static_cast<int>(assigned_field_model_type));
    ssc_data_t_set_number(data, "field_model_type", field_model_type);

    // q_design
    ssc_data_t_get_number(data, "q_rec_des", &q_rec_des);
    q_design = Q_design(q_rec_des);
    ssc_data_t_set_number(data, "q_design", q_design);

    // dni_des_calc
    ssc_data_t_get_number(data, "dni_des", &dni_des);
    dni_des_calc = Dni_des_calc(dni_des);
    ssc_data_t_set_number(data, "dni_des_calc", dni_des_calc);

    // opt_algorithm
    opt_algorithm = Opt_algorithm();
    ssc_data_t_set_number(data, "opt_algorithm", opt_algorithm);

    // opt_flux_penalty
    opt_flux_penalty = Opt_flux_penalty();
    ssc_data_t_set_number(data, "opt_flux_penalty", opt_flux_penalty);
}

void MSPT_Receiver_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }
    double csp_pt_rec_max_oper_frac, q_rec_des, csp_pt_rec_htf_c_avg, t_htf_hot_des, t_htf_cold_des,
        csp_pt_rec_max_flow_to_rec, csp_pt_rec_htf_t_avg, rec_d_spec, csp_pt_rec_cav_ap_hw_ratio, csp_pt_rec_cav_ap_height, d_rec, rec_height, rec_aspect,
        h_tower, piping_length_mult, piping_length_const, piping_length, piping_loss, piping_loss_tot,
        rec_htf;

    util::matrix_t<double> field_fl_props;

    //  This one is not being read in the UI
    // Csp_pt_rec_cav_lip_height
    //double csp_pt_rec_cav_lip_height;
    //csp_pt_rec_cav_lip_height = Csp_pt_rec_cav_lip_height();
    //ssc_data_t_set_number(data, "csp_pt_rec_cav_lip_height", csp_pt_rec_cav_lip_height);

    //  This one is not being read in the UI
    // csp_pt_rec_cav_panel_height
    //double csp_pt_rec_cav_panel_height;
    //csp_pt_rec_cav_panel_height = Csp_pt_rec_cav_panel_height();
    //ssc_data_t_set_number(data, "csp_pt_rec_cav_panel_height", csp_pt_rec_cav_panel_height);

    // csp_pt_rec_htf_t_avg
    ssc_data_t_get_number(data, "t_htf_cold_des", &t_htf_cold_des);
    ssc_data_t_get_number(data, "t_htf_hot_des", &t_htf_hot_des);
    csp_pt_rec_htf_t_avg = Csp_pt_rec_htf_t_avg(t_htf_cold_des, t_htf_hot_des);
    ssc_data_t_set_number(data, "csp.pt.rec.htf_t_avg", csp_pt_rec_htf_t_avg);

    // csp_pt_rec_htf_c_avg
    ssc_data_t_get_number(data, "csp.pt.rec.htf_t_avg", &csp_pt_rec_htf_t_avg);
    ssc_data_t_get_number(data, "rec_htf", &rec_htf);
    ssc_data_t_get_matrix(vt, "field_fl_props", field_fl_props);
    csp_pt_rec_htf_c_avg = Csp_pt_rec_htf_c_avg(csp_pt_rec_htf_t_avg, rec_htf, field_fl_props);
    ssc_data_t_set_number(data, "csp.pt.rec.htf_c_avg", csp_pt_rec_htf_c_avg);

    // csp_pt_rec_max_flow_to_rec
    ssc_data_t_get_number(data, "csp.pt.rec.max_oper_frac", &csp_pt_rec_max_oper_frac);
    ssc_data_t_get_number(data, "q_rec_des", &q_rec_des);
    ssc_data_t_get_number(data, "csp.pt.rec.htf_c_avg", &csp_pt_rec_htf_c_avg);
    ssc_data_t_get_number(data, "t_htf_hot_des", &t_htf_hot_des);
    ssc_data_t_get_number(data, "t_htf_cold_des", &t_htf_cold_des);
    csp_pt_rec_max_flow_to_rec = Csp_pt_rec_max_flow_to_rec(csp_pt_rec_max_oper_frac, q_rec_des, csp_pt_rec_htf_c_avg, t_htf_hot_des, t_htf_cold_des);
    ssc_data_t_set_number(data, "csp.pt.rec.max_flow_to_rec", csp_pt_rec_max_flow_to_rec);

    // This one doesn't seem to be used
    // csp_pt_rec_cav_ap_height
    ssc_data_t_get_number(data, "rec_d_spec", &rec_d_spec);
    ssc_data_t_get_number(data, "csp.pt.rec.cav_ap_hw_ratio", &csp_pt_rec_cav_ap_hw_ratio);
    csp_pt_rec_cav_ap_height = Csp_pt_rec_cav_ap_height(rec_d_spec, csp_pt_rec_cav_ap_hw_ratio);
    ssc_data_t_set_number(data, "csp.pt.rec.cav_ap_height", csp_pt_rec_cav_ap_height);

    // rec_aspect
    ssc_data_t_get_number(data, "d_rec", &d_rec);
    ssc_data_t_get_number(data, "rec_height", &rec_height);
    rec_aspect = Rec_aspect(d_rec, rec_height);
    ssc_data_t_set_number(data, "rec_aspect", rec_aspect);

    // piping_length
    ssc_data_t_get_number(data, "h_tower", &h_tower);
    ssc_data_t_get_number(data, "piping_length_mult", &piping_length_mult);
    ssc_data_t_get_number(data, "piping_length_const", &piping_length_const);
    piping_length = Piping_length(h_tower, piping_length_mult, piping_length_const);
    ssc_data_t_set_number(data, "piping_length", piping_length);

    // piping_loss_tot
    ssc_data_t_get_number(data, "piping_length", &piping_length);
    ssc_data_t_get_number(data, "piping_loss", &piping_loss);
    piping_loss_tot = Piping_loss_tot(piping_length, piping_loss);
    ssc_data_t_set_number(data, "piping_loss_tot", piping_loss_tot);
}

void MSPT_System_Control_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }
    double bop_par, bop_par_f, bop_par_0, bop_par_1, bop_par_2, p_ref, csp_pt_par_calc_bop,
        aux_par, aux_par_f, aux_par_0, aux_par_1, aux_par_2, csp_pt_par_calc_aux,
        disp_wlim_maxspec, constant, disp_wlim_max;

    //double* wlim_series;
    util::matrix_t<double> wlim_series;

    // csp_pt_par_calc_bop
    ssc_data_t_get_number(data, "bop_par", &bop_par);
    ssc_data_t_get_number(data, "bop_par_f", &bop_par_f);
    ssc_data_t_get_number(data, "bop_par_0", &bop_par_0);
    ssc_data_t_get_number(data, "bop_par_1", &bop_par_1);
    ssc_data_t_get_number(data, "bop_par_2", &bop_par_2);
    ssc_data_t_get_number(data, "p_ref", &p_ref);
    csp_pt_par_calc_bop = Csp_pt_par_calc_bop(bop_par, bop_par_f, bop_par_0, bop_par_1, bop_par_2, p_ref);
    ssc_data_t_set_number(data, "csp.pt.par.calc.bop", csp_pt_par_calc_bop);

    // csp_pt_par_calc_aux
    ssc_data_t_get_number(data, "aux_par", &aux_par);
    ssc_data_t_get_number(data, "aux_par_f", &aux_par_f);
    ssc_data_t_get_number(data, "aux_par_0", &aux_par_0);
    ssc_data_t_get_number(data, "aux_par_1", &aux_par_1);
    ssc_data_t_get_number(data, "aux_par_2", &aux_par_2);
    ssc_data_t_get_number(data, "p_ref", &p_ref);
    csp_pt_par_calc_aux = Csp_pt_par_calc_aux(aux_par, aux_par_f, aux_par_0, aux_par_1, aux_par_2, p_ref);
    ssc_data_t_set_number(data, "csp.pt.par.calc.aux", csp_pt_par_calc_aux);

    // disp_wlim_max
    disp_wlim_maxspec = constant = std::numeric_limits<double>::quiet_NaN();
    ssc_data_t_get_number(data, "disp_wlim_maxspec", &disp_wlim_maxspec);       // if coming from UI
    if (std::isnan(disp_wlim_maxspec)) {
        disp_wlim_maxspec = 1.;                                                 // not passed to LK script
    }

    ssc_data_t_get_number(data, "constant", &constant);                         // if coming from UI
    if (std::isnan(constant)) {
        ssc_data_t_get_number(data, "adjust:constant", &constant);              // if coming from LK script
    }

    disp_wlim_max = Disp_wlim_max(disp_wlim_maxspec, constant);
    ssc_data_t_set_number(data, "disp_wlim_max", disp_wlim_max);

    // wlim_series
    if (!vt->is_assigned("wlim_series")) {
        ssc_data_t_get_number(data, "disp_wlim_max", &disp_wlim_max);
        ssc_data_t_get_number(data, "constant", &constant);
        wlim_series = Wlim_series(disp_wlim_max);
        ssc_data_t_set_array(data, "wlim_series", wlim_series.data(), wlim_series.ncells());
    }
}

void Tower_SolarPilot_Capital_Costs_MSPT_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }

    double d_rec, rec_height, receiver_type_double, rec_d_spec, csp_pt_rec_cav_ap_height, csp_pt_cost_receiver_area,
        p_ref, design_eff, tshours, csp_pt_cost_storage_mwht,
        demand_var, csp_pt_cost_power_block_mwe;
    int receiver_type;

    TowerTypes tower_type = TowerTypes::kMoltenSalt;

    receiver_type_double = std::numeric_limits<double>::quiet_NaN();
    ssc_data_t_get_number(data, "d_rec", &d_rec);
    ssc_data_t_get_number(data, "rec_height", &rec_height);
    ssc_data_t_get_number(data, "receiver_type", &receiver_type_double);
    if (std::isnan(receiver_type_double)) {
        receiver_type = 0;                                                 // not passed to LK script, assume a external, non-cavity receiver
    }
    else {
        receiver_type = static_cast<int>(receiver_type_double);
    }
    ssc_data_t_get_number(data, "rec_d_spec", &rec_d_spec);
    ssc_data_t_get_number(data, "csp.pt.rec.cav_ap_height", &csp_pt_rec_cav_ap_height);
    csp_pt_cost_receiver_area = Csp_pt_cost_receiver_area(tower_type, d_rec, rec_height,
        static_cast<int>(receiver_type), rec_d_spec, csp_pt_rec_cav_ap_height);
    ssc_data_t_set_number(data, "csp.pt.cost.receiver.area", csp_pt_cost_receiver_area);

    ssc_data_t_get_number(data, "p_ref", &p_ref);
    ssc_data_t_get_number(data, "design_eff", &design_eff);
    ssc_data_t_get_number(data, "tshours", &tshours);
    csp_pt_cost_storage_mwht = Csp_pt_cost_storage_mwht(tower_type, p_ref, design_eff, tshours);
    ssc_data_t_set_number(data, "csp.pt.cost.storage_mwht", csp_pt_cost_storage_mwht);

    ssc_data_t_get_number(data, "p_ref", &p_ref);
    demand_var = std::numeric_limits<double>::quiet_NaN();
    csp_pt_cost_power_block_mwe = Csp_pt_cost_power_block_mwe(tower_type, p_ref, demand_var);
    ssc_data_t_set_number(data, "csp.pt.cost.power_block_mwe", csp_pt_cost_power_block_mwe);

    Tower_SolarPilot_Capital_Costs_Equations(data);
}

void Tower_SolarPilot_Capital_Costs_DSPT_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }

    double d_rec, rec_height, receiver_type, rec_d_spec, csp_pt_rec_cav_ap_height, csp_pt_cost_receiver_area,
        p_ref, design_eff, tshours, csp_pt_cost_storage_mwht,
        demand_var, csp_pt_cost_power_block_mwe;

    TowerTypes tower_type = TowerTypes::kDirectSteam;

    ssc_data_t_get_number(data, "d_rec", &d_rec);
    ssc_data_t_get_number(data, "rec_height", &rec_height);
    receiver_type = std::numeric_limits<double>::quiet_NaN();
    rec_d_spec = std::numeric_limits<double>::quiet_NaN();
    csp_pt_rec_cav_ap_height = std::numeric_limits<double>::quiet_NaN();
    csp_pt_cost_receiver_area = Csp_pt_cost_receiver_area(tower_type, d_rec, rec_height,
        static_cast<int>(receiver_type), rec_d_spec, csp_pt_rec_cav_ap_height);
    ssc_data_t_set_number(data, "csp.pt.cost.receiver_area", csp_pt_cost_receiver_area);

    p_ref = std::numeric_limits<double>::quiet_NaN();
    design_eff = std::numeric_limits<double>::quiet_NaN();
    tshours = std::numeric_limits<double>::quiet_NaN();
    csp_pt_cost_storage_mwht = Csp_pt_cost_storage_mwht(tower_type, p_ref, design_eff, tshours);
    ssc_data_t_set_number(data, "csp.pt.cost.storage_mwht", csp_pt_cost_storage_mwht);

    p_ref = std::numeric_limits<double>::quiet_NaN();
    ssc_data_t_get_number(data, "demand_var", &demand_var);
    csp_pt_cost_power_block_mwe = Csp_pt_cost_power_block_mwe(tower_type, p_ref, demand_var);
    ssc_data_t_set_number(data, "csp.pt.cost.power_block_mwe", csp_pt_cost_power_block_mwe);

    Tower_SolarPilot_Capital_Costs_Equations(data);
}

void Tower_SolarPilot_Capital_Costs_ISCC_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }

    double d_rec, rec_height, receiver_type, rec_d_spec, csp_pt_rec_cav_ap_height, csp_pt_cost_receiver_area,
        p_ref, design_eff, tshours, csp_pt_cost_storage_mwht,
        demand_var, csp_pt_cost_power_block_mwe;

    TowerTypes tower_type = TowerTypes::kMoltenSalt;

    ssc_data_t_get_number(data, "d_rec", &d_rec);
    ssc_data_t_get_number(data, "rec_height", &rec_height);
    ssc_data_t_get_number(data, "receiver_type", &receiver_type);
    ssc_data_t_get_number(data, "rec_d_spec", &rec_d_spec);
    ssc_data_t_get_number(data, "csp.pt.rec.cav_ap_height", &csp_pt_rec_cav_ap_height);
    csp_pt_cost_receiver_area = Csp_pt_cost_receiver_area(tower_type, d_rec, rec_height,
        static_cast<int>(receiver_type), rec_d_spec, csp_pt_rec_cav_ap_height);
    ssc_data_t_set_number(data, "csp.pt.cost.receiver_area", csp_pt_cost_receiver_area);

    p_ref = std::numeric_limits<double>::quiet_NaN();
    design_eff = std::numeric_limits<double>::quiet_NaN();
    tshours = std::numeric_limits<double>::quiet_NaN();
    csp_pt_cost_storage_mwht = Csp_pt_cost_storage_mwht(tower_type, p_ref, design_eff, tshours);
    ssc_data_t_set_number(data, "csp.pt.cost.storage_mwht", csp_pt_cost_storage_mwht);

    p_ref = std::numeric_limits<double>::quiet_NaN();
    demand_var = std::numeric_limits<double>::quiet_NaN();
    csp_pt_cost_power_block_mwe = Csp_pt_cost_power_block_mwe(tower_type, p_ref, demand_var);
    ssc_data_t_set_number(data, "csp.pt.cost.power_block_mwe", csp_pt_cost_power_block_mwe);

    Tower_SolarPilot_Capital_Costs_Equations(data);
}
