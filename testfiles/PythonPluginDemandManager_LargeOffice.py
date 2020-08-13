from pyenergyplus.plugin import EnergyPlusPlugin


class DetermineCurrentDemandManageState(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.sensors['CurntFacilityElectDemand'] = self.api.exchange.get_variable_handle('Facility Total Electricity Demand Rate', 'Whole Building')
        self.globals['argDmndMngrState'] = self.api.exchange.get_global_handle('argDmndMngrState')
        self.globals['argCrntDmnd'] = self.api.exchange.get_global_handle('argCrntDmnd')
        self.globals['argTrendDirection'] = self.api.exchange.get_global_handle('argTrendDirection')
        self.globals['argTargetDemand'] = self.api.exchange.get_global_handle('argTargetDemand')
        self.trends['FacilityElectTrend'] = self.api.exchange.get_trend_handle('FacilityElectTrend')
        self.trends['Demand_Mgr_State_Trend'] = self.api.exchange.get_trend_handle('Demand_Mgr_State_Trend')
        self.need_to_get_handles = False

    def find_demand_state(self):
        demand_state_x_1 = self.api.exchange.get_trend_value(self.trends['Demand_Mgr_State_Trend'], 1)
        demand_state_x_2 = self.api.exchange.get_trend_value(self.trends['Demand_Mgr_State_Trend'], 2)
        level_1_demand = 0.9 * self.api.exchange.get_global_value(self.globals['argTargetDemand'])
        self.api.exchange.set_global_value(self.globals['argDmndMngrState'], demand_state_x_1)
        current_demand = self.api.exchange.get_global_value(self.globals['argCrntDmnd'])
        trend_dir = self.api.exchange.get_global_value(self.globals['argTrendDirection'])
        target_demand = self.api.exchange.get_global_value(self.globals['argTargetDemand'])
        if level_1_demand < current_demand < target_demand and trend_dir > 0.0:
            if demand_state_x_1 <= 1.0:
                self.api.exchange.set_global_value(self.globals['argDmndMngrState'], 1.0)
            elif demand_state_x_1 == 2.0 and demand_state_x_2 < 2.0:
                self.api.exchange.set_global_value(self.globals['argDmndMngrState'], 2.0)
            elif demand_state_x_1 == 3.0 and demand_state_x_2 == 3.0:
                self.api.exchange.set_global_value(self.globals['argDmndMngrState'], 2.0)
            elif demand_state_x_1 == 3.0 and demand_state_x_2 == 2.0:
                self.api.exchange.set_global_value(self.globals['argDmndMngrState'], 3.0)
        elif current_demand > target_demand and trend_dir < 0.0:
            if demand_state_x_1 <= 2.0:
                self.api.exchange.set_global_value(self.globals['argDmndMngrState'], 2.0)
            elif demand_state_x_1 == 3.0 and demand_state_x_2 == 2.0:
                self.api.exchange.set_global_value(self.globals['argDmndMngrState'], 3.0)
            elif demand_state_x_1 == 3.0 and demand_state_x_2 == 3.0:
                self.api.exchange.set_global_value(self.globals['argDmndMngrState'], 2.0)
        elif current_demand > target_demand and trend_dir >= 0.0:
            self.api.exchange.set_global_value(self.globals['argDmndMngrState'], 3.0)

    def on_begin_timestep_before_predictor(self):
        if self.need_to_get_handles:
            self.get_handles()
        if self.sensors['CurntFacilityElectDemand'] == -1:
            # try to look it up again
            self.sensors['CurntFacilityElectDemand'] = self.api.exchange.get_variable_handle('Facility Total Electricity Demand Rate', 'Whole Building')
            # if it is still -1, then just return for now
            if self.sensors['CurntFacilityElectDemand'] == -1:
                return 0
        local_demand = self.api.exchange.get_variable_value(self.sensors['CurntFacilityElectDemand']) / 1000.0
        current_trend = self.api.exchange.get_trend_direction(self.trends['FacilityElectTrend'], 4)
        self.api.exchange.set_global_value(self.globals['argCrntDmnd'], local_demand)
        self.api.exchange.set_global_value(self.globals['argTrendDirection'], current_trend)
        month = self.api.exchange.month()
        demand = 0.0
        if month == 1:
            demand = 0.85 * 1154.01
        elif month == 2:
            demand = 0.85 * 1150.85
        elif month == 3:
            demand = 0.85 * 1313.56
        elif month == 4:
            demand = 0.85 * 1364.28
        elif month == 5:
            demand = 0.85 * 1506.29
        elif month == 6:
            demand = 0.85 * 1516.93
        elif month == 7:
            demand = 0.85 * 1545.20
        elif month == 8:
            demand = 0.85 * 1555.20
        elif month == 9:
            demand = 0.85 * 1491.38
        elif month == 10:
            demand = 0.85 * 1402.86
        elif month == 11:
            demand = 0.85 * 1418.69
        elif month == 12:
            demand = 0.85 * 1440.48
        self.api.exchange.set_global_value(self.globals['argTargetDemand'], demand)    
        self.find_demand_state()
        return 0


class DispatchDemandControlsByState(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['Set_Cooling_Setpoint_Sched'] = self.api.exchange.get_actuator_handle('Schedule:Compact', 'Schedule Value', 'CLGSETP_SCH')
        self.actuators['Set_Perimeter_top_ZN_4_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Perimeter_top_ZN_4_Lights')
        self.actuators['Set_Basement_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Basement_Lights')
        self.actuators['Set_Perimeter_bot_ZN_4_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Perimeter_bot_ZN_4_Lights')
        self.actuators['Set_Perimeter_mid_ZN_1_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Perimeter_mid_ZN_1_Lights')
        self.actuators['Set_Perimeter_mid_ZN_2_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Perimeter_mid_ZN_2_Lights')
        self.actuators['Set_Perimeter_bot_ZN_1_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Perimeter_bot_ZN_1_Lights')
        self.actuators['Set_Core_mid_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Core_mid_Lights')
        self.actuators['Set_Perimeter_bot_ZN_3_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Perimeter_bot_ZN_3_Lights')
        self.actuators['Set_Perimeter_mid_ZN_4_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Perimeter_mid_ZN_4_Lights')
        self.actuators['Set_Perimeter_bot_ZN_2_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Perimeter_bot_ZN_2_Lights')
        self.actuators['Set_Perimeter_top_ZN_2_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Perimeter_top_ZN_2_Lights')
        self.actuators['Set_Perimeter_mid_ZN_3_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Perimeter_mid_ZN_3_Lights')
        self.actuators['Set_Perimeter_top_ZN_1_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Perimeter_top_ZN_1_Lights')
        self.actuators['Set_Perimeter_top_ZN_3_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Perimeter_top_ZN_3_Lights')
        self.actuators['Set_Core_top_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Core_top_Lights')
        self.actuators['Set_Core_bottom_Lights'] = self.api.exchange.get_actuator_handle('Lights', 'Electric Power Level', 'Core_bottom_Lights')
        self.sensors['BLDG_LIGHT_SCH'] = self.api.exchange.get_variable_handle('Schedule Value', 'BLDG_LIGHT_SCH')
        self.sensors['Cooling_Setpoint_Sched'] = self.api.exchange.get_variable_handle('Schedule Value', 'CLGSETP_SCH_Copy')
        self.globals['argDmndMngrState'] = self.api.exchange.get_global_handle('argDmndMngrState')
        self.internals['Perimeter_bot_ZN_1_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Perimeter_bot_ZN_1_Lights')
        self.internals['Perimeter_top_ZN_4_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Perimeter_top_ZN_4_Lights')
        self.internals['Perimeter_top_ZN_2_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Perimeter_top_ZN_2_Lights')
        self.internals['Perimeter_bot_ZN_2_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Perimeter_bot_ZN_2_Lights')
        self.internals['Perimeter_bot_ZN_4_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Perimeter_bot_ZN_4_Lights')
        self.internals['Perimeter_top_ZN_3_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Perimeter_top_ZN_3_Lights')
        self.internals['Core_mid_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Core_mid_Lights')
        self.internals['Core_top_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Core_top_Lights')
        self.internals['Basement_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Basement_Lights')
        self.internals['Perimeter_bot_ZN_3_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Perimeter_bot_ZN_3_Lights')
        self.internals['Perimeter_mid_ZN_1_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Perimeter_mid_ZN_1_Lights')
        self.internals['Perimeter_mid_ZN_2_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Perimeter_mid_ZN_2_Lights')
        self.internals['Core_bottom_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Core_bottom_Lights')
        self.internals['Perimeter_mid_ZN_3_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Perimeter_mid_ZN_3_Lights')
        self.internals['Perimeter_mid_ZN_4_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Perimeter_mid_ZN_4_Lights')
        self.internals['Perimeter_top_ZN_1_Lights'] = self.api.exchange.get_internal_variable_handle('Lighting Power Design Level', 'Perimeter_top_ZN_1_Lights')
        self.need_to_get_handles = False

    def set_demand_controls(self, light_mult, set_point_shift):
        light_sched_value = self.api.exchange.get_variable_value(self.sensors['BLDG_LIGHT_SCH'])
        cooling_setpoint_value = self.api.exchange.get_variable_value(self.sensors['Cooling_Setpoint_Sched'])
        self.api.exchange.set_actuator_value(self.actuators['Set_Cooling_Setpoint_Sched'], cooling_setpoint_value + set_point_shift)
        self.api.exchange.set_actuator_value(self.actuators['Set_Basement_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Basement_Lights']) * light_sched_value)
        self.api.exchange.set_actuator_value(self.actuators['Set_Core_bottom_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Core_bottom_Lights']) * light_sched_value)
        self.api.exchange.set_actuator_value(self.actuators['Set_Core_mid_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Core_mid_Lights']) * light_sched_value)
        self.api.exchange.set_actuator_value(self.actuators['Set_Core_top_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Core_top_Lights']) * light_sched_value)
        self.api.exchange.set_actuator_value(self.actuators['Set_Perimeter_bot_ZN_3_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Perimeter_bot_ZN_3_Lights']) * light_sched_value)
        self.api.exchange.set_actuator_value(self.actuators['Set_Perimeter_bot_ZN_2_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Perimeter_bot_ZN_2_Lights']) * light_sched_value)
        self.api.exchange.set_actuator_value(self.actuators['Set_Perimeter_bot_ZN_1_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Perimeter_bot_ZN_1_Lights']) * light_sched_value)
        self.api.exchange.set_actuator_value(self.actuators['Set_Perimeter_bot_ZN_4_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Perimeter_bot_ZN_4_Lights']) * light_sched_value)
        self.api.exchange.set_actuator_value(self.actuators['Set_Perimeter_mid_ZN_3_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Perimeter_mid_ZN_3_Lights']) * light_sched_value)
        self.api.exchange.set_actuator_value(self.actuators['Set_Perimeter_mid_ZN_2_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Perimeter_mid_ZN_2_Lights']) * light_sched_value)
        self.api.exchange.set_actuator_value(self.actuators['Set_Perimeter_mid_ZN_1_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Perimeter_mid_ZN_1_Lights']) * light_sched_value)
        self.api.exchange.set_actuator_value(self.actuators['Set_Perimeter_mid_ZN_4_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Perimeter_mid_ZN_4_Lights']) * light_sched_value)
        self.api.exchange.set_actuator_value(self.actuators['Set_Perimeter_top_ZN_3_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Perimeter_top_ZN_3_Lights']) * light_sched_value)
        self.api.exchange.set_actuator_value(self.actuators['Set_Perimeter_top_ZN_2_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Perimeter_top_ZN_2_Lights']) * light_sched_value)
        self.api.exchange.set_actuator_value(self.actuators['Set_Perimeter_top_ZN_1_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Perimeter_top_ZN_1_Lights']) * light_sched_value)
        self.api.exchange.set_actuator_value(self.actuators['Set_Perimeter_top_ZN_4_Lights'], light_mult * self.api.exchange.get_internal_variable_value(self.internals['Perimeter_top_ZN_4_Lights']) * light_sched_value)

    def unset_demand_controls(self):
        self.api.exchange.reset_actuator(self.actuators['Set_Cooling_Setpoint_Sched'])
        self.api.exchange.reset_actuator(self.actuators['Set_Basement_Lights'])
        self.api.exchange.reset_actuator(self.actuators['Set_Core_bottom_Lights'])
        self.api.exchange.reset_actuator(self.actuators['Set_Core_mid_Lights'])
        self.api.exchange.reset_actuator(self.actuators['Set_Core_top_Lights'])
        self.api.exchange.reset_actuator(self.actuators['Set_Perimeter_bot_ZN_3_Lights'])
        self.api.exchange.reset_actuator(self.actuators['Set_Perimeter_bot_ZN_2_Lights'])
        self.api.exchange.reset_actuator(self.actuators['Set_Perimeter_bot_ZN_1_Lights'])
        self.api.exchange.reset_actuator(self.actuators['Set_Perimeter_bot_ZN_4_Lights'])
        self.api.exchange.reset_actuator(self.actuators['Set_Perimeter_mid_ZN_3_Lights'])
        self.api.exchange.reset_actuator(self.actuators['Set_Perimeter_mid_ZN_2_Lights'])
        self.api.exchange.reset_actuator(self.actuators['Set_Perimeter_mid_ZN_1_Lights'])
        self.api.exchange.reset_actuator(self.actuators['Set_Perimeter_mid_ZN_4_Lights'])
        self.api.exchange.reset_actuator(self.actuators['Set_Perimeter_top_ZN_3_Lights'])
        self.api.exchange.reset_actuator(self.actuators['Set_Perimeter_top_ZN_2_Lights'])
        self.api.exchange.reset_actuator(self.actuators['Set_Perimeter_top_ZN_1_Lights'])
        self.api.exchange.reset_actuator(self.actuators['Set_Perimeter_top_ZN_4_Lights'])

    def on_begin_timestep_before_predictor(self):
        if self.need_to_get_handles:
            self.get_handles()
        if self.sensors['BLDG_LIGHT_SCH'] == -1:
            # try to look it up again
            self.sensors['BLDG_LIGHT_SCH'] = self.api.exchange.get_variable_handle('Schedule Value', 'BLDG_LIGHT_SCH')
            # if it is still -1, then just return for now
            if self.sensors['BLDG_LIGHT_SCH'] == -1:
                return 0
        if self.sensors['Cooling_Setpoint_Sched'] == -1:
            # try to look it up again
            self.sensors['Cooling_Setpoint_Sched'] = self.api.exchange.get_variable_handle('Schedule Value', 'CLGSETP_SCH_Copy')
            # if it is still -1, then just return for now
            if self.sensors['Cooling_Setpoint_Sched'] == -1:
                return 0
        manager_state = self.api.exchange.get_global_value(self.globals['argDmndMngrState'])
        if manager_state == 0:
            self.unset_demand_controls()
        elif manager_state == 1:
            self.set_demand_controls(0.9, 0.8)
        elif manager_state == 2:
            self.set_demand_controls(0.8, 1.5)
        elif manager_state == 3:
            self.set_demand_controls(0.7, 2.0)
        return 0


class Vav1Schedsetpoint(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['VAV_1_SAT_setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_1 Supply Equipment Outlet Node')
        self.sensors['Seasonal_Reset_SAT_Sched'] = self.api.exchange.get_variable_handle('Schedule Value', 'Seasonal-Reset-Supply-Air-Temp-Sch')
        self.need_to_get_handles = False

    def on_after_predictor_after_hvac_managers(self):
        if self.need_to_get_handles:
            self.get_handles()
        self.api.exchange.set_actuator_value(self.actuators['VAV_1_SAT_setpoint'], self.api.exchange.get_variable_value(self.sensors['Seasonal_Reset_SAT_Sched']))
        return 0


class Vav2Schedsetpoint(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['VAV_2_SAT_setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_2_CoolC-VAV_2_HeatCNode')
        self.sensors['Seasonal_Reset_SAT_Sched'] = self.api.exchange.get_variable_handle('Schedule Value', 'Seasonal-Reset-Supply-Air-Temp-Sch')
        self.need_to_get_handles = False

    def on_after_predictor_after_hvac_managers(self):
        if self.need_to_get_handles:
            self.get_handles()
        self.api.exchange.set_actuator_value(self.actuators['VAV_2_SAT_setpoint'], self.api.exchange.get_variable_value(self.sensors['Seasonal_Reset_SAT_Sched']))
        return 0


class Vav3Schedsetpoint(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['VAV_3_SAT_setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_3_CoolC-VAV_3_HeatCNode')
        self.sensors['Seasonal_Reset_SAT_Sched'] = self.api.exchange.get_variable_handle('Schedule Value', 'Seasonal-Reset-Supply-Air-Temp-Sch')
        self.need_to_get_handles = False

    def on_after_predictor_after_hvac_managers(self):
        if self.need_to_get_handles:
            self.get_handles()
        self.api.exchange.set_actuator_value(self.actuators['VAV_3_SAT_setpoint'], self.api.exchange.get_variable_value(self.sensors['Seasonal_Reset_SAT_Sched']))
        return 0


class Vav5Schedsetpoint(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['VAV_5_SAT_setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_1_CoolC-VAV_1_HeatCNode')
        self.sensors['Seasonal_Reset_SAT_Sched'] = self.api.exchange.get_variable_handle('Schedule Value', 'Seasonal-Reset-Supply-Air-Temp-Sch')
        self.need_to_get_handles = False

    def on_after_predictor_after_hvac_managers(self):
        if self.need_to_get_handles:
            self.get_handles()
        self.api.exchange.set_actuator_value(self.actuators['VAV_5_SAT_setpoint'], self.api.exchange.get_variable_value(self.sensors['Seasonal_Reset_SAT_Sched']))
        return 0


class Vav1Mixedairmanagers(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['VAV_1_HeatC_Setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_1_HeatC-VAV_1_FanNode')
        self.actuators['VAV_1_OA_Setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_1_OA-VAV_1_CoolCNode')
        self.actuators['VAV_1_CoolC_Setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_1_CoolC-VAV_1_HeatCNode')
        self.sensors['Seasonal_Reset_SAT_Sched'] = self.api.exchange.get_variable_handle('Schedule Value', 'Seasonal-Reset-Supply-Air-Temp-Sch')
        self.sensors['T_VAV1FanOut'] = self.api.exchange.get_variable_handle('System Node Temperature', 'VAV_1 Supply Equipment Outlet Node')
        self.sensors['T_VAV1FanIn'] = self.api.exchange.get_variable_handle('System Node Temperature', 'VAV_1_HeatC-VAV_1_FanNode')
        self.need_to_get_handles = False

    def on_after_predictor_after_hvac_managers(self):
        if self.need_to_get_handles:
            self.get_handles()
        reset_sat_value = self.api.exchange.get_variable_value(self.sensors['Seasonal_Reset_SAT_Sched'])
        vav_fan_out_temp = self.api.exchange.get_variable_value(self.sensors['T_VAV1FanOut'])
        vav_fan_in_temp = self.api.exchange.get_variable_value(self.sensors['T_VAV1FanIn'])
        val_to_set = reset_sat_value - (vav_fan_out_temp - vav_fan_in_temp)
        self.api.exchange.set_actuator_value(self.actuators['VAV_1_CoolC_Setpoint'], val_to_set)
        self.api.exchange.set_actuator_value(self.actuators['VAV_1_HeatC_Setpoint'], val_to_set)
        self.api.exchange.set_actuator_value(self.actuators['VAV_1_OA_Setpoint'], val_to_set)
        return 0


class Vav2Mixedairmanagers(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['VAV_2_HeatC_Setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_2_HeatC-VAV_2_FanNode')
        self.actuators['VAV_2_CoolC_Setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_2_CoolC-VAV_2_HeatCNode')
        self.actuators['VAV_2_OA_Setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_2_OA-VAV_2_CoolCNode')
        self.sensors['T_VAV2FanIn'] = self.api.exchange.get_variable_handle('System Node Temperature', 'VAV_2_HeatC-VAV_2_FanNode')
        self.sensors['Seasonal_Reset_SAT_Sched'] = self.api.exchange.get_variable_handle('Schedule Value', 'Seasonal-Reset-Supply-Air-Temp-Sch')
        self.sensors['T_VAV2FanOut'] = self.api.exchange.get_variable_handle('System Node Temperature', 'VAV_2 Supply Equipment Outlet Node')
        self.need_to_get_handles = False

    def on_after_predictor_after_hvac_managers(self):
        if self.need_to_get_handles:
            self.get_handles()
        reset_sat_value = self.api.exchange.get_variable_value(self.sensors['Seasonal_Reset_SAT_Sched'])
        vav_fan_out_temp = self.api.exchange.get_variable_value(self.sensors['T_VAV2FanOut'])
        vav_fan_in_temp = self.api.exchange.get_variable_value(self.sensors['T_VAV2FanIn'])
        val_to_set = reset_sat_value - (vav_fan_out_temp - vav_fan_in_temp)
        self.api.exchange.set_actuator_value(self.actuators['VAV_2_CoolC_Setpoint'], val_to_set)
        self.api.exchange.set_actuator_value(self.actuators['VAV_2_HeatC_Setpoint'], val_to_set)
        self.api.exchange.set_actuator_value(self.actuators['VAV_2_OA_Setpoint'], val_to_set)
        return 0


class Vav3Mixedairmanagers(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['VAV_3_OA_Setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_3_OA-VAV_3_CoolCNode')
        self.actuators['VAV_3_HeatC_Setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_3_HeatC-VAV_3_FanNode')
        self.actuators['VAV_3_CoolC_Setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_3_CoolC-VAV_3_HeatCNode')
        self.sensors['T_VAV3FanOut'] = self.api.exchange.get_variable_handle('System Node Temperature', 'VAV_3 Supply Equipment Outlet Node')
        self.sensors['Seasonal_Reset_SAT_Sched'] = self.api.exchange.get_variable_handle('Schedule Value', 'Seasonal-Reset-Supply-Air-Temp-Sch')
        self.sensors['T_VAV3FanIn'] = self.api.exchange.get_variable_handle('System Node Temperature', 'VAV_3_HeatC-VAV_3_FanNode')
        self.need_to_get_handles = False

    def on_after_predictor_after_hvac_managers(self):
        if self.need_to_get_handles:
            self.get_handles()
        reset_sat_value = self.api.exchange.get_variable_value(self.sensors['Seasonal_Reset_SAT_Sched'])
        vav_fan_out_temp = self.api.exchange.get_variable_value(self.sensors['T_VAV3FanOut'])
        vav_fan_in_temp = self.api.exchange.get_variable_value(self.sensors['T_VAV3FanIn'])
        val_to_set = reset_sat_value - (vav_fan_out_temp - vav_fan_in_temp)
        self.api.exchange.set_actuator_value(self.actuators['VAV_3_CoolC_Setpoint'], val_to_set)
        self.api.exchange.set_actuator_value(self.actuators['VAV_3_HeatC_Setpoint'], val_to_set)
        self.api.exchange.set_actuator_value(self.actuators['VAV_3_OA_Setpoint'], val_to_set)
        return 0


class Vav5Mixedairmanagers(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['VAV_5_OA_Setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_5_OA-VAV_5_CoolCNode')
        self.actuators['VAV_5_CoolC_Setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_5_CoolC-VAV_5_HeatCNode')
        self.actuators['VAV_5_HeatC_Setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'VAV_5_HeatC-VAV_5_FanNode')
        self.sensors['T_VAV5FanIn'] = self.api.exchange.get_variable_handle('System Node Temperature', 'VAV_5_HeatC-VAV_5_FanNode')
        self.sensors['Seasonal_Reset_SAT_Sched'] = self.api.exchange.get_variable_handle('Schedule Value', 'Seasonal-Reset-Supply-Air-Temp-Sch')
        self.sensors['T_VAV5FanOut'] = self.api.exchange.get_variable_handle('System Node Temperature', 'VAV_5 Supply Equipment Outlet Node')
        self.need_to_get_handles = False

    def on_after_predictor_after_hvac_managers(self):
        if self.need_to_get_handles:
            self.get_handles()
        reset_sat_value = self.api.exchange.get_variable_value(self.sensors['Seasonal_Reset_SAT_Sched'])
        vav_fan_out_temp = self.api.exchange.get_variable_value(self.sensors['T_VAV5FanOut'])
        vav_fan_in_temp = self.api.exchange.get_variable_value(self.sensors['T_VAV5FanIn'])
        val_to_set = reset_sat_value - (vav_fan_out_temp - vav_fan_in_temp)
        self.api.exchange.set_actuator_value(self.actuators['VAV_5_CoolC_Setpoint'], val_to_set)
        self.api.exchange.set_actuator_value(self.actuators['VAV_5_HeatC_Setpoint'], val_to_set)
        self.api.exchange.set_actuator_value(self.actuators['VAV_5_OA_Setpoint'], val_to_set)
        return 0


class Coolsys1Schedsetpoint(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['CoolSys1_Loop_Setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'CoolSys1 Supply Outlet Node')
        self.sensors['CW_Loop_Temp_Schedule'] = self.api.exchange.get_variable_handle('Schedule Value', 'CW-Loop-Temp-Schedule')
        self.need_to_get_handles = False

    def on_after_predictor_after_hvac_managers(self):
        if self.need_to_get_handles:
            self.get_handles()
        self.api.exchange.set_actuator_value(self.actuators['CoolSys1_Loop_Setpoint'], self.api.exchange.get_variable_value(self.sensors['CW_Loop_Temp_Schedule']))
        return 0


class Heatsys1Schedsetpoint(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['HeatSys1_Loop_Setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'HeatSys1 Supply Outlet Node')
        self.sensors['HW_Loop_Temp_Schedule'] = self.api.exchange.get_variable_handle('Schedule Value', 'HW-Loop-Temp-Schedule')
        self.need_to_get_handles = False

    def on_after_predictor_after_hvac_managers(self):
        if self.need_to_get_handles:
            self.get_handles()
        self.api.exchange.set_actuator_value(self.actuators['HeatSys1_Loop_Setpoint'], self.api.exchange.get_variable_value(self.sensors['HW_Loop_Temp_Schedule']))
        return 0


class Shwsys1Schedsetpoint(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['SHWSys1_Loop_Setpoint'] = self.api.exchange.get_actuator_handle('System Node Setpoint', 'Temperature Setpoint', 'SHWSys1 Supply Outlet Node')
        self.sensors['SHWSys1_Loop_Temp_Schedule'] = self.api.exchange.get_variable_handle('Schedule Value', 'SHWSys1-Loop-Temp-Schedule')
        self.need_to_get_handles = False

    def on_after_predictor_after_hvac_managers(self):
        if self.need_to_get_handles:
            self.get_handles()
        self.api.exchange.set_actuator_value(self.actuators['SHWSys1_Loop_Setpoint'], self.api.exchange.get_variable_value(self.sensors['SHWSys1_Loop_Temp_Schedule']))
        return 0


class Vav1Nightcyclemgr(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['VAV_1_NightCycleStatus'] = self.api.exchange.get_actuator_handle('AirLoopHVAC', 'Availability Status', 'VAV_1')
        self.sensors['heating_setpoint'] = self.api.exchange.get_variable_handle('Schedule Value', 'HTGSETP_SCH')
        self.sensors['TzoneVAV1_5'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Perimeter_bot_ZN_4')
        self.sensors['TzoneVAV1_1'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Core_bottom')
        self.sensors['TzoneVAV1_2'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Perimeter_bot_ZN_3')
        self.sensors['TzoneVAV1_3'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Perimeter_bot_ZN_2')
        self.sensors['cooling_setpoint'] = self.api.exchange.get_variable_handle('Schedule Value', 'CLGSETP_SCH')
        self.sensors['TzoneVAV1_4'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Perimeter_bot_ZN_1')
        self.need_to_get_handles = False

    def on_after_predictor_after_hvac_managers(self):
        if self.need_to_get_handles:
            self.get_handles()
        temp_offset = 0.8333
        no_action = 0
        cycle_on = 2
        vav_heating_turn_on = self.api.exchange.get_variable_value(self.sensors['heating_setpoint'])
        vav_heating_turn_off = self.api.exchange.get_variable_value(self.sensors['heating_setpoint']) + (2 * temp_offset)
        vav_cooling_turn_on = self.api.exchange.get_variable_value(self.sensors['cooling_setpoint'])
        vav_cooling_turn_off = self.api.exchange.get_variable_value(self.sensors['cooling_setpoint']) - (2 * temp_offset)
        zone_temps = [
            self.api.exchange.get_variable_value(self.sensors['TzoneVAV1_1']),
            self.api.exchange.get_variable_value(self.sensors['TzoneVAV1_2']),
            self.api.exchange.get_variable_value(self.sensors['TzoneVAV1_3']),
            self.api.exchange.get_variable_value(self.sensors['TzoneVAV1_4']),
            self.api.exchange.get_variable_value(self.sensors['TzoneVAV1_5'])
        ]
        t_min = min(zone_temps)
        t_max = max(zone_temps)
        if t_min < vav_heating_turn_on:
            self.api.exchange.set_actuator_value(self.actuators['VAV_1_NightCycleStatus'], cycle_on)
            return 0
        elif t_min > vav_heating_turn_off:
            self.api.exchange.set_actuator_value(self.actuators['VAV_1_NightCycleStatus'], no_action)
        if t_max > vav_cooling_turn_on:
            self.api.exchange.set_actuator_value(self.actuators['VAV_1_NightCycleStatus'], cycle_on)
            return 0
        elif t_max < vav_cooling_turn_off:
            self.api.exchange.set_actuator_value(self.actuators['VAV_1_NightCycleStatus'], no_action)
        return 0


class Vav2Nightcyclemgr(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['VAV_2_NightCycleStatus'] = self.api.exchange.get_actuator_handle('AirLoopHVAC', 'Availability Status', 'VAV_2')
        self.sensors['TzoneVAV2_3'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Perimeter_mid_ZN_2')
        self.sensors['heating_setpoint'] = self.api.exchange.get_variable_handle('Schedule Value', 'HTGSETP_SCH')
        self.sensors['TzoneVAV2_5'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Perimeter_mid_ZN_4')
        self.sensors['cooling_setpoint'] = self.api.exchange.get_variable_handle('Schedule Value', 'CLGSETP_SCH')
        self.sensors['TzoneVAV2_2'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Perimeter_mid_ZN_3')
        self.sensors['TzoneVAV2_4'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Perimeter_mid_ZN_1')
        self.sensors['TzoneVAV2_1'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Core_mid')
        self.need_to_get_handles = False

    def on_after_predictor_after_hvac_managers(self):
        if self.need_to_get_handles:
            self.get_handles()
        temp_offset = 0.8333
        no_action = 0
        cycle_on = 2
        vav_heating_turn_on = self.api.exchange.get_variable_value(self.sensors['heating_setpoint'])
        vav_heating_turn_off = self.api.exchange.get_variable_value(self.sensors['heating_setpoint']) + (2 * temp_offset)
        vav_cooling_turn_on = self.api.exchange.get_variable_value(self.sensors['cooling_setpoint'])
        vav_cooling_turn_off = self.api.exchange.get_variable_value(self.sensors['cooling_setpoint']) - (2 * temp_offset)
        zone_temps = [
            self.api.exchange.get_variable_value(self.sensors['TzoneVAV2_1']),
            self.api.exchange.get_variable_value(self.sensors['TzoneVAV2_2']),
            self.api.exchange.get_variable_value(self.sensors['TzoneVAV2_3']),
            self.api.exchange.get_variable_value(self.sensors['TzoneVAV2_4']),
            self.api.exchange.get_variable_value(self.sensors['TzoneVAV2_5'])
        ]
        t_min = min(zone_temps)
        t_max = max(zone_temps)
        if t_min < vav_heating_turn_on:
            self.api.exchange.set_actuator_value(self.actuators['VAV_2_NightCycleStatus'], cycle_on)
            return 0
        elif t_min > vav_heating_turn_off:
            self.api.exchange.set_actuator_value(self.actuators['VAV_2_NightCycleStatus'], no_action)
        if t_max > vav_cooling_turn_on:
            self.api.exchange.set_actuator_value(self.actuators['VAV_2_NightCycleStatus'], cycle_on)
            return 0
        elif t_max < vav_cooling_turn_off:
            self.api.exchange.set_actuator_value(self.actuators['VAV_2_NightCycleStatus'], no_action)
        return 0


class Vav3Nightcyclemgr(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['VAV_3_NightCycleStatus'] = self.api.exchange.get_actuator_handle('AirLoopHVAC', 'Availability Status', 'VAV_3')
        self.sensors['TzoneVAV3_2'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Perimeter_top_ZN_3')
        self.sensors['TzoneVAV3_4'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Perimeter_top_ZN_1')
        self.sensors['TzoneVAV3_1'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Core_top')
        self.sensors['heating_setpoint'] = self.api.exchange.get_variable_handle('Schedule Value', 'HTGSETP_SCH')
        self.sensors['TzoneVAV3_5'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Perimeter_top_ZN_4')
        self.sensors['cooling_setpoint'] = self.api.exchange.get_variable_handle('Schedule Value', 'CLGSETP_SCH')
        self.sensors['TzoneVAV3_3'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Perimeter_top_ZN_2')
        self.need_to_get_handles = False

    def on_after_predictor_after_hvac_managers(self):
        if self.need_to_get_handles:
            self.get_handles()
        temp_offset = 0.8333
        no_action = 0
        cycle_on = 2
        vav_heating_turn_on = self.api.exchange.get_variable_value(self.sensors['heating_setpoint'])
        vav_heating_turn_off = self.api.exchange.get_variable_value(self.sensors['heating_setpoint']) + (2 * temp_offset)
        vav_cooling_turn_on = self.api.exchange.get_variable_value(self.sensors['cooling_setpoint'])
        vav_cooling_turn_off = self.api.exchange.get_variable_value(self.sensors['cooling_setpoint']) - (2 * temp_offset)
        zone_temps = [
            self.api.exchange.get_variable_value(self.sensors['TzoneVAV3_1']),
            self.api.exchange.get_variable_value(self.sensors['TzoneVAV3_2']),
            self.api.exchange.get_variable_value(self.sensors['TzoneVAV3_3']),
            self.api.exchange.get_variable_value(self.sensors['TzoneVAV3_4']),
            self.api.exchange.get_variable_value(self.sensors['TzoneVAV3_5'])
        ]
        t_min = min(zone_temps)
        t_max = max(zone_temps)
        if t_min < vav_heating_turn_on:
            self.api.exchange.set_actuator_value(self.actuators['VAV_3_NightCycleStatus'], cycle_on)
            return 0
        elif t_min > vav_heating_turn_off:
            self.api.exchange.set_actuator_value(self.actuators['VAV_3_NightCycleStatus'], no_action)
        if t_max > vav_cooling_turn_on:
            self.api.exchange.set_actuator_value(self.actuators['VAV_3_NightCycleStatus'], cycle_on)
            return 0
        elif t_max < vav_cooling_turn_off:
            self.api.exchange.set_actuator_value(self.actuators['VAV_3_NightCycleStatus'], no_action)
        return 0


class Vav5Nightcyclemgr(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.actuators = {}
        self.sensors = {}
        self.internals = {}
        self.globals = {}
        self.trends = {}
        self.need_to_get_handles = True

    def get_handles(self):
        self.actuators['VAV_5_NightCycleStatus'] = self.api.exchange.get_actuator_handle('AirLoopHVAC', 'Availability Status', 'VAV_5')
        self.sensors['heating_setpoint'] = self.api.exchange.get_variable_handle('Schedule Value', 'HTGSETP_SCH')
        self.sensors['TzoneVAV5'] = self.api.exchange.get_variable_handle('Zone Mean Air Temperature', 'Basement')
        self.sensors['cooling_setpoint'] = self.api.exchange.get_variable_handle('Schedule Value', 'CLGSETP_SCH')
        self.need_to_get_handles = False

    def on_after_predictor_after_hvac_managers(self):
        if self.need_to_get_handles:
            self.get_handles()
        temp_offset = 0.8333
        no_action = 0
        cycle_on = 2
        vav_heating_turn_on = self.api.exchange.get_variable_value(self.sensors['heating_setpoint'])
        vav_heating_turn_off = self.api.exchange.get_variable_value(self.sensors['heating_setpoint']) + (2 * temp_offset)
        vav_cooling_turn_on = self.api.exchange.get_variable_value(self.sensors['cooling_setpoint'])
        vav_cooling_turn_off = self.api.exchange.get_variable_value(self.sensors['cooling_setpoint']) - (2 * temp_offset)
        t_zone_vav_5 = self.api.exchange.get_variable_value(self.sensors['TzoneVAV5'])
        if t_zone_vav_5 < vav_heating_turn_on:
            self.api.exchange.set_actuator_value(self.actuators['VAV_5_NightCycleStatus'], cycle_on)
            return 0
        elif t_zone_vav_5 > vav_heating_turn_off:
            self.api.exchange.set_actuator_value(self.actuators['VAV_5_NightCycleStatus'], no_action)
        if t_zone_vav_5 > vav_cooling_turn_on:
            self.api.exchange.set_actuator_value(self.actuators['VAV_5_NightCycleStatus'], cycle_on)
            return 0
        elif t_zone_vav_5 < vav_cooling_turn_off:
            self.api.exchange.set_actuator_value(self.actuators['VAV_5_NightCycleStatus'], no_action)
        return 0
