
from pyenergyplus.plugin import EnergyPlusPlugin


class AveragePMVAndTests(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.do_setup = True

    @staticmethod
    def hpac_cool_plf_f_plr_curve(x: float) -> float:
        y = 0.75 + 0.25 * x + 0.0 * x ** 2
        y = max(0.0, y)
        y = min(1.0, y)
        return y

    def on_end_of_zone_timestep_before_zone_reporting(self) -> int:
        if not self.api.exchange.api_data_fully_ready():
            return 0
        if self.do_setup:
            self.data["core_zone_pmv_handle"] = self.api.exchange.get_variable_handle(
                "Zone Thermal Comfort Fanger Model PMV", "Core_ZN"
            )
            self.data["pmv_avg_handle"] = self.api.exchange.get_global_handle("PMVrunningAvg")
            self.data["trend_handle"] = self.api.exchange.get_trend_handle("PMVtrendLog1")
            self.do_setup = False
        current_pmv = self.api.exchange.get_variable_value(self.data["core_zone_pmv_handle"])
        self.api.exchange.set_global_value(self.data["pmv_avg_handle"], current_pmv)
        self.do_trend_tests()
        if not self.do_curve_and_table_tests():
            return 1
        if not self.do_psychrometric_tests():
            return 1
        trend_avg = self.api.exchange.get_trend_average(self.data["trend_handle"], 12)
        self.api.exchange.set_global_value(self.data["pmv_avg_handle"], trend_avg)
        if not self.kill_run_if_uncomfortable():
            return 1
        return 0

    def do_trend_tests(self) -> None:
        pass
        # EnergyManagementSystem:Subroutine,
        # DoTrendTests,            !- Name
        # Set p1 = @TrendValue PMVtrendLog1 1,  !- Program Line 1
        # Set p2 = @TrendValue PMVtrendLog1 2,  !- Program Line 2
        # Set p3 = @TrendValue PMVtrendLog1 3,  !- <none>
        # Set p4 = @TrendValue PMVtrendLog1 4,  !- <none>
        # Set p5 = @TrendValue PMVtrendLog1 5,  !- <none>
        # Set Avg1to5 = @TrendAverage PMVtrendLog1 5,  !- <none>
        # Set Avg1to20 = @TrendAverage PMVtrendLog1 20,  !- <none>
        # Set Max1to5 = @TrendMax PMVTrendLog1 5,  !- <none>
        # Set Min1to5 = @TrendMin PMVTrendLog1 5,  !- <none>
        # Set slope1to5 = @TrendDirection PMVTrendLog1 5,  !- <none>
        # Set slope1to20 = @TrendDirection PMVTrendLog1 20,  !- <none>
        # Set slopeAll = @TrendDirection PMVTrendLog1 300,  !- <none>
        # Set Sum1to5 = @TrendSum PMVTrendLog1 5,  !- <none>
        # Set Sum1to20 = @TrendSum PMVTrendLog1 20;  !- <none>

    def do_curve_and_table_tests(self) -> bool:
        if self.hpac_cool_plf_f_plr_curve(0) != 0.75:
            self.api.runtime.issue_severe("Python Curve and Table Tests Failed, Aborting!")
            return False
        if self.hpac_cool_plf_f_plr_curve(1) != 1.0:
            self.api.runtime.issue_severe("Python Curve and Table Tests Failed, Aborting!")
            return False
        return True

    def do_psychrometric_tests(self) -> bool:
        pressure = 101300.0
        temp = 20.0
        hum_ratio = 0.009
        try:
            p = self.api.functional.psychrometrics()
            p.density(pressure, temp, hum_ratio)
            p.specific_heat(hum_ratio, temp)
            p.latent_energy_of_air(temp)
            p.latent_energy_of_moisture_in_air(temp)
            wb = p.wet_bulb(temp, hum_ratio, pressure)
            p.dew_point(hum_ratio, pressure)
            p.dew_point_b(temp, wb, pressure)
            rh = p.relative_humidity_b(temp, hum_ratio, pressure)
            h = p.enthalpy(temp, hum_ratio)
            p.enthalpy_b(temp, rh, pressure)
            p.dry_bulb(h, hum_ratio)
            p.vapor_density(temp, hum_ratio, pressure)
            rhov = p.vapor_density_b(temp, rh)
            p.relative_humidity(temp, rhov)
            p.specific_volume(temp, hum_ratio, pressure)
            p.humidity_ratio(temp, h)
            p.humidity_ratio_b(temp, pressure)
            p.humidity_ratio_c(temp, rh, pressure)
            p.humidity_ratio_d(temp, wb, pressure)
            p.saturation_pressure(temp)
            p.saturation_temperature(h, pressure)
        except:
            self.api.runtime.issue_severe("Psychrometric function evaluation failed, aborting")
            return False
        try:
            f = self.api.functional.glycol("water")
            f.specific_heat(temp)
            f.density(temp)
            f.conductivity(temp)
            f.viscosity(temp)
        except:
            self.api.runtime.issue_severe("Glycol property function evaluation failed, aborting")
            return False
        return True

    def kill_run_if_uncomfortable(self) -> bool:
        current_value = self.api.exchange.get_global_value(self.data["pmv_avg_handle"])
        print("Current value of PMV: " + str(current_value))
        if current_value > 2.5:
            self.api.runtime.issue_severe("Error -- PMV > 2.5")
            return False
        elif current_value < -1.3:
            self.api.runtime.issue_severe("Error -- PMV < -1.3")
            return False
        return True
