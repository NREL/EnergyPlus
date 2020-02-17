from pyenergyplus.plugin import EnergyPlusPlugin


class TestOutTrends(EnergyPlusPlugin):

    def on_begin_zone_timestep_before_init_heat_balance(self) -> int:
        if 'handles_set' not in self.data:
            self.data['outdoor_temp'] = self.api.exchange.get_variable_handle(
                "Site Outdoor Air Drybulb Temperature", "Environment"
            )
            self.data['global_var'] = self.api.exchange.get_global_handle('OutdoorTempCopy')
            self.data['trend_var'] = self.api.exchange.get_trend_handle('ThisTrendVariable')
            self.data['handles_set'] = True
            print("%s, %s, %s" % (
                self.data['outdoor_temp'], self.data['global_var'], self.data['trend_var']
            ))
        cur_temp = self.api.exchange.get_variable_value(self.data['outdoor_temp'])
        self.api.exchange.set_global_value(self.data['global_var'], cur_temp)
        # val_1 = self.api.exchange.get_trend_value(self.data['trend_var'], 0)
        # val_2 = self.api.exchange.get_trend_value(self.data['trend_var'], 1)
        # val_3 = self.api.exchange.get_trend_value(self.data['trend_var'], 2)
        # val_4 = self.api.exchange.get_trend_value(self.data['trend_var'], 3)
        # val_5 = self.api.exchange.get_trend_value(self.data['trend_var'], 4)
        # print("TRENDING: %s, %s, %s, %s, %s" % (
        #     round(val_1, 2), round(val_2, 2), round(val_3, 2), round(val_4, 2), round(val_5, 2)
        # ))
        return 0
