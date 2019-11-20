from pyenergyplus.plugin import EnergyPlusPlugin


class ResizePSZToMatchProductAvailability(EnergyPlusPlugin):

    def select_discrete_nominal_airflow(self, initial_flow):
        if initial_flow <= 0.56628:
            return 0.56628
        elif initial_flow <= 0.75504:
            return 0.75504
        elif initial_flow <= 0.9438:
            return 0.9438
        elif initial_flow <= 1.13256:
            return 1.13256
        elif initial_flow <= 1.4157:
            return 1.4157
        elif initial_flow <= 1.60446:
            return 1.60446
        elif initial_flow <= 1.8879:
            return 1.8879
        else:
            self.api.runtime.issue_warning("Invalid airflow passed into Python Plugin")

    def main(self):
        print("\n\n\n\n\n\n\nHELLLLLLO")
        for i in range(1, 11):
            print("Operating on zone number: %s" % i)
            var_handle = self.api.exchange.get_internal_variable_handle(
                "Intermediate Air System Main Supply Volume Flow Rate",
                "PSZ-AC_%s:%s" % (i, i)
            )
            actuator_handle = self.api.exchange.get_actuator_handle(
                "PSZ-AC_%s:%s" % (i, i),
                "Sizing:System",
                "Main Supply Volume Flow Rate"
            )
            print("Handles are: %s, %s" % (var_handle, actuator_handle))
            initial_value = self.api.exchange.get_internal_variable_value(var_handle)
            new_value = self.select_discrete_nominal_airflow(initial_value)
            print("Original value: %s; New value: %s" % (initial_value, new_value))
            self.api.exchange.set_actuator_value(actuator_handle, new_value)
