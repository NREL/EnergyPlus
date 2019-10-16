from energyplus_plugin_interface import EnergyPlusPlugin


class MyPluginAfterHour(EnergyPlusPlugin):

    def main(self):
        print("Inside main function of " + type(self).__name__)
        return 0
