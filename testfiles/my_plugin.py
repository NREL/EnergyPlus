from pyenergyplus.plugin import EnergyPlusPlugin


class MyPlugin(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        print("Inside my plugin constructor")
        self.version_string = self.api.energyplus_version()
        self.transfer = self.api.data_transfer()
        self.db_handle = 0

    def main(self):
        if self.db_handle == 0:
            self.db_handle = self.transfer.get_variable_handle(u"SITE OUTDOOR AIR DRYBULB TEMPERATURE", u"ENVIRONMENT")
        if self.db_handle <= 0:
            print("COULD NOT GET ODB HANDLE, DELAYING...")
        else:
            odb = self.transfer.get_variable_value(self.db_handle)
            print("Inside main function of " + type(self).__name__ + " in E+ version: " + self.version_string + "; ODB = " + str(odb))
        return 0
