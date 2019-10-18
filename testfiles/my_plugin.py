from pyenergyplus.plugin import EnergyPlusPlugin
from pyenergyplus.api import EnergyPlusAPI


class MyPlugin(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        print("Inside my plugin constructor")
        api = EnergyPlusAPI()
        self.version_string = api.energyplus_version()
        self.transfer = api.data_transfer()
        self.db_handle = self.transfer.get_variable_handle(u"SITE OUTDOOR AIR DRYBULB TEMPERATURE", u"ENVIRONMENT")
        self.error = False
        if self.db_handle <= 0:
            self.error = True
            print("COULD NOT GET ODB HANDLE")

    def main(self):
        if self.error:
            return 1
        odb = self.transfer.get_variable_value(self.db_handle)
        print("Inside main function of " + type(self).__name__ + " in E+ version: " + self.version_string + "; ODB = " + str(odb))
        return 0
