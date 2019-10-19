from pyenergyplus.plugin import EnergyPlusPlugin


class MyPlugin(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.version_string = str(self.functional.ep_version())
        self.db_handle = 0

    def main(self):
        if self.db_handle == 0:
            self.db_handle = self.transfer.get_variable_handle(u"SITE OUTDOOR AIR DRYBULB TEMPERATURE", u"ENVIRONMENT")
        if self.db_handle <= 0:
            print("Could not get Outdoor Dry Bulb handle; delaying...")
        else:
            odb = self.transfer.get_variable_value(self.db_handle)
            print("Inside " + type(self).__name__ + " in E+ version: " + self.version_string + "; ODB = " + str(odb))
        return 0
