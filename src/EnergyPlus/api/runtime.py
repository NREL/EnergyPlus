from ctypes import cdll, c_bool, c_int, c_char_p, c_void_p


class Runtime:

    VariableTypeSensor = 0
    VariableTypeActuator = 1

    def __init__(self, api: cdll):
        self.api = api
        self.api.cRunEnergyPlus.argtypes = [c_char_p]
        self.api.cRunEnergyPlus.restype = c_int
        self.api.cInitializeEnergyPlus.argtypes = [c_char_p]
        self.api.cInitializeEnergyPlus.restype = c_int
        self.api.cWrapUpEnergyPlus.argtypes = []
        self.api.cWrapUpEnergyPlus.restype = c_int
        self.api.cInitializeSimulation.argtypes = []
        self.api.cInitializeSimulation.restype = c_void_p
        self.api.cRunOneTimeStep.argtypes = []
        self.api.cRunOneTimeStep.restype = c_void_p
        self.api.cRunOneHour.argtypes = []
        self.api.cRunOneHour.restype = c_void_p
        self.api.cRunOneDay.argtypes = []
        self.api.cRunOneDay.restype = c_void_p
        self.api.cRunEnvironment.argtypes = []
        self.api.cRunEnvironment.restype = c_void_p
        self.api.cSkipCurrentEnvironment.argtypes = []
        self.api.cSkipCurrentEnvironment.restype = c_bool
        self.api.cRunAllEnvironments.argtypes = []
        self.api.cRunAllEnvironments.restype = c_void_p
        self.api.cWrapUpSimulation.argtypes = []
        self.api.cWrapUpSimulation.restype = c_void_p

    def run_energyplus_fully(self, path_to_dir: str):
        return self.api.cRunEnergyPlus(path_to_dir)
