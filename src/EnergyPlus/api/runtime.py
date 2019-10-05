from ctypes import cdll, c_int, c_char_p, c_void_p


class Runtime:

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
        self.api.cGetNextEnvironment.argtypes = []
        self.api.cGetNextEnvironment.restypes = c_int
        self.api.cRunOneTimeStep.argtypes = []
        self.api.cRunOneTimeStep.restype = c_void_p
        self.api.cRunOneHour.argtypes = []
        self.api.cRunOneHour.restype = c_void_p
        self.api.cRunOneDay.argtypes = []
        self.api.cRunOneDay.restype = c_void_p
        self.api.cRunEnvironment.argtypes = []
        self.api.cRunEnvironment.restype = c_void_p
        self.api.cSkipCurrentEnvironment.argtypes = []
        self.api.cSkipCurrentEnvironment.restype = c_int
        self.api.cRunAllEnvironments.argtypes = []
        self.api.cRunAllEnvironments.restype = c_void_p
        self.api.cWrapUpSimulation.argtypes = []
        self.api.cWrapUpSimulation.restype = c_void_p

    def run_energyplus_fully(self, path_to_dir: str):
        return self.api.cRunEnergyPlus(path_to_dir)

    def run_energyplus_each_environment(self, path_to_dir):
        self.api.cInitializeEnergyPlus(path_to_dir)
        self.api.cInitializeSimulation()
        env_count = 0
        while True:
            env = self.api.cGetNextEnvironment()
            if env == 0:
                env_count += 1
            else:
                break
            skip = self.api.cSkipCurrentEnvironment()
            if skip == 0:
                continue
            self.api.cRunEnvironment()
        self.api.cWrapUpSimulation()
        self.api.cWrapUpEnergyPlus()

    def run_energyplus_each_day(self, path_to_dir):
        self.api.cInitializeEnergyPlus(path_to_dir)
        self.api.cInitializeSimulation()
        env_count = 0
        while True:
            env = self.api.cGetNextEnvironment()
            if env == 0:
                env_count += 1
            else:
                break
            skip = self.api.cSkipCurrentEnvironment()
            if skip == 0:
                continue
            self.api.cRunEnvironment()
        self.api.cWrapUpSimulation()
        self.api.cWrapUpEnergyPlus()
