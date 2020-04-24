import sys
from pyenergyplus.plugin import EnergyPlusPlugin


class UserDefinedCoilSim(EnergyPlusPlugin):

    def __init__(self):
        # init parent class
        super().__init__()

        # get functional api glycol instance
        self.glycol = self.api.functional.glycol(u"water")

        # members
        self.CWLoopExitTemp = 7.0
        self.CWLoopDeltaTemp = 4.0
        self.CW_CP = 0.0

        # handles
        self.need_to_get_handles = True
        self.CWplant_Vdot_Design_handle = None
        self.Chiller_Vdot_Design_handle = None
        self.Chiller_Mdot_Min_handle = None
        self.Chiller_Mdot_Max_handle = None
        self.Chiller_Cap_Min_handle = None
        self.Chiller_Cap_Max_handle = None
        self.Chiller_Cap_Opt_handle = None

        self.CW_MyLoad_handle = None
        self.CW_Tin_handle = None
        self.CW_Mdot_handle = None
        self.CW_Tout_handle = None
        self.CW_Mdot_Request_handle = None
        self.Chiller_ElectEnergy_handle = None

    def get_handles(self):
        self.CWplant_Vdot_Design_handle = self.api.exchange.get_internal_variable_handle(
            "Plant Design Volume Flow Rate",
            "Chilled Water Loop"
        )
        self.Chiller_Vdot_Design_handle = self.api.exchange.get_actuator_handle(
            "Plant Connection 1",
            "Design Volume Flow Rate",
            "Central Chiller"
        )
        self.Chiller_Mdot_Min_handle = self.api.exchange.get_actuator_handle(
            "Plant Connection 1",
            "Minimum Mass Flow Rate",
            "Central Chiller"
        )
        self.Chiller_Mdot_Max_handle = self.api.exchange.get_actuator_handle(
            "Plant Connection 1",
            "Maximum Mass Flow Rate",
            "Central Chiller"
        )
        self.Chiller_Cap_Min_handle = self.api.exchange.get_actuator_handle(
            "Plant Connection 1",
            "Minimum Loading Capacity",
            "Central Chiller"
        )
        self.Chiller_Cap_Max_handle = self.api.exchange.get_actuator_handle(
            "Plant Connection 1",
            "Maximum Loading Capacity",
            "Central Chiller"
        )
        self.Chiller_Cap_Opt_handle = self.api.exchange.get_actuator_handle(
            "Plant Connection 1",
            "Optimal Loading Capacity",
            "Central Chiller"
        )
        self.CW_MyLoad_handle = self.api.exchange.get_internal_variable_handle(
            "Load Request for Plant Connection 1",
            "Central Chiller"
        )
        self.CW_Tin_handle = self.api.exchange.get_internal_variable_handle(
            "Inlet Temperature for Plant Connection 1",
            "Central Chiller"
        )
        self.CW_Mdot_handle = self.api.exchange.get_internal_variable_handle(
            "Inlet Mass Flow Rate for Plant Connection 1",
            "Central Chiller"
        )
        self.CW_Tout_handle = self.api.exchange.get_actuator_handle(
            "Plant Connection 1",
            "Outlet Temperature",
            "Central Chiller"
        )
        self.CW_Mdot_Request_handle = self.api.exchange.get_actuator_handle(
            "Plant Connection 1",
            "Mass Flow Rate",
            "Central Chiller"
        )
        self.Chiller_ElectEnergy_handle = self.api.exchange.get_global_handle(
            "Chiller_ElectEnergy"
        )

        self.need_to_get_handles = False

    def init_sim(self):
        self.CW_CP = self.glycol.specific_heat(self.CWLoopExitTemp)
        CW_rho = self.glycol.density(self.CWLoopExitTemp)
        CWplant_Vdot_Design = self.api.exchange.get_internal_variable_value(self.CWplant_Vdot_Design_handle)
        self.api.exchange.set_actuator_value(self.Chiller_Vdot_Design_handle, CWplant_Vdot_Design)
        self.api.exchange.set_actuator_value(self.Chiller_Mdot_Min_handle, 0.0)
        self.api.exchange.set_actuator_value(self.Chiller_Mdot_Max_handle, CWplant_Vdot_Design * CW_rho)
        Chiller_Cap = self.CW_CP * CW_rho * self.CWLoopDeltaTemp * CWplant_Vdot_Design
        self.api.exchange.set_actuator_value(self.Chiller_Cap_Min_handle, 0.0)
        self.api.exchange.set_actuator_value(self.Chiller_Cap_Max_handle, Chiller_Cap)
        self.api.exchange.set_actuator_value(self.Chiller_Cap_Opt_handle, 0.9 * Chiller_Cap)

    def simulate(self):
        # get load, inlet temp, and mdot
        CW_MyLoad = self.api.exchange.get_internal_variable_value(self.CW_MyLoad_handle)
        CW_Tin = self.api.exchange.get_internal_variable_value(self.CW_Tin_handle)
        CW_Mdot = self.api.exchange.get_internal_variable_value(self.CW_Mdot_handle)

        # leave early if chiller is off
        if CW_MyLoad > -100.0:
            # pass inlet temp through
            self.api.exchange.set_actuator_value(self.CW_Tout_handle, CW_Tin)
            # set mass flow rate and electricity to 0
            self.api.exchange.set_actuator_value(self.CW_Mdot_Request_handle, 0.0)
            self.api.exchange.set_global_value(self.Chiller_ElectEnergy_handle, 0.0)
            return

        # set chiller load
        ABS_CW_MyLoad = abs(self.api.exchange.get_actuator_value(self.CW_MyLoad_handle))
        Chiller_Cap_Max = self.api.exchange.get_actuator_value(self.Chiller_Cap_Max_handle)
        if ABS_CW_MyLoad > Chiller_Cap_Max:
            Qdot = Chiller_Cap_Max
        else:
            Qdot = ABS_CW_MyLoad

        # set chiller mdot
        if CW_Mdot == 0.0:
            Mdot = self.api.exchange.get_actuator_value(self.Chiller_Mdot_Max_handle)
            self.api.exchange.set_actuator_value(self.CW_Mdot_Request_handle, Mdot)
        else:
            Mdot = CW_Mdot

        # set outlet temp
        self.api.exchange.set_actuator_value(self.CW_Tout_handle, CW_Tin - (Qdot / (Mdot * self.CW_CP)))

        # set chiller energy
        EIR = 1 / 4.6
        Chiller_power = Qdot * EIR
        Chiller_ElectEnergy = Chiller_power * 3600.0 * self.api.exchange.system_time_step()
        self.api.exchange.set_global_value(self.Chiller_ElectEnergy_handle, Chiller_ElectEnergy)

    def on_user_defined_component_model(self) -> int:

        if self.api.exchange.api_data_fully_ready():
            if self.need_to_get_handles:
                self.get_handles()
            self.init_sim()
            self.simulate()
            return 0
        else:
            return 0
