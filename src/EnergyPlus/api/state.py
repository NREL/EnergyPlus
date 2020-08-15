from ctypes import cdll, c_void_p


class State:
    """
    This API class enables a client to hook into EnergyPlus at runtime and sense/actuate data in a running simulation.
    The pattern is quite simple: create a callback function in Python, and register it with one of the registration
    methods on this class to allow the callback to be called at a specific point in the simulation.  Inside the callback
    function, the client can get sensor values and set actuator values using the DataTransfer API methods, and also
    look up values and perform calculations using EnergyPlus internal methods via the Functional API methods.
    """

    def __init__(self, api: cdll):
        self.api = api
        self.api.stateNew.argtypes = []
        self.api.stateNew.restype = c_void_p
        self.api.stateReset.argtypes = [c_void_p]
        self.api.stateReset.restype = c_void_p
        self.api.stateDelete.argtypes = [c_void_p]
        self.api.stateDelete.restype = c_void_p

    def new_state(self) -> c_void_p:
        """
        This function creates a new state object that is required to pass into EnergyPlus Runtime API function calls

        :return: A pointer to a new state object in memory
        """
        return self.api.stateNew()

    def reset_state(self, state: c_void_p) -> None:
        """
        This function resets an existing state instance

        :return: Nothing
        """
        self.api.stateReset(state)

    def delete_state(self, state: c_void_p) -> None:
        """
        This function deletes an existing state instance

        :return: Nothing
        """
        self.api.stateDelete(state)
