from ctypes import cdll, c_void_p, c_uint


class StateManager:
    """
    This API class enables a client to create and manage state instance references for using EnergyPlus API methods.
    Nearly all EnergyPlus API methods require a state reference to be passed, and when callbacks are made, the current
    state reference is passed as an argument.  This allows client code to close the loop and pass the current state when
    making API calls inside callbacks.

    The state reference object is at the heart of accessing EnergyPlus via API, however, the client code should simply
    be a courier of this object, and never attempt to manipulate the object.

    This class allows a client to create a new state, reset it, and free the object when finished with it.
    """

    def __init__(self, api: cdll):
        self.api = api
        self.api.stateNew.argtypes = []
        self.api.stateNew.restype = c_uint
        self.api.stateReset.argtypes = [c_uint]
        self.api.stateReset.restype = c_void_p
        self.api.stateDelete.argtypes = [c_uint]
        self.api.stateDelete.restype = c_void_p

    def new_state(self) -> c_void_p:
        """
        This function creates a new state object in EnergyPlus.

        :return: A reference to a new state object in memory
        """
        return self.api.stateNew()

    def reset_state(self, state: c_uint) -> None:
        """
        This function resets an existing state instance, thus resetting the simulation, including any registered
        callback functions.

        :return: Nothing
        """
        self.api.stateReset(state)

    def delete_state(self, state: c_uint) -> None:
        """
        This function deletes an existing state instance, freeing the memory.

        :return: Nothing
        """
        self.api.stateDelete(state)
