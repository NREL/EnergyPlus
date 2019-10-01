from ctypes import cdll, c_double, c_void_p
import os


class Wall:

    def __init__(self, resistance: float):
        this_absolute_dir = os.path.dirname(os.path.realpath(__file__))
        self.api = cdll.LoadLibrary(os.path.join(this_absolute_dir, "libenergyplusapi.dylib"))
        # constructor
        self.api.newCWall.argtypes = [c_double]
        self.api.newCWall.restype = c_void_p
        # destructor
        self.api.delCWall.argtypes = [c_void_p]
        self.api.delCWall.restype = c_void_p
        # setting thickness
        self.api.setCWallThickness.argtypes = [c_void_p, c_double]
        self.api.setCWallThickness.restype = c_void_p
        # calculating
        self.api.calculateCWall.argtypes = [c_void_p, c_double]
        self.api.calculateCWall.restype = c_double
        # now just create one
        self.instance = self.api.newCWall(resistance)

    def __del__(self):
        self.api.delCWall(self.instance)

    def set_thickness(self, thickness: float):
        self.api.setCWallThickness(self.instance, thickness)

    def calculate(self, multiplier: float) -> float:
        return self.api.calculateCWall(self.instance, multiplier)
