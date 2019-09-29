from ctypes import cdll, c_double, c_void_p


class Wall:

    def __init__(self, resistance: float):
        #     # we expect the C++ DLL to be available at the root of the installation.
        #     # we also expect this interface folder to be placed at the root of the installation
        #     # so the DLL is simply one folder up, and at the appropriate name
        #     dir_path = os.path.dirname(os.path.realpath(__file__))
        #     if sys.platform.startswith('linux'):
        #         self.api = cdll.LoadLibrary(os.path.join(dir_path, '..', 'libFakeAPI.so'))
        #     elif sys.platform.startswith('darwin'):
        #         self.api = cdll.LoadLibrary(os.path.join(dir_path, '..', 'libFakeAPI.dylib'))
        #     else:  # assume Windows
        #         self.api = cdll.LoadLibrary(os.path.join(dir_path, '..', 'FakeAPI.dll'))
        self.api = cdll.LoadLibrary("/eplus/repos/myoldmopar/cmake-build-debug/Products/libenergyplusapi.dylib")
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
