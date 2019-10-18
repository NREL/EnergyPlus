from ctypes import cdll
import os
import sys

from pyenergyplus.func import Functional
from pyenergyplus.datatransfer import DataTransfer
from pyenergyplus.runtime import Runtime


def api_path() -> str:
    """
    This function returns a string to the EnergyPlus dynamic library.  The energyplusapi target in the build system
    depends on the Python API build script, so you shouldn't be able to generate the EnergyPlus dynamic library without
    these scripts being successfully set up in the build tree by CMake.

    Inside of a build folder, this binary should be in Products.  For example, in a makefile-style build, this will
    be /path/to/build/dir/Products.  I'm not yet sure what it is in a Visual Studio build.  The Python API files are
    placed inside of the Products directory in a pyenergyplus directory, so the dynamic library will simply be in the
    current script's parent directory.

    In an installation, the binary will be in the installation root.  The Python API files are in a pyenergyplus
    directory inside that install root, so the binary will again just be in this script's parent directory.

    Thus, for either case, utilizing the Python API wrappers is straightforward: if executing from directly from the
    build or install folder, scripts can be imported as `from pyenergyplus.foo import bar`.  If executing from a totally
    separate directory, the build or install directory can be inserted into the beginning of sys.path so that the
    pyenergyplus directory can be found.

    :return:
    """
    this_script_dir = os.path.dirname(os.path.realpath(__file__))
    api_dll_dir = os.path.dirname(os.path.normpath(this_script_dir))
    if sys.platform.startswith('linux'):
        return os.path.join(api_dll_dir, 'libenergyplusapi.so')
    elif sys.platform.startswith('darwin'):
        return os.path.join(api_dll_dir, 'libenergyplusapi.dylib')
    else:  # assume Windows
        return os.path.join(api_dll_dir, 'EnergyPlusAPI.dll')


class EnergyPlusAPI:
    """
    This class exposes the EnergyPlus C API to Python.  The API is split into three categories, and this class exposes
    each through functions.   The functional API provides access to static API calls, such as calculation methods
    and version information.  The runtime API allows a user to provide Python functions as callbacks, which are then
    called from within EnergyPlus at specific points in the simulation.  The data_transfer API allows a user to
    exchange data (get sensor values, set actuator values) from within runtime callback methods, during a simulation.
    """

    def __init__(self):
        self.api = cdll.LoadLibrary(api_path())

    def functional(self) -> Functional:
        return Functional(self.api)

    def data_transfer(self) -> DataTransfer:
        return DataTransfer(self.api)

    def runtime(self) -> Runtime:
        return Runtime(self.api)

    @staticmethod
    def api_version() -> float:
        return 0.1
