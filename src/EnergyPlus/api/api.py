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
    This class exposes the EnergyPlus C Library API to Python.  The API is split into three categories, and this class
    exposes each API category.

    - The functional API provides access to static API calls, such as calculation methods
      and version information.
    - The runtime API allows a user to provide Python functions as callbacks, which are then
      called from within EnergyPlus at specific points in the simulation.
    - The data_transfer API allows a user to exchange data (get sensor values, set actuator values)
      from within runtime callback methods, during a simulation.

    Inside of a build folder, the API library (dll) should be in Products.  For example, in a makefile-style build, this
    will be /path_to_build/Products.  I'm not yet sure what it is in a Visual Studio build.  The Python API files are
    placed inside of the Products directory in a pyenergyplus directory, so the dynamic library will simply be in the
    current script's parent directory. In an installation, the binary will be in the installation root.  The Python API
    files are in a pyenergyplus directory inside that install root, so the binary will again just be in this script's
    parent directory.

    For either case, utilizing the Python API wrappers is straightforward: if executing from directly from the
    build or install folder, scripts can be imported as `from pyenergyplus.foo import bar`.  If executing from a totally
    separate directory, the build or install directory can be inserted into the beginning of sys.path so that the
    pyenergyplus directory can be found.

    To reference this in an IDE to allow writing scripts using autocomplete, etc., most IDEs allow you to add third-
    party library directories.  The directory to add would be the build or install folder, as appropriate, so that
    the `from pyenergyplus` import statements can find a pyenergyplus package inside that third-party directory.

    """

    def __init__(self):
        self.api = cdll.LoadLibrary(api_path())

    def functional(self) -> Functional:
        """
        Returns a new instance of a Functional API class.  In most cases, this should be called once, and the script
        should keep a reference to the new instance.  All functional API calls can then be made on this one instance,
        rather than making multiple instances by calling this function multiple times.

        :return: A "Functional" API class instance.
        """
        return Functional(self.api)

    def data_transfer(self) -> DataTransfer:
        """
        Returns a new instance of a DataTransfer API class.  In most cases, this should be called once, and the script
        should keep a reference to the new instance.  All functional API calls can then be made on this one instance,
        rather than making multiple instances by calling this function multiple times.

        :return: A "DataTransfer" API class instance.
        """
        return DataTransfer(self.api)

    def runtime(self) -> Runtime:
        """
        Returns a new instance of a Runtime API class.  In most cases, this should be called once, and the script
        should keep a reference to the new instance.  All functional API calls can then be made on this one instance,
        rather than making multiple instances by calling this function multiple times.

        :return: A "Runtime" API class instance.
        """
        return Runtime(self.api)

    @staticmethod
    def api_version() -> str:
        """
        Returns a string representation of the version of this API.  The EnergyPlus API will evolve over time,
        but in most cases, it will be simply adding new functionality and methods, not breaking existing API calls.
        The fractional portion of the API will be incremented when new functionality is added, and the whole number
        portion will be incremented when an existing API is broken.
        :return:
        """
        return "${PYTHON_API_VERSION_MAJOR}.${PYTHON_API_VERSION_MINOR}"
