"""
Cross-platform setup.py for building the EnergyPlus Python module.
Barebone library, no pre- or post-processing tools.
"""

from setuptools import setup, Extension
from setuptools.command.build_ext import build_ext
import os
import shutil
import platform
from pathlib import Path
import subprocess as sp
import sys

from wheel.bdist_wheel import bdist_wheel


wheels = {
    "darwin": {
        "x86_64": {
            "wheel": "macosx_10_13_x86_64",
            "zip_tag": "OSX",
            "build_tool": "Ninja",
        },
        "arm64": {
            "wheel": "macosx_11_0_arm64",
            "zip_tag": "OSX_arm64",
            "build_tool": "Ninja",
        },
    },
    "linux": {
        "x86_64": {
            "wheel": "manylinux1_x86_64",
            "zip_tag": "Linux",
            "build_tool": "Ninja",
        }
    },
    "windows": {
        "i386": {
            "wheel": "win32",
            "zip_tag": "Windows",
            "arch": "x64",
            "build_tool": "Visual Studio 16 2019",
        },
        "amd64": {
            "wheel": "win_amd64",
            "zip_tag": "Windows",
            "arch": "x64",
            "build_tool": "Visual Studio 16 2019",
        },
    },
}
platform_file_extension = {"Darwin": "dylib", "Linux": "so", "Windows": "dll"}
libdir = list(Path("build").glob("lib*"))
if len(libdir) > 0:
    shutil.rmtree(libdir[0], ignore_errors=True)
wheel = wheels[platform.system().lower()][platform.machine().lower()]


class PyenergyplusBDistWheel(bdist_wheel):
    # Overwrite wheel tag
    def get_tag(self):
        return "py3", "none", wheel["wheel"]


class CMakeExtension(Extension):
    def __init__(self, name, cmake_source_dir="", sourcedir=""):
        Extension.__init__(self, name, sources=[])
        self.sourcedir = os.path.abspath(sourcedir)
        self.cmake_source_dir = os.path.abspath(cmake_source_dir)


# This class handles the CMake build
class CMakeBuild(build_ext):
    def run(self):
        for ext in self.extensions:
            self.build_cmake(ext)

    def build_cmake(self, ext):
        cwd = os.getcwd()

        build_temp = os.path.abspath(self.build_temp)
        build_lib = os.path.abspath(self.build_lib)

        os.makedirs(build_temp, exist_ok=True)
        os.chdir(build_temp)

        cfg = wheels[platform.system().lower()][platform.machine().lower()]
        if "arch" in cfg:
            arch = cfg['arch']
        else:
            arch = None

        # call cmake to configure the build
        pdir = Path("Products")

        cmake_cmd = [
            "cmake",
            "-G",
            cfg["build_tool"]
        ]
        # Get Python executable path for linking
        pypath = sys.executable

        cmake_build_cmd = ["cmake", "--build", "."]
        if arch:
            cmake_cmd += ["-A", arch]
        cmake_cmd.append("-DBUILD_FORTRAN=OFF")
        if platform.system().lower() != "windows":
            cmake_cmd.append("-DCMAKE_BUILD_TYPE=Release")
        else:
            cmake_cmd.append("-DLINK_WITH_PYTHON:BOOL=ON")
            cmake_cmd.append("-DPython_REQUIRED_VERSION:STRING=3.8")
            cmake_cmd.append(f"-DPython_ROOT_DIR:PATH={os.path.dirname(pypath)}")
            cmake_build_cmd += ["--config", "Release"]
            pdir = Path("Products") / "Release"
        cmake_cmd.append(ext.cmake_source_dir)

        # Call cmake
        sp.check_call(cmake_cmd)
        sp.check_call(cmake_build_cmd)

        output_dir = os.path.join(build_lib, ext.name)
        os.makedirs(output_dir, exist_ok=True)
        file_extension = platform_file_extension[platform.system()]
        # Copy the library files to the output directory
        lib_files = pdir.glob(f"*.{file_extension}*")
        for file in lib_files:
            shutil.move(str(file), build_lib)
        # Copy the Python source code files to the output directory
        sdir = pdir / "pyenergyplus"
        for file in sdir.glob("*.py"):
            shutil.move(str(file), os.path.join(build_lib, "pyenergyplus"))
        os.chdir(cwd)


setup(
    name="energyplus",
    version="23.1.0",
    packages=[],
    license="LICENSE.txt",
    author="Placeholder",
    author_email="placeholder",
    url="https://github.com/NREL/EnergyPlus",
    description="Placeholder",
    long_description=Path("README.md").read_text(),
    ext_modules=[CMakeExtension("pyenergyplus")],
    cmdclass={
        "build_ext": CMakeBuild,
        "bdist_wheel": PyenergyplusBDistWheel,
    },
)
