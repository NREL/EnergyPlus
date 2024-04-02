import os
from pathlib import Path

from ep_testing.config import TestConfiguration, OS
from ep_testing.tests.api import TestPythonAPIAccess, TestCAPIAccess, TestCppAPIDelayedAccess
from ep_testing.tests.energyplus import TestPlainDDRunEPlusFile
from ep_testing.tests.expand_objects import TestExpandObjectsAndRun
from ep_testing.tests.hvacdiagram import HVACDiagram
from ep_testing.tests.transition import TransitionOldFile


class Tester:

    def __init__(self, config: TestConfiguration, install_path: Path, verbose: bool):
        self.install_path = str(install_path)
        self.config = config
        self.verbose = verbose

    def run(self):
        saved_path = os.getcwd()
        TestPlainDDRunEPlusFile().run(
            self.install_path, self.verbose, {'test_file': '1ZoneUncontrolled.idf'}
        )
        TestPlainDDRunEPlusFile().run(
            self.install_path, self.verbose, {'test_file': 'PythonPluginCustomOutputVariable.idf'}
        )
        TestExpandObjectsAndRun().run(
            self.install_path, self.verbose, {'test_file': 'HVACTemplate-5ZoneFanCoil.idf'}
        )
        TransitionOldFile().run(
            self.install_path, self.verbose, {'last_version': self.config.tag_last_version}
        )
        HVACDiagram().run(
            self.install_path, self.verbose, {}
        )
        if self.config.os == OS.Windows:
            print("Windows Symlink runs are not testable on Travis, I think the user needs symlink privilege.")
        else:
            TestPlainDDRunEPlusFile().run(
                self.install_path, self.verbose, {'test_file': '1ZoneUncontrolled.idf', 'binary_sym_link': True}
            )
        TestCAPIAccess().run(
            self.install_path, self.verbose,
            {'os': self.config.os, 'bitness': self.config.bitness, 'msvc_version': self.config.msvc_version}
        )
        TestCppAPIDelayedAccess().run(
            self.install_path, self.verbose,
            {'os': self.config.os, 'bitness': self.config.bitness, 'msvc_version': self.config.msvc_version}
        )
        if self.config.bitness == 'x32':
            print("Travis does not have a 32-bit Python package readily available, so not testing Python API")
        else:
            TestPythonAPIAccess().run(
                self.install_path, self.verbose, {'os': self.config.os}
            )
        os.chdir(saved_path)
