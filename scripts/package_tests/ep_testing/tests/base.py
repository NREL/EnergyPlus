from os import chdir
from tempfile import mkdtemp


class BaseTest:

    def __init__(self):
        self.verbose = False
        temp_dir = mkdtemp()
        print('{Sandbox Dir: \"' + temp_dir + '\"} ', end='')
        chdir(temp_dir)

    def name(self):
        raise NotImplementedError('name() must be overridden by derived classes')

    def run(self, install_root: str, verbose: bool, kwargs: dict):
        raise NotImplementedError('run() must be overridden by derived classes')
