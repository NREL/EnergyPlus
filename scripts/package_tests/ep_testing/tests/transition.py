import os
from subprocess import check_call, CalledProcessError, STDOUT
from urllib import request

from ep_testing.exceptions import EPTestingException
from ep_testing.tests.base import BaseTest


class TransitionOldFile(BaseTest):

    def name(self):
        return 'Test running 1ZoneUncontrolled.idf and make sure it exits OK'

    def run(self, install_root: str, verbose: bool, kwargs: dict):
        if 'last_version' not in kwargs:
            raise EPTestingException('Bad call to %s -- must pass last_version in kwargs' % self.__class__.__name__)
        last_version = kwargs['last_version']
        test_file = kwargs.get('test_file', '1ZoneUncontrolled.idf')
        print('* Running test class "%s" on file "%s"... ' % (self.__class__.__name__, test_file), end='')
        transition_dir = os.path.join(install_root, 'PreProcess', 'IDFVersionUpdater')
        all_transition_binaries = [
            f.path for f in os.scandir(transition_dir) if f.is_file() and f.name.startswith('Transition-')
        ]
        if len(all_transition_binaries) < 1:
            raise EPTestingException('Could not find any transition binaries...weird')
        all_transition_binaries.sort()
        most_recent_binary = all_transition_binaries[-1]
        idf_url = 'https://raw.githubusercontent.com/NREL/EnergyPlus/%s/testfiles/%s' % (last_version, test_file)
        saved_dir = os.getcwd()
        os.chdir(transition_dir)
        idf_path = os.path.join(transition_dir, test_file)
        dev_null = open(os.devnull, 'w')
        try:
            r = request.Request(idf_url)
            response = request.urlopen(r)
            data = response.read()
            with open(idf_path, 'wb') as f:
                f.write(data)
        except Exception as e:
            raise EPTestingException('Could not download file from prior release at %s; error: %s' % (idf_url, str(e)))
        try:
            check_call([most_recent_binary, os.path.basename(idf_path)], stdout=dev_null, stderr=STDOUT)
            print(' [TRANSITIONED] ', end='')
        except CalledProcessError:
            raise EPTestingException('Transition failed!')
        os.chdir(install_root)
        eplus_binary = os.path.join(install_root, 'energyplus')
        try:
            check_call([eplus_binary, '-D', idf_path], stdout=dev_null, stderr=STDOUT)
            print(' [DONE]!')
        except CalledProcessError:
            raise EPTestingException('EnergyPlus failed!')
        os.chdir(saved_dir)


# if __name__ == '__main__':
#     t = TransitionOldFile().run(
#         '/tmp/ep_package/EnergyPlus-9.3.0-5eeaa0ed25-Linux-x86_64', {'last_version': 'v9.2.0'}
#     )
