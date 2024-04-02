import os
from shutil import copyfile
from subprocess import check_call, CalledProcessError, STDOUT

from ep_testing.exceptions import EPTestingException
from ep_testing.tests.base import BaseTest


class TestExpandObjectsAndRun(BaseTest):

    def name(self):
        return 'Test running ExpandObjects on a template file and make sure it exits OK'

    def run(self, install_root: str, verbose: bool, kwargs: dict):
        if 'test_file' not in kwargs:
            raise EPTestingException('Bad call to %s -- must pass test_file in kwargs' % self.__class__.__name__)
        test_file = kwargs['test_file']
        print('* Running test class "%s" on file "%s"... ' % (self.__class__.__name__, test_file), end='')
        original_idf_path = os.path.join(install_root, 'ExampleFiles', test_file)
        target_idf_path = os.path.join(os.getcwd(), 'in.idf')
        try:
            copyfile(original_idf_path, target_idf_path)
        except Exception as e:
            raise EPTestingException(
                'Could not copy file for expansion, original file "%s", target file "%s", reason: %s' % (
                    original_idf_path, target_idf_path, str(e)
                )
            )
        expand_objects_binary = os.path.join(install_root, 'ExpandObjects')
        dev_null = open(os.devnull, 'w')
        try:
            check_call([expand_objects_binary], stdout=dev_null, stderr=STDOUT)
        except CalledProcessError:
            raise EPTestingException('ExpandObjects failed!')
        expanded_idf_path = os.path.join(os.getcwd(), 'expanded.idf')
        if os.path.exists(expanded_idf_path):
            print(' [EXPANDED] ', end='')
        else:
            raise EPTestingException(
                'ExpandObjects did not produce an expanded idf at "%s", aborting' % expanded_idf_path
            )
        os.remove(target_idf_path)
        copyfile(expanded_idf_path, target_idf_path)
        eplus_binary = os.path.join(install_root, 'energyplus')
        try:
            check_call([eplus_binary, '-D', target_idf_path], stdout=dev_null, stderr=STDOUT)
            print(' [DONE]!')
        except CalledProcessError:
            raise EPTestingException('EnergyPlus failed!')
