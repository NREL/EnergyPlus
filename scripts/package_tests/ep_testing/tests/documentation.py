import os
from subprocess import check_call, CalledProcessError, STDOUT

from ep_testing.exceptions import EPTestingException
from ep_testing.tests.base import BaseTest


class TestVersionInfoInDocumentation(BaseTest):

    def name(self):
        return 'Verify contents in a PDF'

    def run(self, install_root: str, verbose: bool, kwargs: dict):
        if 'pdf_file' not in kwargs:
            raise EPTestingException('Bad call to %s -- must pass pdf_file in kwargs' % self.__class__.__name__)
        if 'version_string' not in kwargs:
            raise EPTestingException('Bad call to %s -- must pass version_string in kwargs' % self.__class__.__name__)
        pdf_file = kwargs['pdf_file']
        version_string = kwargs['version_string']
        print('* Running test class "%s" on file "%s"... ' % (self.__class__.__name__, pdf_file), end='')
        documentation_dir = os.path.join(install_root, 'Documentation')
        saved_dir = os.getcwd()
        os.chdir(documentation_dir)
        original_pdf_path = os.path.join(documentation_dir, pdf_file)
        target_pdf_path = os.path.join(documentation_dir, 'FirstPage_%s' % pdf_file)
        dev_null = open(os.devnull, 'w')
        try:
            check_call(
                ['pdftk', original_pdf_path, 'cat', '1', 'output', target_pdf_path], stdout=dev_null, stderr=STDOUT
            )
            print(' [PAGE1_EXTRACTED] ', end='')
        except CalledProcessError:
            raise EPTestingException('PdfTk Page 1 extraction failed!')
        target_txt_path = target_pdf_path + '.txt'
        try:
            check_call(['pdftotext', target_pdf_path, target_txt_path], stdout=dev_null, stderr=STDOUT)
            print(' [PAGE1_CONVERTED] ', end='')
        except CalledProcessError:
            raise EPTestingException('PdfToText Page 1 conversion failed!')
        with open(target_txt_path) as f:
            contents = f.read()
            if version_string in contents:
                print(' [FOUND VERSION STRING, DONE]!')
            else:
                raise EPTestingException(
                    'Did not find matching version string in PDF front page, page contents = \n%s' % contents
                )
        os.chdir(saved_dir)
