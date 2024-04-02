# EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University
# of Illinois, The Regents of the University of California, through Lawrence
# Berkeley National Laboratory (subject to receipt of any required approvals
# from the U.S. Dept. of Energy), Oak Ridge National Laboratory, managed by UT-
# Battelle, Alliance for Sustainable Energy, LLC, and other contributors. All
# rights reserved.
#
# NOTICE: This Software was developed under funding from the U.S. Department of
# Energy and the U.S. Government consequently retains certain rights. As such,
# the U.S. Government has been granted for itself and others acting on its
# behalf a paid-up, nonexclusive, irrevocable, worldwide license in the
# Software to reproduce, distribute copies to the public, prepare derivative
# works, and perform publicly and display publicly, and to permit others to do
# so.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# (1) Redistributions of source code must retain the above copyright notice,
#     this list of conditions and the following disclaimer.
#
# (2) Redistributions in binary form must reproduce the above copyright notice,
#     this list of conditions and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#
# (3) Neither the name of the University of California, Lawrence Berkeley
#     National Laboratory, the University of Illinois, U.S. Dept. of Energy nor
#     the names of its contributors may be used to endorse or promote products
#     derived from this software without specific prior written permission.
#
# (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in
#     stand-alone form without changes from the version obtained under this
#     License, or (ii) Licensee makes a reference solely to the software
#     portion of its product, Licensee must refer to the software as
#     "EnergyPlus version X" software, where "X" is the version number Licensee
#     obtained under this License and may not use a different name for the
#     software. Except as specifically required in this Section (4), Licensee
#     shall not use in a company name, a product name, in advertising,
#     publicity, or other promotional activities any name, trade name,
#     trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or
#     confusingly similar designation, without the U.S. Department of Energy's
#     prior written consent.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.


from json import loads
from distutils import log
from typing import List, Union
import os
from urllib import request
import shutil
from subprocess import check_call, CalledProcessError, STDOUT
from typing import Tuple
import urllib.request

from ep_testing.exceptions import EPTestingException
from ep_testing.config import TestConfiguration, OS


class Downloader:
    Release_url = 'https://api.github.com/repos/NREL/EnergyPlus/releases'
    User_url = 'https://api.github.com/user'

    def __init__(self, config: TestConfiguration, download_dir: str, use_local: str = '', announce: callable = None):
        self.release_tag = config.tag_this_version
        self.download_dir = download_dir
        self.announce = announce  # hijacking this instance method is mildly dangerous, like 1/5 danger stars
        github_token = os.environ.get('GITHUB_TOKEN', None)
        if github_token is None:
            raise EPTestingException('GITHUB_TOKEN not found in environment, cannot continue')
        self.auth_header = {'Authorization': 'token %s' % github_token}
        r = request.Request(self.User_url, headers=self.auth_header)
        response = request.urlopen(r)
        data = response.read()
        user_response = data.decode('utf-8')
        if user_response.status == 403:
            if 'rate limit' in user_response.json()['message']:
                raise EPTestingException('Rate limit somehow exceeded, weird!')
            raise EPTestingException('Permission issue when calling Github API')
        elif user_response.status != 200:
            raise EPTestingException('Invalid call to Github API -- check GITHUB_TOKEN validity')
        self._my_print('Executing download operations as Github user: ' + user_response.json()['login'])
        extract_dir_name = 'ep_package'
        self.extract_path = os.path.join(self.download_dir, extract_dir_name)
        # need to adapt this to the new filename structure when we get there
        self.asset_pattern = config.asset_pattern
        target_file_name, self.extract_command = self._get_extract_vars(config)
        self.download_path = os.path.join(self.download_dir, target_file_name)
        if use_local:
            shutil.copy(use_local, self.download_dir)
        else:
            releases = self._get_all_packages()
            matching_release = self._find_matching_release(releases)
            asset = self._find_matching_asset_for_release(matching_release)
            if asset is None:
                raise EPTestingException('Could not find asset to download, has CI finished it yet?')
            self._download_asset(asset)
        self.extracted_install_path = self._extract_asset()
        # if config.os == OS.Mac:  # TODO: JUST TEMPORARY!!
        #     check_call(['codesign', '--remove-signature', self.extracted_install_path + "/Python"])

    def _get_extract_vars(self, config) -> Tuple[str, str]:
        target_file_name = ''
        extract_command = ''
        if config.os == OS.Linux:
            target_file_name = 'ep.tar.gz'
            # tar -xzf ep.tar.gz -C ep_package
            extract_command = ['tar', '-xzf', target_file_name, '-C', self.extract_path]
        elif config.os == OS.Mac:
            target_file_name = 'ep.tar.gz'
            # tar -xzf ep.tar.gz -C ep_package
            extract_command = ['tar', '-xzf', target_file_name, '-C', self.extract_path]
        elif config.os == OS.Windows:
            target_file_name = 'ep.zip'
            # 7z x ep.zip -oep_package
            extract_command = ['7z.exe', 'x', target_file_name, '-o' + self.extract_path]
        return target_file_name, extract_command

    def _get_all_packages(self) -> List[dict]:
        """Get all releases from this url"""
        try:
            releases = []
            next_page_url = self.Release_url
            while next_page_url:
                r = request.Request(next_page_url, headers=self.auth_header)
                response = request.urlopen(r)
                data = response.read()
                decoded = data.decode('utf-8')
                these_releases = loads(decoded)
                self._my_print('Got release response, number on this query = %i' % len(these_releases), log.DEBUG)
                releases.extend(these_releases)
                raw_next_page_url = response.headers.get('link', None)
                if raw_next_page_url is None:
                    break
                if 'rel="next"' not in raw_next_page_url:
                    break
                next_page_url = self._sanitize_next_page_url(raw_next_page_url)
            self._my_print('Got total release list: Total number of releases = %i' % len(releases), log.DEBUG)
        except Exception as e:
            raise EPTestingException('Could not download list of releases' + str(e))
        return releases

    def _find_matching_release(self, releases: List[dict]) -> dict:
        full_list_of_release_names = []
        for release in releases:
            full_list_of_release_names.append(release['tag_name'])
            if release['tag_name'] == self.release_tag:
                self._my_print('Found release with tag_name = ' + self.release_tag)
                return release
        raise EPTestingException('Did not find matching tag, searching for %s, full list = [\n%s\n]' % (
            self.release_tag,
            '\n '.join(full_list_of_release_names)
        ))

    @staticmethod
    def _sanitize_next_page_url(next_page_url: str) -> Union[str, None]:
        if next_page_url[0] != '<':
            # only sanitize the next pagers with the braces
            return next_page_url
        links = next_page_url.split(',')
        for link in links:
            if 'rel=\"next\"' in link:
                braced_url = link.split(';')[0].strip()
                remove_first_brace = braced_url[1:]
                remove_last_brace = remove_first_brace[:-1]
                return remove_last_brace
        return None

    def _find_matching_asset_for_release(self, release: dict) -> dict:
        asset_url = release['assets_url']
        # making an assumption that I don't need to paginate these -- we won't have > 30 assets per release
        r = request.Request(asset_url, headers=self.auth_header)
        response = request.urlopen(r)
        data = response.read()
        decoded = data.decode('utf-8')
        assets = loads(decoded)
        full_list_of_asset_names = []
        for asset in assets:
            full_list_of_asset_names.append(asset['name'])
            if self.asset_pattern in asset['name']:
                self._my_print('Found asset with name "%s": "%s"' % (self.asset_pattern, asset['name']))
                return asset

    def _download_asset(self, asset: dict) -> None:
        try:
            _, headers = urllib.request.urlretrieve(asset['browser_download_url'], self.download_path)
            self._my_print('Asset downloaded to ' + self.download_path)
        except Exception as e:
            raise EPTestingException(
                'Could not download asset from %s; error: %s' % (asset['browser_download_url'], str(e))
            )

    def _extract_asset(self) -> str:
        """Attempts to extract the downloaded package, returns the path to the E+ install subdirectory"""
        saved_working_directory = os.getcwd()
        os.chdir(self.download_dir)
        if os.path.exists(self.extract_path):
            shutil.rmtree(self.extract_path)
        try:
            os.makedirs(self.extract_path)
        except Exception as e:
            raise EPTestingException('Could not create extraction path at %s; error: %s' % (self.extract_path, str(e)))
        try:
            self._my_print("Extracting asset...")
            dev_null = open(os.devnull, 'w')
            check_call(self.extract_command, stdout=dev_null, stderr=STDOUT)
            self._my_print(" ...Extraction Complete")
        except CalledProcessError as e:
            raise EPTestingException("Extraction failed with this error: " + str(e))
        # should result in a single new directory inside the extract path, like: /extract/path/EnergyPlus-V1-abc-Linux
        all_sub_folders = [f.path for f in os.scandir(self.extract_path) if f.is_dir()]
        if len(all_sub_folders) > 1:
            raise EPTestingException('Extracted EnergyPlus package has more than one directory, problem.')
        os.chdir(saved_working_directory)
        return all_sub_folders[0]

    def _my_print(self, message: str, level: object = log.INFO) -> None:
        if self.announce:
            self.announce(message, level)
        else:
            print(message)
