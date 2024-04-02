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
