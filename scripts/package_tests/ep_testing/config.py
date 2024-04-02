
class OS:
    Windows = 1
    Linux = 2
    Mac = 3


CONFIGURATIONS = {
    'ubuntu2004': {
        'os': OS.Linux, 'bitness': 'x64', 'asset_pattern': 'Linux-Ubuntu20.04-x86_64.tar.gz', 'os_version': '20.04'
    },
    'ubuntu2204': {
        'os': OS.Linux, 'bitness': 'x64', 'asset_pattern': 'Linux-Ubuntu22.04-x86_64.tar.gz', 'os_version': '22.04'
    },
    'mac11': {
        'os': OS.Mac, 'bitness': 'x64', 'asset_pattern': 'Darwin-macOS11.6-x86_64.tar.gz', 'os_version': '11.6'
    },
    'mac12': {
        'os': OS.Mac, 'bitness': 'x64', 'asset_pattern': 'Darwin-macOS12.1-x86_64.tar.gz', 'os_version': '12.1'
    },
    'mac12-arm64': {
        'os': OS.Mac, 'bitness': 'arm64', 'asset_pattern': 'Darwin-macOS12.1-arm64.tar.gz', 'os_version': '12'
    },
    'mac13-arm64': {
        'os': OS.Mac, 'bitness': 'arm64', 'asset_pattern': 'Darwin-macOS13-arm64.tar.gz', 'os_version': '13'
    },
    'win32': {
        'os': OS.Windows, 'bitness': 'x32', 'asset_pattern': 'Windows-i386.zip', 'os_version': '10'
    },
    'win64': {
        'os': OS.Windows, 'bitness': 'x64', 'asset_pattern': 'Windows-x86_64.zip', 'os_version': '10'
    },
    'win64-2022server': {
        'os': OS.Windows, 'bitness': 'x64', 'asset_pattern': 'Windows-x86_64.zip', 'os_version': '2022'
    },
}


class TestConfiguration:

    def __init__(self, run_config_key: str, this_version: str, last_version: str, last_tag: str, msvc_version=None):

        # invalid keys are protected in the command's finalize_options method
        this_config = CONFIGURATIONS[run_config_key]
        self.os_version = this_config['os_version']
        self.os = this_config['os']
        self.msvc_version = None
        if msvc_version is not None:
            self.msvc_version = msvc_version
        elif self.os == OS.Windows and self.os_version == '2022':
            self.msvc_version = 17
        elif self.os == OS.Windows:
            self.msvc_version = 16
        self.asset_pattern = this_config['asset_pattern']
        self.bitness = this_config['bitness']

        self.this_version = this_version
        # self.tag_this_version = 'v24.1.0-RC2'
        self.last_version = last_version
        self.tag_last_version = last_tag
