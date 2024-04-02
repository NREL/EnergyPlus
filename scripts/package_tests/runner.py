import argparse
from pathlib import Path
from re import search
from sys import exit, path

this_file_path = Path(__file__).resolve()
package_test_root_dir = this_file_path.parent
path.insert(0, str(package_test_root_dir))

from ep_testing.config import CONFIGURATIONS, TestConfiguration
from ep_testing.tester import Tester


def run(config: str, this: str, last: str, last_tag: str, package_dir: Path, msvc: str, verbose: bool) -> int:
    c = TestConfiguration(config, this, last, last_tag, msvc)
    print(f'Attempting to test local EnergyPlus package extracted at: {package_dir}')
    t = Tester(c, package_dir, verbose)
    t.run()
    return 0  # TODO: be better about the exit code


def get_version_info() -> tuple:
    repo_root = package_test_root_dir.parent.parent
    version_file = repo_root / 'cmake' / 'Version.cmake'
    contents = version_file.read_text()
    major = search(r'CMAKE_VERSION_MAJOR [0-9]+', contents).group(0).split(' ')[1]
    minor = search(r'CMAKE_VERSION_MINOR [0-9]+', contents).group(0).split(' ')[1]
    last_major = search(r'PREV_RELEASE_MAJOR [0-9]+', contents).group(0).split(' ')[1]
    last_minor = search(r'PREV_RELEASE_MINOR [0-9]+', contents).group(0).split(' ')[1]
    last_patch = search(r'PREV_RELEASE_PATCH [0-9]+', contents).group(0).split(' ')[1]
    this_version = f"{major}.{minor}"
    last_version = f"{last_major}.{last_minor}"
    last_tag = f"v{last_major}.{last_minor}.{last_patch}"
    return this_version, last_version, last_tag


def main() -> int:
    parser = argparse.ArgumentParser(description="Run Package Tests on EnergyPlus")
    arg = parser.add_argument  # readability
    arg('config', choices=CONFIGURATIONS.keys(), help="Specify the run configuration")
    arg('package_dir', help="Path to the extracted EnergyPlus package directory")
    arg('--msvc', type=str, default='2022', help="For MSVC builds, this is the VS 'year', like '2022'")
    arg('--verbose', action='store_true', help="If specified, get verbose output")
    args = parser.parse_args()
    this, last, last_tag = get_version_info()
    r = run(args.config, this, last, last_tag, Path(args.package_dir), args.msvc, args.verbose)
    return r


if __name__ == "__main__":
    exit(main())
