#!/usr/bin/env python3

r"""
Script/module for versioning related activities.

Versions are stored in Git tags.
Valid "versioning" tags are prefixed with 'v' and MUST be PEP 440 compliant.

See also: https://peps.python.org/pep-0440
"""

import argparse
import subprocess
try: from packaging.version import Version
except ModuleNotFoundError:
    raise ModuleNotFoundError('Install via `python3 -m pip install packaging`.')


class VersionManager:
    def __init__(self, repository):
        self.repository = repository

    def get(self) -> Version:
        version_str = subprocess.check_output(
            ['git', 'describe', '--tags', '--abbr=0', '--match=v*'],
            cwd=self.repository,
            text=True,
        )
        return Version(version_str)

    def set(self, version) -> Version:
        version_ = (
            version 
            if isinstance(version, Version) else
            Version(version)
        )
        subprocess.run(
            ['git', 'tag', f'v{version_!s}'], 
            cwd=self.repository,
            check=True, 
        )
        return version_


def main(args=None):
    parser = argparse.ArgumentParser(
        description="Manage versions of a Git repository, PEP 440 compliant.",
    )
    parser.add_argument(
        '-c', '--cwd', 
        default='.', 
        help="Path to the Git repository.",
    )

    subparsers = parser.add_subparsers(dest='command', required=True)
    # command: get
    parser_get = subparsers.add_parser(
        'get', 
        help="Get the version attached to current commit.",
    )
    # command: set
    parser_set = subparsers.add_parser(
        'set', 
        help="Tag current commit to a new version.",
    )
    parser_set.add_argument(
        'version', 
        help="New version to set.",
    )
    
    args = parser.parse_args(args=args)
    vm = VersionManager(args.cwd)
    if args.command == 'get':
        print(str(vm.get()))
    if args.command == 'set':
        print(str(vm.set(args.version)))


if __name__ == '__main__':
    main()


__all__ = [
    'VersionManager',
    'main',
]