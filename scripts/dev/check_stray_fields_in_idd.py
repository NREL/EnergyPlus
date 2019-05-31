#!/usr/bin/env python

import json
import re
import os

IDD_PATH = os.path.abspath('idd/Energy+.idd.in')


def check_for_stray_fields(idd_path):
    """
    Verifies that there aren't any stray fields in IDD that could cause
    parsing problems

    Args:
    -----
    idd_path (str): path to the idd file to check

    Returns:
    --------
    offending_lines (list of dict): one entry per offending line,
    each entry is a dict that can be consumed by decent_ci
    """

    with open(idd_path, 'r') as f:
        lines = f.read().splitlines()

    re_field = re.compile(r'\s*\\(\w+)\s*$')
    exclude = ['autosizable',
               'autocalculatable',
               'retaincase']

    offending_lines = []
    for i, line in enumerate(lines):
        m = re_field.match(line)
        if m:
            field = m.groups()[0]
            if field not in exclude:
                offending_lines.append({'tool': 'check_stray_fields_in_idd',
                                        'filename': idd_path,
                                        'file': idd_path,
                                        'line': i + 1,
                                        'messagetype': 'error',
                                        'message': ('Stray field '
                                                    r'\{}'.format(field))
                                        })

    return offending_lines


if __name__ == '__main__':

    offending_lines = check_for_stray_fields(idd_path=IDD_PATH)
    for offending_line in offending_lines:
        print(json.dumps(offending_line))
