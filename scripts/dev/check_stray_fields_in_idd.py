#!/usr/bin/env python

import re

IDD_PATH = '../../idd/Energy+.idd.in'


def check_for_stray_fields(idd_path):
    """
    Verifies that there aren't any stray fields in IDD that could cause
    parsing problems

    Args:
    -----
    idd_path (str): path to the idd file to check

    Returns:
    --------
    offending_lines (list of tuple): list of tuples = (line number, field)
    that are offending
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
                offending_lines.append((i, field))

    return offending_lines


if __name__ == '__main__':

    offending_lines = check_for_stray_fields(idd_path=IDD_PATH)
    if offending_lines:
        msg = ""
        for (i, field) in offending_lines:
            msg += ("Line {}: \\{}\n".format(i+1, field))
        raise ValueError("There are {} stray fields:\n"
                         "{}".format(len(offending_lines), msg))
    else:
        print("OK.")
