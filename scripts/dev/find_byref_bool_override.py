#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
In a lot of modules, a boolean flag such as `ErrorsFound` is passed by
reference constantly to several functions, and a Fatal will be issued at the
end if this boolean turns out to be `true`. Each single function that takes
this boolean should be responsible to turn it to 'true' as needed, but
shouldn't be forcing it to false, because it risks erasing previous errors.

Eg:
    bool ErrorsFound = false;
    functionA(&ErrorsFound);
    functionB(&ErrorsFound);

if functionB forces ErrorsFound to 'false' at the beginning and does pass, it
would override the output of functionA which may have found errors!

cf: https://github.com/NREL/EnergyPlus/issues/7147
Written in Winter 2019.
"""

__author__ = "Julien Marrec, EffiBEM"
__email__ = "julien@effibem.com"

import os
import re
import json
import glob as gb
import warnings

SRC_DIR = '../../src/EnergyPlus'
INCLUDE_WARNINGS = True
IS_CI =False


EXPECT_MISSING_HEADER = ['main.cc', 'test_ep_as_library.cc']

###############################################################################
#                              F U N C T I O N S                              #
###############################################################################


def infer_header_from_source(source_file):
    """
    Guess the header file that matches a source_file.
    Throws if doesn't exist
    """

    header_file = source_file.replace('.in.cc', '.hh').replace('.cc', '.hh')
    if not os.path.isfile(header_file):
        raise ValueError("Cannot find header file: {}".format(header_file))

    return header_file


def format_found_function(found_function, one_line=False):
    """
    Helper to display a dict entry from `parse_function_signatures_in_header`
    """
    if one_line:
        args = " ".join([line.strip() for line
                         in found_function['args'].splitlines()])
    else:
        args = found_function['args']

    return ("{} {}({}){}".format(found_function['return_type'],
                                 found_function['function_name'],
                                 args,
                                 found_function['post_qualifiers']))


def parse_function_signatures_in_header(header_file):
    """
    Opens the header file, and look for function signatures,
    returning only the ones that do include a bool passed by reference

    Args:
    -----
    * header_file (str): path to the header file.

    Returns:
    --------
    * found_functions (list of dict): each entry of the list is a dict that
    has the following keys:
        ['return_type', 'function_name', 'args', 'post_qualifiers']

    """

    signature_pattern = (r'^(?:\t+| )+(?:static|virtual)?\s*'
                         r'(?P<return_type>[^\s]+)\s+\b(?P<function_name>\w+)'
                         r'\((?P<args>.*?)\)\s*'
                         r'(?P<post_qualifiers>.*?)(?:override)?\s*;')
    signature_re = re.compile(signature_pattern, re.MULTILINE | re.DOTALL)

    re_bool = re.compile(r'bool(?:& | &)(\w+)')

    # Relative path, for cleaner reporting
    rel_file = os.path.relpath(header_file, start=SRC_DIR)

    try:
        with open(header_file, 'r') as f:
            content = f.read()
    except UnicodeDecodeError:
        if INCLUDE_WARNINGS:
            msg = ("Cannot decode {} as UTF-8, falling back to "
                   "latin-1".format(rel_file))
            if IS_CI:
                ci_msg = {'tool': 'find_byref_bool_overide',
                          'filename': rel_file,
                          'messagetype': 'warning',
                          'message': msg
                          }
                print(json.dumps(ci_msg))
            else:
                warnings.warn(msg)
        with open(header_file, 'r', encoding='latin-1') as f:
            content = f.read()

    found_functions = [m.groupdict() for m in signature_re.finditer(content)
                       if re_bool.search(m.groupdict()['args'])]

    return found_functions


def lookup_errors_in_source_file(source_file, found_functions):
    """
    Looks up the function bodies corresponding to each function
    in found_functions, and checks if a passed-by-reference bool is forced to
    false

    Args:
    -----
    * source_file (str): path to the .cc file
    * found_functions (list of dict): see `parse_function_signatures_in_header`

    Returns:
    --------
    * errors (list of dict): one entry per error, with the following keys:
        ['file', 'function', 'line_num', 'line']

    """

    # Relative path, for cleaner reporting
    rel_file = os.path.relpath(source_file, start=SRC_DIR)

    try:
        with open(source_file, 'r') as f:
            content = f.read()
    except UnicodeDecodeError:
        if INCLUDE_WARNINGS:
            msg = ("Cannot decode {} as UTF-8, falling back to "
                   "latin-1".format(rel_file))
            if IS_CI:
                ci_msg = {'tool': 'find_byref_bool_overide',
                          'filename': rel_file,
                          'messagetype': 'warning',
                          'message': msg
                          }
                print(json.dumps(ci_msg))
            else:
                warnings.warn(msg)

        with open(source_file, 'r', encoding='latin-1') as f:
            content = f.read()

    lines = content.splitlines()

    errors = []

    # We look for the opening of the function in question
    cc_pat = r'{r}\s+{n}\s*\((?P<args>.*?)\)\s*{{'

    for found_function in found_functions:

        signature_pattern = cc_pat.format(r=found_function['return_type'],
                                          n=found_function['function_name'])
        re_signature = re.compile(signature_pattern, re.MULTILINE | re.DOTALL)
        m = re_signature.search(content)
        if not m:
            d = format_found_function(found_function)
            if INCLUDE_WARNINGS:
                msg = ("In file {f}, cannot find function {n}:\n"
                       "{d}".format(f=rel_file,
                                    n=found_function['function_name'],
                                    d=d))
                if IS_CI:
                    ci_msg = {'tool': 'find_byref_bool_overide',
                              'filename': rel_file,
                              'messagetype': 'warning',
                              'message': msg
                              }
                    print(json.dumps(ci_msg))
                else:
                    warnings.warn(msg)

                # Skip iteration
                continue

        re_bool = re.compile(r'bool(?:& | &)(\w+)')
        bools = re_bool.findall(m.groupdict()['args'])

        fbody_start_line_num = content[:m.end()].count('\n')
        line_num = fbody_start_line_num
        n_braces = lines[line_num].count('{') - lines[line_num].count('}')
        while n_braces > 0:
            line_num += 1
            line = lines[line_num].strip()
            n_braces += line.count('{') - line.count('}')

            for b in bools:
                pat = r'{b}\s*=\s*false;'.format(b=b)
                re_this_bool = re.compile(pat)
                if re_this_bool.search(line):
                    errors.append({'file': rel_file,
                                   'function': found_function['function_name'],
                                   'line_num': line_num,
                                   'line': line})
                    fname = found_function['function_name']
                    if INCLUDE_WARNINGS and not IS_CI:
                        print("Error in {f}:{function}, on line {n}: "
                              "`{line}`".format(f=rel_file,
                                                function=fname,
                                                n=line_num,
                                                line=line))

        return errors


###############################################################################
#                                   M A I N                                   #
###############################################################################

def get_all_errors(source_files):
    """
    Run everything, given a list of source_files

    Args:
    -----
    * source_files (list of str): list of paths

    Returns:
    --------

    all_errors (list of dict): one entry per error, with the following keys:
        ['file', 'function', 'line_num', 'line']
    """

    all_errors = []

    for source_file in source_files:
        rel_file = os.path.relpath(source_file, start=SRC_DIR)
        try:
            header_file = infer_header_from_source(source_file)
        except ValueError:
            if (rel_file not in EXPECT_MISSING_HEADER) and INCLUDE_WARNINGS:
                msg = ("Cannot find header file for "
                       "{}".format(rel_file))
                if IS_CI:
                    ci_msg = {'tool': 'find_byref_bool_overide',
                              'filename': rel_file,
                              'messagetype': 'warning',
                              'message': msg
                              }
                    print(json.dumps(ci_msg))
                else:
                    warnings.warn(msg)

        found_functions = parse_function_signatures_in_header(header_file)
        if not found_functions:
            # print("No problem for {}".format(rel_file))
            pass
        else:
            errors = lookup_errors_in_source_file(source_file, found_functions)
            if errors:
                all_errors += errors

    return all_errors


def output_errors_for_decent_ci(all_errors):
    for error in all_errors:

        msg = ("Boolean flag reset to false in {file}:{function}(), "
               "on line {line_num}:\n"
               "{line}".format(file=error['file'],
                               function=error['function'],
                               line_num=error['line_num'],
                               line=error['line'].strip()))
        ci_msg = {'tool': 'find_byref_bool_overide',
                  'filename': error['file'],
                  'line': error['line_num'],
                  'messagetype': 'error',
                  'message': msg
                  }
        print(json.dumps(ci_msg))


if __name__ == '__main__':
    # Glob all .cc files
    source_files = gb.glob(os.path.join(SRC_DIR, '*.cc'))
    all_errors = get_all_errors(source_files)
    output_errors_for_decent_ci(all_errors)
