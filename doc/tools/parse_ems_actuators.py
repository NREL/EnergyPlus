"""
This modules parses the C++ source Code for EnergyPlus and returns
some info about the EMS Actuators and Internal Variables available
in the program. It will output a CSV file and a summary to stdout.

Args:
-----
You can do `python parse_ems_actuators --help` to see extra arguments you can
customize such as the path to the `EnergyPlus/src/EnergyPlus` directory or the
name and path of the output CSV file, and set verbosity. If you don't supply
any parameters it will default to `../../src/EnergyPlus`,
`./SetupEMSActuatorAndInternalVariableCalls.csv` and non-verbose.


Known limitations:
-------------------
This is a pretty dumb script that only parses lines that
match a given pattern. There is no semantic analysis, so if a variable is used
in the call to `SetupEMSActuator` instead of a string literal, this script
doesn't know what's inside this variable (eg it willreport
'cCMO_BBRadiator_Electric' for 'Name' when really this variable stores the
string "ZoneHVAC:Baseboard:Convective:Electric").
The only thing of that sort that it tries to do is to parse the list
of 'cCurrentModuleObject'.


Needed modules:
---------------
The only module that isn't part of the standard library is pandas.
You can install it via `pip install pandas` (or `conda install pandas` if you
use conda).
"""

__author__ = "Julien Marrec"
__organization__ = 'EffiBEM, EURL, www.effibem.com'
__copyright__ = "Copyright 2018, EffiBEM EURL"
__email__ = "julien@effibem.com"
__license__ = "MIT"
__version__ = "0.1.0"
__status__ = "Production"


import os
import re
import io
import glob as gb
import pandas as pd


def parse_ems_var(source_dir, verbose=False):
    """
    Parses all *.cc files in the supplied source_dir
    and looks for 'SetupEMSVariable' and 'SetupEMSInternalVariable' statements.
    Search for cc files is non-recursive

    Args:
    -----
        * source_dir (str): path to the local `EnergyPlus/src/EnergyPlus`
            directory

    Returns:
    --------

        * df (pd.DataFrame): a dataframe of the parsed results

    Needs:
    -------
        import os, re, io
        import glob as gb
        import pandas as pd
    """

    # Regex
    pat_cModule_Objects = re.compile(r'\s*static Array1D_string const '
                                     r'cModuleObjects\(\s*([0-9]{1,2}),'
                                     r'\s*{\s*(.*?)}\s*\);')

    cCurrentModuleObject = ''
    cModuleObjects = ''

    ems_arr_dict = []

    # For each cc file in source_dir
    for file_path in gb.iglob(os.path.join(source_dir, '*.cc')):

        file_name = os.path.basename(file_path)

        # Skip this file
        if file_name == 'EMSManager.cc':
            continue

        if verbose:
            print("\nProcessing File: {}".format(file_name))

        # Open a read lines
        with io.open(file_path, 'r', encoding='latin-1') as f:
            lines = f.readlines()

        # Loop on each line
        for i, line in enumerate(lines):

            # create a copy of line to work on
            working_line = line

            # trim off comments first
            if "//" in working_line:
                working_line = working_line[0:working_line.index("//")]

            m = pat_cModule_Objects.match(working_line)
            if m:
                n = int(m.group(1))
                cModuleObjects = [x.replace('"', '').strip()
                                  for x in m.group(2).split(',')]
                if len(cModuleObjects) != n:
                    print('\nPROBLEM')
                    print(n, cModuleObjects)

            if (working_line.strip()[:len('cCurrentModuleObject')]
                    == 'cCurrentModuleObject'):
                cCurrentModuleObject = (working_line.split("=")[1]
                                        .replace('"', '')
                                        .replace(';', '').strip())
                if 'cModuleObjects' not in cCurrentModuleObject:
                    cModuleObjects = [cCurrentModuleObject]

            if 'SetupEMSActuator' in working_line:
                # print("{}: {}".format(cModuleObjects, working_line))
                c_l = [x.replace('"', '').strip() for x in
                       working_line.replace('SetupEMSActuator(', '')
                                   .replace(');', '').split(',')]

                if len(c_l) != 6:
                    print(working_line)

                (cComponentTypeName,
                 cUniqueIDName,
                 cControlTypeName,
                 cUnits,
                 lEMSActuated,
                 irlValue) = c_l

                for cModuleObject in cModuleObjects:

                    d = {'Type': 'Actuator',
                         'Name': cModuleObject,
                         'FileName': file_name,
                         'Line': i,
                         'cComponentTypeName': cComponentTypeName,
                         'cUniqueIDName': cUniqueIDName,
                         'cControlTypeName / cDataTypeName': cControlTypeName,
                         'cUnits': cUnits,
                         'lEMSActuated': lEMSActuated,
                         'Value': irlValue,
                         }

                    ems_arr_dict.append(d)

            if 'SetupEMSInternalVariable' in working_line:
                c_l = [x.replace('"', '').strip() for x in
                       working_line.replace('SetupEMSInternalVariable(', '')
                                   .replace(');', '').split(',')]

                if len(c_l) != 4:
                    print(working_line)

                (cDataTypeName,
                 cUniqueIDName,
                 cUnits,
                 irValue) = c_l

                for cModuleObject in cModuleObjects:

                    d = {'Type': 'InternalVariable',
                         'Name': cModuleObject,
                         'FileName': file_name,
                         'Line': i,
                         'cControlTypeName / cDataTypeName': cDataTypeName,
                         'cUniqueIDName': cUniqueIDName,
                         'cUnits': cUnits,
                         'Value': irValue,
                         }

                    ems_arr_dict.append(d)

    # Create dataframe from list of dict
    df = pd.DataFrame.from_dict(ems_arr_dict)

    col_order = ['cComponentTypeName', 'cControlTypeName / cDataTypeName',
                 'cUniqueIDName', 'cUnits', 'lEMSActuated', 'Value',
                 'FileName', 'Line']

    df = df.set_index(['Name', 'Type'])[col_order]

    df.sort_index(axis=0, level=0, sort_remaining=True, inplace=True)

    return df


def summary(df):
    """
    Create high-level summary info on EMS variables found

    Args:
    -----
        * df (pd.DataFrame): the parse dataframe

    Returns:
    --------
        * out (str): a formatted string ready for output in a file
        or the stdout


    """

    out_str = ''
    for t, gp in df.groupby(level='Type'):
        n_vars = len(gp)
        n_files = len(gp.FileName.unique())
        n_unique_objects = len(gp.index.unique())
        out_str += ('\nWe found {v} unique EMS {t}s defined, '
                    'in {f} files, affecting {o} unique '
                    'objects'.format(v=n_vars, t=t, f=n_files,
                                     o=n_unique_objects))
    return out_str


# If launched from the command line
if __name__ == "__main__":

    import argparse
    parser = argparse.ArgumentParser()

    parser.add_argument("-s", "--source_dir",
                        help=('Local path to the EnergyPlus/src/EnergyPlus/ '
                              'directory'))

    parser.add_argument("-o", "--output", help="Local path Output CSV")

    parser.add_argument("-v", "--verbose", help="increase output verbosity",
                        action="store_true")

    args = parser.parse_args()

    print()

    if args.source_dir:
        source_dir = args.source_dir
    else:
        source_dir = '../../src/EnergyPlus/'
        source_dir = os.path.abspath(source_dir)
        print("Defaulting source_dir to {}".format(source_dir))

    if args.output:
        output_file = args.output
    else:
        output_file = 'SetupEMSActuatorAndInternalVariableCalls.csv'
        print("Defaulting output csv file to "
              "{}".format(os.path.abspath(output_file)))

    df = parse_ems_var(source_dir, verbose=args.verbose)

    # Print output summary
    out_str = summary(df)
    print(out_str)

    # Output csv file
    df.to_csv(output_file)
    print("Saving to {}".format(os.path.abspath(output_file)))
