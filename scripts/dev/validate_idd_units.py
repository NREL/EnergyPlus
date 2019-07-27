#!/usr/bin/env python

# Usage: No arguments necessary, this will find the local unconfigured (raw) IDD file and process it
#        The program will scan unit specifications in the idd header and then validate all field \units tags

import codecs
import json
import os


# There are some missing units in a large number of fields.
# I don't really want to add an ignore list, but I don't want to fix them all at the moment, either.
# Thus, here is an ignore list.  To fix these up, you could add these to the 'not-translated units' section.
ignore_list = []  # this is empty now with eht units added to the IDD itself

# There are also some lines that include more than one unit specification
# I'd like to include the warning for those, but I won't at the moment, so for now this warning is disabled.
# Later on, enable this to ensure unit strings are properly formatted
warn_for_bad_unit_tokens = False


class ReadingMode:
    FindTranslatedUnits = 1
    FindNonTranslatedUnits = 2
    ScanFieldUnits = 3


class Problem:
    def __init__(self, line_num, detail):
        self.line_num = line_num
        self.detail = detail

    def __str__(self):
        return "Line # %s: %s" % (self.line_num, self.detail)


current_script_dir = os.path.dirname(os.path.realpath(__file__))
idd_file = os.path.join(current_script_dir, '..', '..', 'idd', 'Energy+.idd.in')
idd_lines = codecs.open(idd_file, encoding='utf-8', errors='ignore').readlines()

original_units = []
reading_mode = ReadingMode.FindTranslatedUnits
line_num = 0
for line in idd_lines:
    line = line.strip()
    line_num += 1
    if reading_mode == ReadingMode.FindTranslatedUnits:
        if line.startswith("!      ") and "=>   " in line:
            tokens = line.split(" ")
            real_tokens = [t for t in tokens if t]
            original_units.append(real_tokens[1])
        elif "! Units fields that are not translated" in line:
            reading_mode = ReadingMode.FindNonTranslatedUnits
    elif reading_mode == ReadingMode.FindNonTranslatedUnits:
        if line.startswith("!      "):
            tokens = line.split(" ")
            real_tokens = [t for t in tokens if t]
            original_units.append(real_tokens[1])
        else:
            reading_mode = ReadingMode.ScanFieldUnits
    elif reading_mode == ReadingMode.ScanFieldUnits:
        if '\\units ' in line:
            tokens = line.split(" ")
            real_tokens = [t for t in tokens if t]
            if not len(real_tokens) == 2 and warn_for_bad_unit_tokens:
                print(json.dumps({
                    'tool': 'validate_idd_units.py',
                    'filename': '/idd/Energy+.idd.in',
                    'file': '/idd/Energy+.idd.in',
                    'line': line_num,
                    'messagetype': 'warning',
                    'message': "Unexpected number of unit specifications"
                }))
            elif real_tokens[1] not in original_units and real_tokens[1] not in ignore_list:
                print(json.dumps({
                    'tool': 'validate_idd_units.py',
                    'filename': '/idd/Energy+.idd.in',
                    'file': '/idd/Energy+.idd.in',
                    'line': line_num,
                    'messagetype': 'warning',
                    'message': "Unexpected unit type found: " + real_tokens[1]
                }))
