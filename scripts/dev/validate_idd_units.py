#!/usr/bin/env python

# Usage: Pass in the path to an idd file (configured or raw) as the only command line argument
#        The program will scan unit specifications in the idd header and then validate all field \units tags

# Eventually, this should be brought into the custom_check tests
# Just need to add the CMake logic to run it, and then have this report out warnings properly

import sys


# There are some missing units in a large number of fields.
# I don't really want to add an ignore list, but I don't want to fix them all at the moment, either.
# Thus, here is an ignore list.  To fix these up, you could add these to the 'not-translated units' section.
ignore_list = ["hh:mm", "kgWater/kgDryAir"]

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

idd_file = sys.argv[1]
idd_lines = open(idd_file).readlines()

original_units = []
reading_mode = ReadingMode.FindTranslatedUnits
line_num = 0
problem_unit_lines = []
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
        if "\units " in line:
            tokens = line.split(" ")
            real_tokens = [t for t in tokens if t]
            if not len(real_tokens) == 2 and warn_for_bad_unit_tokens:
                problem_unit_lines.append(Problem(line_num, "Unexpected number of unit specifications"))
            elif real_tokens[1] not in original_units and real_tokens[1] not in ignore_list:
                problem_unit_lines.append(Problem(line_num, "Unexpected unit type found: " + real_tokens[1]))

for problem in problem_unit_lines:
    print(str(problem))
