#!/usr/bin/env python

import sys


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

# I don't really want to add an ignore list, but so be it:
ignore_list = ["hh:mm", "kgWater/kgDryAir"]

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
            if not len(real_tokens) == 2:
                problem_unit_lines.append(Problem(line_num, "Unexpected number of unit specifications"))
                continue
            if real_tokens[1] not in original_units and real_tokens[1] not in ignore_list:
                problem_unit_lines.append(Problem(line_num, "Unexpected unit type found: " + real_tokens[1]))

for problem in problem_unit_lines:
    print(str(problem))
