import os
import shutil
import sys
from typing import List

if len(sys.argv) != 3:
    print(f"Invalid arguments passed to {sys.argv[0]}")
    sys.exit(1)
base_idf_path = sys.argv[1]
reverse_dd_test_dir = sys.argv[2]

# find out ow many DD's there are based on the IDF
num_design_days = 0
with open(base_idf_path) as f:
    for line in f:
        if "SizingPeriod:DesignDay," in line:
            num_design_days += 1

# now start processing the reversed output file
base_dir = os.path.join(reverse_dd_test_dir, 'Regular')
base_csv = os.path.join(base_dir, 'eplusout.csv')
base_mtr = os.path.join(base_dir, 'eplusmtr.csv')
reversed_dir = os.path.join(reverse_dd_test_dir, 'Reversed')
reversed_csv = os.path.join(reversed_dir, 'eplusout.csv')
reversed_mtr = os.path.join(reversed_dir, 'eplusmtr.csv')
reversed_csv_backup = os.path.join(reversed_dir, 'eplusout_backup.csv')
reversed_mtr_backup = os.path.join(reversed_dir, 'eplusmtr_backup.csv')

# read all lines from csv
csv_contents = []
with open(reversed_csv) as f:
    for line in f:
        csv_contents.append(line)

meter_exists = os.path.exists(reversed_mtr)
mtr_contents = []
if meter_exists:
    with open(reversed_mtr) as f:
        for line in f:
            mtr_contents.append(line)

# now find out how many lines of data there are in each file and each environment
csv_data_rows = len(csv_contents) - 1
csv_data_rows_per_environment = int(csv_data_rows / num_design_days)
mtr_data_rows = 0
mtr_data_rows_per_environment = 0
if meter_exists:
    mtr_data_rows = len(mtr_contents) - 1
    mtr_data_rows_per_environment = int(mtr_data_rows / num_design_days)

# write the files back out in the appropriate order
shutil.copy(reversed_csv, reversed_csv_backup)
with open(reversed_csv, "w") as f:
    f.write("%s" % csv_contents[0])
    for row_num in range(csv_data_rows_per_environment + 1, 2 * csv_data_rows_per_environment + 1):
        f.write("%s" % csv_contents[row_num])
    for row_num in range(1, csv_data_rows_per_environment + 1):
        f.write("%s" % csv_contents[row_num])
    if num_design_days > 2:
        for row_num in range(2 * csv_data_rows_per_environment + 1, csv_data_rows + 1):
            f.write("%s" % csv_contents[row_num])
if meter_exists:
    shutil.copy(reversed_mtr, reversed_mtr_backup)
    with open(reversed_mtr, "w") as f:
        f.write("%s" % mtr_contents[0])
        for row_num in range(mtr_data_rows_per_environment + 1, 2 * mtr_data_rows_per_environment + 1):
            f.write("%s" % mtr_contents[row_num])
        for row_num in range(1, mtr_data_rows_per_environment + 1):
            f.write("%s" % mtr_contents[row_num])
        if num_design_days > 2:
            for row_num in range(2 * mtr_data_rows_per_environment + 1, mtr_data_rows + 1):
                f.write("%s" % mtr_contents[row_num])


def get_numeric_token_list(file_object) -> List[float]:
    values = []
    for li in file_object.readlines():
        for v in li.split(','):
            try:
                fl = float(v)
                values.append(fl)
            except ValueError:
                continue
    return values


def lists_of_values_match(base: List[float], mod: List[float], tolerance: float = 1.0) -> bool:
    if len(base) != len(mod):
        return False
    for i, base_value in enumerate(base):
        mod_value = mod[i]
        if base_value < tolerance:
            if abs(base_value - mod_value) > tolerance:
                return False
        elif (abs(base_value - mod_value) / base_value) > tolerance:
            return False
    return True


def files_match(base_file_path: str, mod_file_path: str) -> bool:
    with open(base_file_path) as f_base:
        with open(mod_file_path) as f_reversed:
            base_values = get_numeric_token_list(f_base)
            reversed_values = get_numeric_token_list(f_reversed)
            return lists_of_values_match(base_values, reversed_values)


# now ideally we would just call math diff at this point, but for today let's just diff them
csv_match = files_match(base_csv, reversed_csv)
mtr_match = files_match(base_mtr, reversed_mtr)
if csv_match and mtr_match:
    sys.exit(0)
else:
    sys.exit(1)
