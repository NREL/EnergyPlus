from datetime import datetime
import os
import shutil
import sys
from typing import List, TextIO, Tuple


def process_command_arguments() -> str:
    """Verify the number of command line arguments is correct then return the relevant ones"""
    if len(sys.argv) != 2:
        print(f"Invalid arguments passed to {sys.argv[0]}")
        sys.exit(1)
    return sys.argv[1]


def get_time_stamp_from_token(token: str) -> datetime:
    """Deal with the annoying EnergyPlus 24-hour time stamp"""
    eplus_stamp_format = "%m/%d  %H:%M:%S"  # eplusout.csv output is: 01/21  01:00:00
    token = token.strip()
    original_hour_token = token[7:9]
    original_hour = int(original_hour_token)
    new_hour = original_hour - 1
    modified_token = token[0:7] + str(new_hour).zfill(2) + token[9:]
    return datetime.strptime(modified_token, eplus_stamp_format)


def determine_number_of_design_days(rev_file: str) -> int:
    """Find out how many design day environments there are, based on the time series output"""

    num = 0
    with open(rev_file) as f_file:
        line_num = 0
        last_time_stamp = None
        interval = 0
        for li in f_file.readlines():
            line_num += 1
            if line_num == 1:
                continue
            elif line_num == 2:
                num += 1  # always count the first one
                base_time_stamp_token = li.split(',')[0]
                last_time_stamp = get_time_stamp_from_token(base_time_stamp_token)
            elif line_num == 3:
                # here we check to see if we are on a new day, indicating aggregated outputs, so each row is an environ
                this_time_stamp_token = li.split(',')[0]
                this_time_stamp = get_time_stamp_from_token(this_time_stamp_token)
                interval = (this_time_stamp - last_time_stamp).total_seconds()
                if interval > 4000:
                    # we have more than hourly, so each row should be a new day
                    # there is a possible complication here with multiple day sizing periods, we'll deal with that later
                    # for now, go ahead and add a new environment for this row
                    num += 1
                # either way update the previous time stamp so we can rely on it as we move to further rows
                last_time_stamp = this_time_stamp
            else:
                # now for every other row, if the interval was greater than hourly, we just add a new environment
                # if the interval is less, then we need to check the interval and see if we made a different jump
                if interval > 4000:
                    num += 1
                else:
                    this_time_stamp_token = li.split(',')[0]
                    this_time_stamp = get_time_stamp_from_token(this_time_stamp_token)
                    this_interval = (this_time_stamp - last_time_stamp).total_seconds()
                    if this_interval != interval:
                        # we have moved to a new environment!
                        num += 1
                    last_time_stamp = this_time_stamp
    return num


def configure_root_dirs(test_dir: str) -> Tuple[str, str]:
    """Set up the test directories based on prescribed names"""
    return os.path.join(test_dir, 'Regular'), os.path.join(test_dir, 'Reversed')


def configure_paths(b_dir: str, r_dir: str, filename: str) -> Tuple[str, str, str]:
    """Set all the relative file paths for the base, reversed, and reverse-backup files"""
    return (
        os.path.join(b_dir, f'{filename}.csv'),
        os.path.join(r_dir, f'{filename}.csv'),
        os.path.join(r_dir, f'{filename}_backup.csv')
    )


def get_numeric_token_list(file_object: TextIO) -> List[float]:
    """Do a naive split of CSV tokens by comma and convert them to a simple one dimensional list of floats"""
    values = []
    for li in file_object.readlines():
        for v in li.split(','):
            try:
                fl = float(v)
                values.append(fl)
            except ValueError:
                continue
    return values


def write_processed_data(rev_file: str, rev_file_backup: str, num_dds: int) -> None:
    """Open the reverse-dd file and rewrite it in the "original" order"""
    lines = open(rev_file).readlines() if rev_file else []
    num_rows = len(lines) - 1
    rows_per_environment = int(num_rows / num_dds)
    shutil.copy(rev_file, rev_file_backup)
    with open(rev_file, "w") as f_file:
        f_file.write("%s" % lines[0])
        for row in range(rows_per_environment + 1, 2 * rows_per_environment + 1):
            f_file.write("%s" % lines[row])
        for row in range(1, rows_per_environment + 1):
            f_file.write("%s" % lines[row])
        if num_dds > 2:
            for row in range(2 * rows_per_environment + 1, num_rows + 1):
                f_file.write("%s" % lines[row])


def lists_of_values_match(base: List[float], mod: List[float], tolerance: float = 41.0) -> bool:
    """Check the one dimensional list of CSV tokens against a predetermined tolerance"""
    if len(base) != len(mod):
        return False
    for i, base_value in enumerate(base):
        mod_value = mod[i]
        if base_value < tolerance:
            if abs(base_value - mod_value) > tolerance:
                print(f"Found mismatched values: Base = {base_value}, Mod = {mod_value}")
                return False
        elif (abs(base_value - mod_value) / base_value) > tolerance:
            return False
    return True


def do_comparison(base_file: str, rev_file: str, rev_file_backup: str, num_dds: int) -> bool:
    """Write the processed data files and then do a comparison"""
    write_processed_data(rev_file, rev_file_backup, num_dds)
    return files_match(base_file, rev_file)


def files_match(base_file_path: str, mod_file_path: str) -> bool:
    """Gather CSV tokens from each file and then return the comparison"""
    with open(base_file_path) as f_base:
        with open(mod_file_path) as f_reversed:
            base_values = get_numeric_token_list(f_base)
            reversed_values = get_numeric_token_list(f_reversed)
            return lists_of_values_match(base_values, reversed_values)


def main():
    """Handles the main entry point into this script"""

    # validate and get command line arguments
    reverse_dd_test_dir = process_command_arguments()

    # configure up all the paths
    base_dir, reversed_dir = configure_root_dirs(reverse_dd_test_dir)

    # configure paths for both csv and mtr outputs
    base_csv, reversed_csv, reversed_csv_backup = configure_paths(base_dir, reversed_dir, 'eplusout')
    base_mtr, rev_mtr, rev_mtr_backup = configure_paths(base_dir, reversed_dir, 'eplusmtr')

    # find out ow many DD's there are based on the time series output
    num_design_days = determine_number_of_design_days(base_csv)

    # do comparison of the outputs
    csv_match = do_comparison(base_csv, reversed_csv, reversed_csv_backup, num_design_days)
    mtr_match = do_comparison(base_mtr, rev_mtr, rev_mtr_backup, num_design_days) if os.path.exists(rev_mtr) else True

    # report the results of the comparisons
    if csv_match and mtr_match:
        return 0
    return 1


if __name__ == "__main__":
    sys.exit(main())
