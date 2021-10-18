import os
import shutil
import sys
from typing import List, TextIO, Tuple


def process_command_arguments() -> Tuple[str, str]:
    if len(sys.argv) != 3:
        print(f"Invalid arguments passed to {sys.argv[0]}")
        sys.exit(1)
    return sys.argv[1], sys.argv[2]


def determine_number_of_design_days(idf_path: str) -> int:
    # find out ow many DD's there are based on the IDF
    num = 0
    with open(idf_path) as f_file:
        for li in f_file:
            if "SizingPeriod:DesignDay," in li:
                num += 1
    return num


def configure_root_dirs(test_dir: str) -> Tuple[str, str]:
    return (
        os.path.join(test_dir, 'Regular'),
        os.path.join(test_dir, 'Reversed')
    )


def configure_paths(b_dir: str, r_dir: str, filename: str) -> Tuple[str, str, str]:
    return (
        os.path.join(b_dir, f'{filename}.csv'),
        os.path.join(r_dir, f'{filename}.csv'),
        os.path.join(r_dir, f'{filename}_backup.csv')
    )


def get_file_lines(file_path: str) -> List[str]:
    contents = []
    with open(file_path) as f_file:
        for li in f_file:
            contents.append(li)
    return contents


def get_numeric_token_list(file_object: TextIO) -> List[float]:
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


def write_processed_data(
        rev_file: str, rev_file_backup: str, file_lines: List[str], rows_per_environment: int,
        num_dds: int, num_rows: int) -> None:
    shutil.copy(rev_file, rev_file_backup)
    with open(rev_file, "w") as f_file:
        f_file.write("%s" % file_lines[0])
        for row in range(rows_per_environment + 1, 2 * rows_per_environment + 1):
            f_file.write("%s" % file_lines[row])
        for row in range(1, rows_per_environment + 1):
            f_file.write("%s" % file_lines[row])
        if num_dds > 2:
            for row in range(2 * rows_per_environment + 1, num_rows + 1):
                f_file.write("%s" % file_lines[row])


def do_comparison(base_file: str, rev_file: str, rev_file_backup: str, num_dds: int) -> bool:
    file_lines = get_file_lines(rev_file)
    data_rows = len(file_lines) - 1
    data_rows_per_environment = int(data_rows / num_dds)
    write_processed_data(rev_file, rev_file_backup, file_lines, data_rows_per_environment, num_dds, data_rows)
    return files_match(base_file, rev_file)


def files_match(base_file_path: str, mod_file_path: str) -> bool:
    with open(base_file_path) as f_base:
        with open(mod_file_path) as f_reversed:
            base_values = get_numeric_token_list(f_base)
            reversed_values = get_numeric_token_list(f_reversed)
            return lists_of_values_match(base_values, reversed_values)


def main():
    # validate and get command line arguments
    base_idf_path, reverse_dd_test_dir = process_command_arguments()

    # find out ow many DD's there are based on the IDF
    num_design_days = determine_number_of_design_days(base_idf_path)

    # configure up all the paths
    base_dir, reversed_dir = configure_root_dirs(reverse_dd_test_dir)

    # handle the CSV file, check the original contents, write out the reordered data, and compare them
    base_csv, reversed_csv, reversed_csv_backup = configure_paths(base_dir, reversed_dir, 'eplusout')
    csv_match = do_comparison(base_csv, reversed_csv, reversed_csv_backup, num_design_days)

    # do the same thing if the meter file exists
    base_mtr, rev_mtr, rev_mtr_backup = configure_paths(base_dir, reversed_dir, 'eplusmtr')
    mtr_match = do_comparison(base_mtr, rev_mtr, rev_mtr_backup, num_design_days) if os.path.exists(rev_mtr) else True

    # report the results of the comparisons
    if csv_match and mtr_match:
        return 0
    return 1


if __name__ == "__main__":
    sys.exit(main())
