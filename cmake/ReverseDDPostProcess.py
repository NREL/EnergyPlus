import os
import sys
from typing import TextIO, Set, Tuple
import unittest


def process_command_arguments() -> str:
    """Verify the number of command line arguments is correct then return the relevant ones"""
    if len(sys.argv) != 2:
        print(f"Invalid arguments passed to {sys.argv[0]}")
        sys.exit(1)
    return sys.argv[1]


def configure_root_dirs(test_dir: str) -> Tuple[str, str]:
    """Set up the test directories based on prescribed names"""
    return os.path.join(test_dir, 'Regular'), os.path.join(test_dir, 'Reversed')


def configure_paths(b_dir: str, r_dir: str, filename: str) -> Tuple[str, str]:
    """Set all the relative file paths for the base, reversed, and reverse-backup files"""
    return (
        os.path.join(b_dir, f'{filename}.csv'),
        os.path.join(r_dir, f'{filename}.csv'),
    )


def get_processed_rows(file_object: TextIO) -> Set[str]:
    """Process each row by rounding individual tokens and then building back into row strings"""
    rows = set()
    for line_num, li in enumerate(file_object.readlines()):
        if line_num == 0:
            rows.add(li)
            continue
        tokens = []
        for i, v in enumerate(li.split(',')):
            if i == 0:
                tokens.append(v)
            try:
                tokens.append(f'{float(f"{v:.2g}"):g}')
                # fl = float(v)
                # # trims the floats down to a single decimal point, probably too aggressive...
                # rounded = round(fl, 1)
                # padded = format(rounded, '.1f')
                # tokens.append(padded)
            except ValueError:
                continue
        rows.add(','.join([str(x) for x in tokens]))
    return rows


def files_match(base_file_path: str, mod_file_path: str) -> bool:
    """Gather processed rows from each file and then return the comparison"""
    with open(base_file_path) as f_base:
        with open(mod_file_path) as f_reversed:
            base_rows = get_processed_rows(f_base)
            reversed_rows = get_processed_rows(f_reversed)
            # for br in base_rows:
            #     if br in reversed_rows:
            #         pass
            #     else:
            #         print(f"Found bad base row: {br}")
            # for mr in reversed_rows:
            #     if mr in base_rows:
            #         pass
            #     else:
            #         print(f"Found bad mod row:  {mr}")
            return base_rows == reversed_rows


def both_csv_files_missing(b: str, r: str) -> bool:
    base_missing = not os.path.exists(b)
    rev_missing = not os.path.exists(r)
    return base_missing and rev_missing


def main():
    """Handles the main entry point into this script"""

    # validate and get command line arguments
    reverse_dd_test_dir = process_command_arguments()

    # configure up all the paths
    base_dir, reversed_dir = configure_root_dirs(reverse_dd_test_dir)

    # configure paths for both csv and mtr outputs
    base_csv, reversed_csv = configure_paths(base_dir, reversed_dir, 'eplusout')
    base_mtr, rev_mtr = configure_paths(base_dir, reversed_dir, 'eplusmtr')

    if both_csv_files_missing(base_csv, reversed_csv):
        return 0  # assume everything is fine if the CSV is missing in *both* builds

    # do comparison of the outputs
    csv_match = files_match(base_csv, reversed_csv)
    mtr_match = files_match(base_mtr, rev_mtr) if os.path.exists(rev_mtr) else True

    # report the results of the comparisons
    if csv_match and mtr_match:
        return 0
    return 1


if __name__ == "__main__":
    sys.exit(main())


class Test1(unittest.TestCase):
    def test_function(self):
        f1 = '/eplus/repos/8eplus/builds/r/testfiles_ReverseDD/MultiStory/Regular/eplusout.csv'
        f2 = '/eplus/repos/8eplus/builds/r/testfiles_ReverseDD/MultiStory/Reversed/eplusout.csv'
        self.assertTrue(files_match(f1, f2))
