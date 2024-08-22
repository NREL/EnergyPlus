import json
import sys
from pathlib import Path

from energyplus_regressions.runtests import SuiteRunner
from energyplus_regressions.structures import TextDifferences, TestEntry


def print_notice(relative_test_file: str, message: str) -> None:
    # relative_test_file should be relative to the testfiles/ directory, like "1Zone.idf" or "advanced/2a.idf"
    print(f"::notice file=testfiles/{relative_test_file}::{message}")


def print_warning(relative_test_file: str, message: str) -> None:
    # relative_test_file should be relative to the testfiles/ directory, like "1Zone.idf" or "advanced/2a.idf"
    print(f"::warning file=testfiles/{relative_test_file}::{message}")


def print_error(relative_test_file: str, message: str) -> None:
    # relative_test_file should be relative to the testfiles/ directory, like "1Zone.idf" or "advanced/2a.idf"
    print(f"::error file=testfiles/{relative_test_file}::{message}")


def process_diffs(relative_test_file: str, diff_name, diffs, this_has_diffs, this_has_small_diffs):
    # relative_test_file should be relative to the testfiles/ directory, like "1Zone.idf" or "advanced/2a.idf"
    if not diffs:
        return this_has_diffs, this_has_small_diffs
    if diffs.diff_type == 'Big Diffs':
        this_has_diffs = True
        print_warning(relative_test_file, f"{diff_name} Big Diffs")
    elif diffs.diff_type == 'Small Diffs':
        this_has_small_diffs = True
        print_warning(relative_test_file, f"{diff_name} Small Diffs")
    return this_has_diffs, this_has_small_diffs


def single_file_regressions(baseline: Path, modified: Path) -> [bool, bool, bool]:

    import energyplus_regressions
    thresholds = Path(energyplus_regressions.__file__).parent / 'diffs' / 'math_diff.config'

    idf = baseline.name

    entry = TestEntry(idf, "")
    entry, message = SuiteRunner.process_diffs_for_one_case(
        entry,
        {'build_dir': str(baseline)},
        {'build_dir': str(modified)},
        "",
        str(thresholds),
        ci_mode=True
    )  # returns an updated entry

    with open('results.json', 'w') as f:
        f.write(json.dumps(entry.to_dict(), indent=4))

    success = True
    has_diffs = False
    has_small_diffs = False

    # Note, comment out any of the "has_diffs" below if you don't want
    # it to generate an error condition

    if entry.aud_diffs and (entry.aud_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "AUD diffs.")

    if entry.bnd_diffs and (entry.bnd_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "BND diffs.")

    if entry.dl_in_diffs and (entry.dl_in_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "delightin diffs.")

    if entry.dl_out_diffs and (entry.dl_out_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "delightout diffs.")

    if entry.dxf_diffs and (entry.dxf_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "DXF diffs.")

    if entry.eio_diffs and (entry.eio_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "EIO diffs.")

    if entry.err_diffs and (entry.err_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "ERR diffs.")

    if entry.readvars_audit_diffs and (entry.readvars_audit_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "ReadvarsAudit diffs.")

    if entry.edd_diffs and (entry.edd_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "EDD diffs.")

    if entry.wrl_diffs and (entry.wrl_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "WRL diffs.")

    if entry.sln_diffs and (entry.sln_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "SLN diffs.")

    if entry.sci_diffs and (entry.sci_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "SCI diffs.")

    if entry.map_diffs and (entry.map_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "MAP diffs.")

    if entry.dfs_diffs and (entry.dfs_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "DFS diffs.")

    if entry.screen_diffs and (entry.screen_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "SCREEN diffs.")

    if entry.glhe_diffs and (entry.glhe_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "GLHE diffs")

    # numeric diff
    if entry.eso_diffs:
        has_diffs, has_small_diffs = process_diffs(idf, "ESO", entry.eso_diffs, has_diffs, has_small_diffs)

    if entry.mdd_diffs and (entry.mdd_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "MDD diffs.")

    if entry.mtd_diffs and (entry.mtd_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "MTD diffs.")

    # numeric diff
    if entry.mtr_diffs:
        has_diffs, has_small_diffs = process_diffs(idf, "MTR", entry.mtr_diffs, has_diffs, has_small_diffs)

    if entry.rdd_diffs and (entry.rdd_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "RDD diffs.")

    if entry.shd_diffs and (entry.shd_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "SHD diffs.")

    if entry.perf_log_diffs and (entry.perf_log_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "PERF_LOG diffs.")

    # numeric diff
    if entry.ssz_diffs:
        has_diffs, has_small_diffs = process_diffs(idf, "SSZ", entry.ssz_diffs, has_diffs, has_small_diffs)

    # numeric diff
    if entry.zsz_diffs:
        has_diffs, has_small_diffs = process_diffs(idf, "ZSZ", entry.zsz_diffs, has_diffs, has_small_diffs)

    # numeric diff
    if entry.json_diffs:
        has_diffs, has_small_diffs = process_diffs(idf, "JSON", entry.json_diffs, has_diffs, has_small_diffs)

    if entry.table_diffs:
        if entry.table_diffs.big_diff_count > 0:
            has_diffs = True
            print_warning(idf, "Table big diffs.")
        elif entry.table_diffs.small_diff_count > 0:
            has_small_diffs = True
            print_warning(idf, "Table small diffs.")
        if entry.table_diffs.string_diff_count > 1:  # There's always one...the time stamp
            has_diffs = True
            print_warning(idf, "Table string diffs.")

    if entry.idf_diffs and (entry.idf_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "IDF diffs.")

    if entry.stdout_diffs and (entry.stdout_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "StdOut diffs.")

    if entry.stderr_diffs and (entry.stderr_diffs.diff_type != TextDifferences.EQUAL):
        has_small_diffs = True
        print_warning(idf, "StdErr diffs.")

    return success, has_small_diffs, has_diffs


def check_all_regressions(base_testfiles: Path, mod_testfiles: Path) -> bool:
    any_diffs = False
    any_failures = False
    for baseline in base_testfiles.iterdir():
        if not baseline.is_dir():
            continue
        modified = mod_testfiles / baseline.name
        if not modified.exists():
            continue  # TODO: Should we warn that it is missing?
        success, small_diffs, big_diffs = single_file_regressions(baseline, modified)
        if small_diffs or big_diffs:
            any_diffs = True
        if not success:
            any_failures = True
    return any_diffs or any_failures


if __name__ == "__main__":  # pragma: no cover - testing function, not the __main__ entry point

    if len(sys.argv) < 3:
        print("syntax: %s base_dir mod_dir base_sha mod_sha device_id" % sys.argv[0])
        sys.exit(1)
    arg_base_dir = Path(sys.argv[1])
    arg_mod_dir = Path(sys.argv[2])
    sys.exit(1 if check_all_regressions(arg_base_dir, arg_mod_dir) else 0)
