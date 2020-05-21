"""
Parse the 
"""
import re
import sys
import os
import os.path


line_re = re.compile(r"\((.*\.tex)")


def parse_current_tex_file(line):
    """
    Parse a single latex log line to find the current LaTeX file being
    processed if any.
    - line: str, the line to parse
    RETURN: None or string
    """
    m = line_re.search(line)
    if m is not None:
        return m.group(1)
    return None


def main(log_path):
    """
    - log_path: str, the path to the LaTeX log file
    RETURN: {   "log_file_path": str,
                "issues": [{"tex_file": str, "issue": str}],
            }
    """
    errors = {"log_file_path": log_path, "issues": []}
    with open(log_path) as rd:
        current_tex_file = None
        for line in rd.readlines():
            tex_file = parse_current_tex_file(line)
            if tex_file is not None:
                print(f"processing {tex_file} ...")
                current_tex_file = tex_file
            # check for ! message
            # check for actual error
            # check for LaTeX Error: message
            pass
    return errors


def run_tests():
    """
    A simple unit test suite for the parser.
    """
    out = parse_current_tex_file(
            "[70] [71] [72] [73] [74] [75]) " +
            "(./src/overview/group-compliance-objects.tex [76")
    assert out == "./src/overview/group-compliance-objects.tex", f"out = {out}"
    out = parse_current_tex_file(
            "Underfull \hbox (badness 10000) in paragraph at lines 1042--1043")
    assert out is None, f"out = {out}"
    out = parse_current_tex_file(
            "]) (./src/overview/group-location-climate-weather-file-access.tex" +
            " [77] [78] [79")
    assert out == "./src/overview/group-location-climate-weather-file-access.tex", f"out = {out}"


if __name__ == "__main__":
    run_tests()
    if len(sys.argv) != 3:
        print(f"USAGE python {sys.argv[0]} <latex-log-file-path> <err-file-path>")
        print("- <latex-log-file-path>: file path to LaTeX log file to parse")
        print("- <err-file-path>: file path to error file to write if errors found")
        print("NOTE: no error file is written if no significant errors/warnings found")
        sys.exit(1)
    log_path = sys.argv[1]
    err_path = sys.argv[2]
    print(f"log_path: {log_path}")
    print(f"err_path: {err_path}")
    out = main(log_path)
    print(f"out: {out}")
    print("Done!")
