"""
Parse the 
"""
import re
import sys
import os
import os.path


LINE_RE = re.compile(r"\((.*\.tex)")
BANG_MSG = re.compile(r"^! (.*)$")
LATEX_WARNING = re.compile(r"^LaTeX Warning: (.*)$")
LABEL_MULTIPLY_DEFINED_WARN = re.compile(
        r"^Label `([^']*)' multiply defined\.$")
HYPER_UNDEFINED_WARN = re.compile(
        r"^Hyper reference `([^']*)' on page (\d+) " +
        "undefined on input line (\d+)\.$")


def parse_current_tex_file(line):
    """
    Parse a single latex log line to find the current LaTeX file being
    processed if any.
    - line: str, the line to parse
    RETURN: None or string
    """
    m = LINE_RE.search(line)
    if m is not None:
        return m.group(1)
    return None


def parse_latex_warning_start(line):
    """
    - line: str, line
    RETURN: None, or line
    """
    m = LATEX_WARNING.match(line)
    if m is not None:
        return m.group(1)
    return None


def parse_bang_start(line):
    """
    Parse a single latex log line indicating an error/warning message
    beginning.
    - line: str, the line to parse
    RETURN: None or string
    """
    m = BANG_MSG.match(line)
    if m is not None:
        return m.group(1)
    return None


def parse_warning(line):
    """
    - line: string, the line to parse
    RETURN: None or one of:
    - {'type': 'Label multiply defined',
       'label': string,
       'message': string}
    - {'type': 'Hyper reference undefined',
       'label': string,
       'message': string}
    """
    m = LABEL_MULTIPLY_DEFINED_WARN.match(line)
    if m is not None:
        return {'type':"Label multiply defined",
                'message': line,
                'label': m.group(1)}
    m = HYPER_UNDEFINED_WARN.match(line)
    if m is not None:
        return {'type':"Hyper reference undefined",
                'message':line,
                'label': m.group(1)}
    return None


def parse_log(log_path, verbose=False):
    """
    - log_path: str, the path to the LaTeX log file
    - verbose: bool, if True, print to sys.stdout
    RETURN: {   "log_file_path": str,
                "bangs": [{"tex_file": str, "issue": str}*],
                "warnings": [{"type": str, "message": str, "key": str}*]
            }
    """
    errors = {"log_file_path": log_path, "bangs": [], "warnings": []}
    with open(log_path) as rd:
        reading_bang_message = False
        reading_latex_warning = False
        current_tex_file = None
        current_issue = None
        for line in rd.readlines():
            if reading_bang_message:
                if line.strip() == "":
                    reading_bang_message = False
                    errors['bangs'].append({
                        'tex_file': current_tex_file,
                        'issue': current_issue})
                    current_issue = None
                    continue
                current_issue += line
            elif reading_latex_warning:
                if line.strip() == "":
                    reading_latex_warning = False
                    warn = parse_warning(current_issue)
                    if warn is not None:
                        errors['warnings'].append(warn)
                    else:
                        print(f"Unhandled warning {current_issue}")
                    current_issue = None
                    continue
                current_issue += line
            else:
                tex_file = parse_current_tex_file(line)
                if tex_file is not None:
                    if verbose:
                        print(f"processing {tex_file} ...")
                    current_tex_file = tex_file
                    continue
                issue_start = parse_bang_start(line)
                if issue_start is not None:
                    if verbose:
                        print(f"- issue {issue_start}")
                    reading_bang_message = True
                    current_issue = issue_start
                    continue
                warning_start = parse_latex_warning_start(line)
                if warning_start is not None:
                    if verbose:
                        print(f"- warning {warning_start}")
                    reading_latex_warning = True
                    current_issue = warning_start
                    continue
    return errors


def main(log_file, error_file):
    """
    - log_file: string, path to LaTeX log file to read
    - error_file: string, path to error file to write
    RETURNS: None
    SIDE_EFFECTS:
    - if significant errors are found in the log_file, writes error_file with a
      summary of issues
    - will ONLY write the error file if an issue is found
    - exits with a non-zero exit code if the script itself encounters any errors
    """
    try:
        errs = parse_log(log_file)
    except Exception as e:
        print(f"issue encountered in parsing log: {e}")
        sys.exit(1)
    num_bangs = len(errs['bangs'])
    num_warns = len(errs['warnings'])
    if (num_bangs > 0) or (num_warns > 0):
        print(f"! ERRORS: {num_bangs}")
        print(f"WARNINGS: {num_warns}")
        with open(error_file, 'w') as f:
            for b in errs['bangs']:
                f.write("-"*60 + "\n")
                f.write(f"{b['tex_file']}:\n{b['issue']}\n")
            for w in errs['warnings']:
                f.write("-"*60 + "\n")
                f.write(f"{w['type']}[{w['label']}]:\n{w['message']}\n")


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
    out = parse_bang_start("! Misplaced alignment tab character &.")
    assert out == "Misplaced alignment tab character &.", f"out == {out}"
    out = parse_latex_warning_start(
            "LaTeX Warning: Hyper reference " +
            "`airterminalsingleductuncontrolled' on page 2467")
    assert out == "Hyper reference `airterminalsingleductuncontrolled' on page 2467", f"out = {out}"


if __name__ == "__main__":
    if len(sys.argv) == 2 and (sys.argv[1] == 'test'):
        print("TESTING:")
        run_tests()
        print("all tests passed")
        sys.exit(0)
    if len(sys.argv) != 3:
        print(f"USAGE python {sys.argv[0]} <latex-log-file-path> <err-file-path>")
        print("- <latex-log-file-path>: file path to LaTeX log file to parse")
        print("- <err-file-path>: file path to error file to write if errors found")
        print("NOTE: no error file is written if no significant errors/warnings found")
        print(f"To run tests, call `python {sys.argv[0]} test'")
        sys.exit(1)
    log_path = sys.argv[1]
    err_path = sys.argv[2]
    print(f"log_path: {log_path}")
    print(f"err_path: {err_path}")
    main(log_path, err_path)
    print("Done!")
