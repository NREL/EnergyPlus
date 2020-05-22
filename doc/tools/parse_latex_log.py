"""
Parse the LaTeX log file.
"""
import json
import os
import os.path
import pathlib
import re
import sys


SEVERITY_WARNING = 'WARNING'
SEVERITY_ERROR = 'ERROR'


LINE_RE = re.compile(r"\((.*\.tex)(.*)$")
BANG_MSG = re.compile(r"^! (.*)$")
LATEX_WARNING = re.compile(r"^LaTeX Warning: (.*)$")
LABEL_MULTIPLY_DEFINED_WARN = re.compile(
        r"^Label `([^']*)' multiply defined\.$")
HYPER_UNDEFINED_WARN = re.compile(
        r"^Hyper reference `([^']*)' on page (\d+) " +
        "undefined on input line (\d+)\.$")
LINE_NO = re.compile(r"l\.(\d+)")
ON_INPUT_LINE = re.compile("on input line (\d+)\.")


def parse_on_input_line(line):
    """
    - line: str, the line to parse
    RETURN: None or int
    """
    m = ON_INPUT_LINE.search(line)
    if m is not None:
        return int(m.group(1))
    return None


def parse_current_tex_file(line, root_dir):
    """
    Parse a single latex log line to find the current LaTeX file being
    processed if any.
    - line: str, the line to parse
    - root_dir: str, the root directory of the file if found (for normalization)
    RETURN: None or string
    """
    m = LINE_RE.search(line)
    if m is not None:
        pp_dir = pathlib.Path(root_dir)
        pp_file = pp_dir / pathlib.Path(m.group(1))
        try:
            return str(pp_file.relative_to(pp_dir))
        except ValueError:
            # not a file under root_dir -- ignore
            return None
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


def find_locations(root_dir, target, target_ext='.tex', verbose=False):
    """
    - root_dir: str, the root of the directory tree to explore
    - target: str, the target to find
    - target_ext: str, the file extensions to browse
    RETURN: Array of {'file': string, 'line': int} values
    """
    locations = []
    pp_dir = pathlib.Path(root_dir)
    for dir_name, subdirs, files in os.walk(root_dir):
        for file_name in files:
            path = os.path.join(dir_name, file_name)
            _, ext = os.path.splitext(file_name)
            if target_ext is not None and ext != target_ext:
                continue
            if verbose:
                print(f"checking {path}...")
            pp_path = pathlib.Path(path)
            with open(path) as f:
                for line_number, line in enumerate(f.readlines()):
                    if target in line:
                        path_to_log = str(pp_path.relative_to(pp_dir))
                        locations.append({
                            'file': path_to_log,
                            'line': line_number + 1})
    return locations


def find_undefined_hyperref(root_dir, label):
    """
    - root_dir: str, the root of the directory tree to explore
    - label: str, the label that is undefined
    RETURN: Array of {'file': string, 'line': int} values
    """
    target = "\\hyperref[" + label + "]"
    return find_locations(root_dir, target)


def find_multiply_defined_labels(root_dir, label):
    """
    - root_dir: str, the root of the directory tree to explore
    - label: str, the label that is multiply defined
    RETURN: Array of {'file': string, 'line': int} values
    """
    target = "\\label{" + label + "}"
    return find_locations(root_dir, target)


def parse_warning(line, src_dir):
    """
    - line: string, the line to parse
    - src_dir: string, the root directory of the LaTeX source files
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
        label = m.group(1)
        locations = find_multiply_defined_labels(src_dir, label)
        return {'severity': SEVERITY_WARNING,
                'type':"Label multiply defined",
                'locations': locations,
                'message': line,
                'label': m.group(1)}
    m = HYPER_UNDEFINED_WARN.match(line)
    if m is not None:
        label = m.group(1)
        locations = find_undefined_hyperref(src_dir, label)
        return {'severity': SEVERITY_WARNING,
                'type':"Hyper reference undefined",
                'locations': locations,
                'message':line,
                'label': m.group(1)}
    return None


def parse_line_number(message):
    """
    - message: string
    RETURN: None or int, the line number if found
    """
    m = LINE_NO.search(message)
    if m is not None:
        return int(m.group(1))
    return None


def parse_log(log_path, src_dir, verbose=False):
    """
    - log_path: str, the path to the LaTeX log file
    - src_dir: str, the path to the LaTeX source directory
    - verbose: bool, if True, print to sys.stdout
    RETURN: {   "log_file_path": str,
                "bangs": [{"tex_file": str, "issue": str}*],
                "warnings": [{"type": str, "message": str, "key": str}*]
            }
    """
    issues = {"log_file_path": log_path, "issues": []}
    with open(log_path) as rd:
        previous_line = None
        issue_line = None
        reading_bang_message = False
        reading_latex_warning = False
        current_tex_file = None
        current_issue = None
        for line in rd.readlines():
            if reading_bang_message:
                if line.strip() == "" and not ((issue_line == 1) and (len(line) > 1)):
                    reading_bang_message = False
                    line_no = parse_line_number(current_issue)
                    issues['issues'].append({
                        'severity': SEVERITY_WARNING,
                        'type': "!",
                        'locations': [{
                            'file': current_tex_file,
                            'line': line_no}],
                        'message': current_issue})
                    current_issue = None
                else:
                    current_issue += line
                issue_line += 1
            elif reading_latex_warning:
                if line.strip() == "" and not ((issue_line == 1) and (len(line) > 1)):
                    reading_latex_warning = False
                    warn = parse_warning(current_issue, src_dir)
                    if warn is not None:
                        issues['issues'].append(warn)
                    else:
                        line_no = parse_on_input_line(current_issue)
                        issues['issues'].append({
                            'severity': SEVERITY_WARNING,
                            'type': 'LaTeX Warning',
                            'locations': [{
                                'file': current_tex_file,
                                'line': line_no}],
                            'message': current_issue})
                    current_issue = None
                else:
                    current_issue += line
                issue_line += 1
            else:
                issue_line = None
                if previous_line is not None:
                    full_line = previous_line.strip() + line
                else:
                    full_line = line
                tex_file = parse_current_tex_file(full_line, src_dir)
                if tex_file is not None:
                    if verbose:
                        print(f"processing {tex_file} ...")
                    current_tex_file = tex_file
                else:
                    issue_start = parse_bang_start(line)
                    if issue_start is not None:
                        issue_line = 1
                        if verbose:
                            print(f"- issue {issue_start}")
                        reading_bang_message = True
                        current_issue = issue_start
                    else:
                        warning_start = parse_latex_warning_start(line)
                        if warning_start is not None:
                            issue_line = 1
                            if verbose:
                                print(f"- warning {warning_start}")
                            reading_latex_warning = True
                            current_issue = warning_start
            previous_line = line
    return issues


def main(log_path, error_path, json_error_path, src_dir):
    """
    - log_path: string, path to LaTeX log file to read
    - error_path: string, path to error file to write
    - json_error_path: string, path to JSON error file to write
    - src_dir: string, path to the source directory of the processed LaTeX files
    RETURNS: None
    SIDE_EFFECTS:
    - if significant errors are found in the log_path, writes error_path with a
      summary of issues
    - will ONLY write the error file if an issue is found
    - exits with a non-zero exit code if the script itself encounters any errors
    """
    try:
        errs = parse_log(log_path, src_dir)
    except Exception as e:
        import traceback
        print(f"issue encountered in parsing log: {e}")
        traceback.print_tb(sys.last_traceback)
        sys.exit(1)
    num_issues = len(errs['issues'])
    if (num_issues > 0):
        print(f"ISSUES: {num_issues}")
        with open(error_path, 'w') as f:
            for issue in errs['issues']:
                f.write("[LATEX")
                f.write(issue['severity'] + "::")
                locs = [loc['file'] + ":" + str(loc['line']) for loc in issue['locations']]
                f.write(",".join(locs) + "::")
                msg = issue['message'].replace("\r\n", " ")
                msg = msg.replace("\n", " ")
                msg = msg.replace("\r", " ")
                msg = msg.replace("\t", " ")
                msg = " ".join(msg.split())
                f.write(msg + "]\n")
        with open(json_error_path, 'w', encoding='utf-8') as f:
            json.dump(errs, f, ensure_ascii=False, indent=4)


def run_tests():
    """
    A simple unit test suite for the parser.
    """
    out = parse_current_tex_file(
            "[70] [71] [72] [73] [74] [75]) " +
            "(./src/overview/group-compliance-objects.tex [76",
            "/home/user/stuff/input-output-reference")
    assert out == "src/overview/group-compliance-objects.tex", f"out = {out}"
    out = parse_current_tex_file(
            "Underfull \hbox (badness 10000) in paragraph at lines 1042--1043",
            "/home/user/stuff/input-output-reference")
    assert out is None, f"out = {out}"
    out = parse_current_tex_file(
            "]) (./src/overview/group-location-climate-weather-file-access.tex" +
            " [77] [78] [79",
            "/home/user/stuff/input-output-reference")
    assert out == "src/overview/group-location-climate-weather-file-access.tex", f"out = {out}"
    out = parse_bang_start("! Misplaced alignment tab character &.")
    assert out == "Misplaced alignment tab character &.", f"out == {out}"
    out = parse_latex_warning_start(
            "LaTeX Warning: Hyper reference " +
            "`airterminalsingleductuncontrolled' on page 2467")
    assert out == "Hyper reference `airterminalsingleductuncontrolled' on page 2467", f"out = {out}"


if __name__ == "__main__":
    if len(sys.argv) == 1:
        # shim for testing
        doc_tags = [
                'acknowledgments',
                'auxiliary-programs',
                'ems-application-guide',
                'engineering-reference',
                'essentials',
                'external-interfaces-application-guide',
                'getting-started',
                'input-output-reference',
                'interface-developer',
                'module-developer',
                'output-details-and-examples',
                'plant-application-guide',
                'tips-and-tricks-using-energyplus',
                'using-energyplus-for-compliance',
                ]
        current_dir = pathlib.Path('.')
        for tag in doc_tags:
            log_path = (
                    current_dir / pathlib.Path('..') / pathlib.Path(tag) /
                    pathlib.Path(tag + '.log')
                    ).resolve()
            assert log_path.exists()
            err_path = f'{tag}-err.txt'
            json_err = f'{tag}-err.json'
            src_dir = (
                    current_dir / pathlib.Path('..') / pathlib.Path(tag)
                    ).resolve()
            print("="*60)
            print(f"Processing {tag}...")
            if os.path.exists(err_path):
                os.remove(err_path)
            if os.path.exists(json_err):
                os.remove(json_err)
            main(
                    log_path=str(log_path),
                    error_path=err_path,
                    json_error_path=json_err,
                    src_dir=str(src_dir))
        print("Done!")
        sys.exit(0)
    if len(sys.argv) == 2 and (sys.argv[1] == 'test'):
        print("TESTING:")
        run_tests()
        print("all tests passed")
        sys.exit(0)
    if len(sys.argv) != 5:
        print(f"USAGE python {sys.argv[0]} <latex-log-file-path> <latex-source-dir> <err-file-path> <json-err-path>")
        print("- <latex-log-file-path>: file path to LaTeX log file to parse")
        print("- <latex-source-dir>: path to LaTeX source directory")
        print("- <err-file-path>: file path to error file to write if issues found")
        print("- <json-err-path>: file path to a JSON version of the error file; only written if issues found")
        print("NOTE: no error file is written if no significant errors/warnings found")
        print(f"To run tests, call `python {sys.argv[0]} test'")
        sys.exit(1)
    log_path = sys.argv[1]
    src_dir = sys.argv[2]
    err_path = sys.argv[3]
    json_err = sys.argv[4]
    print(f"log_path: {log_path}")
    print(f"src_dir : {src_dir}")
    print(f"err_path: {err_path}")
    print(f"json_err: {json_err}")
    main(log_path, err_path, json_err, src_dir)
    print("Done!")
