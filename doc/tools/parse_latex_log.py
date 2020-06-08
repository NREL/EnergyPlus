"""
Parse the LaTeX log file.
"""
import copy
import json
import os
import os.path
import re
import sys


DEBUG = False
VERBOSE = False
EXIT_CODE_ISSUES_FOUND = 2
EXIT_CODE_ERROR = 1
EXIT_CODE_GOOD = 0


SEVERITY_WARNING = 'WARNING'
SEVERITY_ERROR = 'ERROR'


LINE_RE = re.compile(r".*\((.*?\.tex)")
TEX_ERROR = re.compile(r"^! (.*)$")
GENERAL_ERROR = re.compile(
        r"^(! )?(LaTeX|pdfTeX|Package|Class) ((.*) )?Error.*?:(.*)")
GENERAL_WARNING = re.compile(
        r"^(! )?(LaTeX|pdfTeX|Package|Class) ((.*) )?Warning.*?:(.*)$")
LATEX_WARNING = re.compile(r"^LaTeX Warning: (.*)$")
LABEL_MULTIPLY_DEFINED_WARN = re.compile(
        r"^ Label `([^']*)' multiply defined\.$")
HYPER_UNDEFINED_WARN = re.compile(
        r"^ Hyper reference `([^']*)' on page (\d+) " +
        r"undefined on input line (\d+)\.$")
LINE_NO = re.compile(r"l\.(\d+)")
ON_INPUT_LINE = re.compile(r"on input line (\d+)\.")
ISSUES_TO_SKIP = {
        "There were undefined references.",
        "There were multiply-defined labels.",
        }


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
    - root_dir: str, the root directory of the file if found (for
      normalization)
    RETURN: None or string
    """
    m = LINE_RE.search(line)
    if m is not None:
        pp_dir = root_dir
        pp_file = os.path.join(pp_dir, m.group(1))
        try:
            return os.path.relpath(pp_file, start=pp_dir)
        except Exception:
            print("Exception...")
            # not a file under root_dir -- ignore
            return None
    return None


def parse_warning_start(line):
    """
    - line: str, line
    RETURN: None OR
    (Tuple str, str, str)
    - the warning type
    - warning type continued (usually for package)
    - the message
    """
    m = GENERAL_WARNING.match(line)
    if m is not None:
        return (m.group(2), m.group(4), m.group(5))
    return None


def parse_error_start(line):
    """
    - line: str, line
    RETURN: None OR
    (Tuple str, str, str)
    - the error type
    - error type continued (usually for package)
    - the message
    """
    m = GENERAL_ERROR.match(line)
    if m is not None:
        return (m.group(2), m.group(4), m.group(5))
    return None


def parse_tex_error(line):
    """
    Parse a single latex log line indicating an error/warning message
    beginning.
    - line: str, the line to parse
    RETURN: None or string
    """
    m = TEX_ERROR.match(line)
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
    pp_dir = root_dir
    for dir_name, subdirs, files in os.walk(root_dir):
        for file_name in files:
            path = os.path.join(dir_name, file_name)
            _, ext = os.path.splitext(file_name)
            if target_ext is not None and ext != target_ext:
                continue
            if verbose:
                print("checking {}...".format(path))
            pp_path = path
            try:
                with open(path, encoding="utf8", errors="ignore") as f:
                    for line_number, line in enumerate(f.readlines()):
                        if target in line:
                            path_to_log = os.path.relpath(
                                    pp_path, start=pp_dir)
                            locations.append({
                                'file': path_to_log,
                                'line': line_number + 1})
            except Exception as e:
                import traceback
                print("issue encountered in finding locations: {}".format(e))
                traceback.print_tb(sys.last_traceback)
                sys.exit(EXIT_CODE_ERROR)
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
                'type': "Label multiply defined",
                'locations': locations,
                'message': line,
                'label': m.group(1)}
    m = HYPER_UNDEFINED_WARN.match(line)
    if m is not None:
        label = m.group(1)
        locations = find_undefined_hyperref(src_dir, label)
        return {'severity': SEVERITY_WARNING,
                'type': "Hyper reference undefined",
                'locations': locations,
                'message': line,
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


def to_s(x):
    """
    A custom version of str(.) that prevents None from coming out as "None"
    - x: any, the thing to "stringify"
    RETURN: string
    """
    if x is None:
        return ""
    return str(x)


class LogParser:
    """
    The LaTeX Log Parser.
    """
    def __init__(self, log_path, src_dir, verbose=False):
        self.log_path = log_path
        self.src_dir = src_dir
        self.verbose = verbose

    def _read_tex_error(self, line):
        line_no = parse_line_number(self._current_issue)
        self._issues['issues'].append({
            'severity': SEVERITY_ERROR,
            'type': self._type,
            'locations': [{
                'file': self._current_tex_file,
                'line': line_no}],
            'message': self._current_issue.strip()})

    def _read_warning(self, line):
        warn = parse_warning(self._current_issue, self.src_dir)
        if self._current_issue.strip() not in ISSUES_TO_SKIP:
            if warn is not None:
                self._issues['issues'].append(warn)
            else:
                line_no = parse_on_input_line(self._current_issue)
                self._issues['issues'].append({
                    'severity': SEVERITY_WARNING,
                    'type': self._type,
                    'locations': [{
                        'file': self._current_tex_file,
                        'line': line_no}],
                    'message': self._current_issue.strip()})

    def _read_error(self, line):
        if self._current_issue not in ISSUES_TO_SKIP:
            line_no = parse_on_input_line(self._current_issue)
            self._issues['issues'].append({
                'severity': SEVERITY_ERROR,
                'type': self._type,
                'locations': [{
                    'file': self._current_tex_file,
                    'line': line_no}],
                'message': self._current_issue.strip()})

    def _read_issue(self, line):
        is_ws = (line.strip() == "")
        first_line = (self._issue_line == 1)
        nonempty = len(line) > 1
        if is_ws and (not (first_line and nonempty)):
            if self._in_tex_err:
                self._read_tex_error(line)
            elif self._in_warn:
                self._read_warning(line)
            elif self._in_err:
                self._read_error(line)
            else:
                raise Exception("unhandled issue read")
            self._reset_after_read()
        else:
            self._current_issue += line
        self._issue_line += 1

    def _init_state(self):
        self._previous_line = None
        self._issue_line = None
        self._in_tex_err = False
        self._in_warn = False
        self._in_err = False
        self._type = None
        self._current_tex_file = None
        self._current_issue = None

    def _reset_after_read(self):
        self._in_tex_err = False
        self._in_warn = False
        self._in_err = False
        self._type = None
        self._current_issue = None

    def _report(self, message):
        if self.verbose:
            print(message)

    def _assemble_full_line(self, line):
        if self._previous_line is not None:
            full_line = self._previous_line.strip() + line
        else:
            full_line = line
        return full_line

    def __call__(self):
        self._issues = {"log_file_path": self.log_path, "issues": []}
        try:
            with open(self.log_path, encoding="utf8", errors="ignore") as rd:
                self._init_state()
                for line in rd.readlines():
                    if (self._in_tex_err or self._in_warn or self._in_err):
                        self._read_issue(line)
                    else:
                        self._issue_line = None
                        full_line = self._assemble_full_line(line)
                        tex_file = parse_current_tex_file(
                                full_line, self.src_dir)
                        issue_start = parse_tex_error(line)
                        warning_start = parse_warning_start(line)
                        err_start = parse_error_start(line)
                        if tex_file is not None:
                            self._current_tex_file = tex_file
                            self._report("processing {} ...".format(tex_file))
                        elif issue_start is not None:
                            self._issue_line = 1
                            self._in_tex_err = True
                            self._type = "TeX Error"
                            self._current_issue = issue_start
                            self._report("- issue {}".format(issue_start))
                        elif err_start is not None:
                            self._issue_line = 1
                            self._in_err = True
                            self._type = (
                                    to_s(err_start[0]) +
                                    " " + to_s(err_start[1])).strip()
                            self._current_issue = err_start[2]
                            self._report("- error {}".format(
                                self._current_issue))
                        elif warning_start is not None:
                            self._issue_line = 1
                            self._in_warn = True
                            self._type = (
                                    to_s(warning_start[0]) +
                                    " " + to_s(warning_start[1])).strip()
                            self._current_issue = warning_start[2]
                            self._report("- warning {}".format(
                                self._current_issue))
                    self._previous_line = line
        except Exception:
            return sys.exit(EXIT_CODE_ERROR)
        return self._issues


def parse_log(log_path, src_dir, verbose=VERBOSE):
    """
    - log_path: str, the path to the LaTeX log file
    - src_dir: str, the path to the LaTeX source directory
    - verbose: bool, if True, print to sys.stdout
    RETURN: {   "log_file_path": str,
                "bangs": [{"tex_file": str, "issue": str}*],
                "warnings": [{"type": str, "message": str, "key": str}*]
            }
    """
    log_parser = LogParser(log_path, src_dir, verbose)
    return log_parser()


def update_paths_to_be_from_repo_root(issues, repo_root_to_src):
    """
    - issues: (Array {'file': string, ...})
    - repo_root_to_src: string, path from repository root to directory where
      src/ folder is.
    RETURN: (Array {'file': string, ...})
    """
    updated = []
    for issue in issues:
        update = copy.deepcopy(issue)
        if 'locations' in update:
            new_locs = []
            for location in update['locations']:
                loc = copy.deepcopy(location)
                if 'file' in location:
                    loc['file'] = os.path.join(repo_root_to_src, location['file'])
                new_locs.append(loc)
            update['locations'] = new_locs
        updated.append(update)
    return updated


def find_issues(log_path, json_error_path, src_dir):
    """
    - log_path: string, path to LaTeX log file to read
    - json_error_path: string, path to JSON error file to write
    - src_dir: string, path to the source directory of the processed LaTeX
      files
    RETURNS: Bool
    SIDE_EFFECTS:
    - if significant errors are found in the log_path, writes json_error_path
      with a summary of issues. Also writes issues to stdout
    - will ONLY write the json error file if an issue is found
    - returns True if issues found, else false
    """
    try:
        errs = parse_log(log_path, src_dir)
        repo_root = os.path.abspath(os.path.join(os.path.dirname(log_path), '..', '..'))
        repo_root_to_src = os.path.relpath(src_dir, start=repo_root)
        errs['issues'] = update_paths_to_be_from_repo_root(
                errs['issues'],
                repo_root_to_src)
    except Exception as e:
        import traceback
        print("issue encountered in parsing log: {}".format(e))
        traceback.print_tb(sys.last_traceback)
        sys.exit(EXIT_CODE_ERROR)
    num_issues = len(errs['issues'])
    if (num_issues > 0):
        if VERBOSE:
            print("ISSUES: {}".format(num_issues))
        f = sys.stdout
        for issue in errs['issues']:
            f.write("[LATEX")
            f.write(issue['severity'] + "::")
            locs = [loc['file'] + ":" + str(loc['line'])
                    for loc in issue['locations']]
            f.write(",".join(locs) + "::")
            msg = issue['message'].replace("\r\n", " ")
            msg = msg.replace("\n", " ")
            msg = msg.replace("\r", " ")
            msg = msg.replace("\t", " ")
            msg = " ".join(msg.split())
            f.write(msg + "]\n")
        with open(json_error_path, 'w', encoding='utf-8') as f:
            json.dump(errs, f, ensure_ascii=False, indent=4)
    return (num_issues > 0)


def run_tests():
    """
    A simple unit test suite for the parser.
    """
    out = parse_current_tex_file(
            "[70] [71] [72] [73] [74] [75]) " +
            "(./src/overview/group-compliance-objects.tex [76",
            "/home/user/stuff/input-output-reference")
    msg = "out = {}".format(out)
    assert out == "src/overview/group-compliance-objects.tex", msg
    out = parse_current_tex_file(
            "Underfull \\hbox (badness 10000) in paragraph at lines " +
            "1042--1043",
            "/home/user/stuff/input-output-reference")
    assert out is None, "out = {}".format(out)
    out = parse_current_tex_file(
            "]) (./src/overview/group-location-climate-weather-file-access" +
            ".tex [77] [78] [79",
            "/home/user/stuff/input-output-reference")
    assert out == (
            "src/overview/group-location-climate-weather-file-access.tex")
    out = parse_current_tex_file(
            "(./src/appendix-a-units-and-abbreviations/" +
            "standard-energyplus-conditions.tex) " +
            "(./src/appendix-a-units-and-abbreviations/" +
            "standard-energyplus-units.tex",
            "/home/user/stuff/input-output-reference")
    assert out == (
            "src/appendix-a-units-and-abbreviations/" +
            "standard-energyplus-units.tex"), "out == {}".format(out)
    out = parse_tex_error("! Misplaced alignment tab character &.")
    msg = "out == {}".format(out)
    assert out == "Misplaced alignment tab character &.", msg
    out = parse_warning_start(
            "LaTeX Warning: Hyper reference " +
            "`airterminalsingleductuncontrolled' on page 2467")
    assert out[2] == (" Hyper reference `airterminalsingleductuncontrolled' " +
                      "on page 2467"), "out = {}".format(out)


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
        current_dir = os.path.abspath('.')
        for tag in doc_tags:
            log_path = os.path.abspath(
                    os.path.join(current_dir, '..', tag, tag + '.log'))
            assert os.path.exists(log_path)
            json_err = '{}-err.json'.format(tag)
            src_dir = os.path.abspath(
                    os.path.join(current_dir, '..', tag))
            print("="*60)
            print("Processing {}...".format(tag))
            if os.path.exists(json_err):
                os.remove(json_err)
            find_issues(
                    log_path=str(log_path),
                    json_error_path=json_err,
                    src_dir=str(src_dir))
        print("Done!")
        sys.exit(EXIT_CODE_GOOD)
    if len(sys.argv) == 2 and (sys.argv[1] == 'test'):
        print("TESTING:")
        run_tests()
        print("all tests passed")
        sys.exit(EXIT_CODE_GOOD)
    if len(sys.argv) != 4:
        print("USAGE python " + sys.argv[0] + " <latex-log-file-path> " +
              "<latex-source-dir> <json-err-path>")
        print("- <latex-log-file-path>: file path to LaTeX log file to parse")
        print("- <latex-source-dir>: path to LaTeX source directory")
        print("- <json-err-path>: file path to a JSON version of the error " +
              "file; only written if issues found")
        print("NOTE: no error file is written if no significant " +
              "errors/warnings found")
        print("To run tests, call `python {} test'".format(sys.argv[0]))
        sys.exit(EXIT_CODE_ERROR)
    log_path = sys.argv[1]
    src_dir = sys.argv[2]
    json_err = sys.argv[3]
    if VERBOSE:
        print("log_path: {}".format(log_path))
        print("src_dir : {}".format(src_dir))
        print("json_err: {}".format(json_err))
    issues_found = find_issues(log_path, json_err, src_dir)
    if issues_found:
        sys.exit(EXIT_CODE_ISSUES_FOUND)
