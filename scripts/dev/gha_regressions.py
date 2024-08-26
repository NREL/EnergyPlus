#!/usr/bin/env python
# EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University
# of Illinois, The Regents of the University of California, through Lawrence
# Berkeley National Laboratory (subject to receipt of any required approvals
# from the U.S. Dept. of Energy), Oak Ridge National Laboratory, managed by UT-
# Battelle, Alliance for Sustainable Energy, LLC, and other contributors. All
# rights reserved.
#
# NOTICE: This Software was developed under funding from the U.S. Department of
# Energy and the U.S. Government consequently retains certain rights. As such,
# the U.S. Government has been granted for itself and others acting on its
# behalf a paid-up, nonexclusive, irrevocable, worldwide license in the
# Software to reproduce, distribute copies to the public, prepare derivative
# works, and perform publicly and display publicly, and to permit others to do
# so.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# (1) Redistributions of source code must retain the above copyright notice,
#     this list of conditions and the following disclaimer.
#
# (2) Redistributions in binary form must reproduce the above copyright notice,
#     this list of conditions and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#
# (3) Neither the name of the University of California, Lawrence Berkeley
#     National Laboratory, the University of Illinois, U.S. Dept. of Energy nor
#     the names of its contributors may be used to endorse or promote products
#     derived from this software without specific prior written permission.
#
# (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in
#     stand-alone form without changes from the version obtained under this
#     License, or (ii) Licensee makes a reference solely to the software
#     portion of its product, Licensee must refer to the software as
#     "EnergyPlus version X" software, where "X" is the version number Licensee
#     obtained under this License and may not use a different name for the
#     software. Except as specifically required in this Section (4), Licensee
#     shall not use in a company name, a product name, in advertising,
#     publicity, or other promotional activities any name, trade name,
#     trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or
#     confusingly similar designation, without the U.S. Department of Energy's
#     prior written consent.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
from collections import defaultdict
import json
from shutil import copy
from pathlib import Path
import sys
import tempfile

from energyplus_regressions.runtests import SuiteRunner
from energyplus_regressions.structures import TextDifferences, TestEntry


class RegressionManager:

    def __init__(self):
        self.diffs_by_idf = []
        self.diffs_by_type = defaultdict(list)
        self.all_files_compared = []
        import energyplus_regressions
        self.threshold_file = str(Path(energyplus_regressions.__file__).parent / 'diffs' / 'math_diff.config')

    def single_file_regressions(self, baseline: Path, modified: Path) -> [TestEntry, bool]:

        idf = baseline.name
        this_file_diffs = []

        entry = TestEntry(idf, "")
        entry, message = SuiteRunner.process_diffs_for_one_case(
            entry,
            {'build_dir': str(baseline)},
            {'build_dir': str(modified)},
            "",
            self.threshold_file,
            ci_mode=True
        )  # returns an updated entry

        has_diffs = False

        text_diff_results = {
            "Audit": entry.aud_diffs,
            "BND": entry.bnd_diffs,
            "DELightIn": entry.dl_in_diffs,
            "DELightOut": entry.dl_out_diffs,
            "DXF": entry.dxf_diffs,
            "EIO": entry.eio_diffs,
            "ERR": entry.err_diffs,
            "Readvars_Audit": entry.readvars_audit_diffs,
            "EDD": entry.edd_diffs,
            "WRL": entry.wrl_diffs,
            "SLN": entry.sln_diffs,
            "SCI": entry.sci_diffs,
            "MAP": entry.map_diffs,
            "DFS": entry.dfs_diffs,
            "SCREEN": entry.screen_diffs,
            "GLHE": entry.glhe_diffs,
            "MDD": entry.mdd_diffs,
            "MTD": entry.mtd_diffs,
            "RDD": entry.rdd_diffs,
            "SHD": entry.shd_diffs,
            "PERF_LOG": entry.perf_log_diffs,
            "IDF": entry.idf_diffs,
            "StdOut": entry.stdout_diffs,
            "StdErr": entry.stderr_diffs,
        }
        for diff_type, diffs in text_diff_results.items():
            if diffs is None:
                continue
            if diffs.diff_type != TextDifferences.EQUAL:
                has_diffs = True
                this_file_diffs.append(diff_type)
                self.diffs_by_type[diff_type].append(idf)

        numeric_diff_results = {
            "ESO": entry.eso_diffs,
            "MTR": entry.mtr_diffs,
            "SSZ": entry.ssz_diffs,
            "ZSZ": entry.zsz_diffs,
            "JSON": entry.json_diffs,
        }
        for diff_type, diffs in numeric_diff_results.items():
            if diffs is None:
                continue
            if diffs.diff_type == 'Big Diffs':
                has_diffs = True
                this_file_diffs.append(f"{diff_type} Big Diffs")
                self.diffs_by_type[f"{diff_type} Big Diffs"].append(idf)
            elif diffs.diff_type == 'Small Diffs':
                has_diffs = True
                this_file_diffs.append(f"{diff_type} Small Diffs")
                self.diffs_by_type[f"{diff_type} Big Diffs"].append(idf)

        if entry.table_diffs:
            if entry.table_diffs.big_diff_count > 0:
                has_diffs = True
                this_file_diffs.append(f"Table Big Diffs")
                self.diffs_by_type[f"Table Big Diffs"].append(idf)
            elif entry.table_diffs.small_diff_count > 0:
                has_diffs = True
                this_file_diffs.append(f"Table Small Diffs")
                self.diffs_by_type[f"Table Small Diffs"].append(idf)
            if entry.table_diffs.string_diff_count > 1:  # There's always one...the time stamp
                has_diffs = True
                this_file_diffs.append(f"Table String Diffs")
                self.diffs_by_type[f"Table String Diffs"].append(idf)

        return entry, has_diffs

    @staticmethod
    def single_diff_html(contents: str) -> str:
        return f"""
<!doctype html>
<html>
 <head>
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/styles/default.min.css">
  <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/highlight.min.js"></script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/languages/diff.min.js"></script>
 </head>
 <body>
  <pre>
   <code class="diff">
    {contents}
   </code>
  </pre>
  <script>hljs.highlightAll();</script>
 </body>
</html>"""

    @staticmethod
    def regression_row_in_single_test_case_html(diff_file_name: str) -> str:
        return f"""
   <tr>
    <td>{diff_file_name}</td>
    <td><a href='{diff_file_name}' download='{diff_file_name}'>download</a></td>
    <td><a href='{diff_file_name}.html'>view</a></td>
   </tr>"""

    @staticmethod
    def single_test_case_html(contents: str) -> str:
        return f"""
<!doctype html>
<html>
 <head>
  <link rel="stylesheet" href="https://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css">
  <link rel="stylesheet" href="https://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap-theme.min.css">
  <script src="https://code.jquery.com/jquery-2.1.1.min.js"></script>
  <script src="https://code.jquery.com/jquery-migrate-1.2.1.min.js"></script>
  <script src="https://netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"></script>
 </head>
 <body>
  <table class='table table-hover'>
   <tr>
    <th>filename</th>
    <th></th>
    <th></th>
   </tr>
{contents}
  </table>
 </body>
</html>"""

    @staticmethod
    def bundle_root_index_html(header_info: list[str], no_diffs: list[str], diffs: list[str]) -> str:
        header_content = ""
        for hi in header_info:
            header_content += f"""<li class="list-group-item">{hi}</li>\n"""
        # gather some plural "s" as needed and form up the html entries
        num_no_diff = len(no_diffs)
        nds = 's' if num_no_diff == 0 or num_no_diff > 1 else ''
        no_diff_content = ""
        for nd in no_diffs:
            no_diff_content += f"""<li class="list-group-item">{nd}</li>\n"""
        num_diff = len(diffs)
        ds = 's' if num_diff == 0 or num_diff > 1 else ''
        diff_content = ""
        for d in diffs:
            diff_content += f"""<a href="{d}/index.html" class="list-group-item list-group-item-action">{d}</a>\n"""
        return f"""
<!doctype html>
<html>
 <head>
  <link rel="stylesheet" href="https://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css">
  <link rel="stylesheet" href="https://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap-theme.min.css">
  <script src="https://code.jquery.com/jquery-2.1.1.min.js"></script>
  <script src="https://code.jquery.com/jquery-migrate-1.2.1.min.js"></script>
  <script src="https://netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"></script>
 </head>
 <body><div class="container-fluid">
  <div class="panel-group" id="accordion_header">
   <div class="panel panel-default">
    <div class="panel-heading">
     <h4 class="panel-title">
      <a data-toggle="collapse" data-parent="#accordion_header" href="#header">Header Metadata</a>
     </h4>
    </div>
    <div id="header" class="panel-collapse collapse in">
     <div class="panel-body">
      <ul class="list-group">
       {header_content}
      </ul>
     </div>
    </div>
   </div>
  </div>
  
  <div class="panel-group">
   <div class="panel panel-default">
    <div class="panel-heading">
     <h4 class="panel-title">
      <a data-toggle="collapse" href="#nodiffs">{num_no_diff} File{nds} with No Diffs</a>
     </h4>
    </div>
    <div id="nodiffs" class="panel-collapse collapse">
     <div class="panel-body">
      <ul class="list-group">
       {no_diff_content}
      </ul>
     </div>
    </div>
   </div>
  </div>
  
  <div class="panel-group">
   <div class="panel panel-default">
    <div class="panel-heading">
     <h4 class="panel-title">
      <a data-toggle="collapse" href="#diffs">{num_diff} File{ds} with Diffs</a>
     </h4>
    </div>
    <div id="diffs" class="panel-collapse collapse">
     <div class="panel-body">
      <ul class="list-group">
       {diff_content}
      </ul>
     </div>
    </div>
   </div>
  </div>
 </div></body>
</html>
"""

    def check_all_regressions(self, base_testfiles: Path, mod_testfiles: Path, bundle_root: Path) -> bool:
        any_diffs = False
        root_index_files_no_diff = []
        root_index_files_diffs = []
        for baseline in base_testfiles.iterdir():
            if not baseline.is_dir():
                continue
            if baseline.name == 'CMakeFiles':  # add more ignore dirs here
                continue
            modified = mod_testfiles / baseline.name
            if not modified.exists():
                continue  # TODO: Should we warn that it is missing?
            entry, diffs = self.single_file_regressions(baseline, modified)
            if diffs:
                root_index_files_diffs.append(baseline.name)
                any_diffs = True
                potential_diff_files = baseline.glob("*.*.*")  # TODO: Could try to get this from the regression tool
                target_dir_for_this_file_diffs = bundle_root / baseline.name
                if potential_diff_files:
                    target_dir_for_this_file_diffs.mkdir()
                    index_contents_this_file = ""
                    for potential_diff_file in potential_diff_files:
                        copy(potential_diff_file, target_dir_for_this_file_diffs)
                        diff_file_with_html = target_dir_for_this_file_diffs / (potential_diff_file.name + '.html')
                        if potential_diff_file.name.endswith('.htm'):
                            # already a html file, just upload the raw contents but renamed as ...htm.html
                            copy(potential_diff_file, diff_file_with_html)
                        else:
                            # it's not an HTML file, wrap it inside an HTML wrapper in a temp file and send it
                            contents = potential_diff_file.read_text()
                            wrapped_contents = self.single_diff_html(contents)
                            diff_file_with_html.write_text(wrapped_contents)
                        index_contents_this_file += self.regression_row_in_single_test_case_html(potential_diff_file.name)
                    index_file = target_dir_for_this_file_diffs / 'index.html'
                    index_this_file = self.single_test_case_html(index_contents_this_file)
                    index_file.write_text(index_this_file)
                so_far = ' Diffs! ' if any_diffs else 'No diffs'
                print(f"Diff status so far: {so_far}, this file HAZ DIFFS: {baseline.name}")
            else:
                root_index_files_no_diff.append(baseline.name)
                so_far = ' Diffs! ' if any_diffs else 'No diffs'
                print(f"Diff status so far: {so_far}, this file has no diffs: {baseline.name}")
        bundle_root_index_file_path = bundle_root / 'index.html'
        bundle_root_index_content = self.bundle_root_index_html(
            ['hello', 'world'], root_index_files_no_diff, root_index_files_diffs
        )
        bundle_root_index_file_path.write_text(bundle_root_index_content)

            # print(f"*** Regressions for file {baseline.name}: {diffs=}")
            # print(f"{json.dumps(entry.to_dict(), indent=4)}\n")
            # if not any_diffs:
            #     print(f"*** No regressions or failures found for {baseline.name}\n")
        return any_diffs


if __name__ == "__main__":  # pragma: no cover - testing function, not the __main__ entry point

    if len(sys.argv) < 4:
        print("syntax: %s base_dir mod_dir regression_dir" % sys.argv[0])
        sys.exit(1)
    arg_base_dir = Path(sys.argv[1])
    arg_mod_dir = Path(sys.argv[2])
    arg_regression_dir = Path(sys.argv[3])
    rm = RegressionManager()
    response = rm.check_all_regressions(arg_base_dir, arg_mod_dir, arg_regression_dir)
    sys.exit(1 if response else 0)
