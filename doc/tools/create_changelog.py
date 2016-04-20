#!/usr/bin/python
from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function
import sys
import io
from subprocess import check_output, CalledProcessError
import json
try:
    from urllib import urlencode
except ImportError:
    from urllib.parse import urlencode
try:
    from urllib2 import Request, urlopen
except ImportError:
    from urllib.request import Request, urlopen

# this probably won't change
RepoName = "NREL/EnergyPlus"
EPlusRepoPath = 'https://github.com/' + RepoName
debug = False

def usage():
    print("""Script should be called with 8 positional arguments:
 - the path to a repository
 - the path to a markdown output file
 - the path to a html output file
 - the path to a local git executable
 - a github token for performing authentication API requests
 - the commit SHA for the last major release
 - a program version identifier""")

# command line arguments: repo base path, output markdown and html file paths, a git exe path, and a github token
if len(sys.argv) == 7:
    repo = sys.argv[1]
    md_file = sys.argv[2]
    html_file = sys.argv[3]
    git_exe = sys.argv[4]
    github_token = sys.argv[5]
    last_commit = sys.argv[6]
    program_version = sys.argv[7]
elif len(sys.argv) == 8:
    repo = sys.argv[1]
    md_file = sys.argv[2]
    html_file = sys.argv[3]
    git_exe = sys.argv[4]
    github_token = sys.argv[5]
    last_commit = sys.argv[6]
    program_version = sys.argv[7]
else:
    usage()
    sys.exit(1)

# get the pull request numbers
try:
    log_full = check_output([git_exe, 'log', last_commit+'..']).decode('utf-8')
except CalledProcessError as ex:
    log_full = ''
    pass  # add error handling
log_full_split = log_full.split('\n')
log_merge_prs = [x for x in log_full_split if 'Merge pull request' in x]
pr_tokens = [x.split(' ')[7] for x in log_merge_prs]
pr_numbers = sorted([x[1:] for x in pr_tokens])

# create and initialize the master array, with known keys plus an "Unknown" key
ValidPRTypes = ['Defect', 'NewFeature', 'Performance', 'Refactoring', 'DoNotPublish']
PRS = {'Unknown': []}
for valid_pr_type in ValidPRTypes:
    PRS[valid_pr_type] = []

query_args = urlencode({'access_token': github_token})
# use the GitHub API to get pull request info
for pr_num in pr_numbers:

    # we need to skip very low numbers of pull requests, for example:
    # - a user wants to contribute a change to E+, so they create a fork/branch
    # - their operations result in a pull request into their own repo, so the counting starts at #1...
    # we're at like 5000+, so if we just skip anything less than 1000, we'll be good.
    # And look, I am even using lambdas to prove I don't hate them
    expected_good_num = lambda n : int(pr_num) < 1000
    if expected_good_num(pr_num):
        continue

    # set the url for this pull request
    github_url = "https://api.github.com/repos/NREL/EnergyPlus/issues/" + pr_num + '?' + query_args

    # make the request
    try:
        req = Request(github_url)
        response = urlopen(req)
        the_page = response.read().decode('utf-8')
    except Exception as e:
        print(str(e))

    # read the json response
    j = json.loads(the_page)

    # mine the data
    title = j['title']
    labels = j['labels']
    for label in labels:
		key = 'Unknown'
		label_name = label['name']
		if label_name in ValidPRTypes:
			PRS[label_name].append([pr_num, title])

# Now write the nice markdown output file
with io.open(md_file, 'w') as f:
    def out(s):
        print(s, file=f)

    def out_pr_class(pr_type, descriptor):
        out('')
        out('## ' + descriptor)
        for pr in PRS[pr_type]:
            out(' - [#' + pr[0] + '](' + EPlusRepoPath + '/pull/' + pr[0] + ') : ' + pr[1])

    out('# Changelog for EnergyPlus ' + program_version)
    out('Consists of pull requests merged in since the last release - starting with SHA [' + last_commit + '](https://github.com/' + RepoName + '/commit/' + last_commit + ')')
    out_pr_class('NewFeature', 'New Features')
    out_pr_class('Performance', 'Performance Enhancements')
    out_pr_class('Defect', 'Defects Repaired')
    out_pr_class('Refactoring', 'Under the Hood Restructuring')
    if debug:
        out_pr_class('Unknown', 'Other-DevelopersFixPlease')

with io.open(html_file, 'w') as f2:
    def out(s):
        print(s, file=f2)

    def out_pr_class(pr_type, descriptor):
        out('<h2>' + descriptor + '</h2>')
        out('<table border="1" >')
        out(' <tr>')
        out('  <th>PR #</th>')
        out('  <th>Description</th>')
        out(' </tr>')
        for pr in PRS[pr_type]:
            out(' <tr>')
            out('  <td><a href=\"' + EPlusRepoPath + '/pull/' + pr[0] + '\">' + pr[0] + '</a></td>')
            out('  <td>' + pr[1] + '</td>')
            out(' </tr>')
        out('</table>')

    out('<html>')
    out('<head><title>EnergyPlus ChangeLog</title></head>')
    out('<body>')
    out('<style>')
    out('table, th, td {')
    out(' border: 1px solid black;')
    out(' border-collapse: collapse;')
    out('}')
    out('th,td {')
    out(' padding: 6px;')
    out('}')
    out('</style>')
    out('<h1>ChangeLog for EnergyPlus ' + program_version + '</h1>')
    out('<h1>Consists of pull requests merged in since the last release - starting with SHA <a href = "https://github.com/' + RepoName + '/commit/' + last_commit + '">' + last_commit + '</a>' + '</h1>')
    out_pr_class('NewFeature', 'New Features')
    out_pr_class('Performance', 'Performance Enhancements')
    out_pr_class('Defect', 'Defects Repaired')
    out_pr_class('Refactoring', 'Under the Hood Restructuring')
    out('</body>')
    out('</html>')

print(" +++ AutoDocs: Completed processing changelog: processed %i merged pull requests" % len(pr_numbers))
