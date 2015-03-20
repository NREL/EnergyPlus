#!/usr/bin/python
from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function
import sys
import io
import git
import github
import os

# this date needs to be updated with the date of the previous release
LastReleaseDate = '2014-9-30'

# this probably won't change
RepoName = "NREL/EnergyPlus"
EPlusRepoPath = 'https://github.com/' + RepoName


def usage():
    print("""Script should be called with 4 positional arguments:
 - the path to a repository 
 - the path to a markdown output file
 - the path to a html output file
 - a github token for performing authentication API requests
 - and optionally a "Y" for enabling debug mode""")

# command line arguments: the path to the repo base, output markdown and html file paths, and a github token
if len(sys.argv) == 5:
    repo = sys.argv[1]
    md_file = sys.argv[2]
    html_file = sys.argv[3]
    github_token = sys.argv[4]
    debug = False
elif len(sys.argv) == 6:
    repo = sys.argv[1]
    md_file = sys.argv[2]
    html_file = sys.argv[3]
    github_token = sys.argv[4]
    if sys.argv[5] == "Y":
        debug = True
    else:
        print("Bad debug flag, should be \"Y\" or not passed at all")
        usage()
        sys.exit(1)
else:
    usage()
    sys.exit(1)

# get the pull request numbers
g = git.Git(repo)
log_full = io.open(os.path.join(repo, "doc", "tools", "TESTGITLOG")).read() #g.log('--oneline', '--after="' + LastReleaseDate + '"')
log_full_split = log_full.split('\n')
log_merge_prs = [x for x in log_full_split if 'Merge pull request' in x]
pr_tokens = [x.split(' ')[4] for x in log_merge_prs]
pr_numbers = sorted([x[1:] for x in pr_tokens])

# create and initialize the master array, with known keys plus an "Unknown" key
ValidPRTypes = ['Defect', 'NewFeature', 'Performance', 'DoNotPublish']
PRS = {'Unknown': []}
for valid_pr_type in ValidPRTypes:
    PRS[valid_pr_type] = []

# use the GitHub API to get pull request info
g = github.Github(github_token)
repo = g.get_repo(RepoName)
for pr_num in pr_numbers:
    this_pr = repo.get_issue(int(pr_num))
    if len(this_pr.labels) != 1:
        print(" +++ AutoDocs: %s,%s,Pull request has wrong number of labels...expected 1" % (pr_num, this_pr.title))
    else:
        key = 'Unknown'
        if this_pr.labels[0].name in ValidPRTypes:
            key = this_pr.labels[0].name
        PRS[key].append([pr_num, this_pr.title])
        # print("%s,%s,%s" % (pr_num, this_pr.title, this_pr.labels[0].name))

# Now write the nice markdown output file
with io.open(md_file, 'w') as f:
    def out(s):
        print(s, file=f)

    def out_pr_class(pr_type, descriptor):
        out('')
        out('## ' + descriptor)
        for pr in PRS[pr_type]:
            out(' - [#' + pr[0] + '](' + EPlusRepoPath + '/pull/' + pr[0] + ') : ' + pr[1])

    out('# ChangeLog')
    out('Consists of pull requests merged in GitHub since the last release.')
    out_pr_class('NewFeature', 'New Features')
    out_pr_class('Performance', 'Performance Enhancements')
    out_pr_class('Defect', 'Defects Repaired')
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
    out('<h1>EnergyPlus ChangeLog</h1>')
    out('This file is auto-generated from merged pull requests on GitHub.')
    out_pr_class('NewFeature', 'New Features')
    out_pr_class('Performance', 'Performance Enhancements')
    out_pr_class('Defect', 'Defects Repaired')
    out('</body>')
    out('</html>')

print(" +++ AutoDocs: Completed processing changelog: processed %i merged pull requests" % len(pr_numbers))
