#!/usr/bin/python
from __future__ import print_function
import sys
import git
import getpass
import urllib2
import github

# this date needs to be updated with the date of the previous release
LastReleaseDate = '2014-9-30'

# this probably won't change
EPlusRepoPath = 'http://github.com/NREL/EnergyPlus'

# two command line arguments: the path to the repo base, and an output markdown file
repo = ""
md_file = ""
debug = False
def usage():
	print("""Script should be called with 2 or 3 positional arguments: 
 - the path to a repository 
 - the path to a markdown output file
 - and optionally a "Y" for enabling debug mode""")
if len(sys.argv) == 3:
	repo = sys.argv[1]
	md_file = sys.argv[2]
elif len(sys.argv) == 4:
	repo = sys.argv[1]
	md_file = sys.argv[2]
	if sys.argv[3] == "Y":
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
log_full = g.log('--oneline','--after="' + LastReleaseDate + '"')
log_full_split = log_full.split('\n')
log_mergeprs = [x for x in log_full_split if 'Merge pull request' in x] 
pr_tokens = [x.split(' ')[4] for x in log_mergeprs]
pr_numbers = sorted([x[1:] for x in pr_tokens])

# create and initialize the master array, with known keys plus an "Unknown" key
ValidPRTypes = ['Defect', 'NewFeature', 'Performance', 'DoNotPublish']
PRS = {}
for key in set(ValidPRTypes) | set(['Unknown']):
	PRS[key] = []

# use the GitHub API to find the EnergyPlus repo...probably a better way than this
username = raw_input('GitHub username: ')
password = getpass.getpass('GitHub password: ')
g = github.Github(username, password)
for org in g.get_user().get_orgs():
	if org.name == "National Renewable Energy Laboratory":
		for repo in org.get_repos():
			if repo.name == "EnergyPlus":
				for pr_num in pr_numbers:
					this_pr = repo.get_issue(int(pr_num))
					if len(this_pr.labels) != 1:
						print(" +++ AutoDocs: %s,%s,Pull request has wrong number of labels...expected 1" % (pr_num, this_pr.title))
					else:
						key = 'Unknown'
						if this_pr.labels[0].name in ValidPRTypes:
							key = this_pr.labels[0].name
						PRS[key].append([pr_num,this_pr.title])
						#print("%s,%s,%s" % (pr_num, this_pr.title, this_pr.labels[0].name))

# Now write the nice markdown output file
with open(md_file, 'w') as f:
	def out(s):
		print(s, file=f)
	def outPRClass(key, descriptor):
		out('')
		out('## ' + descriptor)
		for pr in PRS[key]:
			out(' - [#' + pr[0] + '](' + EPlusRepoPath + '/pull/' + pr[0] + ') : ' + pr[1])
	out('# ChangeLog')
	out('Consists of pull requests merged in GitHub since the last release.')
	outPRClass('NewFeature', 'New Features')
	outPRClass('Performance', 'Performance Enhancements')
	outPRClass('Defect', 'Defects Repaired')
	if debug:
		outPRClass('Unknown', 'Other-DevelopersFixPlease')

with open('/tmp/changelog.html', 'w') as f:
	def out(s):
		print(s, file=f)
	def outPRClass(key, descriptor):
		out('<h2>' + descriptor + '</h2>')
		out('<table border="1" >')
		out(' <tr>')
		out('  <th>PR #</th>')
		out('  <th>Description</th>')
		out(' </tr>')
		for pr in PRS[key]:
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
	outPRClass('NewFeature', 'New Features')
	outPRClass('Performance', 'Performance Enhancements')
	outPRClass('Defect', 'Defects Repaired')
	out('</body>')
	out('</html>')

print(" +++ AutoDocs: Completed processing changelog: processed %i merged pull requests" % len(PRS))
