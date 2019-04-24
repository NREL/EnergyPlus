#!/usr/bin/env python

# This verifies PR labels - note it won't do anything if the current branch is not on an open PR
# CI checks out the branch as a detached head state, not a proper branch checkout, so you can't check branch name
# Instead I am checking the current checked out SHA vs the PR head commit SHA, should be good enough

import json
import subprocess
# this should be Python 2/3 compliant
try:
    from urllib.request import urlopen
except ImportError:
    from urllib2 import urlopen

# get the current branch name from this checkout
current_sha = subprocess.check_output(['git', 'rev-parse', 'HEAD']).strip()

# get the full set of open PRs at the moment
response = urlopen('https://api.github.com/repos/NREL/EnergyPlus/pulls')
pulls = json.loads(response.read())

# every PR must have one of these labels or it is a warning
required_labels = ['Defect', 'NewFeature', 'Performance', 'Refactoring', 'DoNotPublish']

# loop over each PR, try to match the HEAD SHA with the current SHA, and verify the labels are correct
for p in pulls:
    this_pr_head_sha = p['head']['sha']
    # could check something here to differentiate NREL:develop vs Fork:develop
    if this_pr_head_sha == current_sha:
        labels = [l['name'] for l in p['labels']]
        if not any([l in required_labels for l in labels]):
            print(json.dumps({
                'tool': 'check_required_labels.py',
                'filename': 'Pull Request # ' + str(p['number']),
                'line': 0,
                'messagetype': 'warning',
                'message': 'PR was missing required labels'
            }))
