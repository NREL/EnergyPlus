#!/usr/bin/python

import os
import subprocess
import shutil

# project settings
srcdir  = "../src/"

# prep the project for html generation
def WholeProjectPrep(main_src_dir):
	# project-wide handling of the concatenation, etc.
	# for now just adjust the mkdocs main yml file
	main_mkdocs_yml = os.path.join(main_src_dir, "mkdocs.yml")
	if os.path.exists(main_mkdocs_yml):
		os.remove(main_mkdocs_yml)
	shutil.copyfile(os.path.join(main_src_dir, "mkdocs_html.yml"), main_mkdocs_yml)

# Do any pre-processing for the whole project
print("***Preparing project")
WholeProjectPrep(srcdir)
print("***Preparing project complete")

# then build with mkdocs
# don't forget:
#  1. need to set python's sys.setrecursionlimit(50000) inside the actual /usr/local/bin/mkdocs run script
#  2. need to set kernel stack limit: 
#      - temporarily this could be done with:   ulimit -s 16384
#        but this only works if you remain in *that* shell
#      - it is easier to just add lines to /etc/security/limits.conf:
#            *                soft    stack           16384
#            *                hard    stack           16384
print("***Building with MKDocs")
os.chdir(srcdir)
subprocess.check_output(["mkdocs", "build", "--clean"])
print("***MKDocs build complete")
