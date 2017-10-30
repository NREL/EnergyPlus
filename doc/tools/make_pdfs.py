#!/usr/bin/python

import sys
import os
import glob
import subprocess
import random
import shutil

# requirements:
# runs well on Linux (Ubuntu 14.04) with Python 2.7
# wkhtmltopdf (with qt built-in)
# mkdocs

class SingleDocInfo():
	def __init__(self, src_dir, site_dir, doc_name, site_md_filename, js_delay, concatenate_function=None):
		self.doc_name = doc_name
		self.site_md_filename = site_md_filename
		self.src_dir = os.path.join(src_dir, "docs", doc_name)
		self.working_dir = os.path.join(site_dir, doc_name, site_md_filename)
		self.delay = js_delay
		self.concat_function = concatenate_function

class DocStateInfo():
	def __init__(self, index_in_pid_array, pid, doc_name):
		self.index = index_in_pid_array
		self.pid = pid
		self.doc_name = doc_name

# Specific concatenation functions
def IORefConcatenate(main_src_dir):
	# IORef is actually just a simple concatenation
	files = ["01a-InputOutputReference.md",
			"01b-InputOutputReference.md",
			"01c-InputOutputReference.md",
			"01d-InputOutputReference.md",
			"02-HVACTemplates.md",
			"03-Economics.md",
			"04-Parametrics.md",
			"05-InputForOutput.md",
			"06-StandardOutputReports.md",
			"07-WeatherData.md",
			"08-RunningEnergyPlus.md",
			"09-Appendix.md"]
	outfile = os.path.join(main_src_dir, "docs", "InputOutputReference", "IORef.md")
	with open(outfile,'w') as f_out:
		for filename in files:
			infile = os.path.join(main_src_dir, "docs", "InputOutputReference", filename)
			with open(infile) as f_in:
				for line in f_in:
					f_out.write(line)

def EngRefConcatenate(main_src_dir):
	# do some stuff to intelligently concatenate the EngRef pieces into one md file
	# also need to clean up the EngRef encyclopedia headings I added to each page
	files = ["01-Overview.md",
			"02-IntegratedSolution.md",
			"03-SurfaceHeatBalance.md",
			"04-AdvancedSurface.md",
			"05-Climate.md",
			"06-SolarRadiation.md",
			"07-Daylighting.md",
			"08-AirHeatBalance.md",
			"09-BuildingSystemSimulation.md",
			"10-Sizing.md",
			"11-DemandLimiting.md",
			"12-AlternativeModeling.md",
			"14-OnSiteGeneration.md",
			"15-PerformanceCurves.md",
			"16-Economics.md",
			"17-SpecialModulesReporting.md",
			"OperationalFaults.md",
			"13a-EncyclopaedicRefs.md",
			"13b-EncyclopaedicRefs.md",
			"13c-EncyclopaedicRefs.md",
			"13d-EncyclopaedicRefs.md",
			"13e-EncyclopaedicRefs.md",
			"13f-EncyclopaedicRefs.md"]
	outfile = os.path.join(main_src_dir, "docs", "EngineeringReference", "EngRef.md")
	with open(outfile,'w') as f_out:
		write = True
		for filename in files:
			infile = os.path.join(main_src_dir, "docs", "EngineeringReference", filename)
			with open(infile) as f_in:
				for line in f_in:
					if '<!--RemoveEnd-->' in line:
						write = True
						continue
					elif '<!--RemoveStart-->' in line:
						write = False
						continue
					elif write:
						f_out.write(line)

def WholeProjectPrep(main_src_dir):
	# project-wide handling of the concatenation, etc.
	# for now just adjust the mkdocs main yml file
	main_mkdocs_yml = os.path.join(main_src_dir, "mkdocs.yml")
	if os.path.exists(main_mkdocs_yml):
		os.remove(main_mkdocs_yml)
	shutil.copyfile(os.path.join(main_src_dir, "mkdocs_htmlpdf.yml"), main_mkdocs_yml)

# set up some local settings
srcdir  = "../src/"
sitedir = "../src/site_htmlpdf"
pdfdir  = "/tmp/pdfs"

# some other one-time settings
FNULL = open(os.devnull, 'w') # open dev/null for stderr

# then build with wkhtmltopdf
# spawn each individually
documents = []
documents.append(SingleDocInfo(srcdir, sitedir, "Acknowledgments", "Acknowledgments", 30000))
documents.append(SingleDocInfo(srcdir, sitedir, "AuxiliaryPrograms", "AuxiliaryPrograms", 30000))
documents.append(SingleDocInfo(srcdir, sitedir, "EMS_Application_Guide", "EMS_Application_Guide", 30000))
documents.append(SingleDocInfo(srcdir, sitedir, "EngineeringReference", "EngRef", 24000000, EngRefConcatenate))  # yes...18000000ms = 18000s = 5h ... don't need that much, but just let it go overnight
documents.append(SingleDocInfo(srcdir, sitedir, "ExternalInterfaces_Application_Guide", "ExternalInterfaces_Application_Guide", 30000))
documents.append(SingleDocInfo(srcdir, sitedir, "GettingStarted", "GettingStarted", 30000))
documents.append(SingleDocInfo(srcdir, sitedir, "InputOutputReference", "IORef", 300000, IORefConcatenate)) # 300000ms = 300s = 5m
documents.append(SingleDocInfo(srcdir, sitedir, "InterfaceDeveloper", "InterfaceDeveloper", 30000))
documents.append(SingleDocInfo(srcdir, sitedir, "ModuleDeveloper", "ModuleDeveloper", 30000))
documents.append(SingleDocInfo(srcdir, sitedir, "OutputDetailsAndExamples", "OutputDetailsAndExamples", 30000))
documents.append(SingleDocInfo(srcdir, sitedir, "PlantApplicationGuide", "PlantApplicationGuide", 30000))
documents.append(SingleDocInfo(srcdir, sitedir, "Tips_and_Tricks_Using_EnergyPlus", "Tips_and_Tricks_Using_EnergyPlus", 30000))
documents.append(SingleDocInfo(srcdir, sitedir, "Using_EnergyPlus_for_Compliance", "Using_EnergyPlus_for_Compliance", 30000))

# Do any pre-processing for the whole project
print("***Preparing project")
WholeProjectPrep(srcdir)
print("***Preparing project complete")

# Do any pre-processing on a per-document basis
print("***Preprocessing documents")
for document in documents:
	if document.concat_function:
		document.concat_function(srcdir)
print("***Preprocessing documents complete")

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

# then build pdfs
print("***Building PDFs with wkhtmltopdf")
pids=[]
docs_running = -1
for document in documents:
	docs_running += 1
	os.chdir
	args = []
	args.extend(["wkhtmltopdf"]) # The main executable name
	args.extend(["-q"]) # Be quiet
	args.extend(["--outline", "--outline-depth", "3"]) # Set up the bookmarks in the pdf
	args.extend(["--javascript-delay", "%i" % document.delay]) # Set the delay for javascript rendering
	args.extend(["--page-size", "Letter"]) # Set the page size
	#if document.doc_name in ["AuxiliaryPrograms","EngineeringReference", "InputOutputReference", "ModuleDeveloper","OutputDetailsAndExamples"]:
	#	args.extend(["--minimum-font-size", "16"]) # Do an override to get the font an appropriate size
	args.extend(["--margin-bottom", "20mm", "--margin-top", "20mm"]) # Top and bottom margins,
	args.extend(["--margin-left", "20mm", "--margin-right", "20mm"]) # Left and right margins
	args.extend(["--footer-font-size", "9", "--footer-line", "--footer-left", "EnergyPlus Documentation", "--footer-right", "Page [page] of [toPage]"]) # Set the footer
	args.extend(["index.html"]) # Input file name
	args.extend(["%s.pdf" % document.doc_name]) # Output file name
	p = subprocess.Popen(args, cwd=document.working_dir, stderr=FNULL)
	pids.append(DocStateInfo(docs_running, p.pid, document.doc_name))
while docs_running > -1:
	pid, retval = os.wait()
	index = [i.index for i in pids if i.pid == pid][0] # find the process that matches this pid
	print("***PDF Completed: %s  (%i left)" % (str(pids[index].doc_name), docs_running))
	docs_running -= 1
print("***PDF build complete")

print("***Storing pdfs")
if os.path.exists(pdfdir):
	shutil.rmtree(pdfdir)
os.makedirs(pdfdir)
for document in documents:
	src_pdf = os.path.join(sitedir, document.doc_name, document.site_md_filename, document.doc_name+".pdf")
	if os.path.exists(src_pdf):
		shutil.copy(src_pdf, pdfdir)
print("***PDFs stored in: " + pdfdir)

print("******Operations Complete******")
