#!/usr/bin/python

# python 2/3 compatibility imports
from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function

# standard library imports
import sys
from os import path
import io
import glob

# local library imports
import idf_parser_library
import idd_parser_library

# 3 arguments: a directory to the idfs, and an output html file
if len(sys.argv) != 4:
    print("ERROR ARGUMENTS")
    sys.exit(1)
else:
    idd_file = sys.argv[1]
    idf_dir = sys.argv[2]
    html_file = sys.argv[3]

mainObjectDictionary = {}
totalReferences = {}

# process the idd first
this_idd = idd_parser_library.iddFile(idd_file)
idd_objects = sorted(this_idd.iddObjects)
for idd_obj in idd_objects:
    mainObjectDictionary[idd_obj.upper()] = []
    totalReferences[idd_obj.upper()] = 0

# then loop over the idfs and create dictionaries as needed
idf_set = sorted(glob.glob(path.join(idf_dir, '*.idf')))
idf_summaries = []
processing_idf_num = 0
for idf in idf_set:
    processing_idf_num += 1
    # filename only
    idf_name = path.basename(idf)
    # create an idf library class, and a summary instance
    this_idf = idf_parser_library.idfFile(idf)

    # loop over objects, processing stuff
    for idf_object in this_idf.idf_objects:
        upperObjName = idf_object.objectName.upper()
        if idf_name == 'GSHPSimple-GLHE.idf':
            pass  # print(upperObjName)
        if upperObjName not in mainObjectDictionary:
            # theres a few things we can ignore:
            if upperObjName in ["LEAD INPUT", "END LEAD INPUT", "END SIMULATION DATA", "REPORT VARIABLE DICTIONARY"]:
                continue
            print("***Found object not in IDD:")
            print("  IDF = " + idf_name)
            print("  OBJ = " + upperObjName)
            mainObjectDictionary[upperObjName] = []
            totalReferences[upperObjName] = 0
        else:
            # if the idf isn't already in the idf list, add it
            if idf_name not in mainObjectDictionary[upperObjName]:
                mainObjectDictionary[upperObjName].append(idf_name)
            # no matter what increment the total count
            totalReferences[upperObjName] += 1

with io.open(html_file, 'w') as f:
    def out(s):
        print(s, file=f)

    def header():
        out(' <tr>\n')
        for headerItem in [
            "ObjectName",
            "Number of Files with Object",
            "Total References in all files",
            "First File with Object",
            "Second File with Object",
            "Third File with Object"
        ]:
            out('  <th>' + headerItem + '</th>\n')
        out(' </tr>\n')

    out('<html>')
    out('<head><title>EnergyPlus Example File Objects Link</title></head>')
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
    out('<h1>EnergyPlus Example File Objects Link</h1>')
    out('This file is auto-generated from the objects and comments in the example files and idd in GitHub.')
    out('<table border="1" >')
    header()
    entry = 0
    for k in sorted(mainObjectDictionary.keys()):
        v = mainObjectDictionary[k]
        entry += 1
        if entry % 20 == 0:
            header()
        out(' <tr>\n')
        out('  <td>' + k + '</td>\n')
        out('  <td>' + str(len(v)) + '</td>\n')
        out('  <td>' + str(totalReferences[k]) + '</td>\n')
        idf_num = 0
        for idf in v:
            idf_num += 1
            out('  <td>' + idf + '</td>\n')
            if idf_num == 3:
                break
        for i in range(0, 3 - idf_num):
            out('  <td></td>\n')
    out('</table>')
    out('</body>')
    out('</html>')

print(" +++ AutoDocs: Completed processing example file links summary: processed %i example files" % processing_idf_num)
