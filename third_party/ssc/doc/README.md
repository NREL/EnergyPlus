# Instructions for Maintaining SSC Documentation

This document explains how to use Doxygen, and LaTeX to create the SSC API documentation.

## Overview


Generating the SSC documentation involves a number of steps, some of which are automated, and some of which are automatic:

* The SSC documentation consists of the 8 chapters in the ssc_guide.tex file.

* The content for Chapters 1-7 is in ssc_guide.tex. It was originally drafted by Aron Dobos and revised by Paul Gilman.

* Doxygen generates content for Chapter 8 (the file reference for sscapi.h} in sscapi_8h.tex from comments in sscapi.h.

* An include command in the ssc_guide.tex file inserts the content of sscapi_8h.tex into ssc_guide.tex.

* The settings in the Doxygen configuration file Doxyfile determine what is included in the function reference


Overall Steps for Maintaining Documentation
-------------------------------------------

1. As needed, revise content in doc/ssc_guide.tex and the Doxygen-formatted comments in ssc/sscapi.h.

2. Run ``doxygen`` on Doxyconfig to generate the latex folder with sscapi_8h.tex and other files

3. Run ``pdflatex`` on ssc_guide.tex to generate a PDF file -- run it twice to generate the table of contents, cross references, etc.
