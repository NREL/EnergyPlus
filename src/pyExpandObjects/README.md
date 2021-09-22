# pyExpandObjects

[![Documentation Status](https://readthedocs.org/projects/epjson-expandobjects/badge/?version=main)](https://epjson-expandobjects.readthedocs.io/en/main/?badge=main)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/john-grando/pyExpandObjects/Unit%20Tests)](https://github.com/john-grando/pyExpandObjects/actions)
[![Coverage Status](https://coveralls.io/repos/github/john-grando/pyExpandObjects/badge.svg?branch=main)](https://coveralls.io/github/john-grando/pyExpandObjects?branch=main)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/john-grando/pyExpandObjects/Flake8?label=pep8)](https://github.com/john-grando/pyExpandObjects/actions)

Support tool for the EnergyPlus simulation program which pre-processes epJSON files containing HVACTemplate objects.

#### Overview

Much like the existing ExpandObjects program that works with IDF files in EnergyPlus, pyExpandObjects expands HVACTemplate objects from a source file into an expanded file that can be directly run in EnergyPlus, but in this case the processed files are in epJSON format.  Additionally, in order to achieve greater alignment with other programming initiatives in EnergyPlus, take advantage of the epJSON schema structure, and make use of object-oriented programming methodologies, this program is written in Python.  This tool provides the same deliverable as its predecessor, which is an output file with regular objects that have been mapped from template objects, but in epJSON format.

#### General Instructions

This program is called via the command line with the following arguments:

* --file: The epJSON file containing HVACTemplate Objects.
* --output_directory: The directory to output expanded files.  If none is specified, the epJSON file directory is used.
* --no_backup: Prevent the creation of optional file outputs.
* --no_schema: Skip epJSON schema verification steps.
* --logger_level: Specify the level of logging output.  This follows the standard Python logging naming structure (e.g. DEBUG, WARN, etc.)

Unless `--no_backup` is specified, this program will output three files.  If `--no_backup` is specified, then only the last item will be created.

* original-file-name_base.epJSON: Contains all non HVACTemplate objects from original file
* original-file-name_hvac_templates.epJSON: Contains all HVACTemplate objects from original file
* original-file-name_expanded.epJSON: Expanded file for simulation.

#### Build Instructions

Option 1 - Executables can be created with PyInstaller by calling spec files.

`pyinstaller --upx-dir ./ --clean linux_onefile_main.spec`

* linux_onefile_main.spec - Used for linux based systems
* main.spec (future) - Used for windows based systems

Option 2 - A Dockerfile is provided to create installations using different operating systems.  An executable can be created and retrieved with the following:

* Build Instructions
    1. Install [Docker](https://docs.docker.com/get-docker/)
    2. Run `docker build -t ubuntu .` from the command line.
    3. Run `docker run -dit ubuntu` from the command line.
    4. A long alphanumeric string will be returned to show you the 'container-id'.  you can also pull this up with `docker ps`, which lists active docker containers.
    5. Run `docker cp <some-container-id-characters>:/home/project/pyExpandObjects/dist/pyExpandObjects /path/to/copy/on/your/machine` from the command line.

* Cleanup  
    Docker uses a lot of disk space when building resources because it saves each step of the process.  To recover this space, do the following after completing a build.

  1. Run `docker ps` to get a list of running containers.
  2. Run `docker stop <first-few-container-id-values> <second-container-id-if-necessary>`
  3. Run `docker system prune -a`.  This will clear all your images that are not in use (see step 2 to stop them).
  4. Confirm nothing is saved using `docker ps` and `docker images`.

#### Example Files

  Input and output example simulation files are provided within this package.

  * **simulation/ExampleFiles**  
    This directory contains files that are directly build from **_ExpandObjects_** for comparison to the outputs of this application.
      * HVACTemplate-<template-name>.idf - Directly copied from EnergyPlus ExampleFiles directory
      * HVACTemplate-<template-name>.epJSON - Converted base template
      * HVACTemplate-<template-name>Expanded.idf - Output of ExpandObjects on base template file
      * HVACTemplate-<template-name>Expanded.epJSON - Converted expanded template file

  * **simulation/ExampleOutputs**
    This directory contains the pyExpandObjects outputs of the conveted base templates in simulation/ExampleFiles (HVACTemplate-<template-name>.epJSON)
      * HVACTemplate-<template-name>_base.epJSON - Non-HVACTemplate objects of the base file
      * HVACTemplate-<template-name>_hvac_template.epJSON - HVACTemplate objects of the base file
      * HVACTemplate-<template-name>_expanded.epJSON - Output of pyExpandObjects on the base template file
