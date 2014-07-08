Continuous Integration is facilitated through the use of the [decent_ci](https://github.com/lefticus/decent_ci) system.

The files `.decent_ci.yaml`, `.decent_ci-Linux.yaml`, `.decent_ci-Windows.yaml` and `.decent_ci-MacOS.yaml` in the root
folder configure the build system. 

For building EnergyPlus, you need to set up the requirements for decent_ci, and follow its instructions here:

https://github.com/lefticus/decent_ci/blob/master/README.md

Additionally, EnergyPlus requires that the boto python package be installed to run regresion tests

`pip install boto`

## Windows Note

pip may not be installed if you are running Python 2.7. An upgrade to python 3.4 is acceptable from the standpoint
of EnergyPlus.

