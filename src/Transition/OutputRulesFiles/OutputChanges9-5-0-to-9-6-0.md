Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### Surface Inside Face Convection Classification Index

For some configurations, this can report a "0", which represents and uninitialized condition. Internal refactoring has changed this invalid, uninitialized condition to "-1". This condition will be investigated and eliminated in the future.

### Surface Outside Face Convection Classification Index

For some configurations, this can report a "0", which represents and uninitialized condition. Internal refactoring has changed this invalid, uninitialized condition to "-1". This condition will be investigated and eliminated in the future.
