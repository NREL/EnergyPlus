Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### Surface Inside Face Convection Classification Index

For some configurations, this can report a "0", which represents and uninitialized condition. Internal refactoring has changed this invalid, uninitialized condition to "-1". This condition will be investigated and eliminated in the future.

### Surface Outside Face Convection Classification Index

For some configurations, this can report a "0", which represents and uninitialized condition. Internal refactoring has changed this invalid, uninitialized condition to "-1". This condition will be investigated and eliminated in the future.

### Daylight Map Report now supports more than two reference points
The header of the map file was changed from only allowing two reference points to allow more than two.  The additional points are listed in comma separated order where there used to be only two points.  The format will show up in the header as something like the following:

“ RefPt1=(2.50:2.00:0.80), RefPt2=(2.50:18.00:0.80), RefPt3=(2.50:18.00:0.50)”

[PR#8889](https://github.com/NREL/EnergyPlus/pull/8889) changed the output format in both the MAP file and the SQL output.

