# Input Data Dictionary

The input data dictionary specifies the "definitions" for each line that will be processed in the input data file.

Structure in the input data dictionary allows for descriptions that may be useful for interface developers.  The Input Processor ignores everything but the essentials for getting the "right stuff" into the program.  Developers have been (and will continue to be) encouraged to include comments and other documentation in the IDD.

Internal to the data dictionary (using special "comment" characters) is a structured set of conventions for including information on each object.  This is shown in section on Input Details below.

## Rules specific to the Input Data Dictionary

In addition to the rules for both files (listed above), the IDD also has the limitation:

- Duplicate Section names and Duplicate Class names are not allowed.  That is, the first class of an item named X will be the one used during processing.  Error messages will appear if you try to duplicate definitions.