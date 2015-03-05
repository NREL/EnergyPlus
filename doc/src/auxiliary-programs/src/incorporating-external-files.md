# Incorporating External Files

**##include** {includefilename}

This command puts all of the lines in an external file into the EnergyPlus input stream starting right after the command line. The name of the file that is included is the concatenation of {prefixpathname}, entered using  **##fileprefix** , and {includefilename}. The lines in the external file will be listed in the resultant IDF file. When all the lines in the external file have been read in, input reverts back to the original input file at the line following the  **##include**  command.

**##fileprefix**  {prefixpathname}

specifies a pathname that will be prefixed to the filename given in an  **##include** command. The **##fileprefix**  command allows commonly-used include files to be kept in a directory other than the directory in which the current input file resides.

Example: on a PC, the combination

**##fileprefix**  C:\\EnergyPlus\\Library

**##include**  SCHEDULES.IDF

will include into the EnergyPlus input stream the file whose full name is

C:\\EnergyPlus\\Library\\SCHEDULES.IDF

**##includesilent**  {includefilename}

This command is identical to  **##include**, except that the lines in the included file will not be listed in the EP-MACRO echo.

**##nosilent**

Overrides the listing suppression of  **##includesilent**. Used for debugging purposes only. After  **##nosilent** , all following  **##includesilent**  commands are treated as  **##include** commands.

Example: Assume the following files contain the indicated lines:

-------------------------------------------
Main input file:           External file:
**input1.inp**              **file2.inp**
------------------------- -----------------
line 1a                    line 2a

**##include**  file2.inp   line 2b

line 1b                    line 2c

line 1c                    --
-------------------------------------------

The end result of processing **##include** input1.inp will be:

    line 1a(from input1.inp)

    line 2a(from file2.inp)

    line 2b(from file2.inp)

    line 2c(from file2.inp)

    line 1b(from input1.inp)

    line 1c(from input1.inp)

External files can also contain  **##include** commands, as shown in the following example:

Main input file:|First external file:|Second external file:
----------------|--------------------|---------------------
**input1.inp**|**file2.inp**|**file3.inp**
line 1a|line 2a|line 3a
**##include**  file2.inp|line 2b|line 3b
line 1b|**##include** file3.inp|line 3c
line 1c|line 2c|line 3d

The end result of processing **##include** input1.tmp will be:

    line 1a(from input1.inp)

    line 2a(from file2.inp)

    line 2b(from file2.inp)

    line 3a(from file3.inp)

    line 3b(from file3.inp)

    line 3c(from file3.inp)

    line 3d(from file3.inp)

    line 2c(from file2.inp)

    line 1b(from input1.inp)

    line 1c(from input1.inp)

Note: Up to nine **##include** commands can be nested. However, there should be no recursion. This is an example of a recursion:

file1.inp contains   **##include**   file2.inp

file2.inp contains   **##include**   file1.inp
