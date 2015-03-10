# Example Error Messages for the Input Processor

The InputProcessor is a part of the EnergyPlus program and scans each input file, matching it against requirements from the Energy+.idd file (Input Data Dictionary). InputProcessor errors all start with IP as their first characters.

## Warning

IP: Note -- Some missing fields have been filled with defaults. See the audit output file for details.

This message notes that you have some objects where the "min-fields" for the object have not been fulfilled and, therefore, the object will be filled with defaults. If you are curious, open the .audit file and search for Warnings.

## Severe

IP: IDF line~345 Did not find "UNTIL: 22:00" in list of Objects

You may have entered a semi-colon character (;) at the end of one of the lines in a  Schedule:Compact when you meant to enter a comma (,). Note that the approximate line number in your file (345) is given to help you locate it in a text editor. Look in the prior line – it probably needs to end in a comma.

IP: IDF line~xxx Did not find "xxxxxx" in list of Objects

Same basic description as the previous error message.  The line number in your file is given to help you locate it.  Look in the prior line (ignoring any comment lines) – it probably needs to end with a comma.

IP: No items found for Required Object=BUILDING

IP: Required Object="BUILDING" not found in IDF.

The Building object is required for all inputs.  It was not found in this input file.

IP: No items found for Required Object=GLOBALGEOMETRYRULES

IP: Required Object="GLOBALGEOMETRYRULES" not found in IDF.

The GlobalGeometryRules object is required for all inputs. It was not found in this input file.

IP: Possible incorrect IDD File

IDD Version:"IDD_Version xxx"

Possible Invalid Numerics or other problems

This message means the program is about to terminate. You look at previous error messages in the .err file to determine the most likely cause(s). The IDD version number is given in case you have an "x" version file and you are running it with a "y" version IDD (which may or may not work, in general).

## Fatal

IP: Errors occurred on processing IDF file. Preceding condition(s) cause termination.

Just the final note before the program terminates. Look at previous error messages in the .err file.