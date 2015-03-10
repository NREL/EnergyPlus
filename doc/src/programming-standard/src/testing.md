# Testing

This section describes testing that is done by the developer; testing for program releases is described in a separate document.

The kind of testing that will be done by a developer is somewhat dependent on the type of development project undertaken and thus will always rely to some extent on the developer's good judgement. A straightforward case to discuss is the development of a new HVAC component module. The development and testing might proceed in the following stages.

- Specify the input with a Data Dictionary (IDD) entry. The new IDD can be tested for syntactical correctness by simply running one of the EnergyPlus test suite inputs using the new IDD.
- Design the module data structure and write the input (Get) subroutine. Create test input for the component and add it to an existing EnergyPlus input (IDF) file. Test that the input data is being correctly read by the input subroutine and stored correctly in the module data structures. This testing will be done in the debugger. Create informative error messages for incorrect user input.
- Write the rest of the component module code and test it in a standalone fashion.  This allows for rapid, repetitive testing that covers the range of possible component inputs. These tests will also be done in the debugger.
- Create a full EnergyPlus input incorporating the new component in a realistic manner. Sometimes you can modify an existing test suite file.
- Add the new EnergyPlus module to the full program. Using the debugger test that the new, full input is functioning correctly
- Using the EnergyPlus reporting capabilities and a spreadsheet, test that the new component is functioning as expected in a variety of conditions.
- Run the entire EnergyPlus test suite and make sure nothing has changed when the new component is not part of the input.
- Add one or more new test files to the EnergyPlus test suite.  These are really examples to the user for EnergyPlus Features.
- Document the test files using the documentation template (see below) as your guide.  Highlights of the input file is placed in the ExampleFiles.xls spreadsheet.
- Perform full annual runs on the test files that you will add as example files.
- Perform the "reverse DD" test – this requires that you put in two design days (preferably summer and winter); run once with "normal" design days; reverse the design days and run again.  The results (when you reverse the results for the second run) should be **exactly** the same.