Pull request overview
---------------------
Please change this line to a description of the pull request, with useful supporting information including whether it is a new feature, or fixes a defect, a cross reference to any defects addressed in this PR, the type of changes to be made, and whether diffs are expected/justified based on this change.

### Work Checklist
Add to this list or remove from it as applicable.  This is a simple templated set of guidelines.
 - [ ] Title of PR should be user-synopsis style (clearly understandable in a standalone changelog context)
 - [ ] At least one of the following appropriate labels must be added to this PR to be consumed into the changelog:
   - Defect: This pull request repairs a github defect issue.  The github issue should be referenced in the PR description
   - Refactoring: This pull request includes code changes that don't change the functionality of the program, just perform refactoring
   - NewFeature: This pull request includes code to add a new feature to EnergyPlus
   - Performance: This pull request includes code changes that are directed at improving the runtime performance of EnergyPlus
   - DoNoPublish: This pull request includes changes that shouldn't be included in the changelog

### Review Checklist
This will not be exhaustively relevant to every PR.
 - [ ] Code style (parentheses padding, variable names)
 - [ ] Functional code review (it has to work!)
 - [ ] If defect, results of running current develop vs this branch should exhibit the fix
 - [ ] CI status: all green or justified
 - [ ] Performance: CI Linux results include performance check -- verify this
 - [ ] Unit Test(s)
 - C++ checks:
   - [ ] Argument types
   - [ ] If any virtual classes, ensure virtual destructor included, other things
 - IDD changes:
   - [ ] Verify naming conventions and styles, memos and notes and defaults
   - [ ] If transition, add rules to spreadsheet
   - [ ] If transition, add transition source
   - [ ] If transition, update idfs
 - [ ] If new idf included, locally check the err file and other outputs
 - [ ] Documentation changes in place
 - [ ] ExpandObjects changes??
 - [ ] If output changes, including tabular output structure, add to output rules file for interfaces
