Output Change Documentation
===========================

While the input of EnergyPlus has changes in each release, the output side does not move as often.  This isn't referring to the numeric output of EnergyPlus, but rather the structure that is interpreted by other interfaces or scripts.  This folder will contain the rules files that describe any structural output changes to EnergyPlus between releases.  The format of this rules file is still being formed, so nothing is added yet.  To make sure the purpose of the rules file is understood, consider the following use cases for the file:

 - A report variable name changes: this is already documented in the ```Report Variables XYZ.csv``` file.
 - A graphical interface may read the html output of EnergyPlus in order to perform calculations or results presentation to the user.  The interface may expect the table headings in a certain form, and if this form changes -- even minorly -- it could break the interface's ability to display the output.  While it is understood that changes/improvements may occur, they need to be clearly documented so that interfaces can adopt the changes easily.
 - A graphical interface may read the sql output.  If the structure of this database changes, it could break interfaces as well.  

Some examples of changes that need to be documented:

 - A change in the header row of an html table
 - EIO file changes??

Some examples of changes that don't need to be documented:

 - Numeric output changes within tables due to bug fixes/improvements.

