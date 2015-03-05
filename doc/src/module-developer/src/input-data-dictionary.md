# Input Data Dictionary

An entry in the IDD consists of comma-separated text terminated by a semicolon. For instance:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Water,
           \min-fields 14
      A1 , \field Name
           \required-field
           \type alpha
           \reference HeatingCoilName
           \reference HeatingCoilsWater
      A2 , \field Availability Schedule Name
           \type object-list
           \object-list ScheduleNames
      N1 , \field U-Factor Times Area Value
           \units W/K
           \autosizable
           \default Autosize
      N2 , \field Maximum Water Flow Rate
           \units m3/s
           \autosizable
           \ip-units gal/min
           \default Autosize
      A3 , \field Water Inlet Node Name
           \required-field
      A4 , \field Water Outlet Node Name
           \required-field
      A5 , \field Air Inlet Node Name
           \required-field
      A6 , \field Air Outlet Node Name
           \required-field
      A7 , \field Performance Input Method
           \type Choice
           \key UFactorTimesAreaAndDesignWaterFlowRate
           \key NominalCapacity
           \default UFactorTimesAreaAndDesignWaterFlowRate
      N3 , \field Nominal Capacity
           \type real
           \units W
           \autosizable
           \minimum 0
           \default Autosize
      N4 , \field Design Inlet Water Temperature
           \units C
           \type real
           \default 82.2
      N5, \field Design Inlet Air Temperature
          \units C
          \type real
          \default 16.6
      N6, \field Design Outlet Water Temperature
          \units C
          \type real
          \default 71.1
      N7; \field Design Outlet Air Temperature
          \units C
          \type real
          \default 32.2
~~~~~~~~~~~~~~~~~~~~

This entry defines a simple water-heating coil and specifies all of the input data needed to model it. The following rules apply.

The first element Coil:Heating:Water is the class name (also called a keyword or key).  This class name must be unique in the IDD.  The maximum length for the class name is 100 characters.  Embedded spaces are allowed and are significant.

In most cases, one should have fields following the object name.  An object name by itself (terminated with a semicolon) is a "section" – there may be uses for sections in input but the "Getting" of input is not hierarchical – one typically gets all objects of one type and then all objects of the next type.

In most cases, the second field of an object should be an "alpha" and the field name should contain the word "name".  (This will allow for certain validations later on.)

Commas separate fields.  They always act as separators – thus there is no way to include a comma in a class name or as part of a data field.

Similarly, semicolons are terminators – a semicolon is always interpreted as the end of an EnergyPlus "sentence". So, avoid embedded semicolons in class names or data fields.

Blank lines are allowed.

Each line can be up to 500 characters in length.

The comment character is an exclamation or a backslash.  Anything on a line after an "!" or a "\\" is ignored during EnergyPlus input.

The only significant syntax elements are the commas, the semicolon, the N's (denoting numeric data), and the A's (denoting alphanumeric data) and the exclamation and backslash.  Everything else including blanks, end of lines, or even text that is not a comma, semicolon, N, or A is ignored.  There are several style conventions in use however.

Sequence numbers are appended to the letters A or N denoting each data element.  Thus, A2 is the second alphanumeric data item and N3 is the third numeric data item.

The class name contains a naming convention: type:subtype:subsubtype. For further naming conventions, please see the next section of this document.

Backslashes denote specially formatted comments. These comments provide information about the input, such as a description of the item, units, limits, mins & maxes, etc., in a form that can be processed by an input editor or interface. A complete description of the backslash comment format is given at the start of the IDD file and in the Guide for Interface Developers.  While these are "comments", they are quite important and allow the InputProcessor module to do some error checking for you.   They are also used by the IDFEditor that many users continue to use and by interface developers in understanding EnergyPlus.

- **\\default** – the number (N fields) or phrase (A fields) after this special field will be filled for any input file that has a blank in that field.
- **\\minimum** or **\\minimum>** -- the number following this special field will be automatically checked during input
- **\\maximum** or **\\maximum<** -- the number following this special field will be automatically checked during input
- **\\extensible:#** – allows you to structure your GetInput routine so that the object arguments can be expanded (you include the number of fields in the "extension" and the Input Processor can automatically extend IDD definitions) – you will still need to determine how many maximum arguments are in the object. The IDF Editor does not use this field and cannot auto-extend such objects if an IDF is encountered that has a greater number of fields than IDD allows.
- **\\type integer** – (or **real** or **alpha**) – this field has gained increased importance after a user kept hitting an internal maximum detected by the program and kept increasing their input number until it overflowed the system's integer size. Until all types are shown on numeric fields it will be hard for the InputProcessor to provide proper error detection.
- **There are many more \\ fields** – these are described at the top of the IDD.

Overall, the IDD file has very little structure.  Generally, a new entry should be placed next to entries describing similar components.  *Coil:Heating:Water*, for instance, is grouped with entries describing other water coils.

**Summary**

One of the early tasks for a module developer is to create a new entry in the Input Data Dictionary.  This entry defines the data needed to model the new component.