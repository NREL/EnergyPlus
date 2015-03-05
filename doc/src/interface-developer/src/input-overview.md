# Input Overview

The general structure of the input files for EnergyPlus is plain text.  Fields are comma delimited and each "line" is terminated with a semicolon.  This allows for a very rudimentary input processor that can be instantly flexible to developer's needs.  However, it puts more burdens on the EnergyPlus developers to process the input information, supply defaults as needed, and perform validity checks.  Guidelines were established for the input:

- Input will be a flat ASCII file with comma-delimited columns and each "line" (where each line can run over several physical file records) terminated with a semicolon.
- Input should be "readable", "editable", "simply parsed with few value checks or consistency checks".
- Input, to the extent possible, should be easily maintainable and extendable.
- Input will be "object based".
- Definitions in a data dictionary will define the input.  The data dictionary should be self-documenting.
- All input units will be metric (SI).  Conversions from "user units" will be done in the interface agents.

Two input files are necessary for the input processing.  The first is the "data dictionary" which will specify the requirements for each item.  The EnergyPlus Input Processor uses these requirements to process the "input data file" and report any anomalies found.  Both input files have similar structures: 1) Sections – single lines/commands, which may help group the simulation input for readability and 2) Classes/Objects – data attributes for the simulation.  Classes are the term used in the data dictionary – each class will specify the kind of data (alpha or numeric) that will be included in the simulation input.  Objects are instances of these classes and appear in the IDF with appropriate data.