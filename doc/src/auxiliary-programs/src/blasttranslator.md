# BLASTTranslator

The BLAST Translator will produce an IDF file from an existing BLAST Input File (usually called <something>.bin. For anyone that is unfamiliar, BLAST stands for the Building Loads Analysis and Systems Thermodynamics computer program.  Documentation is included here though the BLAST Translator is no longer included with the EnergyPlus Installation – it is available as a special download for those who need it.

A special batch file (**RunXLate.bat**) can be used to run the translation program. Similar to running EnergyPlus (see above), you run this batch file:

RunXLate <blastinputfile>

Where <blastinputfile> is the part of the file name without extension. The program produces a .idf file of the same name.

The BLASTTranslator uses an "Energy+.ini" file for some parameters. For example:

~~~~~~~~~~~~~~~~~~~~
    [program]
    dir=
    ver=
    surf=group

    [weather]
    dir=
~~~~~~~~~~~~~~~~~~~~

The above BLASTTranslator ini file sets the "version" of EnergyPlus to the current version and has the "surf" parameter set to "group". This BLASTTranslator run will produce an EnergyPlus input file for the current release version format and will name surfaces by Zone and Class (e.g. ZN001:Wall001). The alternative "Consecutive" will number surfaces in sequence.
