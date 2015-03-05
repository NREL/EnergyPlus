# View Factor Calculation Program

EnergyPlus has the capability of accepting user defined view factors for special research situations. This option is not recommended for general use because the normal approximate view factor determination within EnergyPlus accounts for such practical things as thermal mass (furniture), and realistic exchange limits. However, when a user desires to supply view factors, this auxiliary program can be used to calculate them for a variety of configurations. The program is named View3D, and was developed at NIST (1). This document will describe how to use the program.

# Associated Files

Three files come with the auxiliary view factor package. They are:

- View3D.exe
- ViewFactorInterface.xls
- View3D32.doc

The first is the executable program that calculates the view factors. The second is an excel interface that will set up the input files and execute View3D.exe. The third file is the documentation file from NIST that contains some explanation of the program.