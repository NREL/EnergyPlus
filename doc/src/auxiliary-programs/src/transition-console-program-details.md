# Transition Console Program Details

For those who are interested, this is the detailed description of the Transition console applications including the current one. There are methods to use the program set from the command line; those details are not included in this document but available from EnergyPlus Support group.

TransitionV6-0-0-to-V7-0-0.exe is the current transition program that is distributed with the V7.0 release. It uses several important files that are included in the main "EnergyPlus" folder.

Table: Transition files for current release

File Name|Description
---------|-----------
TransitionV6-0-0-to-V7-0-0.exe|The executable program
V6-0-0-Energy+.idd|Version 6.0.0.037 release Energy+.idd file
V7-0-0-Energy+.idd|Identical to Energy+.idd when distributed
Report Variables 6-0-0-023 to 7-0-0.csv|Report variable name changes

 Another file "Rules6-0-0-to-7-0-0.xls" is not used directly by the program but contains the "rules" for translating objects from version 6.0 release to the 7.0 release. The ObjectStatus file can also be viewed – it will show if deleted objects are automatically transitioned to the new input file versions.

There are several methods to executing the transition program – these methods give you the most flexibility in changing files from one version to the next. The easiest transition is through the EP-Launch program which can detect if the input file about to be run is of the same version as the IDD or not and suggest transitioning. You can also manually transition from the file menu in EP-Launch. (To have this feature, you must also have the files from the preceding table in the same folder as EP-Launch – which is usually the folder that also has the EnergyPlus.exe program).

There are two command line methods to execute the transition version (from the Command Prompt). One is to simply use the file name you want transitioned (including .rvi or .mvi file names) or you can use a file name with a .lst extension and simply enter file names to be done in a text file. When you execute the transition program in this fashion, you will get the "typical" program defaults of a "full" transition, field names will be shown at each field with units, and any blank fields will be left blank rather than filled in with the current defaults.