How to compile and run EnergyPlus with the ExternalInterface (e.g. on Windows 32 bit)

Compilation

to compile EnergyPlus with the ExternalInterface, following libraries need to be included in the project:

1) bcvtb.lib located under SourceCode\ExternalInterface\BCVTB\Windows\Release\32\

2) fmi.lib located under SourceCode\ExternalInterface\FMI\Windows\Release\32\

Debug/Run

To debug or run EnergyPlus.exe, following dlls need to be in the same folder as EnergPlus.exe

1) bcvtb.dll located under SourceCode\ExternalInterface\BCVTB\Windows\Release\32\

2) epexpat.dll located under SourceCode\ExternalInterface\BCVTB\Windows\Release\32\

3) fmi.dll located under SourceCode\ExternalInterface\FMI\Windows\Release\32\

4) libexpatMT.dll located under SourceCode\ExternalInterface\FMI\Windows\Release\32\