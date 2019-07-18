// Associate file types

function Component()
{
  Component.prototype.createOperations = function()
  {
    // call default implementation
    component.createOperations();

    // ... add custom operations

    var kernel = systemInfo.kernelType;

    // On Windows
    if( kernel == "winnt" ) {

      // Note JM: you have to quote the %1 which represents the file path, otherwise any space in the path will think there are multiple args
      component.addElevatedOperation("RegisterFileType", "idf", "@TargetDir@/EP-Launch.exe \"%1\"", "EnergyPlus Input Data File", "text/plain", "@TargetDir@/EP-Launch.exe,1");
      component.addElevatedOperation("RegisterFileType", "imf", "@TargetDir@/EP-Launch.exe \"%1\"", "EnergyPlus Input Macro File", "text/plain", "@TargetDir@/EP-Launch.exe,1");
      component.addElevatedOperation("RegisterFileType", "epg", "@TargetDir@/EP-Launch.exe \"%1\"", "EnergyPlus Group File", "text/plain", "@TargetDir@/EP-Launch.exe,1");

      component.addElevatedOperation("RegisterFileType", "ddy", "@TargetDir@/IDFEditor.exe \"%1\"", "EnergyPlus Location and Design Day Data", "text/plain", "@TargetDir@/IDFEditor.exe,1");
      component.addElevatedOperation("RegisterFileType", "expidf", "@TargetDir@/IDFEditor.exe \"%1\"", "EnergyPlus Expand Objects Input Data File", "text/plain", "@TargetDir@/IDFEditor.exe,1");

    }
  }
}
