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

      // Note JM: you normally have to quote the %1 which represents the file path, otherwise any space in the path will think there are multiple args
      // That is "@TargetDir@/EP-Launch.exe \"%1\""
      // Except that EP-Launch.exe doesn't behave like most programs.
      // It does its internal escaping/considers whatever is passed as a single argument.
      // eg: `C:\EnergyPlusV9-2-0\EP-Launch.exe 5ZoneFPIU - Copy.idf`
      // successfully loads a file named ``5ZoneFPIU - Copy.idf
      // RegisterFileType, extension, command, description of filetype, contentType, icon
      // Note: Here you do **not** want to specify the specific iconID, it won't work
      //  eg: var iconId = 1; icon = "@TargetDir@\\EP-Launch.exe," + iconId
      // EP-Launch.exe
      component.addElevatedOperation("RegisterFileType", "idf", "@TargetDir@\\EP-Launch.exe %1", "EnergyPlus Input Data File", "text/plain");
      component.addElevatedOperation("RegisterFileType", "imf", "@TargetDir@\\EP-Launch.exe %1", "EnergyPlus Input Macro File", "text/plain");
      component.addElevatedOperation("RegisterFileType", "epg", "@TargetDir@\\EP-Launch.exe %1", "EnergyPlus Group File", "text/plain");
      // IDFEditor.exe
      component.addElevatedOperation("RegisterFileType", "ddy", "@TargetDir@\\IDFEditor.exe %1", "EnergyPlus Location and Design Day Data", "text/plain");
      component.addElevatedOperation("RegisterFileType", "expidf", "@TargetDir@\\IDFEditor.exe %1", "EnergyPlus Expand Objects Input Data File", "text/plain");

      // We also need to remove the registry entries that the previous
      // installers based on NSIS were doing, since there was no clean up back
      // then and they are getting in the way of our new RegisterFileType
      // (icons not showing on IDF, IMF etc)

      // The old stuff also installed things in "HKCR\.idf" etc, but I'm wary
      // of touching these since they might have been set to something else
      // manually like a text editor for eg
      var reg = installer.environmentVariable("SystemRoot") + "\\System32\\reg.exe";

      var keyNamesToDelete = [
        "EP-Launch.epg", "EP-Launch.idf", "EP-Launch.imf",
        "IDFEditor.ddy", "IDFEditor.expidf"
      ];

      for (i = 0; i < keyNamesToDelete.length; i++) {
        var keyName = "HKEY_CLASSES_ROOT\\" + keyNamesToDelete[i];
        // Delete the entry, silently (/f)
        component.addOperation("Execute", reg, "DELETE", keyName, "/f");
      }
    }
  }
}
