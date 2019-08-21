// Regular install commands to be performed
function Component()
{
  Component.prototype.createOperations = function()
  {
    // call default implementation
    component.createOperations();

    // ... add custom operations

    var kernel = systemInfo.kernelType;
    if( kernel == "darwin" ) {

      // Chmod +x for apps
      component.addOperation("Execute", "chmod", "+x", "@TargetDir@/PreProcess/IDFVersionUpdater/IDFVersionUpdater.app/Contents/MacOS/IDFVersionUpdater");
      component.addOperation("Execute", "chmod", "+x", "@TargetDir@/PreProcess/EP-Launch-Lite.app/Contents/MacOS/EP-Launch-Lite");
      component.addOperation("Execute", "chmod", "+x", "@TargetDir@/PreProcess/EP-Launch-Lite.app/Contents/MacOS/python");
      component.addOperation("Execute", "chmod", "+x", "@TargetDir@/PostProcess/EP-Compare/EP-Compare.app/Contents/MacOS/EP-Compare");

      // Not sure necessary so not doing it yet
      // component.addOperation("Execute", "chmod", "-R", "a+w", "@TargetDir@");
      // component.addOperation("Execute", "chmod", "-R", "a+w", "@TargetDir@/PreProcess/IDFVersionUpdater/IDFVersionUpdater.app/Contents/MacOS/");
      // component.addOperation("Execute", "chmod", "-R", "a+w", "@TargetDir@/PreProcess/IDFVersionUpdater/");

    }

    // On Windows
    if( kernel == "winnt" ) {

      // Create Shortcuts in the Windows Start Menu
      component.addOperation("CreateShortcut", "@TargetDir@/Documentation/index.html", "@StartMenuDir@/EnergyPlus Documentation.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/PostProcess/EP-Compare/EP-Compare.exe", "@StartMenuDir@/EP-Compare.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/PreProcess/EPDraw/EPDrawGUI.exe", "@StartMenuDir@/EPDrawGUI.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/EP-Launch.exe", "@StartMenuDir@/EP-Launch.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/ExampleFiles/ExampleFiles.html", "@StartMenuDir@/Example Files Summary.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/ExampleFiles/ExampleFiles-ObjectsLink.html", "@StartMenuDir@/ExampleFiles Link to Objects.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/PreProcess/IDFEditor/IDFEditor.exe", "@StartMenuDir@/IDFEditor.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/PreProcess/IDFVersionUpdater/IDFVersionUpdater.exe", "@StartMenuDir@/IDFVersionUpdater.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/readme.html", "@StartMenuDir@/Readme Notes.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/PreProcess/WeatherConverter/Weather.exe", "@StartMenuDir@/Weather Statistics and Conversions.lnk");


      // Note: Associate file types: done separately (optional)

      // Here's what stuff gets weird. We need to write stuff to the registry apparently for EP-Launch
      // In the registry under KEY_CURRENT_USER\Software\VB and VBA Program Settings\EP-Launch\UpdateCheck:
      // Name,       Type,   Data
      // AutoCheck,  REG_SZ, True
      // CheckURL,   REG_SZ, http://nrel.github.io/EnergyPlus/epupdate.htm
      // LastAnchor, REG_SZ, #9.1.0-mqjdfsiojf

      // REG ADD KeyName /v ValueName, /d Data, /f = force overwrite
      var reg = installer.environmentVariable("SystemRoot") + "\\System32\\reg.exe";

      var keyName = "HKEY_CURRENT_USER\\Software\\VB and VBA Program Settings\\EP-Launch\\UpdateCheck";

      // Note by passing arguments separately to the (reg) command in addElevatedOperation (supports up to 10 args),
      // Qt will escape each argument, so do not include double quote (eg: "\"AutoCheck\"") or they'll be interpreted literally
      // Mind the "/f" flag which avoids displaying a [Yes/No] prompt that you
      // can't answer and making the installer freeze
      var valueName = "AutoCheck";
      var data = "True";
      component.addOperation("Execute", reg, "ADD", keyName, "/v", valueName, "/d", data, "/f");

      var valueName = "CheckURL";
      var data = "http://nrel.github.io/EnergyPlus/epupdate.htm";
      component.addOperation("Execute", reg, "ADD", keyName, "/v", valueName, "/d", data, "/f");

      var valueName = "LastAnchor";
      var data = "#@Version@";
      component.addOperation("Execute", reg, "ADD", keyName, "/v", valueName, "/d", data, "/f");

      // Delete the entire keyName upon uninstallation
      var keyName = "HKEY_CURRENT_USER\\Software\\VB and VBA Program Settings\\EP-Launch";
      component.addOperation("Execute", "cmd", "/C", "echo Set up uninstall operation to delete EP-Launch registry keys", "UNDOEXECUTE", reg, "DELETE", keyName, "/f");

      // And weirder still, to copy and register DLLs: done separately
    }
  }
}








