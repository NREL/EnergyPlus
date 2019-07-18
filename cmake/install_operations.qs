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
      component.addElevatedOperation("CreateShortcut", "@TargetDir@/Documentation/index.html", "@StartMenuDir@/EnergyPlus Documentation.lnk")
      component.addElevatedOperation("CreateShortcut", "@TargetDir@/PostProcess/EP-Compare/EP-Compare.exe", "@StartMenuDir@/EP-Compare.lnk")
      component.addElevatedOperation("CreateShortcut", "@TargetDir@/PreProcess/EPDraw/EPDrawGUI.exe", "@StartMenuDir@/EPDrawGUI.lnk")
      component.addElevatedOperation("CreateShortcut", "@TargetDir@/EP-Launch.exe", "@StartMenuDir@/EP-Launch.lnk")
      component.addElevatedOperation("CreateShortcut", "@TargetDir@/ExampleFiles/ExampleFiles.html", "@StartMenuDir@/Example Files Summary.lnk")
      component.addElevatedOperation("CreateShortcut", "@TargetDir@/ExampleFiles/ExampleFiles-ObjectsLink.html", "@StartMenuDir@/ExampleFiles Link to Objects.lnk")
      component.addElevatedOperation("CreateShortcut", "@TargetDir@/PreProcess/IDFEditor/IDFEditor.exe", "@StartMenuDir@/IDFEditor.lnk")
      component.addElevatedOperation("CreateShortcut", "@TargetDir@/PreProcess/IDFVersionUpdater/IDFVersionUpdater.exe", "@StartMenuDir@/IDFVersionUpdater.lnk")
      component.addElevatedOperation("CreateShortcut", "@TargetDir@/readme.html", "@StartMenuDir@/Readme Notes.lnk")
      component.addElevatedOperation("CreateShortcut", "@TargetDir@/PreProcess/WeatherConverter/Weather.exe", "@StartMenuDir@/Weather Statistics and Conversions.lnk")


      // Associate file types
      // TODO: should I Add a "Associate Files" checkbox to the installer options?
      // "Associate *.idf, *.imf, and *.epg files with EP-Launch"

      // Note JM: you have to quote the %1 which represents the file path, otherwise any space in the path will think there are multiple args
      component.addElevatedOperation("RegisterFileType", "idf", "@TargetDir@/EP-Launch.exe \"%1\"", "EnergyPlus Input Data File", "text/plain", "@TargetDir@/EP-Launch.exe,1");
      component.addElevatedOperation("RegisterFileType", "imf", "@TargetDir@/EP-Launch.exe \"%1\"", "EnergyPlus Input Macro File", "text/plain", "@TargetDir@/EP-Launch.exe,1");
      component.addElevatedOperation("RegisterFileType", "epg", "@TargetDir@/EP-Launch.exe \"%1\"", "EnergyPlus Group File", "text/plain", "@TargetDir@/EP-Launch.exe,1");

      component.addElevatedOperation("RegisterFileType", "ddy", "@TargetDir@/IDFEditor.exe \"%1\"", "EnergyPlus Location and Design Day Data", "text/plain", "@TargetDir@/IDFEditor.exe,1");
      component.addElevatedOperation("RegisterFileType", "expidf", "@TargetDir@/IDFEditor.exe \"%1\"", "EnergyPlus Expand Objects Input Data File", "text/plain", "@TargetDir@/IDFEditor.exe,1");

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
      // For some reason I can't make it work, I end up with extra quotes in there...
      var valueName = "AutoCheck";
      var data = "True";
      // Thinks everything is a KeyName = Computer\HKEY_CURRENT_USER\Software\VB and VBA Program Settings\EP-Launch\UpdateCheck" /v AutoCheck /d "True" /f
      //component.addElevatedOperation("Execute", reg, "ADD", [keyName, "/v", valueName, "/d", data, "/f"].join(" "),
      //                               "UNDOEXECUTE", reg, "DELETE", [keyName, "/v", valueName, "/f"].join(" "));
      // fails
      //var keyName = "HKEY_CURRENT_USER\\Software\\VB and VBA Program Settings\\EP-Launch\\UpdateCheck";

      //component.addElevatedOperation("Execute", [reg, "ADD", keyName, "/v", valueName, "/d", data, "/f"].join(" "),
      //                               "UNDOEXECUTE", [reg, "DELETE", keyName, "/v", valueName, "/f"].join(" "));
      component.addElevatedOperation("Execute", reg, "ADD", keyName, "/v", valueName, "/d", data, "/f",
                                     "UNDOEXECUTE", reg, "DELETE", keyName, "/v", valueName, "/f");

      var valueName = "CheckURL";
      var data = "http://nrel.github.io/EnergyPlus/epupdate.htm";
      // fails
      //var data = "\"http://nrel.github.io/EnergyPlus/epupdate.htm\"";
      //var keyName = "\"HKEY_CURRENT_USER\\Software\\VB and VBA Program Settings\\EP-Launch\\UpdateCheck\"";

      //component.addElevatedOperation("Execute", "cmd", "/C", [reg, "ADD", keyName, "/v", valueName, "/d", data, "/f"].join(" "),
      //                               "UNDOEXECUTE", "cmd", "/C", [reg, "DELETE", keyName, "/v", valueName, "/f"].join(" "));
      component.addElevatedOperation("Execute", reg, "ADD", keyName, "/v", valueName, "/d", data, "/f",
                                     "UNDOEXECUTE", reg, "DELETE", keyName, "/v", valueName, "/f");

      // fails
      //component.addElevatedOperation("Execute", reg, ["ADD", keyName, "/v", valueName, "/d", data, "/f"].join(" "),
      //                               "UNDOEXECUTE", reg, ["DELETE", keyName, "/v", valueName, "/f"].join(" "));

      var valueName = "LastAnchor";
      var data = "#@Version@";

      // This creates the value and data fine, but the KeyName has an extra trailing quote:
      // var keyName = "\"HKEY_CURRENT_USER\\Software\\VB and VBA Program Settings\\EP-Launch\\UpdateCheck\"";
      //component.addElevatedOperation("Execute", reg, "ADD", keyName, "/v", valueName, "/d", data, "/f",
      //                               "UNDOEXECUTE", reg, "DELETE", keyName, "/v", valueName, "/f");

      var keyName = "HKEY_CURRENT_USER\\Software\\VB and VBA Program Settings\\EP-Launch\\UpdateCheck";
      component.addElevatedOperation("Execute", reg, "ADD", keyName, "/v", valueName, "/d", data, "/f",
                                     "UNDOEXECUTE", reg, "DELETE", keyName, "/v", valueName, "/f");

      // And weirder still, to copy and register DLLs
      var systemArray = ["MSCOMCTL.OCX", "ComDlg32.OCX", "Msvcrtd.dll", "Dforrt.dll", "Gswdll32.dll", "Gsw32.exe", "Graph32.ocx", "MSINET.OCX", "Vsflex7L.ocx", "Msflxgrd.ocx"];
      var systemTargetDir = installer.environmentVariable("WINDIR");
      if( systemInfo.currentCpuArchitecture == "x86_64") {
        systemTargetDir += "/SysWOW64/";
      } else {
        systemTargetDir += "/System32/";
      }
      for (i = 0; i < systemArray.length; i++) {
        var sourceFile = "@TargetDir@/temp/" + systemArray[i];
        var targetFile = systemTargetDir + systemArray[i];
        if (!installer.fileExists(targetFile)) {
          component.addElevatedOperation("Copy", sourceFile, targetFile);
        }
      }
      // Delete this temp directory: TODO use execute to avoid uninstall create
      // the opposite (= Mkdir)
      component.addElevatedOperation("Rmdir", "@TargetDir@/temp/");

    }
  }
}








